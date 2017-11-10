{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Interface to Blocks DB.

module Pos.DB.Block
       (
         deleteBlock

       , prepareBlockDB

       -- * Pure implementation
       , dbGetBlockPureDefault
       , dbGetUndoPureDefault
       , dbPutBlundPureDefault

       -- * Rocks implementation
       , dbGetBlockRealDefault
       , dbGetUndoRealDefault
       , dbPutBlundRealDefault

       -- * DBSum implementation
       , dbGetBlockSumDefault
       , dbGetUndoSumDefault
       , dbPutBlundSumDefault
       ) where

import           Universum

import           Control.Exception.Safe (handle)
import           Control.Lens (at)
import qualified Data.ByteString as BS (hPut, readFile)
import           Data.Default (Default (def))
import           Ether.Internal (HasLens (..))
import           Formatting (build, formatToString, sformat, (%))
import           System.Directory (createDirectoryIfMissing, removeFile)
import           System.FilePath ((</>))
import           System.IO (IOMode (WriteMode), hClose, hFlush, openBinaryFile)
import           System.IO.Error (IOError, isDoesNotExistError)

import           Pos.Binary.Class (Bi, decodeFull, serialize')
import           Pos.Binary.Core.Block ()
import           Pos.Block.BHelpers ()
import           Pos.Core (HasConfiguration, HeaderHash, headerHash)
import           Pos.Core.Block (Block, Blund, GenesisBlock, SlogUndo (..), Undo (..))
import qualified Pos.Core.Block as CB
import           Pos.Crypto (hashHexF)
import           Pos.DB.Class (MonadDB (..), MonadDBRead)
import           Pos.DB.Error (DBError (..))
import           Pos.DB.Functions (dbSerializeValue)
import           Pos.DB.GState.Common (blockIndexKey)
import           Pos.DB.Pure (DBPureVar, MonadPureDB, atomicModifyIORefPure, pureBlockIndexDB,
                              pureBlocksStorage)
import           Pos.DB.Rocks (MonadRealDB, blockDataDir, getBlockIndexDB, getNodeDBs, rocksDelete,
                               rocksPutBi)
import           Pos.DB.Sum (MonadDBSum, eitherDB)
import           Pos.Delegation.Types (DlgUndo (..))

----------------------------------------------------------------------------
-- Implementations for 'MonadRealDB'
----------------------------------------------------------------------------

-- Get block with given hash from Block DB.  This function has too
-- strict constraint, consider using 'blkGetBlock'.

getBlock
    :: forall ctx m. (HasConfiguration, MonadRealDB ctx m)
    => HeaderHash -> m (Maybe Block)
getBlock = blockDataPath >=> getData

-- Get undo data for block with given hash from Block DB. This
-- function has too strict constraint, consider using 'blkGetUndo'.
getUndo :: (HasConfiguration, MonadRealDB ctx m) => HeaderHash -> m (Maybe Undo)
getUndo = undoDataPath >=> getData

-- Put given block, its metadata and Undo data into Block DB. This
-- function uses 'MonadRealDB' constraint which is too
-- severe. Consider using 'dbPutBlund' instead.
putBlund
    :: (HasConfiguration, MonadRealDB ctx m)
    => Blund -> m ()
putBlund (blk, undo) = do
    let h = headerHash blk
    liftIO . createDirectoryIfMissing False =<< dirDataPath h
    flip putData blk =<< blockDataPath h
    flip putData undo =<< undoDataPath h
    putBi (blockIndexKey h) (CB.getBlockHeader blk)

deleteBlock :: (MonadRealDB ctx m) => HeaderHash -> m ()
deleteBlock hh = do
    delete (blockIndexKey hh)
    deleteData =<< blockDataPath hh
    deleteData =<< undoDataPath hh

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareBlockDB
    :: MonadDB m
    => GenesisBlock -> m ()
prepareBlockDB blk =
    dbPutBlund (Left blk, genesisUndo)
  where
    genesisUndo =
        Undo
        { undoTx = mempty
        , undoDlg = DlgUndo mempty mempty
        , undoUS = def
        , undoSlog = SlogUndo Nothing
        }

----------------------------------------------------------------------------
-- Pure implementation
----------------------------------------------------------------------------

decodeOrFailPureDB
    :: HasConfiguration
    => ByteString
    -> Either Text (Block, Undo)
decodeOrFailPureDB = decodeFull

dbGetBlundPureDefault ::
       (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe (Block, Undo))
dbGetBlundPureDefault h = do
    (blund :: Maybe ByteString) <-
        view (pureBlocksStorage . at h) <$> (view (lensOf @DBPureVar) >>= readIORef)
    case decodeOrFailPureDB <$> blund of
        Nothing        -> pure Nothing
        Just (Left e)  -> throwM (DBMalformed e)
        Just (Right v) -> pure (Just v)

dbGetBlockPureDefault
    :: (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe Block)
dbGetBlockPureDefault h = fmap fst <$> dbGetBlundPureDefault h

dbGetUndoPureDefault
    :: forall ctx m. (HasConfiguration, MonadPureDB ctx m)
    => HeaderHash
    -> m (Maybe Undo)
dbGetUndoPureDefault h = fmap snd <$> dbGetBlundPureDefault @ctx @m h

dbPutBlundPureDefault ::
       forall ctx m. (HasConfiguration, MonadPureDB ctx m)
    => Blund
    -> m ()
dbPutBlundPureDefault (blk,undo) = do
    let h = headerHash blk
    (var :: DBPureVar) <- view (lensOf @DBPureVar)
    flip atomicModifyIORefPure var $
        (pureBlocksStorage . at h .~ Just (serialize'  (blk,undo))) .
        (pureBlockIndexDB . at (blockIndexKey h) .~ Just (dbSerializeValue $ CB.getBlockHeader blk))

----------------------------------------------------------------------------
-- Rocks implementation
----------------------------------------------------------------------------

-- instance MonadBlockDBGeneric Block

type BlockDBGenericEnv ctx m =
    ( MonadDBRead m
    , MonadRealDB ctx m
    , HasConfiguration)

dbGetBlockRealDefault ::
       forall ctx m. (BlockDBGenericEnv ctx m)
    => HeaderHash
    -> m (Maybe Block)
dbGetBlockRealDefault = getBlock

dbGetUndoRealDefault ::
       forall ctx m. BlockDBGenericEnv ctx m
    => HeaderHash
    -> m (Maybe Undo)
dbGetUndoRealDefault = getUndo

dbPutBlundRealDefault :: (HasConfiguration, MonadDBRead m, MonadRealDB ctx m) => Blund -> m ()
dbPutBlundRealDefault = putBlund

----------------------------------------------------------------------------
-- DBSum implementation
----------------------------------------------------------------------------

type DBSumEnv ctx m =
    ( MonadDBRead m
    , MonadDBSum ctx m
    , HasConfiguration
    )

dbGetBlockSumDefault
    :: forall ctx m. (DBSumEnv ctx m)
    => HeaderHash -> m (Maybe Block)
dbGetBlockSumDefault hh = eitherDB (dbGetBlockRealDefault hh) (dbGetBlockPureDefault hh)

dbGetUndoSumDefault
    :: forall ctx m. DBSumEnv ctx m
    => HeaderHash -> m (Maybe Undo)
dbGetUndoSumDefault hh =
    eitherDB (dbGetUndoRealDefault hh) (dbGetUndoPureDefault hh)

dbPutBlundSumDefault
    :: forall ctx m. (DBSumEnv ctx m)
    => Blund -> m ()
dbPutBlundSumDefault b = eitherDB (dbPutBlundRealDefault b) (dbPutBlundPureDefault b)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

putBi
    :: (MonadRealDB ctx m, Bi v)
    => ByteString -> v -> m ()
putBi k v = rocksPutBi k v =<< getBlockIndexDB

delete :: (MonadRealDB ctx m) => ByteString -> m ()
delete k = rocksDelete k =<< getBlockIndexDB

getData ::  forall m v . (MonadIO m, MonadCatch m, Bi v) => FilePath -> m (Maybe v)
getData fp = handle handler $ liftIO $
    either onDecodeError (pure . Just) . decodeFull =<< BS.readFile fp
  where
    onDecodeError :: Text -> IO a
    onDecodeError err =
        throwM $ DBMalformed $ sformat
        ("Couldn't deserialize "%build%", reason: "%build) fp err
    handler :: IOError -> m (Maybe x)
    handler e
        | isDoesNotExistError e = pure Nothing
        | otherwise = throwM e

putData ::  (MonadIO m, Bi v) => FilePath -> v -> m ()
putData fp v = liftIO $
    bracket (openBinaryFile fp WriteMode) hClose $ \h ->
        BS.hPut h (serialize' v) >> hFlush h

deleteData :: (MonadIO m, MonadCatch m) => FilePath -> m ()
deleteData fp = (liftIO $ removeFile fp) `catch` handler
  where
    handler e
        | isDoesNotExistError e = pure ()
        | otherwise = throwM e

dirDataPath :: MonadRealDB ctx m => HeaderHash -> m FilePath
dirDataPath (formatToString hashHexF -> fn) = gitDirDataPath fn

blockDataPath :: MonadRealDB ctx m => HeaderHash -> m FilePath
blockDataPath (formatToString (hashHexF%".block") -> fn) =
    gitDirDataPath fn <&> (</> drop 2 fn)

undoDataPath :: MonadRealDB ctx m => HeaderHash -> m FilePath
undoDataPath (formatToString (hashHexF%".undo") -> fn) =
    gitDirDataPath fn <&> (</> drop 2 fn)

gitDirDataPath :: MonadRealDB ctx m => [Char] -> m FilePath
gitDirDataPath fn = getNodeDBs <&> \dbs -> dbs ^. blockDataDir </> take 2 fn
