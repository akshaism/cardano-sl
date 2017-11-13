{-# LANGUAGE RecursiveDo #-}

module Lang.Parser
       ( parse
       , ParseError(..)
       ) where

import           Universum

import           Control.Applicative.Combinators.NonEmpty (sepBy1)
import           Control.Lens (Getting)
import           Data.Monoid (First)
import           Text.Earley (Grammar, Parser, Prod, Report, fullParses, parser, rule, terminal,
                              (<?>))

import           Lang.Lexer (BracketSide, Token, getFilePath', tokenize, _BracketSideClosing,
                             _BracketSideOpening, _TokenAddress, _TokenBlockVersion, _TokenFilePath,
                             _TokenHash, _TokenKey, _TokenName, _TokenNumber, _TokenParenthesis,
                             _TokenPublicKey, _TokenSemicolon, _TokenSoftwareVersion,
                             _TokenStakeholderId, _TokenString)
import           Lang.Syntax (Arg (..), Expr (..), Lit (..), ProcCall (..))

tok :: Getting (First a) Token a -> Prod r e Token a
tok p = terminal (preview p)

inBrackets
    :: Getting (First ()) Token BracketSide
    -> Prod r e Token a
    -> Prod r e Token a
inBrackets p r =
    tok (p . _BracketSideOpening) *> r <* tok (p . _BracketSideClosing)

gExpr :: Grammar r (Prod r Text Token Expr)
gExpr = mdo
    ntName <- rule $ tok _TokenName
    ntKey <- rule $ tok _TokenKey
    ntExprLit <- rule $ ExprLit <$> asum
        [ LitNumber <$> tok _TokenNumber
        , LitString <$> tok _TokenString
        , LitAddress <$> tok _TokenAddress
        , LitPublicKey <$> tok _TokenPublicKey
        , LitStakeholderId <$> tok _TokenStakeholderId
        , LitHash <$> tok _TokenHash
        , LitBlockVersion <$> tok _TokenBlockVersion
        , LitSoftwareVersion <$> tok _TokenSoftwareVersion
        , LitFilePath . getFilePath' <$> tok _TokenFilePath
        ] <?> "literal"
    ntArg <- rule $ asum
        [ ArgKw <$> ntKey <*> ntExprAtom
        , ArgPos <$> ntExprAtom
        ] <?> "argument"
    ntExpr1 <- rule $ asum
        [ ExprProcCall <$> ntProcCall
        , ntExprAtom
        , pure ExprUnit
        ] <?> "expression"
    ntExpr <- rule $ ExprGroup <$> ntExpr1 `sepBy1` tok _TokenSemicolon
    ntProcCall <- rule $ ProcCall <$> ntName <*> some ntArg <?> "procedure call"
    ntProcCall0 <- rule $
        (\name -> ExprProcCall $ ProcCall name []) <$> ntName
        <?> "procedure call w/o arguments"
    ntExprAtom <- rule $ asum
        [ ntExprLit
        , ntProcCall0
        , inBrackets _TokenParenthesis ntExpr <?> "parenthesized expression"
        ] <?> "atom"
    return ntExpr

pExpr :: Parser Text [Token] Expr
pExpr = parser gExpr

newtype ParseError = ParseError (Report Text [Token])
    deriving (Eq, Show)

instance Exception ParseError

parse :: Text -> Either ParseError Expr
parse = first ParseError . toEither . fullParses pExpr . tokenize
  where
    toEither = \case
      ([] , r) -> Left r
      (a:_, _) -> Right a
