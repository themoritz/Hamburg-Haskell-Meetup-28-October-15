{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Monad.Identity

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Token    as Tok
import qualified Text.Parsec.Language as Tok
import qualified Text.Parsec.Expr     as Ex

import qualified Exists
import qualified Reducer

-- Untyped Expressions

data Op
  = Add
  | And
  | Equal

instance Show Op where
  show Add = "+"
  show And = "and"
  show Equal = "=="

data UExpr
  = UNumber Integer
  | UBinop Op UExpr UExpr
  deriving (Show)

-- Lexer

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where
    style = Tok.emptyDef
      { Tok.reservedOpNames = ["+", "and", "=="]
      }

natural :: Parser Integer
natural = Tok.natural lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

-- Parser

type Operator a = Ex.Operator String () Identity a

binary :: Op -> Operator UExpr
binary op = Ex.Infix (reservedOp (show op) >> return (UBinop op)) Ex.AssocLeft

expression :: Parser UExpr
expression = Ex.buildExpressionParser opTable terms <?> "expression"
  where
    opTable =
      [ [ binary Add   ]
      , [ binary Equal ]
      , [ binary And   ]
      ]
    terms =
          try (UNumber <$> natural)
      <|> try (parens expression)
      <?> "simple expression"

parseExpression :: String -> Either ParseError UExpr
parseExpression s = parse (whiteSpace *> expression <* eof) "" s

-- Typed Expressions

data TExpr a where
  TNumber :: Integer -> TExpr Integer
  TAdd :: TExpr Integer -> TExpr Integer -> TExpr Integer
  TAnd :: TExpr Bool -> TExpr Bool -> TExpr Bool
  TEqual :: TExpr Integer -> TExpr Integer -> TExpr Bool

deriving instance Show (TExpr a)

data TType a where
  TypeInt :: TType Integer
  TypeBool :: TType Bool

deriving instance Show (TType a)

data ATExpr
  = forall a. (Show a) => TExpr a ::: TType a

deriving instance Show ATExpr

-- Typechecker

typecheckExpression :: UExpr -> Either String ATExpr
typecheckExpression (UNumber x) = Right $ TNumber x ::: TypeInt
typecheckExpression (UBinop op lhs rhs) = do
  (leftChecked ::: leftType) <- typecheckExpression lhs
  (rightChecked ::: rightType) <- typecheckExpression rhs
  case (op, leftType, rightType) of
    (Add,   TypeInt,  TypeInt)  -> Right $ TAdd   leftChecked rightChecked ::: TypeInt
    (And,   TypeBool, TypeBool) -> Right $ TAnd   leftChecked rightChecked ::: TypeBool
    (Equal, TypeInt,  TypeInt)  -> Right $ TEqual leftChecked rightChecked ::: TypeBool
    _ -> Left $ "Type error! Expressions `" ++ show leftChecked ++ "` and `" ++ show rightChecked ++ "` have wrong types for operator `" ++ show op ++ "`: `" ++ show leftType ++ "`, `" ++ show rightType ++ "`."

-- Eval


eval :: TExpr a -> a
eval (TNumber x)      = x
eval (TAdd lhs rhs)   = eval lhs + eval rhs
eval (TAnd lhs rhs)   = eval lhs && eval rhs
eval (TEqual lhs rhs) = eval lhs == eval rhs

-- Alternative: Eval the untyped expression

evalUInt :: UExpr -> Either String Integer
evalUInt (UNumber x) = Right x
evalUInt (UBinop op lhs rhs) = case op of
  Add -> (+) <$> evalUInt lhs <*> evalUInt rhs
  _ -> Left "type error"

evalUBool :: UExpr -> Either String Bool
evalUBool (UNumber _) = Left "type error"
evalUBool (UBinop op lhs rhs) = case op of
  And -> (&&) <$> evalUBool lhs <*> evalUBool rhs
  Equal -> (==) <$> evalUInt lhs <*> evalUInt rhs
  _ -> Left "type error"

-- Main

main :: IO ()
main = do
  Exists.test
  putStrLn ""
  Reducer.test
  putStrLn ""
  case parseExpression "3 == 3 and 2 == 1 + 1" of
    Left err -> print err
    Right expr -> do
      print expr
      print $ evalUBool expr
      case typecheckExpression expr of
        Left err -> putStrLn err
        Right (checkedExpr ::: _) -> do
          print checkedExpr
          print $ eval checkedExpr
