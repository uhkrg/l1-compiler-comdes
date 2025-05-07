module Compile.InstrSel
  ( codeGen,
  )
where

import Compile.AST (AST (..), Expr (..), Op (..), Stmt (..))
import Compile.Instr (Argument (..), Arguments (..), Instruction (..), Register (..))
import Control.Monad.State
import Data.Functor ((<$>))
import Data.Map as Map
import Data.Maybe (fromJust)

type VarMap = Map.Map String Integer

type CodeGen = State CodeGenState

data CodeGenState = CodeGenState
  { varMap :: VarMap,
    nextVarId :: Integer,
    code :: [Instruction]
  }

codeGen :: AST -> [Instruction]
codeGen (Block stmts _) = code $ execState (genBlock stmts) initialState
  where
    initialState = CodeGenState Map.empty 0 []

nextVar :: CodeGen Integer
nextVar = do
  curr <- get
  let r = nextVarId curr
  put curr {nextVarId = r + 1}
  return r

lookupVar :: String -> CodeGen Integer
lookupVar var = gets (fromJust . Map.lookup var . varMap)

assignVar :: String -> Integer -> CodeGen ()
assignVar name var = modify $ \s -> s {varMap = Map.insert name var $ varMap s}

emit :: Instruction -> CodeGen ()
emit instr = modify $ \s -> s {code = code s ++ [instr]}

genBlock :: [Stmt] -> CodeGen ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> CodeGen ()
genStmt (Decl var _) = pure ()
genStmt (Init var expr _) = do
  res <- genExpr expr
  assignedVar <- nextVar
  assignVar var assignedVar
  emit Instruction {instr = "MOV", args = Arguments {source = res, target = ArgumentVariable assignedVar}}
genStmt (Asgn var asgnOp expr _) = do
  res <- genExpr expr
  oldVar <- lookupVar var
  assignedVar <- nextVar
  assignVar var assignedVar
  let varArg = ArgumentVariable assignedVar
  case asgnOp of
    Nothing -> emit Instruction {instr = "MOV", args = Arguments {source = res, target = varArg}}
    Just op -> emit Instruction {instr = "MOV", args = Arguments {source = ArgumentVariable oldVar, target = varArg}} >> genOp op res varArg
genStmt (Ret expr _) = do
  res <- genExpr expr
  emit Instruction {instr = "MOV", args = Arguments {source = res, target = ArgumentRegister RegEAX}}
  emit Instruction {instr = "RET", args = EmptyArgument}

genExpr :: Expr -> CodeGen Argument
genExpr (IntExpr value _) = pure . ArgumentConstant . show $ value
genExpr (Ident var _) = ArgumentVariable <$> lookupVar var
genExpr (UnExpr op expr) = do
  arg <- genExpr expr
  res <- ArgumentVariable <$> nextVar
  case op of
    Neg ->
      emit Instruction {instr = "MOV", args = Arguments {source = arg, target = res}}
        >> emit Instruction {instr = "NEG", args = SingleArgument res}
    _ -> error "Unreachable, neagtion is the only unop"
  return res
genExpr (BinExpr op expr1 expr2) = do
  arg1 <- genExpr expr1
  arg2 <- genExpr expr2
  res <- ArgumentVariable <$> nextVar
  emit Instruction {instr = "MOV", args = Arguments {source = arg1, target = res}}
  genOp op arg2 res
  return res

genOp :: Op -> Argument -> Argument -> CodeGen ()
genOp op source target = emit Instruction {instr = asmOp, args = Arguments {source = source, target = target}}
  where
    asmOp = case op of
      Mul -> "MUL"
      Add -> "ADD"
      Sub -> "SUB"
      Div -> "DIV"
      Mod -> "MOD"
      x -> error $ "Unreachable \"" ++ show x ++ "\" is not a binOp"
