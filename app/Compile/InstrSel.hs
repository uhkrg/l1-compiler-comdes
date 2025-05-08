module Compile.InstrSel
  ( codeGen,
  )
where

import Compile.AST (AST (..), Expr (..), Op (..), Stmt (..))
import Compile.Instr (Argument (..), Arguments (..), Instruction (..), Register (..))
import Control.Monad.State
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
emit inst = modify $ \s -> s {code = code s ++ [inst]}

genBlock :: [Stmt] -> CodeGen ()
genBlock = mapM_ genStmt

genStmt :: Stmt -> CodeGen ()
genStmt (Decl _var _) = pure ()
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
    Just Div -> genDiv Div (ArgumentVariable oldVar) res varArg
    Just Mod -> genDiv Mod (ArgumentVariable oldVar) res varArg
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
  case op of
    Div -> genDiv Div arg1 arg2 res
    Mod -> genDiv Mod arg1 arg2 res
    _ -> emit Instruction {instr = "MOV", args = Arguments {source = arg1, target = res}} >> genOp op arg2 res
  return res

genDiv :: Op -> Argument -> Argument -> Argument -> CodeGen ()
genDiv op dividend divisor targ = do
  emit Instruction {instr = "MOV", args = Arguments {source = dividend, target = ArgumentRegister RegEAX}}
  divisorVar <- case divisor of
    ArgumentConstant _ -> ArgumentVariable <$> nextVar
    x -> pure x
  case divisor of
    ArgumentConstant c -> emit Instruction {instr = "MOV", args = Arguments {source = divisor, target = divisorVar}}
  emit Instruction {instr = "CDQ", args = EmptyArgument}
  emit Instruction {instr = "IDIV", args = SingleArgument divisorVar}
  emit Instruction {instr = "MOV", args = Arguments {source = ArgumentRegister ret, target = targ}}
  where
    ret = case op of
      Div -> RegEAX
      Mod -> RegEDX
      x -> error $ "Unreachable \"" ++ show x ++ "\" is not a supported division op"

genOp :: Op -> Argument -> Argument -> CodeGen ()
genOp op sourc targ = emit Instruction {instr = asmOp, args = Arguments {source = sourc, target = targ}}
  where
    asmOp = case op of
      Mul -> "MUL"
      Add -> "ADD"
      Sub -> "SUB"
      x -> error $ "Unreachable \"" ++ show x ++ "\" is not supported op"
