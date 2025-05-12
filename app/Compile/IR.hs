module Compile.IR (IR(..), IRStatement(..), ValueContainer(..), Var(..), Op(..), UnOp(..), translateAST) where

import Data.Array
import qualified Compile.AST as AST
import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type IR = Array Integer IRStatement
data IRStatement = Ret ValueContainer | Var :|<-: ValueContainer | Var :<-: (ValueContainer, Op, ValueContainer) | Var :<-^: (UnOp, ValueContainer)

data ValueContainer = Variable Var | Constant Integer

newtype Var = Var Integer

data Op = Add | Sub | Mul | Div | Mod
data UnOp = Neg

type IRGen = State IRGenState

data IRGenState = IRGenState {
    varMap :: Map.Map String Integer,
    nextVarId :: Integer,
    code :: [IRStatement]
}

nextVar :: IRGen Integer
nextVar = do
    curr <- get
    let r = nextVarId curr
    put curr {nextVarId = r + 1}
    return r

lookupVar :: String -> IRGen Integer
lookupVar var = gets (fromJust . Map.lookup var . varMap)

assignVar :: String -> Integer -> IRGen ()
assignVar name var = modify $ \s -> s {varMap = Map.insert name var $ varMap s}

emit :: IRStatement -> IRGen ()
emit inst = modify $ \s -> s {code = code s ++ [inst]}

translateAST :: AST.AST -> IR
translateAST (AST.Block stmts _) = listArray (1, toInteger $ length ir) ir
    where 
        ir = code $ execState (translateBlock stmts) initialState
        initialState = IRGenState Map.empty 0 []

translateBlock :: [AST.Stmt] -> IRGen ()
translateBlock = mapM_ translateStmt


translateStmt :: AST.Stmt -> IRGen ()

translateStmt (AST.Decl _var _) = pure ()

translateStmt (AST.Init var e _) = do
    v <- nextVar
    case e of
        AST.IntExpr val _ -> emit $ Var v :|<-: Constant (read val)
        AST.Ident v2 _ -> do
            v2' <- lookupVar v2
            emit $ Var v :|<-: Variable (Var v2')
        e' -> translateExpr v e'
    assignVar var v

translateStmt (AST.Asgn var Nothing e _) = do
    v <- nextVar
    case e of
        AST.IntExpr val _ -> emit $ Var v :|<-: Constant (read val)
        AST.Ident v2 _ -> do
            v2' <- lookupVar v2
            emit $ Var v :|<-: Variable (Var v2')
        e' -> translateExpr v e'
    assignVar var v

translateStmt (AST.Asgn var (Just op) e _) = do
    v <- lookupVar var
    vNew <- nextVar
    case e of
        AST.IntExpr val _ -> emit $ Var vNew :<-: (Variable $ Var v, translateBinOp op, Constant $ read val)
        AST.Ident v2 _ -> do
            v2' <- lookupVar v2
            emit $ Var vNew :<-: (Variable $ Var v, translateBinOp op, Variable $ Var v2')
        e' -> do
            res <- nextVar
            translateExpr res e'
            emit $ Var vNew :<-: (Variable $ Var v, translateBinOp op, Variable $ Var res)
    assignVar var vNew

translateStmt (AST.Ret e _) = do
    case e of
        AST.IntExpr val _ -> emit $ Ret $ Constant $ read val
        AST.Ident v _ -> do
            v' <- lookupVar v
            emit $ Ret $ Variable $ Var v'
        e' -> do
            res <- nextVar
            translateExpr res e'
            emit $ Ret $ Variable $ Var res


translateExpr :: Integer -> AST.Expr -> IRGen ()

translateExpr _ (AST.IntExpr _ _) = error "We should not translate constants directly"
translateExpr _ (AST.Ident _ _) = error "We should not translate identifiers directly"

translateExpr target (AST.UnExpr op e) = 
    case e of
        AST.IntExpr val _ -> emit $ Var target :<-^: (translateUnOp op, Constant $ read val)
        AST.Ident v _ -> do
            v' <- lookupVar v
            emit $ Var target :<-^: (translateUnOp op, Variable $ Var v')
        e' -> do
            t <- nextVar
            translateExpr t e'
            emit $ Var target :<-^: (translateUnOp op, Variable $ Var t)

translateExpr target (AST.BinExpr op e1 e2) = do
    r1 <- case e1 of
        AST.IntExpr val _ -> pure $ Constant $ read val
        AST.Ident v _ -> Variable . Var <$> lookupVar v
        e -> do
            t <- nextVar
            translateExpr t e
            return $ Variable $ Var t
    r2 <- case e2 of
        AST.IntExpr val _ -> pure $ Constant $ read val
        AST.Ident v _ -> Variable . Var <$> lookupVar v
        e -> do
            t <- nextVar
            translateExpr t e
            return $ Variable $ Var t
    emit $ Var target :<-: (r1, translateBinOp op, r2)


translateBinOp :: AST.Op -> Op
translateBinOp op = case op of
    AST.Add -> Add
    AST.Sub -> Sub
    AST.Mul -> Mul
    AST.Div -> Div
    AST.Mod -> Mod
    o -> error $ show o ++ " is not a bin op"

translateUnOp :: AST.Op -> UnOp
translateUnOp op = case op of
    AST.Neg -> Neg
    o -> error $ show o ++ " is not a un op"


instance Show IRStatement where
    show (Ret v) = "return " ++ show v
    show ((:|<-:) v v2) = show v ++ " <- " ++ show v2
    show ((:<-:) v (v2, op, v3)) = show v ++ " <- " ++ show v2 ++ " " ++  show op ++ " " ++ show v3
    show ((:<-^:) v (op, v2)) = show v ++ " <- " ++ show op ++ show v2

instance Show ValueContainer where
    show (Variable v) = show v
    show (Constant v) = show v

instance Show Var where
    show (Var v) = 'v' : show v

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Mod = "%"

instance Show UnOp where
    show Neg = "-"
