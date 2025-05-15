module Compile
  ( Job (..),
    compile,
  )
where

import Compile.Asm (genAsm)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Compile.IR (translateAST)
import Compile.Unreachable (removeUnreachable)
import Compile.VarAlloc (allocVars)
import Control.Monad.IO.Class
import Error (L1ExceptT)
import System.Process (readProcess)
import Data.Array (elems)
--import Compile.LocalPredicates (readLocal)
--import Compile.Liveness (calcLiveness)
--import Optimization.Neededness (calcNeededness)
import Optimization.DeadCode (removeDeadCode)

data Job = Job
  { src :: FilePath,
    out :: FilePath
  }
  deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  semanticAnalysis ast
  let ir' = translateAST ast
  liftIO $ putStrLn $ unlines $ map show $ elems ir'
  let ir'' = removeUnreachable ir'
  --liftIO $ putStrLn $ unlines $ map show $ elems ir''
  --liftIO $ print $ readLocal ir''
  --liftIO $ print $ calcNeededness ir''
  let ir = removeDeadCode ir''
  liftIO $ putStrLn $ unlines $ map show $ elems ir
  --liftIO $ print $ calcLiveness ir
  let locs = allocVars ir
  --liftIO $ print locs
  let code = genAsm locs ir
  liftIO $ putStr $ unlines code
  _ <- liftIO $ readProcess "gcc" ["-x", "assembler", "-nostdlib", "-o", out job, "-"] (unlines code)
  return ()
