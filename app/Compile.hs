module Compile
  ( Job (..),
    compile,
  )
where

import Compile.Asm (genAsm)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Compile.IR (translateAST)
import Compile.VarAlloc (allocVars)
import Control.Monad.IO.Class
import Error (L1ExceptT)
import System.Process (readProcess)

data Job = Job
  { src :: FilePath,
    out :: FilePath
  }
  deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  semanticAnalysis ast
  let ir = translateAST ast
  let locs = allocVars ir
  let code = genAsm locs ir
  liftIO $ putStr $ unlines code
  _ <- liftIO $ readProcess "gcc" ["-x", "assembler", "-nostdlib", "-o", out job, "-"] (unlines code)
  return ()
