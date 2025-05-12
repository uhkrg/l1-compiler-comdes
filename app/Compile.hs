module Compile
  ( Job (..),
    compile,
  )
where

import Compile.Asm (genAsm)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Compile.IR (translateAST)
import Control.Monad.IO.Class
import Error (L1ExceptT)
import Data.Array (elems)

data Job = Job
  { src :: FilePath,
    out :: FilePath
  }
  deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  semanticAnalysis ast
  --liftIO $ putStrLn $ unlines $ map show $ elems $ translateAST ast
  let code = genAsm $ translateAST ast
  liftIO $ putStr $ unlines code
  --_ <- liftIO $ readProcess "gcc" ["-x", "assembler", "-o", out job, "-"] (unlines code)
  return ()
