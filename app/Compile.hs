module Compile
  ( Job (..),
    compile,
  )
where

import Compile.Asm (genAsm)
import Compile.InstrSel (codeGen)
import Compile.Parser (parseAST)
import Compile.Semantic (semanticAnalysis)
import Compile.VarAlloc (replaceVars)
import Control.Monad.IO.Class
import Error (L1ExceptT)

data Job = Job
  { src :: FilePath,
    out :: FilePath
  }
  deriving (Show)

compile :: Job -> L1ExceptT ()
compile job = do
  ast <- parseAST $ src job
  semanticAnalysis ast
  let code = genAsm $ replaceVars $ codeGen ast
  liftIO $ writeFile (out job) (unlines code)
  return ()
