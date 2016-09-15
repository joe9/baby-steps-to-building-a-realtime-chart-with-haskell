
{-# LANGUAGE PackageImports         #-}

module GLException
  (GLException(..))
  where

import Control.Exception.Safe
import Quine.GL.Error

data GLException
  = ShaderProgramCompilationFailed String String String [Error]
  | ProgramCompilationFailed String [Error]
  | GLFWInitFailed
  deriving (Eq,Show,Typeable)

instance Exception GLException
