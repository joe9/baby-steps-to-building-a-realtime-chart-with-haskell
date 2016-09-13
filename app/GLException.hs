
{-# LANGUAGE PackageImports         #-}

module GLException
  (GLException(..))
  where

import qualified "gl" Graphics.GL as GL
import Control.Exception.Safe
import Quine.GL.Error

data GLException
  = ShaderProgramCompilationFailed String [Error]
  | ProgramCompilationFailed String [Error]
  deriving (Eq,Show,Typeable)

instance Exception GLException
