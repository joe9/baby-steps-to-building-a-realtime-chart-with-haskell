{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PackageImports            #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

-- | A port of the code presented at [Modern OpenGL with
-- Haskell](http://www.arcadianvisions.com/blog/?p=224) to use the
-- GLFW-b package.
import Prelude hiding (init)
-- import           System.Exit (exitFailure)
-- import Control.Applicative
import Control.Concurrent
import Control.Exception.Safe
import Control.Monad          (unless, when)
import Data.Maybe             (isNothing)
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.Exit
import System.IO
-- import Graphics.GLUtil
-- import Graphics.Rendering.OpenGL
-- import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageImportsProposal
import qualified Data.ByteString      as BS
import           Data.Monoid
import qualified Data.Vector.Storable as V
import           "gl" Graphics.GL
import           Linear.V4
import           Quine.GL.Error
import           Quine.Debug
--
import GLException

-- | Geometry data is a list of four 2D vertices.
vertexBufferData :: [GLfloat]
vertexBufferData = [-1,-1,1,-1,-1,1,1,1]

vertices :: V.Vector Float
vertices = V.fromList [-0.6,-0.4,0,0.6,-0.4,0,0,0.6,0]

--            , "attribute vec2 inPosition;"
--   http://stackoverflow.com/questions/27407774/get-supported-glsl-versions
vertexShaderSource :: BS.ByteString
vertexShaderSource =
  BS.intercalate
    "\n"
    ["#version 330"
    ,"layout (location=0) in vec2 inPosition;"
    ,"void main(void) { "
    ,"  gl_Position = vec4(inPosition.x, inPosition.y, 0.0f, 1.0f);"
    ,"}"]

fragmentShaderSource :: BS.ByteString
fragmentShaderSource =
  BS.intercalate
    "\n"
    ["#version 530"
    ,"uniform vec4 color = vec4(1.0f,0.0f,0.0f,1.0f);"
    ,"out vec4 out_Color;"
    ,"void main(void) {out_Color = color;}"]

-- type ErrorCallback = Error -> String -> IO ()
errorCallback :: GLFW.ErrorCallback
errorCallback _ = hPutStrLn stderr

keyCallback :: GLFW.KeyCallback
keyCallback window key _ action _ =
  when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
  GLFW.setWindowShouldClose window True

-- https://www.opengl.org/wiki/Shader_Compile_Error
main =
  do GLFW.setErrorCallback (Just errorCallback)
     successfulInit <- GLFW.init
     -- if init failed, we exit the program
     if not successfulInit
        then exitFailure
        else do
                GLFW.windowHint (GLFW.WindowHint'OpenGLDebugContext True)
                --       GLFW.windowHint $ GLFW.WindowHint'DepthBits 16
                GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
                GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
                GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
                GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
                mw <- GLFW.createWindow 640 480 "test GLFW" Nothing Nothing
                case mw of
                  Nothing -> GLFW.terminate >> exitFailure
                  Just window ->
                    do GLFW.makeContextCurrent mw
                       GLFW.setKeyCallback window
                                           (Just keyCallback)
                       -- OpenGL stuff
                       withProgram (\programId -> return ())
                       GLFW.destroyWindow window
                       GLFW.terminate
                       exitSuccess

--                 withProgram
--                   (\programId -> withVertexArrayObject (drawWindow programId))
drawWindow programId =
  do mw <- GLFW.createWindow 640 480 "test GLFW" Nothing Nothing
     case mw of
       Nothing -> GLFW.terminate >> exitFailure
       Just window ->
         do GLFW.makeContextCurrent mw
            GLFW.setKeyCallback window
                                (Just keyCallback)
            threadDelay (10 * 1000 * 1000)
            GLFW.destroyWindow window
            GLFW.terminate
            exitSuccess

loadBuffer :: V.Vector GLfloat -> IO ()
loadBuffer bufferData =
  V.unsafeWith
    bufferData
    (\ptr ->
       glBufferData
         GL_ARRAY_BUFFER
         (fromIntegral (sizeOf (undefined :: GLfloat) * V.length bufferData))
         (castPtr ptr)
         GL_STREAM_DRAW)

withVertexArrayObject :: IO a -> IO a
withVertexArrayObject f =
  bracket (alloca (\vertexArrayObjectPtr ->
                     do checkGLErrors $
                          glGenVertexArrays 1 vertexArrayObjectPtr
                        peek vertexArrayObjectPtr))
          (\vertexArrayObjectId ->
             alloca (\vertexArrayObjectPtr ->
                       do poke vertexArrayObjectPtr vertexArrayObjectId
                          checkGLErrors $
                            glDeleteVertexArrays 1 vertexArrayObjectPtr))
          (\vertexArrayObjectId ->
             do glBindVertexArray vertexArrayObjectId
                withVertexBufferObject f)

type AttributeIndex = GLuint

withVertexAttribArray
  :: AttributeIndex -> IO d -> IO d
withVertexAttribArray attributeIndex f =
  bracket_ (checkGLErrors $ glEnableVertexAttribArray attributeIndex)
           (checkGLErrors $ glDisableVertexAttribArray attributeIndex)
           f

-- following https://www.opengl.org/wiki/Vertex_Specification
-- separating format specification from buffers ARB_vertex_attrib_binding
withVertexBufferObject :: IO d -> IO d
withVertexBufferObject f =
  withBufferObject $
  \bufferObjectId ->
    do
       -- hardcoding bindingindex to 0
       -- stride = 2, as all my vertexes have 2 coordinates (x & y)
       checkGLErrors $
         glBindVertexBuffer 0
                            bufferObjectId
                            0
                            (fromIntegral (2 * (sizeOf (undefined :: GLfloat))))
       checkGLErrors $ glVertexAttribFormat 0 2 GL_FLOAT GL_FALSE 0
       checkGLErrors $ glVertexAttribBinding 0 0
       withVertexAttribArray 0 f

--   https://www.opengl.org/wiki/Buffer_Object
-- bufferObjectType : mostly GL_ARRAY_BUFFER
-- usage : for axis and frame: GL_DYNAMIC_DRAW -- data changes occassionally
--                   others: GL_STREAM_DRAW -- data changes after every use
-- GLsizeiptr: It's not a pointer. It's an integral type the same size as a pointer.
--   http://stackoverflow.com/questions/26758129/glbufferdata-second-arg-is-glsizeiptr-not-glsizei-why
type BufferObjectId = GLuint

withBufferObject
  :: (BufferObjectId -> IO c) -> IO c
withBufferObject f =
  bracket (alloca (\bufferObjectPtr ->
                     do checkGLErrors $ glGenBuffers 1 bufferObjectPtr
                        peek bufferObjectPtr))
          (\bufferObjectId ->
             alloca (\bufferObjectPtr ->
                       do poke bufferObjectPtr bufferObjectId
                          checkGLErrors $ glDeleteBuffers 1 bufferObjectPtr))
          (\bufferObjectId ->
             do glBindBuffer GL_ARRAY_BUFFER bufferObjectId
                f bufferObjectId)

type ProgramId = GLuint

attachShader :: ProgramId -> ShaderId -> IO ()
attachShader p = checkGLErrors . glAttachShader p

-- withProgram :: (ProgramId -> IO r) -> IO r
withProgram :: (ProgramId -> IO a) -> IO a
withProgram f =
  do
     installDebugHook
     vertexShaderId <- getShaderId Vertex vertexShaderSource
     fragmentShaderId <- getShaderId Fragment fragmentShaderSource
     bracket (checkGLErrors glCreateProgram)
             (\programId -> glUseProgram 0 >> glDeleteProgram programId)
             (\programId ->
                do (flip finally
                         (glDetachShader programId vertexShaderId >>
                          glDeleteShader vertexShaderId))
                     (do attachShader programId vertexShaderId
                         (flip finally
                               (glDetachShader programId fragmentShaderId >>
                                glDeleteShader fragmentShaderId))
                           (do attachShader programId fragmentShaderId
                               glLinkProgram programId
                               -- check compile status
                               linkStatus <-
                                 alloca (\linkStatusPtr ->
                                           do glGetProgramiv programId GL_LINK_STATUS linkStatusPtr
                                              peek linkStatusPtr)
                               infoLogLength <-
                                 alloca (\infoLogLengthPtr ->
                                           do glGetProgramiv programId
                                                             GL_INFO_LOG_LENGTH
                                                             infoLogLengthPtr
                                              peek infoLogLengthPtr)
                               putStrLn ("infoLogLength: " ++
                                         show infoLogLength)
                               let getInfoLog =
                                     alloca (\infoLogPtr ->
                                               do glGetProgramInfoLog programId
                                                                      infoLogLength
                                                                      nullPtr
                                                                      infoLogPtr
                                                  peekCString infoLogPtr)
                               if linkStatus == GL_TRUE
                                  then do
                                          -- the infoLogLength includes the size of the null termination character
                                          -- do not bother printing the infoLog if it just has the null
                                          --   termination character
                                          when
                                            (infoLogLength > 1)
                                            (do
                                                -- the info log also has warnings, etc. So, it is a good idea to
                                                -- check it even if there are no errors
                                                infoLog <- getInfoLog
                                                putStrLn ("infoLog: " ++
                                                          infoLog))
                                          checkGLErrors (glUseProgram programId)
                                  else do if infoLogLength == 0
                                             then errors >>=
                                                  (\es ->
                                                     throw (ProgramCompilationFailed "No information log"
                                                                                     es))
                                             else do infoLog <- getInfoLog
                                                     putStrLn ("infoLog: " ++
                                                               infoLog)
                                                     errors >>=
                                                       (\es ->
                                                          throw (ProgramCompilationFailed infoLog es))))
                   f programId)

-- sticking to using the raw gl calls as I can be sure where something
-- got messed up when things go wrong
--   http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html
--   http://www.cse.unsw.edu.au/~chak/haskell/ffi/ffi/ffise5.html#x8-340005.9
-- https://www.opengl.org/wiki/Shader_Compile_Error
data ShaderType
  = Vertex
  | Fragment
  deriving (Eq,Show)

type ShaderId = GLuint

getShaderId
  :: ShaderType -> BS.ByteString -> IO ShaderId
getShaderId shaderType shaderSource =
  let shaderTypeGL Vertex   = GL_VERTEX_SHADER
      shaderTypeGL Fragment = GL_FRAGMENT_SHADER
  in
     bracketOnError (checkGLErrors (glCreateShader (shaderTypeGL shaderType)))
       -- prevent leak
       glDeleteShader
       (\shaderId ->
          do BS.useAsCString
               shaderSource
               (\c_string ->
                  let gl_string = castPtr c_string :: Ptr GLchar
                  in withArray [gl_string]
                               (\ptrToArrayOfStrings ->
                                  glShaderSource shaderId 1 ptrToArrayOfStrings nullPtr))
             glCompileShader shaderId
             -- check compile status
             compileStatus <-
               alloca (\compileStatusPtr ->
                         do glGetShaderiv shaderId GL_COMPILE_STATUS compileStatusPtr
                            peek compileStatusPtr)
             putStrLn ("compileStatus: " ++ show compileStatus)
             threadDelay (1 * 1000 * 1000)
             infoLogLength <-
               alloca (\infoLogLengthPtr ->
                         do glGetShaderiv shaderId GL_INFO_LOG_LENGTH infoLogLengthPtr
                            peek infoLogLengthPtr)
             putStrLn ("infoLogLength: " ++ show infoLogLength)
             let getInfoLog =
                   alloca (\infoLogPtr ->
                             --                              do glGetShaderInfoLog shaderId infoLogLength nullPtr infoLogPtr
                             do glGetShaderInfoLog shaderId 256 nullPtr infoLogPtr
                                peekCString infoLogPtr)
             if compileStatus == GL_TRUE
                then do
                        -- the infoLogLength includes the size of the null termination character
                        -- do not bother printing the infoLog if it just has the null
                        --   termination character
                        when
                          (infoLogLength > 1)
                          (do
                              -- the info log also has warnings, etc. So, it is a good idea to
                              -- check it even if there are no errors
                              infoLog <- getInfoLog
                              putStrLn ("infoLog: " ++ infoLog))
                        return shaderId
                else do if infoLogLength == 0
                           then errors >>=
                                (\es ->
                                   throw (ShaderProgramCompilationFailed (show shaderType)
                                                                         (show shaderSource)
                                                                         ""
                                                                         es))
                           else do infoLog <- getInfoLog
                                   putStrLn ("infoLog: " ++ infoLog)
                                   errors >>=
                                     (\es ->
                                        throw (ShaderProgramCompilationFailed (show shaderType)
                                                                              (show shaderSource)
                                                                              infoLog
                                                                              es)))

-- according to the docs, this should be a loop checking until there
-- are no errors
-- https://www.opengl.org/wiki/GLAPI/glGetError
checkGLErrors :: IO a -> IO a
checkGLErrors f = f >>= (\v -> throwErrors >> return v)

-- | Set drawing parameters that won't change during execution.
-- drawInit   = do clearColor $= Color4 1 1 1 1
--                 clear [ColorBuffer]
-- setupDrawable = do
--                 vao <- makeVAO $ do
--                     -- do this for static objects
--                     GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
--                     V.unsafeWith vertices $ \ptr -> do
--                         GL.vertexAttribPointer (GL.AttribLocation 0) $=
--                             (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)
--                     GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
--                 return vao
-- draw vao = withVAO vao $ do
--     sp <- shaderProgram
--     GL.currentProgram $= Just (program sp)
--     setUniform sp "color" (V4 (0 :: GLfloat) 1 0 1)
--     GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Enabled
--     V.unsafeWith vertices $ \ptr -> do
--         GL.vertexAttribPointer (GL.AttribLocation 0) $=
--             (GL.ToFloat, GL.VertexArrayDescriptor 3 GL.Float 0 ptr)
--     GL.drawArrays GL.Triangles 0 3
--     GL.vertexAttribArray (GL.AttribLocation 0) $= GL.Disabled
-- main :: IO ()
-- main = do ok <- init
--           when (not ok) (error "Error initializing GLFW!")
--           windowHint (WindowHint'ContextVersionMajor 3)
--           windowHint (WindowHint'ContextVersionMinor 0)
--           windowHint $ WindowHint'RefreshRate 100
--           m@(~(Just w)) <- createWindow 500 500 "Chapter 2" Nothing Nothing
--           when (isNothing m) (error "Couldn't create window!")
--           makeContextCurrent m
--           setWindowTitle w "Chapter 2"
--           vao <- setupDrawable
--           drawInit
--           let keyIsPressed k = (== KeyState'Pressed) <$> getKey w k
--               go   = do draw vao
--                         swapBuffers w
--                         pollEvents
--                         keyIsPressed Key'Escape >>= flip unless go
--           go
-- I do not care about the others
data BufferObjectType
  = ARRAY_BUFFER
  | ELEMENT_ARRAY_BUFFER

data BufferUsageType
  = STATIC_DRAW
  | DYNAMIC_DRAW
  | STREAM_DRAW

bufferObjectTypeToEnum :: (Eq a
                          ,Num a)
                       => BufferObjectType -> a
bufferObjectTypeToEnum ARRAY_BUFFER = GL_ARRAY_BUFFER
bufferObjectTypeToEnum ELEMENT_ARRAY_BUFFER = GL_ELEMENT_ARRAY_BUFFER

bufferUsageTypeToEnum :: (Eq a
                         ,Num a)
                      => BufferUsageType -> a
bufferUsageTypeToEnum STATIC_DRAW  = GL_STATIC_DRAW
bufferUsageTypeToEnum DYNAMIC_DRAW = GL_DYNAMIC_DRAW
bufferUsageTypeToEnum STREAM_DRAW  = GL_STREAM_DRAW
