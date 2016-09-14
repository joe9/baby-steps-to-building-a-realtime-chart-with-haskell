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
    ,"  gl_Position = vec4(inPosition.x, inPosition.y, 0.0, 1.0);"
    ,"}"]

fragmentShaderSource :: BS.ByteString
fragmentShaderSource =
  BS.intercalate
    "\n"
    ["#version 550"
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
        else do GLFW.windowHint (GLFW.WindowHint'OpenGLDebugContext True)
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
                       programId <- withProgram
                       GLFW.destroyWindow window
                       GLFW.terminate
                       exitSuccess

--               return window
attachShader :: ProgramId -> ShaderId -> IO ()
attachShader p = checkGLErrors . glAttachShader p

type ProgramId = GLuint

-- withProgram :: (ProgramId -> IO r) -> IO r
withProgram :: IO ProgramId
withProgram =
  bracket (checkGLErrors glCreateProgram)
          glDeleteProgram $ \programId -> do
  withShader Vertex vertexShaderSource $ \vertexShaderId -> do
  withShader Fragment fragmentShaderSource $ \fragmentShaderId -> do
  bracket_ (attachShader programId vertexShaderId)
           (glDetachShader programId vertexShaderId) $
    bracket_ (attachShader programId fragmentShaderId)
            (glDetachShader programId fragmentShaderId) $ do
    glLinkProgram programId
    -- check compile status
    linkStatus <- alloca (\linkStatusPtr -> do
                                glGetProgramiv programId GL_LINK_STATUS linkStatusPtr
                                peek linkStatusPtr)
    infoLogLength <- alloca (\infoLogLengthPtr -> do
                                glGetProgramiv programId GL_INFO_LOG_LENGTH infoLogLengthPtr
                                peek infoLogLengthPtr)
    putStrLn ("infoLogLength: " ++ show infoLogLength)
    let getInfoLog = alloca (\infoLogPtr -> do
                                glGetProgramInfoLog programId infoLogLength nullPtr infoLogPtr
                                peekCString infoLogPtr)
    if linkStatus == GL_TRUE
        then do
            -- the infoLogLength includes the size of the null termination character
            -- do not bother printing the infoLog if it just has the null
            --   termination character
            when (infoLogLength > 1)
             (do
                -- the info log also has warnings, etc. So, it is a good idea to
                -- check it even if there are no errors
                infoLog <- getInfoLog
                putStrLn ("infoLog: " ++ infoLog))
            checkGLErrors (glUseProgram programId)
            -- add more functions using the programId here
            return programId
        else do if infoLogLength == 0
                then errors >>= (\es -> throw (ProgramCompilationFailed "No information log" es))
                else do infoLog <- getInfoLog
                        putStrLn ("infoLog: " ++ infoLog)
                        errors >>= (\es -> throw (ProgramCompilationFailed infoLog es))

-- withProgram :: (ProgramId -> IO r) -> IO r
withProgram1 :: IO ProgramId
withProgram1 =
  do bracket (withShader
                Vertex
                vertexShaderSource
                (\vertexShaderId ->
                   do withShader
                        Fragment
                        fragmentShaderSource
                        (\fragmentShaderId ->
                           do bracket (checkGLErrors glCreateProgram)
                                      glDeleteProgram
                                      (\programId ->
                                         do bracket_ (attachShader programId vertexShaderId)
                                                     (glDetachShader programId vertexShaderId)
                                                     (bracket_ (attachShader programId
                                                                             fragmentShaderId)
                                                               (glDetachShader programId
                                                                               fragmentShaderId)
                                                               (do glLinkProgram programId
                                                                   -- check compile status
                                                                   linkStatus <-
                                                                     (alloca (\linkStatusPtr ->
                                                                                do glGetProgramiv programId
                                                                                                  GL_LINK_STATUS
                                                                                                  linkStatusPtr
                                                                                   peek linkStatusPtr))
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
                                                                   if linkStatus ==
                                                                      GL_TRUE
                                                                      then do
                                                                              -- the infoLogLength includes the size of the null termination character
                                                                              -- do not bother printing the infoLog if it just has the null
                                                                              --   termination character
                                                                              when
                                                                                (infoLogLength >
                                                                                 1)
                                                                                (do
                                                                                    -- the info log also has warnings, etc. So, it is a good idea to
                                                                                    -- check it even if there are no errors
                                                                                    infoLog <-
                                                                                      getInfoLog
                                                                                    putStrLn ("infoLog: " ++
                                                                                              infoLog))
                                                                              checkGLErrors (glUseProgram programId)
                                                                              return programId
                                                                      else do if infoLogLength ==
                                                                                 0
                                                                                 then errors >>=
                                                                                      (\es ->
                                                                                         throw (ProgramCompilationFailed "No information log"
                                                                                                                         es))
                                                                                 else do infoLog <-
                                                                                           getInfoLog
                                                                                         putStrLn ("infoLog: " ++
                                                                                                   infoLog)
                                                                                         errors >>=
                                                                                           (\es ->
                                                                                              throw (ProgramCompilationFailed infoLog
                                                                                                                              es))))))))
             (\programId    --TODO remove the program
                ->
                return ())
             (\programId -> return programId -- TODO change later
              )

-- sticking to using the raw gl calls as I can be sure where something
-- got messed up when things go wrong
--   http://book.realworldhaskell.org/read/interfacing-with-c-the-ffi.html
--   http://www.cse.unsw.edu.au/~chak/haskell/ffi/ffi/ffise5.html#x8-340005.9
-- https://www.opengl.org/wiki/Shader_Compile_Error
data ShaderType
  = Vertex
  | Fragment

type ShaderId = GLuint

withShader
  :: ShaderType -> BS.ByteString -> (ShaderId -> IO r) -> IO r
withShader shaderType shaderSource f =
  let shaderTypeGL Vertex   = GL_VERTEX_SHADER
      shaderTypeGL Fragment = GL_FRAGMENT_SHADER
  in bracket (checkGLErrors (glCreateShader (shaderTypeGL shaderType)))
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
                   (glCompileShader shaderId)
                   -- check compile status
                   compileStatus <-
                     (alloca (\compileStatusPtr ->
                                do glGetShaderiv shaderId GL_COMPILE_STATUS compileStatusPtr
                                   peek compileStatusPtr))
                   infoLogLength <-
                     alloca (\infoLogLengthPtr ->
                               do glGetShaderiv shaderId GL_INFO_LOG_LENGTH infoLogLengthPtr
                                  peek infoLogLengthPtr)
                   putStrLn ("infoLogLength: " ++ show infoLogLength)
                   let getInfoLog =
                         alloca (\infoLogPtr ->
                                   do glGetShaderInfoLog shaderId infoLogLength nullPtr infoLogPtr
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
                              f shaderId
                      else do if infoLogLength == 0
                                 then errors >>=
                                      (\es ->
                                         throw (ShaderProgramCompilationFailed "No information log"
                                                                               es))
                                 else do infoLog <- getInfoLog
                                         putStrLn ("infoLog: " ++ infoLog)
                                         errors >>=
                                           (\es ->
                                              throw (ShaderProgramCompilationFailed infoLog es)))

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
