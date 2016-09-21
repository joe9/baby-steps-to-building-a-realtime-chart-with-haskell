{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}

module OpenGLStuff where

--   https://ghc.haskell.org/trac/ghc/wiki/Commentary/Packages/PackageImportsProposal
import Control.Concurrent
import Control.Exception.Safe
import qualified Data.ByteString as BS
import Data.Colour.SRGB
import Data.Maybe
import Data.Bits
import qualified Data.Vector.Storable as VS
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import "gl" Graphics.GL
import Graphics.UI.GLFW as GLFW
import Prelude hiding (init)
import Quine.Debug
import Quine.GL.Error
--
import GLException

--   http://stackoverflow.com/questions/27407774/get-supported-glsl-versions
vertexShaderSource
    :: BS.ByteString
vertexShaderSource =
    BS.intercalate
        "\n"
        [ "#version 330"
        , "layout (location=0) in vec2 inPosition;"
        , "void main(void) { "
        , "  gl_Position = vec4(inPosition.x, inPosition.y, 0.0f, 1.0f);"
        , "}"]

fragmentShaderSource :: BS.ByteString
fragmentShaderSource =
    BS.intercalate
        "\n"
        [ "#version 330"
        , "uniform vec4 color = vec4(1.0f,0.0f,0.0f,1.0f);"
        , "out vec4 out_Color;"
        , "void main(void) {out_Color = color;}"]

type ColorUniformLocation = GLint

-- TODO use glPolygonMode instead of GL_LINES or GL_TRIANGLE_STRIP
data Picture =
    Picture !(VS.Vector Float) -- x and y vertices
            !GLenum -- drawing primitive
            !(Colour Double) -- colour
            !(Maybe Double) -- Transparency

doubleToGLfloat :: Double -> GLfloat
doubleToGLfloat = realToFrac :: Double -> GLfloat

rgb :: Colour Double -> (GLfloat, GLfloat, GLfloat)
rgb =
    (\(RGB r g b) ->
          (doubleToGLfloat r, doubleToGLfloat g, doubleToGLfloat b)) .
    toSRGB

colorUniformLocationInProgram
    :: ProgramId -> IO ColorUniformLocation
colorUniformLocationInProgram programId =
    withCString "color" (checkGLErrors . glGetUniformLocation programId)

loadColor :: ColorUniformLocation
          -> GLfloat
          -> GLfloat
          -> GLfloat
          -> GLfloat
          -> IO ()
loadColor colorUniformLocation r g b t =
    checkGLErrors (glUniform4f colorUniformLocation r g b t)

-- the timing here is rudimentary
-- the proper way to time the gpu is
-- https://github.com/ekmett/vr/blob/master/timer.h
--      previousmt <- GLFW.getTime
--      mt <- GLFW.getTime
--      putStrLn ("time taken to draw: " ++
--                         show (1000 * (fromMaybe 0 mt - fromMaybe 0 previousmt)) ++
--                         " milliseconds")
drawUsingVertexArray
    :: Window
    -> ColorUniformLocation
    -> VertexArrayId
    -> Colour Double
    -> Maybe Double
    -> (IO ())
    -> IO ()
drawUsingVertexArray _ colorUniformLocation vertexArrayId color maybet f = do
    let (r,g,b) = rgb color
    loadColor colorUniformLocation r g b (doubleToGLfloat (fromMaybe 1 maybet))
    putStrLn ("drawing buffer: vertex array id: " ++ show vertexArrayId)
    usingVertexArray vertexArrayId f
--     usingVertexArray vertexArrayId (glDrawArrays GL_LINES 0 2)

--      writeFile "/tmp/temp-haskell-data" (show v2s)
--   putStrLn ("size of float is: " ++ show ((sizeOf (undefined :: GLfloat))))
--   putStrLn ("loading number of elements: " ++ show ((sizeOf (undefined :: GLfloat) * V.length bufferData)))
--   putStrLn ("loading elements: " ++ show bufferData)
-- http://stackoverflow.com/a/11700577
-- VAO here is like a profile that contains a lot of properties
--   (imagine a smart device profile). Instead of changing color,
--   desktop, fonts.. etc every time you wish to change them, you do
--   that once and save it under a profile name. Then you just switch
--   the profile.
-- but a VAO does not store the current GL_ARRAY_BUFFER
--   binding. Hence, we need the glBindBuffer before glBufferData
-- http://stackoverflow.com/a/21652930
usingVertexArray
    :: VertexArrayId -> (IO ()) -> IO ()
usingVertexArray vertexArrayId f =
    checkGLErrors (glBindVertexArray vertexArrayId) >> f

-- glBufferData deletes the pre-existing data store and creates a new
--   data store
loadBuffer :: BufferId -> VS.Vector GLfloat -> IO ()
loadBuffer bufferId bufferData =
    let size =
            fromIntegral (sizeOf (undefined :: GLfloat) * VS.length bufferData)
    in do checkGLErrors (glBindBuffer GL_ARRAY_BUFFER bufferId)
          putStrLn
                ("loading buffer: vertex array id: " ++
                show bufferId)
          VS.unsafeWith
               bufferData
               (\ptr ->
                     checkGLErrors
                         (glBufferData
                              GL_ARRAY_BUFFER
                              size
                              (castPtr ptr)
                              GL_STREAM_DRAW))
          putStrLn ("loadBuffer completed" ++ show bufferData)

-- -- basic draw function without using the picture
justDrawThis
    :: Window
    -> ColorUniformLocation
    -> VS.Vector GLfloat
    -> Int
    -> GLenum
    -> GLfloat
    -> GLfloat
    -> GLfloat
    -> GLfloat
    -> GLuint
    -> GLuint
    -> IO ()
justDrawThis window colorUniformLocation vertices noOfElementsToDraw drawType r g b t vertexArrayId bufferId = do
    glClearColor 0.05 0.05 0.05 1
    glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
    loadColor colorUniformLocation r g b t
    putStrLn
        ("justDrawThis: size of float is: " ++
         show (sizeOf (undefined :: GLfloat)))
    putStrLn
        ("justDrawThis: length of vertices: " ++ show (VS.length vertices))
    putStrLn
        ("justDrawThis: loading number of bytes: " ++
         show (sizeOf (undefined :: GLfloat) * VS.length vertices))
    putStrLn ("justDrawThis: loading elements: " ++ show vertices)
    loadBuffer bufferId vertices
--     checkGLErrors (glBindVertexArray vertexArrayId)
--     let size =
--             fromIntegral (sizeOf (undefined :: GLfloat) * VS.length vertices)
--     VS.unsafeWith
--         vertices
--         (\ptr ->
--               checkGLErrors
--                   (glBufferData
--                        GL_ARRAY_BUFFER
--                        size
--                        (castPtr ptr)
--                        GL_STREAM_DRAW))
    putStrLn
        ("justDrawThis: draw this many vertices: " ++
         show (div (fromIntegral (VS.length vertices)) 2 :: Int))
    putStrLn
        ("justDrawThis: draw this many vertices: " ++
         show (fromIntegral (div (VS.length vertices) 2) :: Int))
    glDrawArrays drawType 0 (fromIntegral noOfElementsToDraw)
--     glDrawArrays GL_TRIANGLE_STRIP 0 4
    GLFW.swapBuffers window
    glFlush  -- not necessary, but someone recommended it
    checkGLErrors (glFlush)

-- http://stackoverflow.com/questions/7218147/render-multiple-objects-with-opengl-es-2-0
-- VAO here is like a profile that contains a lot of properties
-- (imagine a smart device profile). Instead of changing color,
-- desktop, fonts.. etc every time you wish to change them, you
-- do that once and save it under a profile name. Then you just
-- switch the profile.
type VertexArrayId = GLuint

withVertexArray :: (VertexArrayId -> BufferId -> IO a) -> IO a
withVertexArray f =
    bracket
        (alloca
             (\vertexArrayPtr -> do
                  checkGLErrors $ glGenVertexArrays 1 vertexArrayPtr
                  peek vertexArrayPtr))
        (\vertexArrayId ->
              alloca
                  (\vertexArrayPtr -> do
                       poke vertexArrayPtr vertexArrayId
                       checkGLErrors (glBindVertexArray 0)
                       checkGLErrors (glDeleteVertexArrays 1 vertexArrayPtr)))
        (\vertexArrayId -> do
             glBindVertexArray vertexArrayId
             withVertexBuffer (f vertexArrayId))

type AttributeIndex = GLuint

-- following https://www.opengl.org/wiki/Vertex_Specification
-- separating format specification from buffers ARB_vertex_attrib_binding
withVertexBuffer
    :: (BufferId -> IO a) -> IO a
withVertexBuffer f =
    withBuffer $
    \bufferId -> do
        -- hardcoding bindingindex to 0
        --  checkGLErrors $ glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE 0 nullPtr
        finally
            (do checkGLErrors
                    (glBindVertexBuffer
                         0
                         bufferId
                         0
                         (fromIntegral (2 * sizeOf (undefined :: GLfloat))))
                checkGLErrors (glVertexAttribFormat 0 2 GL_FLOAT GL_FALSE 0)
                checkGLErrors (glVertexAttribBinding 0 0)
                withVertexAttribArray 0 (f bufferId))
            (glBindVertexBuffer
                 0
                 0
                 0
                 (fromIntegral (2 * sizeOf (undefined :: GLfloat))))

--   https://www.opengl.org/wiki/BufferObject
-- bufferObjectType : mostly GL_ARRAY_BUFFER
-- usage : for axis and frame: GL_DYNAMIC_DRAW -- data changes occassionally
--                   others: GL_STREAM_DRAW -- data changes after every use
-- GLsizeiptr: It's not a pointer. It's an integral type the same size as a pointer.
--   http://stackoverflow.com/questions/26758129/glbufferdata-second-arg-is-glsizeiptr-not-glsizei-why
type BufferId = GLuint

withBuffer :: (BufferId -> IO c) -> IO c
withBuffer f =
    bracket
        (alloca
             (\bufferPtr -> do
                  checkGLErrors $ glGenBuffers 1 bufferPtr
                  peek bufferPtr))
        (\bufferId ->
              alloca
                  (\bufferPtr -> do
                       poke bufferPtr bufferId
                       checkGLErrors (glBindBuffer GL_ARRAY_BUFFER 0)
                       checkGLErrors (glDeleteBuffers 1 bufferPtr)))
        (\bufferId -> do
             glBindBuffer GL_ARRAY_BUFFER bufferId
             f bufferId)

withVertexAttribArray :: AttributeIndex -> IO a -> IO a
withVertexAttribArray attributeIndex =
    bracket_
        (checkGLErrors $ glEnableVertexAttribArray attributeIndex)
        (checkGLErrors $ glDisableVertexAttribArray attributeIndex)

type ProgramId = GLuint

attachShader :: ProgramId -> ShaderType -> BS.ByteString -> IO () -> IO ()
attachShader programId shaderType shaderSource f =
    bracket
        (getShaderId shaderType shaderSource)
        (\shaderId ->
              glDetachShader programId shaderId >> glDeleteShader shaderId)
        (\shaderId ->
              checkGLErrors (glAttachShader programId shaderId) >> f)

-- withProgram :: (ProgramId -> IO r) -> IO r
withProgram
    :: (ProgramId -> IO a) -> IO a
withProgram f =
    installDebugHook >>
    bracket
        (checkGLErrors glCreateProgram)
        (\programId ->
              glUseProgram 0 >> glDeleteProgram programId)
        (\programId -> do
             attachShader
                 programId
                 Vertex
                 vertexShaderSource
                 (attachShader
                      programId
                      Fragment
                      fragmentShaderSource
                      (do glLinkProgram programId
                          -- check compile status
                          linkStatus <-
                              alloca
                                  (\linkStatusPtr -> do
                                       glGetProgramiv
                                           programId
                                           GL_LINK_STATUS
                                           linkStatusPtr
                                       peek linkStatusPtr)
                          infoLogLength <-
                              alloca
                                  (\infoLogLengthPtr -> do
                                       glGetProgramiv
                                           programId
                                           GL_INFO_LOG_LENGTH
                                           infoLogLengthPtr
                                       peek infoLogLengthPtr)
                          putStrLn ("infoLogLength: " ++ show infoLogLength)
                          infoLog <- getProgramInfoLog programId infoLogLength
                          putStrLn ("infoLog: " ++ infoLog)
                          if linkStatus == GL_TRUE
                              then checkGLErrors (glUseProgram programId)
                              else errors >>=
                                   throw . ProgramCompilationFailed infoLog))
             -- do not bother rendering on the back face
             -- I like writing the vertices is clockwise order
             -- https://www.opengl.org/wiki/Face_Culling
             -- TODO uncomment the below culling code
--              glFrontFace
--                  GL_CW
--              glCullFace GL_BACK
--              glEnable GL_CULL_FACE
             f programId)

-- the infoLogLength includes the size of the null termination character
-- do not bother printing the infoLog if it just has the null
--   termination character
type InfoLogLength = GLsizei

getProgramInfoLog :: ProgramId -> InfoLogLength -> IO String
getProgramInfoLog programId infoLogLength
  | infoLogLength > 1 =
      alloca
          (\infoLogPtr -> do
               glGetProgramInfoLog programId infoLogLength nullPtr infoLogPtr
               peekCString infoLogPtr)
  | otherwise = return "No Info Log"

getShaderInfoLog :: ShaderId -> InfoLogLength -> IO String
getShaderInfoLog shaderId infoLogLength
  | infoLogLength > 1 =
      alloca
          (\infoLogPtr -> do
               glGetShaderInfoLog shaderId infoLogLength nullPtr infoLogPtr
               peekCString infoLogPtr)
  | otherwise = return "No Info Log"

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

-- https://www.opengl.org/wiki/Shader_Compile_Error
getShaderId
    :: ShaderType -> BS.ByteString -> IO ShaderId
getShaderId shaderType shaderSource =
    let shaderTypeGL Vertex = GL_VERTEX_SHADER
        shaderTypeGL Fragment = GL_FRAGMENT_SHADER
    in bracketOnError
           (checkGLErrors (glCreateShader (shaderTypeGL shaderType)))
           -- prevent leak
           glDeleteShader
           (\shaderId -> do
                BS.useAsCString
                    shaderSource
                    (\c_string ->
                          let gl_string = castPtr c_string :: Ptr GLchar
                          in withArray
                                 [gl_string]
                                 (\ptrToArrayOfStrings ->
                                       glShaderSource
                                           shaderId
                                           1
                                           ptrToArrayOfStrings
                                           nullPtr))
                glCompileShader shaderId
                -- check compile status
                compileStatus <-
                    alloca
                        (\compileStatusPtr -> do
                             glGetShaderiv
                                 shaderId
                                 GL_COMPILE_STATUS
                                 compileStatusPtr
                             peek compileStatusPtr)
                putStrLn ("compileStatus: " ++ show compileStatus)
                threadDelay (1 * 1000 * 1000)
                infoLogLength <-
                    alloca
                        (\infoLogLengthPtr -> do
                             glGetShaderiv
                                 shaderId
                                 GL_INFO_LOG_LENGTH
                                 infoLogLengthPtr
                             peek infoLogLengthPtr)
                putStrLn ("infoLogLength: " ++ show infoLogLength)
                putStrLn ("infoLogLength: " ++ show infoLogLength)
                infoLog <- getShaderInfoLog shaderId infoLogLength
                putStrLn ("infoLog: " ++ infoLog)
                if compileStatus == GL_TRUE
                    then return shaderId
                    else errors >>=
                         throw .
                         ShaderProgramCompilationFailed
                             (show shaderType)
                             (show shaderSource)
                             infoLog)

-- according to the docs, this should be a loop checking until there
-- are no errors
-- https://www.opengl.org/wiki/GLAPI/glGetError
checkGLErrors
    :: IO a -> IO a
checkGLErrors f =
    f >>=
    (\v ->
          throwErrors >> return v)
