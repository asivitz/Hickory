{-# LANGUAGE NamedFieldPuns #-}

module Graphics.Shader (
              Shader(..),
              loadShader,
              getShader,
              useShader,
              attachVertexArray
              )
              where

import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Graphics.GLSupport
import Control.Monad

data Shader = Shader {
            program :: GLuint,
            vertShader :: GLuint,
            fragShader :: GLuint,

            sp_ATTR_POSITION :: GLuint,
            sp_ATTR_TEX_COORDS :: GLuint,
            sp_ATTR_COLOR :: GLuint,
            sp_ATTR_COLOR2 :: GLuint,
            sp_ATTR_NORMALS :: GLuint,

            sp_UNIFORM_TEXID :: GLint,
            sp_UNIFORM_COLOR :: GLint,
            sp_UNIFORM_COLOR2 :: GLint,
            sp_UNIFORM_MODEL_MAT :: GLint,
            sp_UNIFORM_VIEW_MAT :: GLint,
            sp_UNIFORM_SIZE :: GLint
            } deriving (Eq, Ord, Show)

getShader Shader { program } = program

loadShader resPath vert frag = do
   let prefix = resPath ++ "/Shaders/"
   let (vsp, fsp) = ( prefix ++ vert, prefix ++ frag)

   buildShaderProgram vsp fsp

shaderSourceForPath path = do
        source <- readFile path
        -- TODO: iOS shouldn't have this header
        return $ "#version 150\n" ++ source

glGetBoolean boolean = (== GL_TRUE) <$> alloca (\ptr -> glGetBooleanv boolean ptr >> peek ptr)

glGetShaderi shaderId x = alloca (\ptr -> glGetShaderiv shaderId x ptr >> peek ptr)

glGetProgrami programId x = alloca (\ptr -> glGetProgramiv programId x ptr >> peek ptr)

retrieveLog lenFun infoFun = do
        infoLen <- lenFun GL_INFO_LOG_LENGTH
        let infoLen' = fromIntegral infoLen
        if (infoLen > 1)
            then do
                info <- allocaArray infoLen' $ \buf -> do
                    infoFun infoLen nullPtr buf
                    peekCStringLen (buf, infoLen' - 1)
                return $ Just info
            else return Nothing


compileShader :: String -> GLenum -> IO GLuint
compileShader path shaderType = do
        compileSupported <- glGetBoolean GL_SHADER_COMPILER
        unless compileSupported (error "ERROR: Shader compilation not supported.")

        source <- shaderSourceForPath path
        shaderId <- glCreateShader shaderType

        withMany withCString [source] $ \strs ->
            withArray strs $ \ptr ->
                glShaderSource shaderId 1 (castPtr ptr) nullPtr
        glCompileShader shaderId
        compiled <- glGetShaderi shaderId GL_COMPILE_STATUS
        let didCompile = compiled /= 0

        unless didCompile $ do
            infoLog <- retrieveLog (glGetShaderi shaderId) (glGetShaderInfoLog shaderId)
            case infoLog of
                Just i -> do
                    print "*** ERROR: Couldn't compile shader"
                    print i
                _ -> return ()

            glDeleteShader shaderId
            error $ "Failed to compile shader: " ++ path

        return shaderId


linkProgram :: GLuint -> GLuint -> GLuint -> IO Bool
linkProgram programId vertShader fragShader = do
        glAttachShader programId vertShader
        glAttachShader programId fragShader
        glLinkProgram programId

        linked <- glGetProgrami programId GL_LINK_STATUS
        let didLink = linked /= 0

        unless didLink $ do
            infoLog <- retrieveLog (glGetProgrami programId) (glGetProgramInfoLog programId)
            case infoLog of
                Just i -> do
                    print "*** ERROR: Can't link shader program"
                    print i
                _ -> return ()

        return didLink

buildShaderProgram :: String -> String -> IO Shader
buildShaderProgram vertShaderPath fragShaderPath = do
        vertShader <- compileShader vertShaderPath GL_VERTEX_SHADER
        fragShader <- compileShader fragShaderPath GL_FRAGMENT_SHADER

        programId <- glCreateProgram

        linked <- linkProgram programId vertShader fragShader
        unless linked $ do
            glDeleteShader vertShader
            glDeleteShader fragShader

            error $ "Failed to link shader program: " ++ vertShaderPath ++ "-" ++ fragShaderPath

        let sh = Shader programId vertShader fragShader
        sh <$>
            (fromIntegral <$> withCString "position" (glGetAttribLocation programId)) <*>
            (fromIntegral <$> withCString "texCoords" (glGetAttribLocation programId)) <*>
            (fromIntegral <$> withCString "color" (glGetAttribLocation programId)) <*>
            (fromIntegral <$> withCString "color2" (glGetAttribLocation programId)) <*>
            (fromIntegral <$> withCString "normals" (glGetAttribLocation programId)) <*>

            (withCString "tex" (glGetUniformLocation programId)) <*>
            (withCString "color" (glGetUniformLocation programId)) <*>
            (withCString "color2" (glGetUniformLocation programId)) <*>
            (withCString "modelMat" (glGetUniformLocation programId)) <*>
            (withCString "viewMat" (glGetUniformLocation programId)) <*>
            (withCString "size" (glGetUniformLocation programId))

useShader (Shader { program }) = glUseProgram program


attachVertexArray :: GLuint -> GLint -> GLint -> GLint -> IO ()
attachVertexArray attrLoc len stride offset = do
        glEnableVertexAttribArray attrLoc
        glVertexAttribPointer attrLoc len GL_FLOAT GL_FALSE (stride * fsize) (plusPtr nullPtr (fromIntegral $ offset * fsize))
    where fsize = fromIntegral $ sizeOf (0 :: GLfloat)
