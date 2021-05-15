{-# LANGUAGE QuasiQuotes #-}

module Hickory.Graphics.DeferredRendering where

import Hickory.Types (Size(..))
import Graphics.GL.Compatibility41
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (nullPtr)
import Control.Monad (when)
import Data.String.QM (qt)
import Hickory.Graphics.Shader (loadShader, Shader)
import Hickory.Graphics.GLSupport (alloc1)
import Hickory.Graphics.Textures (TexID(..))
import Data.IORef (IORef, readIORef)
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bits ((.|.))

data GBuffer = GBuffer
  { frameBuffer :: GLuint
  , position    :: TexID
  , normal      :: TexID
  , albedo      :: TexID
  }

createGBuffer :: Size Int -> IO GBuffer
createGBuffer (Size (fromIntegral -> w) (fromIntegral -> h)) = do
  frameBuffer <- alloc1 glGenFramebuffers
  glBindFramebuffer GL_FRAMEBUFFER frameBuffer

  position   <- mkTex GL_RGBA16F GL_FLOAT GL_COLOR_ATTACHMENT0
  normal     <- mkTex GL_RGBA16F GL_FLOAT GL_COLOR_ATTACHMENT1
  albedo     <- mkTex GL_RGBA GL_UNSIGNED_BYTE GL_COLOR_ATTACHMENT2

  withArray [ GL_COLOR_ATTACHMENT0, GL_COLOR_ATTACHMENT1, GL_COLOR_ATTACHMENT2 ] $
    glDrawBuffers 3

  depthRb <- alloc1 glGenRenderbuffers
  glBindRenderbuffer GL_RENDERBUFFER depthRb
  glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT w h
  glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER depthRb

  glCheckFramebufferStatus GL_FRAMEBUFFER >>= \status -> when ( status /= GL_FRAMEBUFFER_COMPLETE) $
    print "Warning: Framebuffer not complete!"

  glBindFramebuffer GL_FRAMEBUFFER 0

  pure $ GBuffer {..}

  where
  mkTex internalFormat typ attachment = do
    tex <- alloc1 glGenTextures
    glBindTexture GL_TEXTURE_2D tex
    glTexImage2D GL_TEXTURE_2D 0 (fromIntegral internalFormat) w h 0 GL_RGBA typ nullPtr
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER (fromIntegral GL_NEAREST)
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER (fromIntegral GL_NEAREST)
    glFramebufferTexture2D GL_FRAMEBUFFER attachment GL_TEXTURE_2D tex 0
    pure $ TexID tex

withGBufferBound :: (MonadReader r m, MonadIO m) => (r -> IORef GBuffer) -> m a -> m a
withGBufferBound getter f = do
  gBufferRef <- asks getter

  GBuffer { frameBuffer  } <- liftIO $ readIORef gBufferRef
  glClearColor 0 0 0 1
  glBindFramebuffer GL_FRAMEBUFFER frameBuffer
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  a <- f
  glBindFramebuffer GL_FRAMEBUFFER 0

  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  pure a

loadGBufShader :: IO Shader
loadGBufShader =
  loadShader "330 core" gbufvs gbuffs ["modelMat", "normalMat", "viewMat", "projectionMat", "tex"]

loadDeferredShader :: IO Shader
loadDeferredShader =
  loadShader "330 core" deferredvs deferredfs
    ["positionTex", "normalTex", "albedoTex", "noiseTex", "lightDir", "lightColor", "ambientLightColor", "sampleKernel", "projectionMat", "radius", "bias"]

gbufvs = [qt|
layout (location = 0) in vec3 position;
layout (location = 1) in vec2 texCoords;
layout (location = 2) in vec3 normal;

out vec2 texCoordsF;
out vec3 fragPos;
out vec3 normalF;

uniform mat4 modelMat;
uniform mat3 normalMat;
uniform mat4 viewMat;
uniform mat4 projectionMat;

void main()
{
    vec4 viewPos = viewMat * modelMat * vec4(position, 1.0);
    fragPos = viewPos.xyz;
    texCoordsF = texCoords;

    normalF = normalMat * normal;

    gl_Position = projectionMat * viewPos;
}
|]

gbuffs = [qt|
layout (location = 0) out vec3 positionTexFrag;
layout (location = 1) out vec3 normalTexFrag;
layout (location = 2) out vec4 albedoTexFrag;

in vec2 texCoordsF;
in vec3 fragPos;
in vec3 normalF;

uniform sampler2D tex;

void main()
{
    positionTexFrag = fragPos;
    normalTexFrag   = normalize(normalF);
    albedoTexFrag.rgb = texture(tex, texCoordsF).rgb;
    albedoTexFrag.a = 1;
}
|]

deferredvs = [qt|
layout (location = 0) in vec2 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 texCoords;

out vec2 texCoordsF;

void main()
{
    texCoordsF = texCoords;
    gl_Position = vec4(position, 0.0, 1.0);
}
|]

deferredfs = [qt|
out vec4 fragColor;

in vec2 texCoordsF;

uniform sampler2D positionTex;
uniform sampler2D normalTex;
uniform sampler2D albedoTex;

uniform sampler2D noiseTex;

uniform vec3 ambientLightColor;

uniform vec3 lightDir;
uniform vec3 lightColor;

uniform mat4 projectionMat;
uniform vec3 sampleKernel[64];

uniform float radius;
uniform float bias;

const vec2 noiseScale = vec2(750.0/4.0, 750.0/4.0);

int kernelSize = 64;


void main()
{
    vec3 fragPos = texture(positionTex, texCoordsF).rgb;
    vec3 normal = texture(normalTex, texCoordsF).rgb;
    vec3 albedo = texture(albedoTex, texCoordsF).rgb;

    vec3 random = texture(noiseTex, texCoordsF * noiseScale).xyz;

    vec3 tangent = normalize(randomVec - normal * dot(random, normal));
    vec3 bitangent = cross(normal, tangent);
    mat3 TBN = mat3(tangent, bitangent, normal);

    float occlusion = 0.0;
    for(int i = 0; i < kernelSize; i++)
    {
        vec3 samplePos = fragPos + TBN * sampleKernel[i] * radius;

        vec4 offset = projectionMat * vec4(samplePos, 1.0);    // view space -> clip space
        offset.xyz /= offset.w;
        offset.xyz  = offset.xyz * 0.5 + 0.5;
        float sampleDepth = texture(positionTex, offset.xy).z;

        float rangeCheck = smoothstep(0.0, 1.0, radius / abs(fragPos.z - sampleDepth));
        occlusion += (sampleDepth >= samplePos.z + bias ? 1.0 : 0.0) * rangeCheck;
    }

    occlusion = 1.0 - (occlusion / kernelSize);

    vec3 totalLight = albedo * ambientLightColor * occlusion;

    vec3 diffuse = max(dot(normal, lightDir), 0.0) * albedo * lightColor;

    totalLight += diffuse;

    fragColor = vec4(occlusion, occlusion, occlusion, 1.0);
    //fragColor = vec4(totalLight, 1.0);
}
|]
