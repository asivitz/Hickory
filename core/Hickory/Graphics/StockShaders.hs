{-# LANGUAGE QuasiQuotes #-}

module Hickory.Graphics.StockShaders where

import Data.String.QM (qt)
import Data.Text (Text)
import Hickory.Graphics.Shader (loadShader, Shader)

-- These shaders likely work with different opengl versions
-- Known to work are: 410, 300 es

texturedVertexShader :: String -> Text
texturedVertexShader version = [qt|
#version ${version}

in vec4 position;
in vec2 texCoords;

out vec2 texCoordsVarying;

uniform mat4 modelMat;

void main()
{
    gl_Position = modelMat * position;
    texCoordsVarying = texCoords;
}
|]

texturedFragmentShader :: String -> Text
texturedFragmentShader version = [qt|
#version ${version}

uniform lowp vec4 color;
uniform sampler2D tex;

in mediump vec2 texCoordsVarying;
out lowp vec4 outcolor;

void main()
{
   outcolor = texture(tex, texCoordsVarying) * color;
}
|]

solidVertexShader :: String -> Text
solidVertexShader version = [qt|
#version ${version}

in vec4 position;

uniform mat4 modelMat;

void main()
{
    gl_Position = modelMat * position;
}
|]

solidColorFragmentShader :: String -> Text
solidColorFragmentShader version = [qt|
#version ${version}

uniform lowp vec4 color;
out lowp vec4 outcolor;

void main()
{
   outcolor = color;
}
|]

litVertexShader :: String -> Text
litVertexShader version = [qt|
#version ${version}

in vec4 position;
in vec3 normal;
in vec2 texCoords;

out vec4 vcolor;
out vec2 texCoordsVarying;

uniform mat4 modelMat;
uniform mat3 normalMat;
uniform mat4 justModelMat;
uniform lowp vec4 color;

void main()
{
    vec3 normal_adj;
    vec3 surfaceToLight;
    float brightness;
    vec4 surfaceColor;
    vec3 lightPos;

    gl_Position = modelMat * position;
    texCoordsVarying = texCoords;

    /* Lighting */
    normal_adj = normalize(normalMat * normal);
    lightPos = vec3(1000, 1000, 3000);
    surfaceToLight = lightPos - vec3(justModelMat * position);

    brightness = dot(normal_adj, surfaceToLight) / (length(surfaceToLight) * length(normal_adj));
    brightness = clamp(brightness, 0.0, 1.0);

    vcolor = vec4((0.7 + brightness * 0.3) * color.rgb, color.a);
}
|]

litFragmentShader :: String -> Text
litFragmentShader version = [qt|
#version ${version}

uniform sampler2D tex;

in mediump vec2 texCoordsVarying;
in lowp vec4 vcolor;
out lowp vec4 outcolor;

void main()
{
   outcolor = texture(tex, texCoordsVarying) * vcolor;
}
|]

skinnedVertexShader :: String -> Text
skinnedVertexShader version = [qt|
#version ${version}

in vec4 position;
in vec3 normal;
/*in vec2 texCoords;*/
in float boneIndex;
in float materialIndex;

out vec4 color;
/*out float fmaterialIndex;*/
/*out vec2 texCoordsVarying;*/

uniform mat4 modelMat;
uniform mat4 boneMat[32];
uniform vec4 colors[6];
uniform mat3 normalMat;
uniform mat4 justModelMat;

void main()
{
    vec3 normal_adj;
    vec3 surfaceToLight;
    float brightness;
    vec4 surfaceColor;
    vec3 lightPos;

    /*gl_Position = modelMat * position;*/
    /*gl_Position = modelMat * boneMat[2] * position;*/
    gl_Position = modelMat * boneMat[int(boneIndex)] * position;
    /*fmaterialIndex = materialIndex;*/
    /*texCoordsVarying = texCoords;*/

    /* Lighting */
    normal_adj = normalize(normalMat * normal);
    /*normal_adj = normal;*/
    lightPos = vec3(50,-50,5);
    surfaceToLight = lightPos - vec3(justModelMat * position);

    brightness = dot(normal_adj, surfaceToLight) / (length(surfaceToLight) * length(normal_adj));
    brightness = clamp(brightness, 0.0, 1.0);

    surfaceColor = colors[int(materialIndex)];
    color = vec4((0.6 + brightness * 0.4) * surfaceColor.rgb, surfaceColor.a);

    /*color = colors[int(materialIndex)];*/
}
|]

skinnedFragmentShader :: String -> Text
skinnedFragmentShader version = [qt|
#version ${version}

/*uniform vec4 color;*/
/*uniform sampler2D tex;*/

/*in vec2 texCoordsVarying;*/
/*in float fmaterialIndex;*/
in lowp vec4 color;
out lowp vec4 outcolor;

void main()
{
    outcolor = color;
   /*outcolor = vec4(vec3(fmaterialIndex * 0.2), 1.0);*/
   /*outcolor = texture(tex, texCoordsVarying) * color;*/
}
|]

perVertColorVertexShader :: String -> Text
perVertColorVertexShader version = [qt|
#version ${version}

in vec3 position;
in vec2 texCoords;
in vec4 color;

out vec2 texCoordsVarying;
out vec4 colorVarying;
uniform mat4 modelMat;

void main()
{
    gl_Position = modelMat * vec4(position, 1.0);
    texCoordsVarying = texCoords;
    colorVarying = color;
}
|]

perVertColorFragmentShader :: String -> Text
perVertColorFragmentShader version = [qt|
#version ${version}

uniform sampler2D tex;
in mediump vec2 texCoordsVarying;
in lowp vec4 colorVarying;
out lowp vec4 outcolor;

void main()
{
    outcolor = texture(tex, texCoordsVarying) * colorVarying;
}
|]

particleVertexShader :: String -> Text
particleVertexShader version = [qt|
#version ${version}

uniform lowp vec4 color;
in vec3 position;

uniform mat4 modelMat;
uniform float size;

void main()
{
    gl_Position = modelMat * vec4(position, 1.0);

    gl_PointSize = size;
}
|]

particleFragmentShader :: String -> Text
particleFragmentShader version = [qt|
#version ${version}

uniform lowp vec4 color;
uniform lowp vec4 color2;
out lowp vec4 outcolor;

void main()
{
    outcolor = color;
}
|]

loadPVCShader :: String -> IO Shader
loadPVCShader version =
  loadShader (perVertColorVertexShader version) Nothing (perVertColorFragmentShader version) ["modelMat", "tex"]

loadSolidShader :: String -> IO Shader
loadSolidShader version =
  loadShader (solidVertexShader version) Nothing (solidColorFragmentShader version) ["modelMat", "color"]

loadTexturedShader :: String -> IO Shader
loadTexturedShader version =
  loadShader (texturedVertexShader version) Nothing (texturedFragmentShader version) ["modelMat", "color", "tex"]

loadSkinnedShader :: String -> IO Shader
loadSkinnedShader version =
  loadShader (skinnedVertexShader version) Nothing (skinnedFragmentShader version) ["modelMat", "boneMat", "colors", "normalMat", "justModelMat"]

loadLitTexturedShader :: String -> IO Shader
loadLitTexturedShader version =
  loadShader (litVertexShader version) Nothing (litFragmentShader version) ["modelMat", "color", "tex", "normalMat", "justModelMat"]
