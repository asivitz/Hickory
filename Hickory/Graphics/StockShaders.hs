{-# LANGUAGE QuasiQuotes #-}

module Hickory.Graphics.StockShaders where

import Data.String.QM (qt)
import Data.Text (Text)
import Hickory.Graphics.Shader (loadShader, Shader)

texturedVertexShader :: Text
texturedVertexShader = [qt|
#version 410

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

texturedFragmentShader :: Text
texturedFragmentShader = [qt|
#version 410

uniform lowp vec4 color;
uniform sampler2D tex;

in mediump vec2 texCoordsVarying;
out lowp vec4 outcolor;

void main()
{
   outcolor = texture(tex, texCoordsVarying) * color;
}
|]

solidColorFragmentShader :: Text
solidColorFragmentShader = [qt|
#version 410

uniform lowp vec4 color;
out lowp vec4 outcolor;

void main()
{
   outcolor = color;
}
|]

litVertexShader :: Text
litVertexShader = [qt|
#version 410

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
    lightPos = vec3((-2), 1, 3);
    surfaceToLight = lightPos - vec3(justModelMat * position);

    brightness = dot(normal_adj, surfaceToLight) / (length(surfaceToLight) * length(normal_adj));
    brightness = clamp(brightness, 0.0, 1.0);

    vcolor = vec4((0.6 + brightness * 0.4) * color.rgb, color.a);
}
|]

litFragmentShader :: Text
litFragmentShader = [qt|
#version 410

uniform sampler2D tex;

in mediump vec2 texCoordsVarying;
in lowp vec4 vcolor;
out lowp vec4 outcolor;

void main()
{
   outcolor = texture(tex, texCoordsVarying) * vcolor;
}
|]

skinnedVertexShader :: Text
skinnedVertexShader = [qt|
#version 410

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

skinnedFragmentShader :: Text
skinnedFragmentShader = [qt|
#version 410

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

perVertColorVertexShader :: Text
perVertColorVertexShader = [qt|
#version 410

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

perVertColorFragmentShader :: Text
perVertColorFragmentShader = [qt|
#version 410

uniform sampler2D tex;
in mediump vec2 texCoordsVarying;
in lowp vec4 colorVarying;
out lowp vec4 outcolor;

void main()
{
    outcolor = texture(tex, texCoordsVarying) * colorVarying;
}
|]

particleVertexShader :: Text
particleVertexShader = [qt|
#version 410

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

particleFragmentShader :: Text
particleFragmentShader = [qt|
#version 410

uniform lowp vec4 color;
uniform lowp vec4 color2;
out lowp vec4 outcolor;

void main()
{
    outcolor = color;
}
|]

loadPVCShader :: IO Shader
loadPVCShader =
  loadShader perVertColorVertexShader Nothing perVertColorFragmentShader ["modelMat", "tex"]

loadSolidShader :: IO Shader
loadSolidShader =
  loadShader texturedVertexShader Nothing solidColorFragmentShader ["modelMat", "color"]

loadTexturedShader :: IO Shader
loadTexturedShader =
  loadShader texturedVertexShader Nothing texturedFragmentShader ["modelMat", "color", "tex"]

loadSkinnedShader :: IO Shader
loadSkinnedShader =
  loadShader skinnedVertexShader Nothing skinnedFragmentShader ["modelMat", "boneMat", "colors", "normalMat", "justModelMat"]

loadLitTexturedShader :: IO Shader
loadLitTexturedShader =
  loadShader litVertexShader Nothing litFragmentShader ["modelMat", "color", "tex", "normalMat", "justModelMat"]
