{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Hickory.Utils.DirectX where

import Control.Monad (void, when)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import Data.Maybe
import Hickory.Math.Vector
import Hickory.Math.Matrix
import Hickory.Utils.Parsing
import qualified Data.Vector.Unboxed as Vector
import Control.Applicative
import Data.List
import Hickory.Color

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reserved :: String -> Parser ()
reserved w = string w *> notFollowedBy alphaNumChar *> sc

identifier :: Parser String
identifier = lexeme $ (:) <$> letterChar <*> many (alphaNumChar <|> char '.' <|> char '_')

anySignedNumber = do
        n <- signed anyNumber
        optional (satisfy (== ' '))
        return n

number :: Parser Double
number = lexeme (signed anyNumber)

integer :: Parser Integer
integer = lexeme L.integer

int = fromIntegral <$> integer

signed p = do
        n <- optional (symbol "-")
        num <- p
        return $ if isJust n then negate num else num

type Face = V3 (Int, Int, Int) -- Faces are triples of indices into the other lists

{-
data OBJ a = OBJ {
         vertices :: [V3 a],
         textureCoords :: [V2 a],
         normals :: [V3 a],
         faces :: [Face]
         }
         deriving (Show)
         -}

lstToV3 [x,y,z] = V3 x y z
lstToV3 _ = error "Wrong size list for V3"
lstToV2 [x,y] = V2 x y
lstToV2 _ = error "Wrong size list for V2"

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "//") empty

braces = between (symbol "{") (symbol "}")

angleBrackets = between (symbol "<") (symbol ">")
squareBrackets = between (symbol "[") (symbol "]")
doubleQuoted = between (symbol "\"") (symbol "\"")
doubleQuotedString = doubleQuoted (many $ noneOf ['"'])

terminate x = x <* lexeme (char ';')

lSepBy x y = x `sepBy` lexeme (char y)

parseArray p = terminate (p `lSepBy` ',')
parseVector x = (Vector.fromList . concat) <$> parseArray x

parseVectorArray = parseVector (count 3 $ terminate anySignedNumber)
parseMeshFaceArray = parseVector meshFace
parseCoord2dArray = parseVector (count 2 $ terminate anySignedNumber)

meshFace = do
        nfs <- terminate int
        when (nfs /= 3) (error "Mesh has non-triangle face")
        terminate (sepByCount int ',' 3)

parseColorRGBA = terminate $ rgba <$> terminate anySignedNumber
                                  <*> terminate anySignedNumber
                                  <*> terminate anySignedNumber
                                  <*> terminate anySignedNumber

parseColorRGB = terminate $ rgb <$> terminate anySignedNumber
                                <*> terminate anySignedNumber
                                <*> terminate anySignedNumber

data DirectXFrame = DirectXFrame Mat44 [DirectXFrame] (Maybe Mesh)
           deriving (Show)

data Mesh = Mesh {
          nVertices :: Int,
          vertices :: Vector.Vector Double,
          nFaces :: Int,
          faces :: Vector.Vector Int,
          meshNormals :: MeshNormals,
          meshTextureCoords :: MeshTextureCoords,
          meshMaterialList :: MeshMaterialList,
          xSkinMeshHeader :: XSkinMeshHeader,
          skinWeights :: [SkinWeights]
          }
          deriving (Show)

data MeshNormals = MeshNormals {
                 nNormals :: Int,
                 normals :: Vector.Vector Double,
                 nFaceNormals :: Int,
                 faceNormals :: Vector.Vector Int
                 }
                 deriving (Show)

data MeshTextureCoords = MeshTextureCoords {
                       nTextureCoords :: Int,
                       textureCoords :: Vector.Vector Double
                       }
                       deriving (Show)

data MeshMaterialList = MeshMaterialList {
                      nMaterials :: Int,
                      nFaceIndexes :: Int,
                      faceIndexes :: Vector.Vector Int,
                      materials :: [Material]
                      }
                      deriving (Show)

data Material = Material {
              faceColor :: Color,
              power :: Double,
              specularColor :: Color,
              emissiveColor :: Color
              }
              deriving (Show)

data XSkinMeshHeader = XSkinMeshHeader {
                     nMaxSkinWeightsPerVertex :: Int,
                     nMaxSkinWeightsPerFace :: Int,
                     nBones :: Int
                     }
                     deriving (Show)

data SkinWeights = SkinWeights {
                 transformNodeName :: String,
                 nWeights :: Int,
                 vertexIndices :: Vector.Vector Int,
                 weights :: Vector.Vector Double,
                 matrixOffset :: Mat44
                 }
                 deriving (Show)

parseSkinWeights = parseSection "SkinWeights" $
    SkinWeights <$> terminate doubleQuotedString
                <*> terminate int
                <*> (Vector.fromList <$> parseArray int)
                <*> (Vector.fromList <$> parseArray anySignedNumber)
                <*> terminate parseMatrix4x4

parseXSkinMeshHeader = parseSection "XSkinMeshHeader" $
    XSkinMeshHeader <$> terminate int
                    <*> terminate int
                    <*> terminate int

parseMaterial = parseNamedSection "Material" $
    Material <$> parseColorRGBA
             <*> terminate anySignedNumber
             <*> parseColorRGB
             <*> parseColorRGB

parseMeshTextureCoords = parseSection "MeshTextureCoords" $
    MeshTextureCoords <$> terminate int
                      <*> parseCoord2dArray

parseMeshNormals = parseSection "MeshNormals" $
    MeshNormals <$> terminate int
                <*> parseVectorArray
                <*> terminate int
                <*> parseMeshFaceArray

parseMeshMaterialList = parseSection "MeshMaterialList" $
    MeshMaterialList <$> terminate int
                     <*> terminate int
                     <*> (Vector.fromList <$> parseArray int)
                     <*> many parseMaterial

parseMesh = parseSection "Mesh" $
    Mesh <$> terminate int
         <*> parseVectorArray
         <*> terminate int
         <*> parseMeshFaceArray
         <*> parseMeshNormals
         <*> parseMeshTextureCoords
         <*> parseMeshMaterialList
         <*> parseXSkinMeshHeader
         <*> many parseSkinWeights

parseSection name p = lexeme (string name) >> braces p
parseNamedSection name p = lexeme (string name) >> identifier >> braces p

sepByCount_ :: Alternative m => m a -> m sep -> Int -> m [a]
sepByCount_ p sep c = (:) <$> p <*> count (c - 1) (sep *> p)

sepByCount p sepChar = sepByCount_ p (lexeme (char sepChar))

mat44FromList :: [a] -> M44 a
mat44FromList [a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4] =
        V4 (V4 a1 a2 a3 a4) (V4 b1 b2 b3 b4) (V4 c1 c2 c3 c4) (V4 d1 d2 d3 d4)

parseMatrix4x4 :: Parser Mat44
parseMatrix4x4 = mat44FromList <$> terminate (sepByCount anySignedNumber ',' 16)

memberItems = mapM terminate

parseFrame = parseNamedSection "Frame" $
    Frame <$> (head <$> parseSection "FrameTransformMatrix" (memberItems [parseMatrix4x4]))
          <*> many parseFrame
          <*> optional parseMesh

parseHeader = lexeme (manyTill anyChar eol)
parseTemplate = string "template" >> manyTill anyChar (char '}')

parseX :: Parser DirectXFrame
parseX = do
        parseHeader
        many (lexeme parseTemplate)
        f <- parseFrame
        manyTill anyChar eof
        return f

loadX :: String -> IO DirectXFrame
loadX filePath = do
        res <- parseFromFile parseX filePath
        case res of
            Left err -> error (show err)
            Right obj -> return obj
