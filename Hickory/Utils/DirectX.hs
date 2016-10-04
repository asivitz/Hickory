{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

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
import Hickory.Color
import Linear.Quaternion
import Data.List

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

parseArraySize 0 p = return []
parseArraySize size p = terminate (sepByCount p ',' size)

parseVectorSize size x = (Vector.fromList . concat) <$> parseArraySize size x

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

data Frame = Frame {
                  frameName :: String,
                  mat :: Mat44,
                  children :: [Frame],
                  mesh :: Maybe Mesh,
                  actionMats :: [(String, [Mat44])]
                  }
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
                 vertexIndices :: Vector.Vector Int,
                 weights :: Vector.Vector Double,
                 matrixOffset :: Mat44
                 }
                 deriving (Show)

parseSkinWeights = parseSection "SkinWeights" $ do
    name <- terminate doubleQuotedString
    num <- terminate int
    SkinWeights name <$>
                (Vector.fromList <$> parseArraySize num int) <*>
                (Vector.fromList <$> parseArraySize num anySignedNumber) <*>
                terminate parseMatrix4x4

parseXSkinMeshHeader = parseSection "XSkinMeshHeader" $
    XSkinMeshHeader <$> terminate int
                    <*> terminate int
                    <*> terminate int

parseMaterial = parseNamedSection "Material" $ \name ->
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

parseMeshMaterialList = parseSection "MeshMaterialList" $ do
    num_mats <- terminate int
    num_indices <- terminate int
    MeshMaterialList <$> (Vector.fromList <$> parseArraySize num_indices int)
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
parseNamedSection name p = lexeme (string name) >> identifier >>= \x -> braces (p x)

sepByCount_ :: Alternative m => m a -> m sep -> Int -> m [a]
sepByCount_ p sep c = (:) <$> p <*> count (c - 1) (sep *> p)

sepByCount p sepChar = sepByCount_ p (lexeme (char sepChar))

quaternionFromList :: Num a => [a] -> Quaternion a
quaternionFromList [a,b,c,d] = Quaternion (negate a) (V3 b c d)
quaternionFromList _ = error "Can't build quaternion. Wrong size list."

parseMatrix4x4 :: Parser Mat44
parseMatrix4x4 = mat44FromList <$> terminate (sepByCount anySignedNumber ',' 16)

memberItems = mapM terminate

parseFrame :: Parser Frame
parseFrame = parseNamedSection "Frame" $ \name ->
    Frame name <$> (head <$> parseSection "FrameTransformMatrix" (memberItems [parseMatrix4x4]))
          <*> many parseFrame
          <*> optional parseMesh
          <*> return []

parseHeader = lexeme (manyTill anyChar eol)

skipSection :: String -> Parser ()
skipSection name = lexeme $ string name >> manyTill anyChar (char '}') >> return ()

parseAnimationKey element = parseSection "AnimationKey" $ do
    terminate int
    nkeys <- terminate int
    parseArraySize nkeys $ terminate $ element

parseRotationKey = parseAnimationKey $ do
        _ <- terminate int
        _ <- terminate int
        quaternionFromList <$> parseArraySize 4 anySignedNumber

parseScaleKey = parseAnimationKey $ do
        _ <- terminate int
        _ <- terminate int
        v3FromList <$> parseArraySize 3 anySignedNumber

parsePositionKey = parseAnimationKey $ do
        _ <- terminate int
        _ <- terminate int
        v3FromList <$> parseArraySize 3 anySignedNumber

parseBoneAnimation :: Parser BoneAnimation
parseBoneAnimation = parseSection "Animation" $ do
    name <- braces identifier
    rotationKeys <- parseRotationKey
    scaleKeys <- parseScaleKey
    positionKeys <- parsePositionKey
    let transforms = map (\(r, V3 x y z, p) -> mkTransformation r p !*! scaled (V4 x y z 1)) (zip3 rotationKeys scaleKeys positionKeys)
    return $ BoneAnimation name transforms

parseAnimationSet :: Int -> Parser Animation
parseAnimationSet tps = parseNamedSection "AnimationSet" $ \name ->
    Animation name tps <$> (many parseBoneAnimation)

fillFrame :: [Animation] -> Frame -> Frame
fillFrame animations frame@Frame { frameName, children } = frame { actionMats = filtered, children = map (fillFrame animations) children }
    where mapped = for animations (\Animation { actionName, boneAnimations } ->
                        (actionName, fromMaybe [] (_transforms <$> find (\BoneAnimation { boneName } -> boneName == frameName) boneAnimations)))
          filtered = filter (\(name, xforms) -> (not . null) xforms) mapped
          for = flip map

parseX :: Parser Frame
parseX = do
        parseHeader
        many (lexeme $ skipSection "template")
        f <- parseFrame

        tps <- parseSection "AnimTicksPerSecond" (terminate int)
        sets <- many (parseAnimationSet tps)

        manyTill anyChar eof
        return $ fillFrame sets f

data Animation = Animation {
               actionName :: String,
               ticksPerSecond :: Int,
               boneAnimations :: [BoneAnimation]
               }
               deriving (Show)

data BoneAnimation = BoneAnimation {
                   boneName :: String,
                   _transforms :: [M44 Double]
                   }
                   deriving (Show)

loadX :: String -> IO Frame
loadX filePath = do
        res <- parseFromFile parseX filePath
        case res of
            Left err -> error (show err)
            Right obj -> return obj
