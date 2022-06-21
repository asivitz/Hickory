{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Hickory.ModelLoading.DirectX where

import Control.Monad (void, when)
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char
import Hickory.Math.Vector
import Hickory.Math.Matrix
import Hickory.ModelLoading.Parsing
import qualified Data.Vector.Unboxed as Vector
import Control.Applicative hiding (many)
import Hickory.Color
import Linear.Quaternion
import Data.List
import Data.Text (Text, pack)
import Linear (V3(..), V4(..), scaled, M44, mkTransformation, (!*!))

{- Types -}

type Face = V3 (Int, Int, Int) -- Faces are triples of indices into the other lists

data Frame = Frame
  { frameName  :: Maybe Text
  , mat        :: Mat44
  , children   :: [Frame]
  , mesh       :: Maybe Mesh
  , actionMats :: [(Text, [Mat44])]
  } deriving (Show)

data Mesh = Mesh
  { nVertices :: Int
  , vertices :: Vector.Vector (Double, Double, Double)
  , nFaces :: Int
  , faces :: Vector.Vector Int
  , meshNormals :: MeshNormals
  , meshTextureCoords :: MeshTextureCoords
  , meshMaterialList :: MeshMaterialList
  , xSkinMeshHeader :: Maybe XSkinMeshHeader
  , skinWeights :: [SkinWeights]
  } deriving (Show)

data MeshNormals = MeshNormals
  { nNormals :: Int
  , normals :: Vector.Vector (Double, Double, Double)
  , nFaceNormals :: Int
  , faceNormals :: Vector.Vector Int
  } deriving (Show)

data MeshTextureCoords = MeshTextureCoords
  { nTextureCoords :: Int
  , textureCoords :: Vector.Vector Double
  } deriving (Show)

data MeshMaterialList = MeshMaterialList
  { faceIndexes :: Vector.Vector Int
  , materials :: [Material]
  } deriving (Show)

data Material = Material
  { faceColor :: Color
  , power :: Double
  , specularColor :: Color
  , emissiveColor :: Color
  } deriving (Show)

data XSkinMeshHeader = XSkinMeshHeader
  { nMaxSkinWeightsPerVertex :: Int
  , nMaxSkinWeightsPerFace :: Int
  , nBones :: Int
  } deriving (Show)

data SkinWeights = SkinWeights
  { transformNodeName :: Text
  , vertexIndices :: Vector.Vector Int
  , weights :: Vector.Vector Double
  , matrixOffset :: Mat44
  } deriving (Show)

data Animation = Animation
  { actionName     :: Text
  , ticksPerSecond :: Int
  , boneAnimations :: [BoneAnimation]
  } deriving (Show)

data BoneAnimation = BoneAnimation
  { boneName :: Text
  , _transforms :: [M44 Double]
  } deriving (Show)

{- Lexing -}

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

reserved :: Text -> Parser ()
reserved w = lexeme (string w *> notFollowedBy alphaNumChar)

identifier :: Parser Text
identifier = lexeme $ pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '.' <|> char '_'))

anySignedNumber :: Parser Double
anySignedNumber = L.signed sc anyNumber

integer :: Parser Integer
integer = lexeme L.decimal

int :: Parser Int
int = fromIntegral <$> integer

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "//" <|> L.skipLineComment "#") empty

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

angleBrackets :: Parser a -> Parser a
angleBrackets = between (symbol "<") (symbol ">")

squareBrackets :: Parser a -> Parser a
squareBrackets = between (symbol "[") (symbol "]")

doubleQuoted :: Parser a -> Parser a
doubleQuoted = between (symbol "\"") (symbol "\"")

doubleQuotedString :: Parser Text
doubleQuotedString = pack <$> doubleQuoted (many $ noneOf ['"'])

terminate :: Parser a -> Parser a
terminate x = x <* lexeme (char ';')

lSepBy :: Parser a -> Char -> Parser [a]
lSepBy x y = x `sepBy` lexeme (char y)

parseArray :: Parser a -> Parser [a]
parseArray p = terminate (p `lSepBy` ',')

parseArraySize :: Int -> Parser a -> Parser [a]
parseArraySize 0 _ = return []
parseArraySize size p = terminate (sepByCount p ',' size)

{- Parsing -}

parseVector :: Vector.Unbox a => Parser [a] -> Parser (Vector.Vector a)
parseVector x = Vector.fromList . concat <$> parseArray x

parseVectorSize :: Vector.Unbox a => Int -> Parser [a] -> Parser (Vector.Vector a)
parseVectorSize size x = Vector.fromList . concat <$> parseArraySize size x

parseVectorArray :: Parser (Vector.Vector (Double, Double, Double))
parseVectorArray = Vector.fromList . map (\[x,y,z] -> (x,y,z)) <$> parseArray (count 3 $ terminate anySignedNumber)

parseMeshFaceArray :: Parser (Vector.Vector Int)
parseMeshFaceArray = parseVector meshFace

parseCoord2dArray :: Parser (Vector.Vector Double)
parseCoord2dArray = parseVector (count 2 $ terminate anySignedNumber)

meshFace :: Parser [Int]
meshFace = do
  nfs <- terminate int
  when (nfs /= 3) (error "Mesh has non-triangle face")
  terminate (sepByCount int ',' 3)

parseColorRGBA :: Parser Color
parseColorRGBA = terminate $ rgba <$> terminate anySignedNumber
                                  <*> terminate anySignedNumber
                                  <*> terminate anySignedNumber
                                  <*> terminate anySignedNumber

parseColorRGB :: Parser Color
parseColorRGB = terminate $ rgb <$> terminate anySignedNumber
                                <*> terminate anySignedNumber
                                <*> terminate anySignedNumber


parseSkinWeights :: Parser SkinWeights
parseSkinWeights = parseSection "SkinWeights" $ \_ -> do
  name <- terminate doubleQuotedString
  num <- terminate int
  SkinWeights name <$>
              (Vector.fromList <$> parseArraySize num int) <*>
              (Vector.fromList <$> parseArraySize num anySignedNumber) <*>
              terminate parseMatrix4x4

parseXSkinMeshHeader :: Parser XSkinMeshHeader
parseXSkinMeshHeader = parseSection "XSkinMeshHeader" $ \_ ->
  XSkinMeshHeader <$> terminate int
                  <*> terminate int
                  <*> terminate int

parseMaterial :: Parser Material
parseMaterial = parseSection "Material" $ \_ -> do
  mat <- Material
    <$> parseColorRGBA
    <*> terminate anySignedNumber
    <*> parseColorRGB
    <*> parseColorRGB
  void $ optional (parseSection "TextureFilename" (const $ terminate doubleQuotedString))
  pure mat


parseMeshTextureCoords :: Parser MeshTextureCoords
parseMeshTextureCoords = parseSection "MeshTextureCoords" $ \_ ->
  MeshTextureCoords <$> terminate int
                    <*> parseCoord2dArray

parseMeshNormals :: Parser MeshNormals
parseMeshNormals = parseSection "MeshNormals" $ \_ ->
  MeshNormals <$> terminate int
              <*> parseVectorArray
              <*> terminate int
              <*> parseMeshFaceArray

parseMeshMaterialList :: Parser MeshMaterialList
parseMeshMaterialList = parseSection "MeshMaterialList" $ \_ -> do
  _num_mats <- terminate int
  num_indices <- terminate int
  MeshMaterialList <$> (Vector.fromList <$> parseArraySize num_indices int)
                     <*> many parseMaterial

parseMesh :: Parser Mesh
parseMesh = parseSection "Mesh" $ \_ ->
  Mesh <$> terminate int
        <*> parseVectorArray
        <*> terminate int
        <*> parseMeshFaceArray
        <*> parseMeshNormals
        <*> parseMeshTextureCoords
        <*> parseMeshMaterialList
        <*> optional parseXSkinMeshHeader
        <*> many parseSkinWeights

parseSection :: Text -> (Maybe Text -> Parser b) -> Parser b
parseSection name p = reserved name >> optional identifier >>= \x -> braces (p x)

sepByCount_ :: (Monad m, Alternative m) => m a -> m sep -> Int -> m [a]
sepByCount_ p sep c = (:) <$> p <*> count (c - 1) (sep *> p)

sepByCount :: Parser a -> Char -> Int -> Parser [a]
sepByCount p sepChar = sepByCount_ p (lexeme (char sepChar))

quaternionFromList :: Num a => [a] -> Quaternion a
quaternionFromList [a,b,c,d] = Quaternion (negate a) (V3 b c d)
quaternionFromList _ = error "Can't build quaternion. Wrong size list."

parseMatrix4x4 :: Parser Mat44
parseMatrix4x4 = m44FromList <$> terminate (sepByCount anySignedNumber ',' 16)

memberItems :: [Parser b] -> Parser [b]
memberItems = mapM terminate

parseFrame :: Parser Frame
parseFrame = parseSection "Frame" $ \name ->
    Frame name <$> (head <$> parseSection "FrameTransformMatrix" (const $ memberItems [parseMatrix4x4]))
          <*> many parseFrame
          <*> optional (try parseMesh)
          <*> return []

parseHeader :: Parser [Char]
parseHeader = lexeme (manyTill anySingle eol)

parseAnimationKey :: Parser a -> Parser [a]
parseAnimationKey element = parseSection "AnimationKey" $ \_ -> do
    void $ terminate int
    nkeys <- terminate int
    parseArraySize nkeys $ terminate element

parseRotationKey :: Parser [Quaternion Double]
parseRotationKey = parseAnimationKey $ do
  _ <- terminate int
  _ <- terminate int
  quaternionFromList <$> parseArraySize 4 anySignedNumber

parseScaleKey :: Parser [V3 Double]
parseScaleKey = parseAnimationKey $ do
  _ <- terminate int
  _ <- terminate int
  v3FromList <$> parseArraySize 3 anySignedNumber

parsePositionKey :: Parser [V3 Double]
parsePositionKey = parseAnimationKey $ do
  _ <- terminate int
  _ <- terminate int
  v3FromList <$> parseArraySize 3 anySignedNumber

parseBoneAnimation :: Parser BoneAnimation
parseBoneAnimation = parseSection "Animation" $ \_ -> do
  name <- braces identifier
  rotationKeys <- parseRotationKey
  scaleKeys <- parseScaleKey
  positionKeys <- parsePositionKey
  let transforms = map (\(r, V3 x y z, p) -> mkTransformation r p !*! scaled (V4 x y z 1)) (zip3 rotationKeys scaleKeys positionKeys)
  return $ BoneAnimation name transforms

parseAnimationSet :: Int -> Parser Animation
parseAnimationSet tps = parseSection "AnimationSet" $ \case
  Just name -> Animation name tps <$> many parseBoneAnimation
  Nothing   -> fail "Animation requires a name"

fillFrame :: [Animation] -> Frame -> Frame
fillFrame animations frame@Frame { frameName, children } = frame { actionMats = filtered, children = map (fillFrame animations) children }
    where mapped = for animations (\Animation { actionName, boneAnimations } ->
                        (actionName, maybe [] _transforms (find (\BoneAnimation { boneName } -> Just boneName == frameName) boneAnimations)))
          filtered = filter (\(_name, xforms) -> (not . null) xforms) mapped
          for = flip map

templateLine :: Parser ()
templateLine = choice
  [ void $ angleBrackets (lSepBy (many alphaNumChar) '-')
  , void $ squareBrackets (many (alphaNumChar <|> char '.'))
  , void $ terminate $ many (lexeme (alphaNumChar <|> char '[' <|> char ']'))
  ]

parseX :: Parser Frame
parseX = do
  sc
  void parseHeader
  void $ many (parseSection "template" (const $ many templateLine))
  f <- parseFrame

  sets <- optional (try $ parseSection "AnimTicksPerSecond" (const $ terminate int)) >>= \case
    Just tps -> many (parseAnimationSet tps)
    Nothing -> pure []

  return $ fillFrame sets f

loadX :: String -> IO Frame
loadX filePath = do
  res <- parseFromFile parseX filePath
  case res of
    Left err -> error (show err)
    Right obj -> return obj
