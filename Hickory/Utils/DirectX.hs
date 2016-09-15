{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Hickory.Utils.DirectX where

import Control.Monad (void, when)
import Control.Applicative (empty)
import Text.Megaparsec
import Text.Megaparsec.Text
import qualified Text.Megaparsec.Lexer as L
import Data.Maybe
import Hickory.Math.Vector
import Hickory.Utils.Parsing
import qualified Data.Vector.Unboxed as Vector

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

data OBJ a = OBJ {
         vertices :: [V3 a],
         textureCoords :: [V2 a],
         normals :: [V3 a],
         faces :: [Face]
         }
         deriving (Show)

lstToV3 [x,y,z] = V3 x y z
lstToV3 _ = error "Wrong size list for V3"
lstToV2 [x,y] = V2 x y
lstToV2 _ = error "Wrong size list for V2"

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "//") empty

parseVertex = lstToV3 <$> try (reserved "v" *> count 3 (lexeme anySignedNumber))
parseTextureCoord = lstToV2 <$> try (reserved "vt" *> count 2 (lexeme anyNumber))
parseNormal = lstToV3 <$> try (reserved "vn" *> count 3 (lexeme anySignedNumber))
parseFace = lstToV3 <$> try (reserved "f" *> count 3 (lexeme parseTriple))
    where parseTriple = do
                            v <- fromIntegral <$> L.integer
                            char '/'
                            t <- fromIntegral <$> L.integer
                            char '/'
                            n <- fromIntegral <$> L.integer
                            return (v,t,n)

braces = between (symbol "{") (symbol "}")

angleBrackets = between (symbol "<") (symbol ">")
squareBrackets = between (symbol "[") (symbol "]")
doubleQuoted = between (symbol "\"") (symbol "\"")
doubleQuotedString = doubleQuoted (many $ noneOf ['"'])

parseUUID = angleBrackets (many (alphaNumChar <|> char '-'))

data MemberItem = SimpleItem String String
                | ArrayMemberItem String String ArraySize
                deriving (Show)

data ObjectItem = ObjectItem String Repeatable
                deriving (Show)

data Section = Section String Bool [MemberItem] [ObjectItem]
              deriving (Show)

data MemberTemplate = MemberTemplate String [MemberItem]
              deriving (Show)

data Repeatable = One | OneOrNone | ZeroOrMore
              deriving (Show)

parseTemplateItemType = identifier

parseArrayNum = do
        var <- optional identifier
        case var of
            Just v -> return $ Ref v
            Nothing -> Num <$> integer

parseTemplateSingleItem =
        SimpleItem <$> parseTemplateItemType <*> identifier

parseTemplateArrayMemberItem =
        ArrayMemberItem <$> (reserved "array" >> parseTemplateItemType)
                  <*> identifier
                  <*> squareBrackets parseArrayNum

parseTemplateItem = terminate (parseTemplateArrayMemberItem <|> parseTemplateSingleItem)

parseTemplate = lexeme $ do
        reserved "template"
        templateName <- identifier
        items <- braces $ do
            parseUUID
            many parseTemplateItem
        return $ MemberTemplate templateName items

data Item = IntegerItem Integer
          | FloatItem Double
          | StringItem String
          | MatrixItem [Double]
          | ArrayItem [Item]
          | FloatArrayItem (Vector.Vector Double)
          | IntegerArrayItem (Vector.Vector Int)
          | Instance String [Item]
          deriving (Show)

data SectionInstance = SectionInstance String [Item] [[SectionInstance]]
                     deriving (Show)

parseSection :: Section -> Parser SectionInstance
parseSection (Section name arg memberItems objectItems) = do
        lexeme (string name)
        when arg $ do
            _ <- identifier
            return ()

        braces $ do
            mitems <- mapM (terminate . parseTemplateMemberItemInstance) memberItems
            {-when (not $ null memberItems) (lexeme (char ';') >> return ())-}
            oitems <- mapM parseTemplateObjectItemInstance objectItems
            return $ SectionInstance name mitems oitems

parseInstance (MemberTemplate name memberItems) = Instance name <$> mapM (terminate . parseTemplateMemberItemInstance) memberItems

parseTemplateObjectItemInstance :: ObjectItem -> Parser [SectionInstance]
parseTemplateObjectItemInstance (ObjectItem itemType repeatable) =
        case repeatable of
            One -> (\x -> [x]) <$> parseSection tem
            OneOrNone -> maybeToList <$> optional (parseSection tem)
            ZeroOrMore -> many (parseSection tem)
    where tem = sectionNamed itemType

terminate x = x <* lexeme (char ';')

lSepBy x y = x `sepBy` lexeme (char y)

meshFace = do
        nfs <- terminate int
        ns <- terminate (int `lSepBy` ',')
        return $ nfs : ns

parseArrayToVector parser = Vector.fromList <$> parser `lSepBy` ','
parseConcatArrayToVector parser = Vector.fromList . concat <$> parser `lSepBy` ','

parseTemplateMemberItemInstance :: MemberItem -> Parser Item
parseTemplateMemberItemInstance (SimpleItem itemType ident) =
        case itemType of
            "DWORD" -> IntegerItem <$> integer
            "FLOAT" -> FloatItem <$> anySignedNumber
            "STRING" -> StringItem <$> doubleQuotedString
            x -> parseInstance (templateNamed x)
parseTemplateMemberItemInstance (ArrayMemberItem itemType ident size) =
        case itemType of
            "FLOAT" -> FloatArrayItem <$> parseArrayToVector anySignedNumber
            "DWORD" -> IntegerArrayItem <$> parseArrayToVector int
            "Coords2d" -> FloatArrayItem <$> parseConcatArrayToVector (count 2 (terminate anySignedNumber))
            "MeshFace" -> IntegerArrayItem <$> parseConcatArrayToVector meshFace
            "Vector" -> FloatArrayItem <$> parseConcatArrayToVector (count 3 (terminate anySignedNumber))
            x -> ArrayItem <$> parseTemplateMemberItemInstance (SimpleItem x "") `lSepBy` ','

data ArraySize = Num Integer | Ref String
               deriving (Show)

sectionNamed :: String -> Section
sectionNamed x = case x of
    "Frame" -> Section "Frame" True [] [ObjectItem "FrameTransformMatrix" One, ObjectItem "Frame" ZeroOrMore, ObjectItem "Mesh" OneOrNone]
    "FrameTransformMatrix" -> Section "FrameTransformMatrix" False [SimpleItem "Matrix4x4" "frameMatrix"] []
    "Mesh" -> Section "Mesh" False [SimpleItem "DWORD" "nVertices",
                                    ArrayMemberItem "Vector" "vertices" (Ref "nVertices"),
                                    SimpleItem "DWORD" "nFaces",
                                    ArrayMemberItem "MeshFace" "faces" (Ref "nFaces")]
                                   [ObjectItem "MeshNormals" OneOrNone,
                                    ObjectItem "MeshTextureCoords" OneOrNone,
                                    ObjectItem "MeshMaterialList" OneOrNone,
                                    ObjectItem "XSkinMeshHeader" OneOrNone,
                                    ObjectItem "SkinWeights" ZeroOrMore
                                    ]
    "MeshNormals" -> Section "MeshNormals" False [SimpleItem "DWORD" "nNormals",
                                                  ArrayMemberItem "Vector" "normals" (Ref "nNormals"),
                                                  SimpleItem "DWORD" "nFaceNormals",
                                                  ArrayMemberItem "MeshFace" "faceNormals" (Ref "nFaceNormals")] []
    "MeshTextureCoords" -> Section "MeshTextureCoords" False [SimpleItem "DWORD" "nTextureCoords",
                                                              ArrayMemberItem "Coords2d" "textureCoords" (Ref "nTextureCoords")] []
    "MeshMaterialList" -> Section "MeshMaterialList" False [SimpleItem "DWORD" "nMaterials",
                                                            SimpleItem "DWORD" "nFaceIndexes",
                                                            ArrayMemberItem "DWORD" "FaceIndexes" (Ref "nFaceIndexes")] [ObjectItem "Material" ZeroOrMore]
    "Material" -> Section "Material" True [SimpleItem "ColorRGBA" "faceColor",
                                           SimpleItem "FLOAT" "power",
                                           SimpleItem "ColorRGB" "specularColor",
                                           SimpleItem "ColorRGB" "emissiveColor"] []
    "XSkinMeshHeader" -> Section "XSkinMeshHeader" False [SimpleItem "DWORD" "nMaxSkinWeightsPerVertex",
                                                          SimpleItem "DWORD" "nMaxSkinWeightsPerFace",
                                                          SimpleItem "DWORD" "nBones"] []
    "SkinWeights" -> Section "SkinWeights" False [SimpleItem "STRING" "transformNodeName",
                                                  SimpleItem "DWORD" "nWeights",
                                                  ArrayMemberItem "DWORD" "vertexIndices" (Ref "nWeights"),
                                                  ArrayMemberItem "FLOAT" "weights" (Ref "nWeights"),
                                                  SimpleItem "Matrix4x4" "matrixOffset"] []

templateNamed :: String -> MemberTemplate
templateNamed x = case x of
    "Vector" -> MemberTemplate "Vector" [SimpleItem "FLOAT" "x", SimpleItem "FLOAT" "y", SimpleItem "FLOAT" "z"]
    "Matrix4x4" -> MemberTemplate "Matrix4x4" [ArrayMemberItem "FLOAT" "matrix" (Num 16)]
    "MeshFace" -> MemberTemplate "MeshFace" [SimpleItem "DWORD" "nFaceVertexIndices", ArrayMemberItem "DWORD" "faceVertexIndices" (Ref "nFaceVertexIndices")]
    "Coords2d" -> MemberTemplate "Coords2d" [SimpleItem "FLOAT" "u", SimpleItem "FLOAT" "v"]
    "ColorRGBA" -> MemberTemplate "ColorRGBA" [SimpleItem "FLOAT" "red", SimpleItem "FLOAT" "green", SimpleItem "FLOAT" "blue", SimpleItem "FLOAT" "alpha"]
    "ColorRGB" -> MemberTemplate "ColorRGBA" [SimpleItem "FLOAT" "red", SimpleItem "FLOAT" "green", SimpleItem "FLOAT" "blue"]
    z -> error $ "No template named: " ++ z

{-parseItem :: Template -> Parser Item-}
{-parseItem (Template name templateItems) = do-}
        {-lexeme (string name)-}
        {-return $ Instance name <$> (braces $ many parseItem)-}

parseX :: Parser ([MemberTemplate], SectionInstance)
parseX = do
        lexeme (manyTill anyChar eol)
        templates <- many parseTemplate
        f <- parseSection (sectionNamed "Frame")
        manyTill anyChar eof
        return (templates, f)

loadX :: String -> IO ([MemberTemplate], SectionInstance)
loadX filePath = do
        res <- parseFromFile parseX filePath
        case res of
            Left err -> error (show err)
            Right obj -> return obj
