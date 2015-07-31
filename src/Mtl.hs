{-# LANGUAGE FlexibleContexts #-}

module Mtl where

{- Parses .mtl files -}
import Types

import Control.Exception (evaluate)
import Control.Monad.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO (hGetContents, IOMode (..),withFile)
import Text.Parsec
import Text.Parsec.Char (string,char,alphaNum)

readObjAndMtl fname = withFile fname ReadMode (\h -> do
  input <- hGetContents h
  case runParser readObj () fname input of
    Left e -> return $ Left e
    Right (mtllib,meshes) -> withFile mtllib ReadMode (\h2 -> do
      mtlInput <- hGetContents h2
      evaluate (length mtlInput)
      case runParser readMtl () mtllib mtlInput of
        Left e -> return $ Left e
        Right mtls -> return $ Right (mtls,meshes)))
  

readObj = do
  skipMany ignorable
  mtllib <- string "mtllib" >> spaces >> manyTill anyChar endOfLine
  meshes <- manyTill (skipMany ignorable >> mesh) eof
  return (mtllib,meshes)

mesh = do
  _ <- char 'o'
  spaces
  name <- manyTill anyChar endOfLine
  vs <- many (try vertex)
  ts <- many (try texture)
  ns <- many (try normal)
  pss <- many (try parameterSpace)
  mfaces <- many materialAndFaces
  return (vs,ts,ns,pss,mfaces)

vertex = char 'v' >> spaces >> parseV3

normal = string "vn" >> spaces >> parseV3

texture = string "vt" >> spaces >> parseV3'

line = do
  string "l"
  v1 <- spaces >> uint
  v2 <- spaces >> uint
  endOfLine
  return (v1,v2)


parameterSpace = string "vp" >> spaces >> parseV3''

materialAndFaces = do
  mtl <- string "usemtl" >> spaces >> manyTill anyChar endOfLine
  s <- try (string "s" >> spaces >> (try (string "off") <|> string "on"))
       <|> return "on"
  faces <- skipMany endOfLine >> many1 ngonFace
  ls <- many (try line)
  return (mtl, s == "on", faces,ls)

ngonFace = do 
  _ <- char 'f'
  spaces
  vs <- sepEndBy faceEl spaces
  return (Face vs)

faceEl = do
  v <- uint
  vt <- try (char '/' >> uint) <|>
        return (-1)
  vn <- try (char '/' >> uint) <|>
        try (char '/' >> char '/' >> uint) <|>
        return (-1)
  return (FaceV v vt vn)

vertexPair = do
  v1 <- uint
  v2 <- char '/' >> uint
  return (v1,v2)

readMtl :: ParsecT String u Identity [(String,Material)]
readMtl = do
  skipMany ignorable 
  manyTill (do skipMany ignorable 
               m <- material
               skipMany ignorable
               return m) eof

comments = char '#' >> manyTill anyToken endOfLine >> return ()

ignorable = comments <|> void endOfLine

material :: ParsecT String u Identity (String,Material)
material = do
  _ <- string "newmtl"
  spaces
  name <- manyTill anyChar endOfLine
  ns <- parseNs
  endOfLine
  ka <- parseKa
  kd <- parseKd
  ks <- parseKs
  ni <- parseNi
  endOfLine
  d <- parseD
  endOfLine
  illum <- parseIllum
  return (name,Material ns ka kd ks ni d illum)

parseNs = string "Ns" >> spaces >> float

parseKa = string "Ka" >> spaces >> parseV3

parseKd = string "Kd" >> spaces >> parseV3

parseKs = string "Ks" >> spaces >> parseV3

parseNi = string "Ni" >> spaces >> float

parseD = string "d" >> spaces >> float

parseIllum = string "illum" >> spaces >> uint

parseV3 = do
  xs <- sepEndBy float spaces
  case xs of
    (r:g:b:[]) -> return (V3 r g b)
    _ -> fail "unexpected number of floats"

parseV3' = do
  xs <- sepEndBy float spaces
  case xs of
    (u:v:[]) -> return (V3 u v 0)
    (u:v:w:[]) -> return (V3 u v w)
    _ -> fail "unexpected number of floats"

parseV3'' = do
  xs <- sepEndBy float spaces
  case xs of
    (u:[]) -> return (V3 u 0 0)
    (u:v:[]) -> return (V3 u v 0)
    (u:v:w:[]) -> return (V3 u v w)
    _ -> fail "unexpected number of floats"

(<++>) a b = (++) <$> a <*> b
(<:>) a b = (:) <$> a <*> b

number = many1 digit

plus = char '+' *> number

minus = char '-' <:> number

integer = plus <|> minus <|> number

uint = let rd = read :: String -> Int in rd <$> number

float = fmap rd $ integer <++> decimal <++> exponent
    where rd       = read :: String -> Float
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> integer
