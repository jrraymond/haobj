{-# LANGUAGE FlexibleContexts #-}

module Mtl where

{- Parses .mtl files -}
import Types

import Control.Monad.Identity
import Text.Parsec
import Text.Parsec.Char (string,char,alphaNum)

parseObj = do
  _ <- char 'o'
  spaces
  name <- manyTill anyChar endOfLine
  vs <- vertices
  ns <- normals
  mtl <- string "usemtl" >> spaces >> manyTill anyChar endOfLine
  return ()


vertices = sepEndBy vertex endOfLine
normals = sepEndBy normal endOfLine

vertex = char 'v' >> spaces >> parseV3

normal = string "vn" >> spaces >> parseV3

ngonFaceElement = char 'f' >> spaces >> manyTill faceEl endOfLine

faceEl = do
  v <- uint
  vt <- try (char '/' >> uint) <|> return (-1)
  vn <- try (char '/' >> char '/' >> uint) <|> return (-1)
  return (Face v vt vn)

  
  

parseMtls :: ParsecT String u Identity [(String,Material)]
parseMtls = many (skipMany ignorable >> parseMtl)

comments = char '#' >> manyTill anyToken endOfLine >> return ()

ignorable = comments <|> void endOfLine

parseMtl :: ParsecT String u Identity (String,Material)
parseMtl = do
  _ <- string "newmtl"
  spaces
  name <- manyTill alphaNum endOfLine
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
