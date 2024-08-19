{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Applicative
import Data.List.Split qualified as LS

main :: IO ()
main =
  readFile "samples/test.csv"
    >>= print . parseCsv

type ColName = String

type Row = Int

type Col = Int

data CsvCell a
  = Cell ColName Row Col a
  | NA
  deriving (Show)

instance Functor CsvCell where
  fmap _ NA = NA
  fmap fn (Cell n r c v) = Cell n r c (fn v)

parseCsv :: String -> Maybe [CsvCell String]
parseCsv src = case lines src of
  [] -> Nothing
  (colnames : rows) ->
    Just csv
    where
      -- FIX: list comprehension maps all possible values. change into something narrower.
      csv = [Cell name c r cell | name <- names, c <- indices names, r <- indices rows, cell <- concat cells]
      names = LS.splitOn "," colnames
      cells = map (LS.splitOn ",") rows
      indices xs = [1 .. (length xs)]

getColFromName :: [CsvCell a] -> ColName -> [CsvCell a]
getColFromName [] _ = []
getColFromName [x] colname =
  case x of
    NA -> []
    (Cell name _ _ _) -> [x | name == colname]
getColFromName xs colname =
  filter f xs
  where
    f NA = False
    f (Cell n _ _ _) = n == colname

getCol :: [CsvCell a] -> Col -> [CsvCell a]
getCol [] _ = []
getCol [x] col =
  case x of
    NA -> []
    (Cell _ _ c _) -> [x | c == col]
getCol xs col =
  filter f xs
  where
    f NA = False
    f (Cell _ _ c _) = c == col

getRow :: [CsvCell a] -> Row -> [CsvCell a]
getRow [] _ = []
getRow [x] row = case x of
  NA -> []
  (Cell _ r _ _) -> [x | row == r]
getRow xs row = filter f xs
  where
    f NA = False
    f (Cell _ r _ _) = r == row

getCell :: [CsvCell a] -> Col -> Row -> Maybe (CsvCell a)
getCell [] _ _ = Nothing
getCell [x] col row = case x of
  NA -> Just x
  (Cell _ r c _) ->
    if col == c && r == row
      then Just x
      else Nothing
getCell (x : xs) col row = getCell [x] col row <|> getCell xs col row
