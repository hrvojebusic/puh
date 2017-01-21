module CSVUtils
( Separator
, Document
, CSV
, Entry
, Field
, parseCSV
, showCSV
, colFields
, readCSV
, writeCSV
) where

import Data.List
import Data.List.Split

type Separator = String
type Document = String
type CSV = [Entry]
type Entry = [Field]
type Field = String

-- Function checks if first string passed as argument is
-- prefix of second string passed as argument.
prefix :: String -> String -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

-- Function checks if first string passed as argument is
-- substring of second string passed as argument.
substring :: String -> String -> Bool
substring _ [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

-- Function checks if separator is used in document.
checkEntrySeparator :: Separator -> Document -> Bool
checkEntrySeparator = substring

-- Helper function which performs actual check if CSV file is
-- well-formed by comparing number of columns for each row.
checkFieldNumber' :: Int -> CSV -> Bool
checkFieldNumber' _ [] = True
checkFieldNumber' n (xs:xss)
  | length xs == n = checkFieldNumber' n xss
  | otherwise = False

-- Function checks if CSV is well-formed, meaning that entries have
-- same number of fields accross all rows.
checkFieldNumber :: CSV -> Bool
checkFieldNumber [] = True
checkFieldNumber (ys:yss) = checkFieldNumber' (length ys) yss

-- Function checks if column for given index exists in passed CSV.
hasField :: Int -> CSV -> Bool
hasField _ [] = error "Empty CSV"
hasField n csv@(xs:_)
  | checkFieldNumber csv = n > -1 && n < length xs
  | otherwise = error "The CSV file is not well-formed"

-- Function takes a separator and a string representing a CSV
-- document and returns a CSV representation of the document.
parseCSV :: Separator -> Document -> CSV
parseCSV [] _ = error "No separator was provided"
parseCSV s doc
  | not $ checkEntrySeparator s doc = error $ "The separator '" ++ s ++
                                              "' does not occur in the text"
  | checkFieldNumber csv = csv
  | otherwise = error "The CSV file is not well-formed"
  where
    csv = map (splitOn s) $ lines doc

-- Function takes a separator and a CSV representation of a
-- document and creates a CSV string from it.
showCSV :: Separator -> CSV -> Document
showCSV s csv
  | checkFieldNumber csv = intercalate "\n" $ map (intercalate s) csv
  | otherwise = error "The CSV file is not well-formed"

-- Function takes a CSV document and a field number and
-- returns a list of fields in that column.
colFields :: Int -> CSV -> [Field]
colFields n csv
  | hasField n csv = getCol n csv
  | otherwise = error $ "There is no column " ++ show n ++ " in the CSV document"
  where
    getCol :: Int -> CSV -> [Field]
    getCol _ [] = []
    getCol i (xs:xss) = xs !! i : getCol n xss

-- IO function takes a file path and a separator and returns
-- the CSV representation of the file (wrapped due to impurity).
readCSV :: Separator -> FilePath -> IO CSV
readCSV s filePath = do
  contents <- readFile filePath
  return $ parseCSV s contents

-- Function takes a separator, a file path, and a CSV document
-- and writes the document into a file.
writeCSV :: Separator -> FilePath -> CSV -> IO ()
writeCSV separator filePath csv = do
  let
    doc = showCSV separator csv
  writeFile filePath doc
