module Main where

import System.Environment
import Data.List
import Data.Char

main = do
	args <- getArgs
	let inputFilePath = args !! 0
	    outputFilePath = args !! 1
	testCases <- parseCaseTests inputFilePath
	let matchIndexes = map resolveTestCase testCases
	writeFile outputFilePath $ getPrintLine matchIndexes

data TestCase = TestCase { 
	credit :: Int ,
	numOfItems :: Int ,
	items :: [Int]
} deriving (Show)

-- 테스트 케이스를 파싱한다.
parseCaseTests :: FilePath -> IO [TestCase]
parseCaseTests filePath = do
	contents <- readFile filePath
	let tcs = splitList 3 $ tail $ lines contents
	    testCases = map createTestCase tcs
	return testCases

-- 테스트 케이스를 생성한다.
createTestCase :: [String] -> TestCase
createTestCase [c, n, items] = TestCase (read c :: Int) (read n :: Int) (map read $ words items :: [Int])

-- 리스트를 갯수 단위로 나눈다.
splitList :: Int -> [a] -> [[a]]
splitList n [] = []
splitList n xs = (take n xs) : splitList n (drop n xs)

-- 테스트 케이스를 받아 문제를 해결한다.
resolveTestCase :: TestCase -> [Int]
resolveTestCase tc = map (+1) (getIndicies (items tc) matchElems)
	where matchElems = sumSubSeq (filter (<=(credit tc)) $ items tc) (credit tc)

-- 리스트의 아이템 두개의 합이 지정된 값과 같을 경우 두개의 아이템을 리턴한다.
sumSubSeq :: [Int] -> Int -> [Int]
sumSubSeq (x:xs) s | not . null $ matchNums = x:matchNums
	               | otherwise				= sumSubSeq xs s
	where matchNums = filter (\a -> a + x == s) xs
sumSubSeq [] s = []

data GetIndex = GetIndex {
	startIndex :: Int,
	indexList :: [Int],
	list :: [Int]
}

-- 배열안에 여러 인덱스 찾아낸다.
getIndicies :: [Int] -> [Int] -> [Int]
getIndicies xs ys = sort $ indexList $ foldl func (GetIndex 0 [] xs) ys
	where func acc y = case (y `elemIndex` (drop (startIndex acc) (list acc))) of Just a -> GetIndex (a+1) ((a + (startIndex acc)):indexList acc) (list acc)
	                                                                              Nothing -> acc

-- 출력형식에 맞는 문자열을 만들어 준다.
getPrintLine :: [[Int]] -> String
getPrintLine xxs = trim $ unlines $ map messageCase $ zip [1..] xxs
	where messageCase (i,xs) =  "Case #" ++ show i ++ ": " ++ (intercalate " " $ map show xs)
	      trim s | isSpace $ last s = init s
	             | otherwise 		= s