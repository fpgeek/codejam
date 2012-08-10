module StreetTrees where

import System.Environment
import System.IO
import Control.Monad
import Control.Monad.State
import Data.List

main = do
	(srcFilePath:_) <- getArgs
	contents <- readFile srcFilePath
	let caseN = toInt $ head $ lines contents
	    lns 	= tail $ lines contents
	    powers = totalPowers caseN lns
	printPowers powers

data StreetTrees = StreetTrees {
	stCount :: Int,
	stNeedPower :: Int,
	poles :: [Pole]
} deriving (Show)

data Pole = Pole {
	pPower :: Int,
	pCount :: Int
} deriving (Show)

totalPowers :: Int -> [String] -> [Int]
totalPowers n lns = map calcPowers  $ totalStreetTrees n lns

calcPowers :: StreetTrees -> Int
calcPowers st | allNeedPowers <= tPolePower = tPolePower
			  | otherwise					= -1
	where tPolePower = snd $ foldl (calcFunc st) (0,0) polePowers
	      polePowers = sort $ getPolePowers (poles st)
	      calcFunc st (cP,tP) p | allNeedPowers <= tP = (cP, tP)
							    | cP < needPower 		= (cP + p,tP)
							    | otherwise		 	= (p, tP + cP)
	      needPower = stNeedPower st
	      allNeedPowers = needPower * (stCount st)

getPolePowers :: [Pole] -> [Int]
getPolePowers ps = do
	p <- ps
	power <- replicate (pCount p) (pPower p)
	return power

totalStreetTrees :: Int -> [String] -> [StreetTrees]
totalStreetTrees n lns = do
	evalState (sequence $ (replicate n makeStreetTrees)) lns

printPowers :: [Int] -> IO ()
printPowers ps = mapM_ printPower $ zip ps [1..]
	where printPower (p, index) = print $ "case #" ++ (show index) ++ ": " ++ (show p)

makeStreetTrees :: State [String] StreetTrees
makeStreetTrees = state $ \lns -> let [treeNum, poleNum, tpower] = toIntList $ words $ head lns
                                      ps = makePoles $ take poleNum $ tail lns
                                  in (StreetTrees treeNum tpower ps, drop poleNum $ tail lns)

makePoles :: [String] -> [Pole]
makePoles lns = map makePole lns
	where makePole ln = let [p,c] = toIntList $ words ln
	                    in Pole p c

toIntList :: [String] -> [Int]
toIntList xs = map toInt xs

toInt :: String -> Int
toInt s = read s :: Int


------------------------------------------------------------------