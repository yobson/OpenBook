{-# LANGUAGE FlexibleInstances #-}

module Search (getResults, getIName, getIData, Info, ButtonType(..)) where

import Brick
import Parser (Entry(..))
import Data.Char (toUpper)

type Info = Entry
data ButtonType = Extern String | Intern String deriving (Eq, Ord)

getIName = fst . eTitle

getIData :: Info -> Widget ButtonType
getIData i = vBox   [printSegment "" (eTitle i)
                    , vBox $ map (printSegment "Definition") (eDef i)
                    , vBox $ map (printSegment "Equation")   (eEqn i)
                    , vBox $ map (printSegment "See also")   (eIs i)
                    , vBox $ map (printSegment "Related")   (eRel i)
                    ]

printSegment :: String -> (String, String) -> Widget ButtonType
printSegment title@"See also" (h,b) = clickable (Intern h) $ str $ concat ["# ", title, if title == "" then "" else ": ", h, "\n", b, "\n\n"]
printSegment title@"Related"  (h,b) = clickable (Intern h) $ str $ concat ["# ", title, if title == "" then "" else ": ", h, "\n", b, "\n\n"]
printSegment title (h,b)            = str $ concat ["# ", title, if title == "" then "" else ": ", h, "\n", b, "\n\n"]

data RankPred a = Pred (a -> Bool) | (RankPred a) :|: (RankPred a) | Not (RankPred a)

filterRanked :: RankPred a -> [a] -> [a]
filterRanked (Pred f)  xs       = filter f xs
filterRanked (Not (Pred f)) xs  = filterRanked (Pred (not . f)) xs
filterRanked (Not (f :|: g)) xs = filterRanked ((Not f) :|: (Not g)) xs
filterRanked (Not (Not f)) xs   = filterRanked f xs
filterRanked (f :|: g) xs       = filterRanked f xs ++ filterRanked g (filterRanked (Not f) xs)

buildPreds = foldr1 (\x xs -> x :|: xs) . map Pred

class EqIsh a where
  (===) :: a -> a -> Bool
  
instance EqIsh Char where
  a === b = (toUpper a) == (toUpper b)

instance EqIsh String where
  a === b = foldr (&&) True (zipWith (===) a b)

beginsWith :: String -> String -> Bool
beginsWith _ [] = True
beginsWith [] _ = True
beginsWith (x:xs) (y:ys) = x === y && beginsWith xs ys

contains :: String -> String -> Bool
contains [] _ = False
contains _ [] = False
contains (x:xs) (y:ys) | x === y    = beginsWith xs ys || contains (x:xs) ys
                       | otherwise = contains (x:xs) ys
 

findRelated :: [Info] -> Info -> Info
findRelated xs (Entry t d e i r) = Entry t d e i (map out $ filter (\y -> (fst $ t) == mid y ) (concatMap buildTriple xs))
  where buildTriple :: Info -> [(String,String,String)]
        buildTriple info = map (\(x,y) -> (fst $ eTitle info, x, y)) (eIs info)
        mid (a,b,c) = b
        out (a,b,c) = (a,c)




getResults :: [Info] -> String -> [Info]
getResults xs "*" = map (findRelated xs) xs
getResults xs s = map (findRelated xs) (filterRanked (buildPreds [beginsWith s. getIName, contains s . getIName]) xs)
