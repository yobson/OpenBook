module Search (getResults, getIName, getIData, Info) where

import Parser (Entry(..))

type Info = Entry

getIName, getIData :: Info -> String
getIName = fst . eTitle
getIData i = concat [printSegment "" (eTitle i)
                    , concatMap (printSegment "Definition") (eDef i)
                    , concatMap (printSegment "Equation")   (eDef i)
                    , concatMap (printSegment "See also")   (eIs i)
                    , concatMap (printSegment "Related")   (eRel i)
                    ]

printSegment :: String -> (String, String) -> String
printSegment title (h,b) = concat ["# ", title, ": ", h, "\n", b, "\n\n"]

data RankPred a = Pred (a -> Bool) | (RankPred a) :|: (RankPred a) | Not (RankPred a)

filterRanked :: RankPred a -> [a] -> [a]
filterRanked (Pred f)  xs       = filter f xs
filterRanked (Not (Pred f)) xs  = filterRanked (Pred (not . f)) xs
filterRanked (Not (f :|: g)) xs = filterRanked ((Not f) :|: (Not g)) xs
filterRanked (Not (Not f)) xs   = filterRanked f xs
filterRanked (f :|: g) xs       = filterRanked f xs ++ filterRanked g (filterRanked (Not f) xs)

buildPreds = foldr1 (\x xs -> x :|: xs) . map Pred

beginsWith :: String -> String -> Bool
beginsWith _ [] = True
beginsWith [] _ = True
beginsWith (x:xs) (y:ys) = x == y && beginsWith xs ys

contains :: String -> String -> Bool
contains [] _ = False
contains _ [] = False
contains (x:xs) (y:ys) | x == y    = beginsWith xs ys || contains (x:xs) ys
                       | otherwise = contains (x:xs) ys
 

findRelated :: [Info] -> Info -> Info
findRelated xs (Entry t d e i r) = Entry t d e i (map out $ filter (\y -> (fst $ t) == mid y ) (concatMap buildTriple xs))
  where buildTriple :: Info -> [(String,String,String)]
        buildTriple info = map (\(x,y) -> (fst $ eTitle info, x, y)) (eIs info)
        mid (a,b,c) = b
        out (a,b,c) = (a,c)


getResults :: [Info] -> String -> [Info]
getResults xs s = map (findRelated xs) (filterRanked (buildPreds [beginsWith s. getIName, contains s . getIName]) xs)
