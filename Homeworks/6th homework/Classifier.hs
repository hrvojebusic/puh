module Classifier
( nbDecide
, nbDecideAll
) where

import CSVUtils
import Data.List
import Data.Ord

-- Decision triplet.
-- Stores necessary decision informations.
-- 1.) Decision identifier
-- 2.) Number of decision occurrences
-- 3.) Decision's probability
type DTriplet = (String, Int, Double)

-- Feature.
-- Feature is defined by it's identifier and set
-- of decision triplets, which link feature to
-- all decisions that can occurr with various
-- probabilities in corelation to feature.
type Feature = (String, [DTriplet])

-- Decision - decision for given classifier.
type Decision = String

-- Functions tells whether decision is already present
-- in decision set.
containsDecision :: String -> [DTriplet] -> Bool
containsDecision pd = any (\(d,_,_) -> pd == d)

-- Retrieves decision 'd' from decision set. If
-- no such decision exists, error is raised.
getDecision :: String -> [DTriplet] -> DTriplet
getDecision d [] = error $ "No such decision " ++ d
getDecision pd ((d,o,p):ds)
  | pd == d = (d,o,p)
  | otherwise = getDecision pd ds

-- Stores decision in decision map - if decision is
-- already present in map, it's occurrence counter will
-- be incremented, otherwise new DTriplet will be stored.
putDecision :: String -> [DTriplet] -> [DTriplet]
putDecision d ds
  | containsDecision d ds = incDecisionOccurrence d ds
  | otherwise = (d, 1, -1) : ds

-- Increments occurrence counter for decision passed
-- in decision set.
incDecisionOccurrence :: String -> [DTriplet] -> [DTriplet]
incDecisionOccurrence d [] = error $ "No such decision " ++ d
incDecisionOccurrence pd ((d,o,p):xs)
  | pd == d = (d, o + 1, p) : xs
  | otherwise = (d,o,p) : incDecisionOccurrence pd xs

-- Updates decision's probability value.
-- 1st arg - decision's identifier
-- 2nd arg - decision set
-- 3rd arg - new decision's probability
updDecisionProbability :: String -> [DTriplet] -> Double -> [DTriplet]
updDecisionProbability _ [] _ = []
updDecisionProbability pn ((n,o,p):ds) pp
  | pn == n = (n,o,pp) : ds
  | otherwise = (n,o,p) : updDecisionProbability pn ds pp

-- Counts occurrence for each decision in passed
-- column and forms set of decision triplets who's
-- probability is not yet set.
countOccurrence :: [Field] -> [DTriplet]
countOccurrence = co []
  where
    co :: [DTriplet] -> [Field] -> [DTriplet]
    co = foldl (flip putDecision)

-- Computes a priori probability for decisions in
-- passed decision set, where first argument given is
-- total number of decisions - divisor of occurrence
-- counter for each decision in set.
computeProbability :: Int -> [DTriplet] -> [DTriplet]
computeProbability _ [] = []
computeProbability n ((f,o,_) : xs) =
  (f, o, fromIntegral o / fromIntegral n) : computeProbability n xs

-- Computes a priori probability of decisions in CSV
-- file passed - values in last column of CSV file.
computeAPriori :: CSV -> [DTriplet]
computeAPriori [] = error "No CSV file was provided"
computeAPriori csv = dec
  where
    col = colFields (length (head csv) - 1) (tail csv)
    dec = computeProbability (length csv - 1) $ countOccurrence col

-- Creates fetature 'f' with decision 'd' whose
-- occurrence is initialy set to one.
createFeature :: String -> String -> Feature
createFeature f d = (f, [(d,1,-1)])

-- For specified feature increments specified decision
-- occurrence.
-- 1st arg - specified feature
-- 2nd arg - feature set
-- 3rd arg - specified decision
incFeatureDecision :: String -> [Feature] -> String -> [Feature]
incFeatureDecision f [] d = [createFeature f d]
incFeatureDecision f ((fn,ds):xs) d
  | f == fn = (fn, putDecision d ds) : xs
  | otherwise = (fn, ds) : incFeatureDecision f xs d

-- Consumes one entry (row) in CSV file and updates
-- collection of features.
-- 1st arg - entry (row) without last column
-- 2nd arg - entry (row) decision (last column)
featuresFromEntry :: Entry -> Field -> [Feature] -> [Feature]
featuresFromEntry [] _ fs = fs
featuresFromEntry (x:xs) d fs = featuresFromEntry xs d (incFeatureDecision x fs d)

-- Consumes CSV file and outputs collection of retrieved
-- features. Features returned have set number of occurrences
-- with decisions they occurred with, for each decision they
-- ever occurred with.
featuresFromCSV :: CSV -> [Feature]
featuresFromCSV [] = error "No CSV file was provided"
featuresFromCSV csv = ccsv (tail csv) []
  where
    ccsv :: CSV -> [Feature] -> [Feature]
    ccsv xss fs = foldl (\fs xs -> featuresFromEntry (init xs) (last xs) fs) fs xss

-- Computes probabilities for feature when each decision is made
-- using set of a priori decisions from overall decisions.
setOneFeature :: Feature -> [DTriplet] -> Feature
setOneFeature (f,fds) ds = (f, cff fds ds)
  where
    cff :: [DTriplet] -> [DTriplet] -> [DTriplet]
    cff fds [] = fds
    cff fds ((n,o,p):ds)
      | containsDecision n fds = cff fds' ds
      | otherwise = cff ((n,0,0):fds) ds
      where
        (_,o1,_) = getDecision n fds
        p1 = fromIntegral o1 / fromIntegral o
        fds' = updDecisionProbability n fds p1

-- Computes probabilities for multiple features.
setMultipleFeatures :: [Feature] -> [DTriplet] -> [Feature]
setMultipleFeatures [] _ = []
setMultipleFeatures fs [] = error "No decisions were given for computation"
setMultipleFeatures fs ds = map (`setOneFeature` ds) fs

-- Filters features passed by only returning those whose
-- identifier is element of second argument passed.
getFeatures :: [Feature] -> [String] -> [Feature]
getFeatures [] _ = []
getFeatures (f:fs) xs
  | fst f `elem` xs = f : getFeatures fs xs
  | otherwise = getFeatures fs xs

--------------------------------------------------------------------------------
-- Following functions are utilized during result computation for given
-- classifier (identifier vector).
--------------------------------------------------------------------------------

-- Final decision for given classifier that consists
-- of decision's identifier and probability computed
-- for given classifier.
type FDecision = (String, Double)

-- Function tells whether there is final decision with given
-- identifier already present in set of final decisions.
containsFDecision :: String -> [FDecision] -> Bool
containsFDecision _ [] = False
containsFDecision pn ((n,p):xs)
  | pn == n = True
  | otherwise = containsFDecision pn xs

-- Updates final decision specified by identifier in set of
-- final decisions by multiplying it's probability with
-- provided probability.
-- 1st arg - final decision's identifier
-- 2nd arg - set of final decisions
-- 3rd arg - probability (multiplier)
updateFDecision :: String -> [FDecision] -> Double -> [FDecision]
updateFDecision pn [] _ = error $ "No such final decision " ++ pn
updateFDecision pn ((n,p):xs) pp
  | pn == n = (n, p * pp) : xs
  | otherwise = (n,p) : updateFDecision pn xs pp

-- Updates values of final decision by inspecting one
-- decision triplet.
foldOne :: [FDecision] -> DTriplet -> [FDecision]
foldOne fds t@(n,_,p)
  | containsFDecision n fds = updateFDecision n fds p
  | otherwise = (n,p) : fds

-- Updates values of final decision by inspecting multiple
-- decision triplets.
foldMultiple :: [DTriplet] -> [FDecision] -> [FDecision]
foldMultiple ts ds = foldl foldOne ds ts

-- 'Walks' through provided decision triplets and updates
-- final decisions.
walkDecisions :: [DTriplet] -> [FDecision] -> [FDecision]
walkDecisions = foldMultiple

-- 'Walks' through provided features and updates final decisions
-- provided with information stored in decision-triplets of features.
walkFeatures :: [Feature] -> [FDecision] -> [FDecision]
walkFeatures fs ds = foldl (flip foldMultiple) ds (map snd fs)

-- Retrieves final decision with highest probability - decision
-- for given classifier.
sortAndTake :: [FDecision] -> Decision
sortAndTake = fst . maximumBy (comparing snd)

nbDecide :: CSV -> [String] -> Decision
nbDecide csv xs = r
  where
    decisionsWithProbabilities = computeAPriori csv
    featuresWithProbabilities =
      setMultipleFeatures (featuresFromCSV csv) decisionsWithProbabilities
    filteredFeatures = getFeatures featuresWithProbabilities xs
    r = sortAndTake $
        walkFeatures filteredFeatures $
        walkDecisions decisionsWithProbabilities []

nbDecideAll :: CSV -> [[String]] -> [Decision]
nbDecideAll csv = foldr (\xs acc -> nbDecide csv xs : acc) []
