-- | The "computation" module contains a library of computations to be used in Style files.
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, FlexibleContexts #-}
module Computation where
import Shapes
import Utils
import Functions
import qualified Data.Map.Strict as M
-- import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import System.Random
import System.Random.Shuffle
import Data.List (sort)

-- Temporary solution: register every single different function type as you write it
-- and pattern-match on it later.
-- Fix typechecking: applyComputation in Runtime is essentially doing ad-hoc typechecking
-- TODO figure out how arguments work
-- Doesn't deal well with polymorphism (all type variables need to go in the datatype)

type Point a = (a, a)

data Computation a = ComputeColor (() -> Color a)
                   | ComputeColorArgs (String -> a -> Color a)
                   | ComputeRadius (Circ a -> a -> a)
                   | ComputeColorRGBA (a -> a -> a -> a -> Color a)
                   | ComputeSurjection (StdGen -> Integer -> Point a -> Point a -> ([Point a], StdGen))
                   | TestNone

-- | 'computationDict' stores a mapping from the name of computation to the actual implementation
-- | All functions must be registered
computationDict :: (Real a, Floating a, Show a, Ord a, Random a) => M.Map String (Computation a)
computationDict = M.fromList flist
    where
        flist :: (Real a, Floating a, Show a, Ord a, Random a) => [(String, Computation a)]
        flist = [
                        ("computeColor", ComputeColor computeColor), -- pretty verbose
                        ("computeColor2", ComputeColor computeColor2),
                        ("computeColorArgs", ComputeColorArgs computeColorArgs),
                        ("computeRadiusAsFrac", ComputeRadius computeRadiusAsFrac),
                        ("computeColorRGBA", ComputeColorRGBA computeColorRGBA),
                        ("computeSurjection", ComputeSurjection computeSurjection)
                ]

-- Generate n random values uniformly randomly sampled from interval and return generator.
randomsIn :: (Real a, Floating a, Show a, Ord a, Random a) => StdGen -> Integer -> (a, a) -> ([a], StdGen)
randomsIn g 0 _        =  ([], g)
randomsIn g n interval = let (x, g') = randomR interval g in -- First value
                         let (xs, g'') = randomsIn g' (n - 1) interval in -- Rest of values
                         (x : xs, g'')

-- Given a generator, number of points, and lower left and top right of bbox, return points for a surjection.
-- Points generated lie in the bbox given, whether in math space or screen space
-- TODO pass randomness around in Runtime
computeSurjection :: (Real a, Floating a, Show a, Ord a, Random a) => StdGen -> Integer -> Point a -> Point a -> ([Point a], StdGen)
computeSurjection g numPoints (lowerx, lowery) (topx, topy) =
                  if numPoints < 2 then error "Surjection needs to have >= 2 points"
                  else let (xs, g') = randomsIn g numPoints (lowerx, topx) in -- In interval, not nec endpts
                       let xs_increasing = sort xs in

                       let (ys_inner, g'') = randomsIn g' (numPoints - 2) (lowery, topy) in
                       let ys = lowery : ys_inner ++ [topy] in -- Include endpts so function is onto
                       let ys_perm = shuffle' ys (length ys) g'' in -- Random permutation. TODO return g3?

                       (zip xs_increasing ys_perm, g'') -- len xs == len ys

-- | No arguments for now, to avoid typechecking
-- Does this only work in gloss?
computeColor :: (Real a, Floating a, Show a, Ord a) => () -> Color a
-- computeColor () = Colo { redc = 0, greenc = 0, bluec = 0 * 0, opacityc = 50 }
computeColor () = makeColor 0.5 0.1 (0.2 / 3) 0.5

computeColor2 :: (Real a, Floating a, Show a, Ord a) => () -> Color a
computeColor2 () = makeColor (0.1 * 0.5) 0.1 0.5 0.5

computeColorArgs :: (Real a, Floating a, Show a, Ord a) => String -> a -> Color a
computeColorArgs ref1 mag = trace ("computeColorArgs " ++ ref1) $
                                 makeColor (scale mag) (scale mag) (scale mag) 0.5
                 where scale c = c * 0.1

-- Compute the radius of the inner set to always be half the radius of the outer set, overriding optimization.
computeRadiusAsFrac :: (Real a, Floating a, Show a, Ord a) => Circ a -> a -> a
computeRadiusAsFrac circ mag = trace ("computeRadiusAsFrac") $ mag * (r circ)

computeColorRGBA :: (Real a, Floating a, Show a, Ord a) => a -> a -> a -> a -> Color a
computeColorRGBA = makeColor
