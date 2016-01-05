module Main where
-- References:
-- 
-- https://hackage.haskell.org/package/implicit-0.0.5/docs/Graphics-Implicit.html
-- https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language

import           Graphics.Implicit
import           Graphics.Implicit.Definitions
import           Graphics.Implicit.Primitives  (rotate3)
import           Control.Monad                 (join)
import           Options.Applicative
import qualified Options.Applicative           as A
import Paths_haskmas                           (version)
import Data.Version                            (showVersion)

data BaubleLocation = R | L


main :: IO ()
main = join $ execParser optsInfo

optsInfo :: ParserInfo (IO ())
optsInfo = info (helper <*> opts)
      ( fullDesc
     <> header   "Haskmas generates the Haskmas logo of arbitrary size.")

opts :: Parser (IO ())
opts = go <$> versionArg <*> sizeArg <*> classicTreeTypeArg

sizeArg :: Parser Integer
sizeArg = A.option auto
    ( long    "size"
   <> short   'n'
   <> value    3
   <> metavar "N"
   <> help    "The number of components of tree to generate.")

versionArg :: Parser Bool
versionArg = A.switch
    ( long   "version"
   <> short  'v'
   <> help   "Display the version."
   )

classicTreeTypeArg :: Parser Bool
classicTreeTypeArg = A.switch
    ( long   "classic"
   <> short  'c'
   <> help   "Output the \"classic\" tree (size will be ignored)."
   )


-- | Using OpenSCAD to generate STL for now until https://github.com/colah/ImplicitCAD/pull/67 
--   is fixed.
go :: Bool     -- display version
   -> Integer  -- size
   -> Bool     -- output "classic" tree
   -> IO ()
go True _ _    = putStrLn (showVersion version) 
go _    _ True = writeSCAD3 1 "haskmas.scad" (rotate3deg (0,0,90) tree)
go _    n _    = writeSCAD3 1 "haskmas.scad" (rotate3deg (0,0,90) (ntree n))


height = 10

-- | Rotate but specify degrees instead of radians.
rotate3deg (x, y, z) = rotate3 rads
    where
        f x  = x * (pi/180)
        rads = (f x, f y, f z)


-- | the typical haskell logo with some additional connectedness so it prints
--   as a single object.
logo :: SymbolicObj3
logo = union [
    -- /\
      translate (-6.5,  0, 0) $ rotate3deg (0, 0, -16.2) bar
    , translate (   0, 19, 0) $ rotate3deg (0, 0,  16.2) bar
    
    -- connecting bars.
    , translate (  1, 10, 0) $ rect3R 0 (0,0,0) (8, 2, 3)
    , translate (  3, 25, 0) $ rect3R 0 (0,0,0) (8, 2, 3)
    
    -- λ
    , translate (   4,  0, 0) $ rotate3deg (0, 0, -16.2) bar
    , translate (  18, -1, 0) $ rotate3deg (0, 0,  16.2) longBar

    -- equals
    , translate (  15, 10, 0) $ rect3R 0 (0,0,0) (24, 6, height)
    , translate (  13, 22, 0) $ rect3R 0 (0,0,0) (26, 6, height)
    ]
      where
          bar     = rect3R 0 (0, 0, 0) (6, 24, height)
          longBar = rect3R 0 (0, 0, 0) (6, 44, height)


tree :: SymbolicObj3
tree = union [
    -- build the tree
      logoBauble R
    , translate (40, 4, 0) $ scale ( 0.8,  0.8,  0.8) (logoBauble L)
    , translate (72, 5, 0) $ scale (0.64, 0.64, 0.64) (logoBauble L)

    -- put the star on top.
    , translate (92, 17.5, 0) star
    ]


-- | Build a tree of an arbitrary depth.
ntree :: Integer -> SymbolicObj3
ntree k = finalObj
  where
      dec     = 0.8
      ratios  = 0 : [dec^j | j <- [0..(k-2)]]
      -- build up logo structure
      ((lx, ly, lz), objs) = foldl f ((0, 0, 0), []) (zip [0..(k-1)] ratios)
      -- position of logos
      (x,y,z) = (40, 4, 0)
      f :: ((ℝ, ℝ, ℝ), [SymbolicObj3]) -> (Integer, Float) -> ((ℝ, ℝ, ℝ), [SymbolicObj3])
      f ((x', y', z'), xs) (j, r) =
                let newPos = (x' + r*x, y' + r*y, z' + r*z)
                    s      = dec ^ j
                    loc    = if (even j) then R else L
                    obj3   = translate newPos $ scale (s, s, s) (logoBauble loc)
                 in (newPos, obj3 : xs)
      -- star
      (a,b,c)   = (40.5, 24.5, 0)
      starScale = dec ** (fromIntegral (k-3))
      posScale  = dec ** (fromIntegral k)
      starObj   = translate (lx + (posScale * a), ly + (posScale * b), lz + (posScale * c))
                    $ scale (starScale, starScale, starScale) star
      finalObj = union (starObj : objs)


logoBauble :: BaubleLocation -> SymbolicObj3
logoBauble loc =
    case loc of
         R -> union [logo, translate (14,  1, 4) bauble]
         L -> union [logo, translate (-8, 38, 4) bauble]

bauble = sphere 4

-- | Hand-drawn star in 2d.
star2d :: SymbolicObj2
star2d = polygon [
      (   0,   8)
    , (   8,   2)
    , ( 3.6,  11)
    , (  10,  18)
    , (   2,  14)
    , (   0,  25)
    , (  -2,  14)
    , ( -10,  18)
    , (-3.5,  10)
    , (  -7, 2.3)
    ]

-- | Extrude to three dimensions, also rotate around
--   so that it is facing the way we want.
star :: SymbolicObj3
star = rotate3deg (0, 0, -90) $ extrudeR 0 star2d height

