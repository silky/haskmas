module Main where

import Graphics.Implicit
import Graphics.Implicit.Definitions
import Graphics.Implicit.Primitives  (rotate3)

-- https://hackage.haskell.org/package/implicit-0.0.5/docs/Graphics-Implicit.html
-- https://en.wikibooks.org/wiki/OpenSCAD_User_Manual/The_OpenSCAD_Language

height = 10

rotate3deg (x, y, z) = rotate3 rads
    where
        f x  = x * (pi/180)
        rads = (f x, f y, f z)

-- | the typical haskell logo with some additional connectedness so it prints
--   as a single object.
logo = union [
    -- /\
      translate (-6.5,  0, 0) $ rotate3deg (0, 0, -16.2) bar
    , translate (   0, 19, 0) $ rotate3deg (0, 0,  16.2) bar
    
    -- connecting bars.
    , translate (  3, 10, 0) $ rect3R 0 (0,0,0) (3, 2, 3)
    , translate (  3, 25, 0) $ rect3R 0 (0,0,0) (8, 2, 3)
    
    -- λ
    , translate (   4,  5, 0) $ rotate3deg (0, 0, -16.2) bar
    , translate (  18, -1, 0) $ rotate3deg (0, 0,  16.2) longBar

    -- equals
    , translate (  15, 10, 0) $ rect3R 0 (0,0,0) (24, 6, 10)
    , translate (  13, 22, 0) $ rect3R 0 (0,0,0) (26, 6, 10)
    ]
      where
          bar     = rect3R 0 (0, 0, 0) (6, 24, height)
          longBar = rect3R 0 (0, 0, 0) (6, 44, height)

--
tree = union [
    -- build the tree
      logoBauble R
    , translate (40, 4, 0) $ scale ( 0.8,  0.8,  0.8) (logoBauble L)
    , translate (72, 5, 0) $ scale (0.64, 0.64, 0.64) (logoBauble L)

    -- put the star on top.
    , translate (93, 18, 0) star
    ]

data BaubleLocation = R | L
logoBauble loc =
    case loc of
         R -> union [logo, translate (14,  1, 4) bauble]
         L -> union [logo, translate (-8, 38, 4) bauble]

bauble = sphere (4)

-- | Hand-drawn star in 2d.
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
star = rotate3deg (0, 0, -90) $ extrudeR 0 star2d 10

main :: IO ()
-- main = writeSTL 1 "haskmas.stl" tree
main = writeSCAD3 1 "haskmas.scad" tree


