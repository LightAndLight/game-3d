{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# language RecordWildCards #-}
{-# language RecursiveDo #-}
module Main where

import Reflex
import Reflex.Gloss
import Control.Applicative (liftA2)
import Control.Monad (void, join)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Semigroup ((<>))
import Graphics.Gloss (Display(..), Picture, white, blank, line, lineLoop, color, black, circle)
import Linear.Epsilon (Epsilon)
import Linear.Matrix ((!*!), (!*), M44, M24, mkTransformation)
import Linear.Metric (dot, normalize, norm)
import Linear.Projection (perspective)
import Linear.Quaternion (axisAngle, rotate)
import Linear.V2 (V2(..))
import Linear.V3 (V3(..), cross)
import Linear.V4 (V4(..))
import Linear.Vector ((^*))

data Triangle a = Triangle a a a
  deriving (Functor, Foldable, Traversable)

newtype Mesh a = Mesh { unMesh :: [Triangle (V4 a)] }

cube
  :: Num a
  => V3 a -- ^ Position, top-left close corner
  -> a -- ^ Side length
  -> Mesh a
cube (V3 x y z) side =
  Mesh
  -- front
  [ Triangle (V4 x y z 1) (V4 (x + side) y z 1) (V4 x (y-side) z 1) -- tlc trc blc
  , Triangle (V4 (x + side) y z 1) (V4 (x+side) (y-side) z 1) (V4 x (y-side) z 1) -- trc brc blc

  -- back
  , Triangle (V4 x y (z-side) 1) (V4 (x + side) y (z-side) 1) (V4 x (y-side) (z-side) 1) -- tlf trf blf
  , Triangle (V4 (x + side) y (z-side) 1) (V4 (x+side) (y-side) (z-side) 1) (V4 x (y-side) (z-side) 1) -- trf brf blf

  -- left
  , Triangle (V4 x y z 1) (V4 x (y-side) z 1) (V4 x y (z-side) 1) -- tlc blc tlf
  , Triangle (V4 x (y-side) z 1) (V4 x y (z-side) 1) (V4 x (y-side) (z-side) 1) -- blc tlf blf

  -- right
  , Triangle (V4 (x+side) y z 1) (V4 (x+side) (y-side) z 1) (V4 (x+side) y (z-side) 1) -- trc brc trf
  , Triangle (V4 (x+side) (y-side) z 1) (V4 (x+side) y (z-side) 1) (V4 (x+side) (y-side) (z-side) 1) -- brc trf brf

  -- top
  , Triangle (V4 x y z 1) (V4 (x+side) y z 1) (V4 x y (z-side) 1) -- tlc trc tlf
  , Triangle (V4 (x+side) y z 1) (V4 x y (z-side) 1) (V4 (x+side) y (z-side) 1) -- trc tlf trf

  -- bottom
  , Triangle (V4 x (y-side) z 1) (V4 (x+side) (y-side) z 1) (V4 x (y-side) (z-side) 1) -- blc brc blf
  , Triangle (V4 (x+side) (y-side) z 1) (V4 x (y-side) (z-side) 1) (V4 (x+side) (y-side) (z-side) 1) -- brc blf brf
  ]

square
  :: Num a
  => V3 a -- ^ Position, top-left close corner
  -> a -- ^ Side length
  -> Mesh a
square (V3 x y z) side =
  Mesh
  [ Triangle (V4 x y z 1) (V4 (x + side) y z 1) (V4 x (y-side) z 1) -- tl tr bl
  , Triangle (V4 (x + side) y z 1) (V4 (x+side) (y-side) z 1) (V4 x (y-side) z 1) -- tr br bl
  ]

data Camera a
  = Camera
  { eyeVector :: V3 a
  , gazeVector :: V3 a
  }

nearPlane, farPlane :: Floating a => a
nearPlane = 1
farPlane = 1024

-- |
-- Given a camera with a position and gaze in world coordinates,
-- transform world coordinates such that the camera is at the origin,
-- looking straight ahead
mCamera :: (Epsilon a, Floating a) => Camera a -> M44 a
mCamera Camera{..} =
  -- mkTransformation (axisAngle axis angle) (-eyeVector + V3 0 0 nearPlane) -- +near
  mkTransformation (axisAngle axis angle) 0 !*!
  mkTransformation (axisAngle 0 0) (-eyeVector + V3 0 0 nearPlane) -- +near
  where
    x = gazeVector
    y = V3 0 0 1
    axis = normalize $ cross gazeVector y
    angle = acos $ dot gazeVector y / norm x -- norm y = 1

mFrustum :: Floating a => M44 a
mFrustum = perspective (60/180*pi) 1.333 nearPlane farPlane

mProject :: Num a => M44 a
mProject =
  V4
    (V4 640 0   0    0)
    (V4 0   480 0    0)
    (V4 0   0   1    0)
    (V4 0   0   (-1) 0)

renderMesh :: Camera Float -> Mesh Float -> Picture
renderMesh camera =
  foldMap
    (lineLoop .
     fmap ((\(V4 x y _ w) -> (x/w, y/w)) . ((mProject !*! mFrustum !*! mCamera camera) !*)) .
     toList) .
    unMesh

main :: IO ()
main =
  playReflex (InWindow "game 3d" (640, 480) (10, 10)) white 24 $
  \eRefresh inputs -> do
    ePostBuild <- getPostBuild
    let
      eForwardPressed = select inputs $ GE_Key (Just $ Char 'w') (Just Down) Nothing
      eForwardReleased = select inputs $ GE_Key (Just $ Char 'w') (Just Up) Nothing

      eBackwardPressed = select inputs $ GE_Key (Just $ Char 's') (Just Down) Nothing
      eBackwardReleased = select inputs $ GE_Key (Just $ Char 's') (Just Up) Nothing

      eLeftPressed = select inputs $ GE_Key (Just $ Char 'a') (Just Down) Nothing
      eLeftReleased = select inputs $ GE_Key (Just $ Char 'a') (Just Up) Nothing

      eRightPressed = select inputs $ GE_Key (Just $ Char 'd') (Just Down) Nothing
      eRightReleased = select inputs $ GE_Key (Just $ Char 'd') (Just Up) Nothing

      eTurnLeftPressed =
        void (select inputs $ GE_Key (Just $ SpecialKey KeyLeft) (Just Down) Nothing) <>
        void (select inputs $ GE_Key (Just $ Char 'q') (Just Down) Nothing)
      eTurnLeftReleased =
        void (select inputs $ GE_Key (Just $ SpecialKey KeyLeft) (Just Up) Nothing) <>
        void (select inputs $ GE_Key (Just $ Char 'q') (Just Up) Nothing)

      eTurnRightPressed =
        void (select inputs $ GE_Key (Just $ SpecialKey KeyRight) (Just Down) Nothing) <>
        void (select inputs $ GE_Key (Just $ Char 'e') (Just Down) Nothing)
      eTurnRightReleased =
        void (select inputs $ GE_Key (Just $ SpecialKey KeyRight) (Just Up) Nothing) <>
        void (select inputs $ GE_Key (Just $ Char 'e') (Just Up) Nothing)

      vel = 3
      avel = 3

    dMoveForward <- holdDyn False $ leftmost [True <$ eForwardPressed, False <$ eForwardReleased]
    dMoveBackward <- holdDyn False $ leftmost [True <$ eBackwardPressed, False <$ eBackwardReleased]
    dMoveLeft <- holdDyn False $ leftmost [True <$ eLeftPressed, False <$ eLeftReleased]
    dMoveRight <- holdDyn False $ leftmost [True <$ eRightPressed, False <$ eRightReleased]
    dTurnRight <- holdDyn False $ leftmost [True <$ eTurnRightPressed, False <$ eTurnRightReleased]
    dTurnLeft <- holdDyn False $ leftmost [True <$ eTurnLeftPressed, False <$ eTurnLeftReleased]

    rec
      dCameraAngularVelocity <-
        holdDyn (axisAngle 0 0) $
        (\tl tr ->
           foldr (.) id
           [ if tl then (+) (axisAngle (V3 0 1 0) (-avel/180*pi)) else id
           , if tr then (+) (axisAngle (V3 0 1 0) (avel/180*pi)) else id
           ]
           (axisAngle 0 0)) <$>
        current dTurnLeft <*>
        current dTurnRight <@
        eRefresh

    rec
      dCameraDirection <-
        fmap (fmap normalize) .
        holdDyn (V3 0 0 1) $
        rotate <$>
        current dCameraAngularVelocity <*>
        current dCameraDirection <@
        eRefresh

    rec
      dCameraVelocity <-
        holdDyn (V3 0 0 0) $
        (\dir f b l r ->
           foldr (.) id
           [ if f then (+) (dir ^* vel) else id
           , if b then (+) (dir ^* negate vel) else id
           , if l then (+) (normalize (cross (V3 0 1 0) dir) ^* negate vel) else id
           , if r then (+) (normalize (cross (V3 0 1 0) dir) ^* vel) else id
           ]
           (V3 0 0 0)) <$>
         current dCameraDirection <*>
         current dMoveForward <*>
         current dMoveBackward <*>
         current dMoveLeft <*>
         current dMoveRight <@
         eRefresh

    rec
      dCameraPosition <-
        holdDyn (V3 0 0 0) $
        (+) <$>
        current dCameraPosition <*>
        current dCameraVelocity <@
        eRefresh

    let
      dCamera = Camera <$> dCameraPosition <*> dCameraDirection
      pic = renderMesh <$> dCamera <*> pure (cube (V3 0 0 500) 100)
    dPic <- join <$> holdDyn (pure blank) (pic <$ ePostBuild)
    pure (dPic, never)
