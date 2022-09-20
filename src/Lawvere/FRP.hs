{-# LANGUAGE Arrows #-}
{-# LANGUAGE CPP #-}

module Lawvere.FRP where

import Data.MonadicStreamFunction.Core hiding (reactimate)
import FRP.BearRiver as Yampa
import Protolude
import SDL hiding ((^-^), (^/))

say :: Text -> IO ()
say = putStrLn

main :: IO ()
main = do
  initializeAll
  window <- createWindow "LAWVERE" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  run renderer
  destroyWindow window

run :: Renderer -> IO ()
run renderer =
  reactimate
    (pure ())
    (\_ -> pure (0.01, Just ()))
    (\_ e -> render renderer e)
    bothPos

-- Front starts at position 300, and has constant velocity 20
frontPos :: SF Identity () Float
frontPos = (+ 300) ^<< integral <<^ const 20

frontVel :: SF Identity () Float
frontVel = derivative' <<< frontPos

egoVel :: SF Identity (Float, Float) Float
egoVel = _
  

-- Ego starts at position 60
egoPos :: SF Identity () Float
egoPos = (+ 60) ^<< integral <<< egoVel

ease :: Float -> Float
ease x | x <= 0 = 0
       | x >= 1 = 1
       | otherwise = -(cos (x * pi) - 1) / 2

mix :: (Float, Float, Float) -> Float
mix (x, a, b) = x * a + (1 - x) * b

bothPos :: SF Identity () (Float, Float, Float, Float)
bothPos = proc () -> do
  eV <- egoVel -< ()
  eX <- egoPos -< ()
  fV <- frontVel -< ()
  fX <- frontPos -< ()
  returnA -< (eV, eX, fV, fX)

-- sf :: SF Identity a (Float, Float)
-- sf = bouncingBall (100.0 :: Float) 0.0

-- bouncingBall :: (Monad m, VectorSpace t s, Ord t, Fractional t) => t -> t -> SF m a (t, t)
-- bouncingBall p0 v0 =
--   switch
--     ( proc (_) -> do
--         (p, v) <- fallingBall p0 v0 -< ()
--         bounce <- edge -< (p <= 0 && v < 0)
--         returnA -< ((p, v), bounce `tag` (p, v))
--     )
--     (\(p, v) -> bouncingBall p (- v))

-- fallingBall :: (VectorSpace b s, Monad m, Fractional b) => b -> b -> MSF (ClockInfo m) () (b, b)
-- fallingBall p0 v0 = proc () -> do
--   v <- (v0 +) ^<< integral -< (-99.8)
--   p <- (p0 +) ^<< integral -< v
--   returnA -< (p, v)

render :: Renderer -> (Float, Float, Float, Float) -> IO Bool
render renderer _inp@(_egoV, egoX, _frontV, frontX) = do
  -- say $ show inp
  events <- pollEvents
  let eventIsQPress sdlEvent =
        case eventPayload sdlEvent of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed
              && keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  rendererDrawColor renderer $= V4 0 0 255 255
  clear renderer
  rendererDrawColor renderer $= V4 255 0 255 255
  drawRect renderer (Just (Rectangle (P (V2 (round egoX) 100)) (V2 10 10)))
  drawRect renderer (Just (Rectangle (P (V2 (round frontX) 100)) (V2 10 10)))
  present renderer
  threadDelay 1000
  pure qPressed

-- Like Yampa's derivative, except simply returns derivative 0 for the first
-- sample.
derivative' :: (Monad m, VectorSpace a s) => SF m a a
derivative' = proc a -> do
  dt <- constM ask -< ()
  aOld_ <- iPre' -< a
  returnA -< case aOld_ of
               Just aOld -> (a ^-^ aOld) ^/ realToFrac dt
               Nothing -> zeroVector

-- | Delay a signal by one sample, returning 'Nothing' for the first sample.
iPre' :: Monad m => MSF m a (Maybe a)
iPre' = feedback Nothing $ arr go
  where
    go (a, b) = (b, Just a)
