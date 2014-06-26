-- | Keyboard-based camera control.
module Keyboard2D (moveCamera) where
import Data.Foldable (Foldable, foldMap,foldl',fold)
import Data.Monoid (All(..), Any(..))
import qualified Data.Set as S
import Linear (Conjugate, Epsilon, V2(..))
import Graphics.UI.GLFW (Key(..))
import Graphics.GLUtil.Camera2D (Camera, track, roll)
import Window (UI(..))

-- | Evaluate a boolean formula in conjunctive normal form (CNF) by
-- applying the predicate to each atom according to the logic of its
-- nesting in the formula.
cnf :: (Foldable s, Foldable t) => s (t Bool) -> Bool
cnf = getAll . foldMap (All . getAny . foldMap Any)

-- | Perform a left fold over a set of guarded update functions,
-- evaluating the guards left-to-right. For each guard that passes,
-- its associated update function is composed into a final composite
-- update function.
cnfEndo :: (k -> s -> Bool) -> (k -> s -> s) -> [([[k]], a -> a)] -> s -> a -> a
cnfEndo p del = go
  where go [] _ = id
        go ((k,f):fs) s | cnf (fmap (fmap (`p` s)) k) = go fs (delAll k s) . f
                        | otherwise = go fs s
        delAll k s = foldl' (flip del) s (fold k)

-- | Translate and rotate a 'Camera' based on 'UI' input.
moveCamera :: (Conjugate a, Epsilon a, RealFloat a) => UI -> Camera a -> Camera a
moveCamera ui = cnfEndo S.member S.delete 
                  [ ([shift, [Key'Left]], roll na)
                  , ([shift, [Key'Right]], roll pa)
                  , ([[Key'Left]], track (V2 np 0))
                  , ([[Key'Right]], track (V2 pp 0))
                  , ([[Key'Up]], track (V2 0 pp))
                  , ([[Key'Down]], track (V2 0 np)) ]
                  (keysPressed ui)
  where shift = [Key'LeftShift, Key'RightShift]

        -- Normalize speeds to 60Hz update
        timeScale = realToFrac $ timeStep ui * 60 
        pp = 0.08 * timeScale -- 1D speed
        np = negate pp
        pa = 2 * timeScale    -- angular step
        na = negate pa
