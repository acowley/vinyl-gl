-- | Keyboard-based 3D camera control.
module Keyboard3D (moveCamera) where
import Data.Foldable (Foldable, foldMap,foldl',fold)
import Data.Monoid (All(..), Any(..))
import qualified Data.Set as S
import Linear (Conjugate, Epsilon, V3(..))
import Graphics.UI.GLFW (Key(..))
import Graphics.GLUtil.Camera3D (Camera, pan, roll, tilt, dolly)
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
                  [ ([shift, ctrl, [Key'Left]], roll na)
                  , ([shift, ctrl, [Key'Right]], roll pa)
                  , ([shift, [Key'Left]], pan pa)
                  , ([shift, [Key'Right]], pan na)
                  , ([shift, [Key'Up]], tilt pa)
                  , ([shift, [Key'Down]], tilt na) 
                  , ([[Key'Left]], dolly (V3 np 0 0))
                  , ([[Key'Right]], dolly (V3 pp 0 0))
                  , ([[Key'Up]], dolly (V3 0 0 np))
                  , ([[Key'Down]], dolly (V3 0 0 pp))
                  , ([[Key'PageUp]], dolly (V3 0 pp 0))
                  , ([[Key'PageDown]], dolly (V3 0 np 0)) ]
                  (keysPressed ui)
  where shift = [Key'LeftShift, Key'RightShift]
        ctrl = [Key'LeftControl, Key'RightControl]

        -- Normalize speeds to 60Hz update
        timeScale = realToFrac $ timeStep ui * 60 
        pp = 0.08 * timeScale -- 1D speed
        np = negate pp
        pa = 2 * timeScale    -- angular step
        na = negate pa
