import Debug.Trace
import Control.Monad (forM_)

data Term  = TVar Int | TTag Int | TCon Term Term | Nil deriving (Eq)
type Sub   = [(Int, Term)]
type State = (Sub, Int)
type Goal  = State -> [State]

pretty :: Term -> String
pretty (TVar idx)     = "x" ++ show idx
pretty (TTag lab)     = "#" ++ show lab
pretty (TCon fst snd) = "(" ++ pretty fst ++ "," ++ pretty snd ++ ")"
pretty Nil            = "()"

instance Show Term where
  show = pretty

chase :: Term -> Sub -> Term
chase t@(TVar idx) sub = got (lookup idx sub) where
  got (Just t) = chase t sub
  got Nothing  = t
chase t _ = t

unify :: Term -> Term -> Sub -> Maybe Sub
unify a b sub = {-trace ("unify: " ++ show a ++ " ~ " ++ show b) $-} go (chase a sub) (chase b sub) where
  -- go (TVar a_idx)       (TVar b_idx) | a_idx == b_idx = Just sub
  go (TVar a_idx)       b                             = {-trace ("found: x" ++ show a_idx ++ " = " ++ show b) $-} Just ((a_idx, b) : sub)
  go a                  (TVar b_idx)                  = {-trace ("found: x" ++ show b_idx ++ " = " ++ show a) $-} Just ((b_idx, a) : sub)
  go (TTag a_tag)       (TTag b_tag) | a_tag == b_tag = Just sub
  go (TCon a_fst a_snd) (TCon b_fst b_snd)            = do { x <- go a_fst b_fst; unify a_snd b_snd x }
  go _                  _                             = Nothing

(===) :: Term -> Term -> Goal
(a === b) (s, c) = case unify a b s of
  Just s_ -> [(s_, c)]
  Nothing -> []

meta :: (Term -> Goal) -> Goal
meta f (s, c) = f (TVar c) (s, c + 1)

conj :: Goal -> Goal -> Goal
conj g1 g2 sc = do { x0 <- g1 sc; g2 x0 }

disj :: Goal -> Goal -> Goal
disj g1 g2 sc = g1 sc ++ g2 sc

initial :: State
initial = ([], 0)

-- Helper function to fully resolve a term
resolve :: Term -> Sub -> Term
resolve (TVar idx)     s = case lookup idx s of { Just t_ -> resolve t_ s; Nothing -> (TVar idx) }
resolve (TCon fst snd) s = TCon (resolve fst s) (resolve snd s)
resolve t              _ = t

-- Nat number representation
nZ   = TTag 0
nS n = TCon (TTag 1) n

-- Addition for Nat numbers
add :: Term -> Term -> Term -> Goal
add a b c =
  (disj (conj (a === nZ) (b === c))
  (meta (\a_ ->
  (meta (\c_ ->
  (conj (a === nS a_)
  (conj (c === nS c_)
  (add a_ b c_))))))))

-- Helper function to convert Int to Nat Term
intToNat :: Int -> Term
intToNat 0 = nZ
intToNat n = nS (intToNat (n-1))

-- Helper function to convert Nat Term to Int
natToInt :: Term -> Maybe Int
natToInt (TTag 0) = Just 0
natToInt (TCon (TTag 1) n) = fmap (+1) (natToInt n)
natToInt _ = Nothing

main :: IO ()
main = do
  let goal = meta (\x -> add (intToNat 16000) x (intToNat 16042))
  forM_ (goal initial) $ \(s, _) -> case lookup 0 s of
    Just term -> putStrLn $ "X = " ++ show (resolve term s)
    Nothing   -> putStrLn $ "No solution found"
  putStrLn "Done"
