-- A demo, minimal Program Search in Haskell

-- Given a test (input/output) pairs, it will find a function that passes it.
-- This file is for demo purposes, so, it is restricted to just simple, single
-- pass recursive functions. The idea is to use HVM superpositions to try many
-- functions "at once". Obviously, Haskell does not have them, so, we just use
-- the Omega Monad to convert to a list of functions, and try each separately.

import Control.Monad (forM_)

-- PRELUDE
----------

newtype Omega a = Omega { runOmega :: [a] }

instance Functor Omega where
  fmap f (Omega xs) = Omega (map f xs)

instance Applicative Omega where
  pure x                = Omega [x]
  Omega fs <*> Omega xs = Omega [f x | f <- fs, x <- xs]

instance Monad Omega where
  Omega xs >>= f = Omega $ diagonal $ map (\x -> runOmega (f x)) xs

diagonal :: [[a]] -> [a]
diagonal xs = concat (stripe xs) where
  stripe []             = []
  stripe ([]     : xss) = stripe xss
  stripe ((x:xs) : xss) = [x] : zipCons xs (stripe xss)
  zipCons []     ys     = ys
  zipCons xs     []     = map (:[]) xs
  zipCons (x:xs) (y:ys) = (x:y) : zipCons xs ys

-- ENUMERATOR
-------------

-- A bit-string
data Bin
  = O Bin
  | I Bin
  | E

-- A simple DSL for `Bin -> Bin` terms
data Term
  = MkO Term      -- emits the bit 0
  | MkI Term      -- emits the bit 1
  | Mat Term Term -- pattern-matches on the argument
  | Rec           -- recurses on the argument
  | Ret           -- returns the argument
  | Sup Term Term -- a superposition of two functions

-- Checks if two Bins are equal
bin_eq :: Bin -> Bin -> Bool
bin_eq (O xs) (O ys) = bin_eq xs ys
bin_eq (I xs) (I ys) = bin_eq xs ys
bin_eq E      E      = True
bin_eq _      _      = False

-- Stringifies a Bin
bin_show :: Bin -> String
bin_show (O xs) = "O" ++ bin_show xs
bin_show (I xs) = "I" ++ bin_show xs
bin_show E      = "E"

-- Checks if two term are equal
term_eq :: Term -> Term -> Bool
term_eq (Mat l0 r0) (Mat l1 r1) = term_eq l0 l1 && term_eq r0 r1
term_eq (MkO t0)    (MkO t1)    = term_eq t0 t1
term_eq (MkI t0)    (MkI t1)    = term_eq t0 t1
term_eq Rec         Rec         = True
term_eq Ret         Ret         = True
term_eq _           _           = False

-- Stringifies a term
term_show :: Term -> String
term_show (MkO t)    = "(O " ++ term_show t ++ ")"
term_show (MkI t)    = "(I " ++ term_show t ++ ")"
term_show (Mat l r)  = "{O:" ++ term_show l ++ "|I:" ++ term_show r ++ "}"
term_show (Sup a b)  = "{" ++ term_show a ++ "|" ++ term_show b ++ "}"
term_show Rec        = "@"
term_show Ret        = "*"

-- Enumerates all terms
enum :: Bool -> Term
enum s = (if s then Sup Rec else id) $ Sup Ret $ Sup (intr s) (elim s) where
  intr s = Sup (MkO (enum s)) (MkI (enum s))
  elim s = Mat (enum True) (enum True)

-- Converts a Term into a native function
make :: Term -> (Bin -> Bin) -> Bin -> Bin
make Ret       _ x = x
make Rec       f x = f x
make (MkO trm) f x = O (make trm f x)
make (MkI trm) f x = I (make trm f x)
make (Mat l r) f x = case x of
  O xs -> make l f xs
  I xs -> make r f xs
  E    -> E

-- Finds a program that satisfies a test
search :: Int -> (Term -> Bool) -> [Term] -> IO ()
search n test (tm:tms) = do
  if test tm then
    putStrLn $ "FOUND " ++ term_show tm ++ " (after " ++ show n ++ " guesses)"
  else
    search (n+1) test tms

-- Collapses a superposed term to a list of terms, diagonalizing
collapse :: Term -> Omega Term
collapse (MkO t) = do
  t' <- collapse t
  return $ MkO t'
collapse (MkI t) = do
  t' <- collapse t
  return $ MkI t'
collapse (Mat l r) = do
  l' <- collapse l
  r' <- collapse r
  return $ Mat l' r'
collapse (Sup a b) =
  let a' = runOmega (collapse a) in
  let b' = runOmega (collapse b) in
  Omega (diagonal [a',b'])
collapse Rec = return Rec
collapse Ret = return Ret

-- Some test cases:
-- ----------------

test_not :: Term -> Bool
test_not tm = e0 && e1 where
  fn = make tm fn
  x0 = (O (I (O (O (O (I (O (O E))))))))
  y0 = (I (O (I (I (I (O (I (I E))))))))
  e0 = (bin_eq (fn x0) y0)
  x1 = (I (I (I (O (O (I (I (I E))))))))
  y1 = (O (O (O (I (I (O (O (O E))))))))
  e1 = (bin_eq (fn x1) y1)

test_inc :: Term -> Bool
test_inc tm = e0 && e1 where
  fn = make tm fn
  x0 = (O (I (O (O (O (I (O (O E))))))))
  y0 = (I (I (O (O (O (I (O (O E))))))))
  e0 = (bin_eq (fn x0) y0)
  x1 = (I (I (I (O (O (I (I (I E))))))))
  y1 = (O (O (O (I (O (I (I (I E))))))))
  e1 = (bin_eq (fn x1) y1)

test_mix :: Term -> Bool
test_mix tm = e0 && e1 where
  fn = make tm fn
  x0 = (O (I (O (O (O (I (O (O E))))))))
  y0 = (I (O (I (I (I (O (I (O (I (O (I (I (I (O (I (O E))))))))))))))))
  e0 = (bin_eq (fn x0) y0)
  x1 = (I (I (I (O (O (I (I (I E))))))))
  y1 = (I (I (I (I (I (I (I (O (I (O (I (I (I (I (I (I E))))))))))))))))
  e1 = (bin_eq (fn x1) y1)

test_xors :: Term -> Bool
test_xors tm = e0 && e1 where
  fn = make tm fn
  x0 = (I (I (O (O (O (I (O (O E))))))))
  y0 = (I (I (O (I E))))
  e0 = (bin_eq (fn x0) y0)
  x1 = (I (O (O (I (I (I (O (I E))))))))
  y1 = (O (O (I (O E))))
  e1 = (bin_eq (fn x1) y1)

test_xor_xnor :: Term -> Bool
test_xor_xnor tm = e0 && e1 where
  fn = make tm fn
  x0 = (I (O (O (I (I (O (I (I (I (O E))))))))))
  y0 = (I (O (I (O (I (O (O (I (I (O E))))))))))
  e0 = (bin_eq (fn x0) y0)
  x1 = (O (I (O (O (O (I (O (I (O (O E))))))))))
  y1 = (I (O (O (I (I (O (I (O (O (I E)))))))))) 
  e1 = (bin_eq (fn x1) y1)

main :: IO ()
main = search 0 test_xor_xnor $ runOmega $ collapse $ enum False
