import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty `mappend` a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a `mappend` mempty) == a

newtype Mem s a =
  Mem {
    runMem :: s -> (a, s)
  }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = appendMem

appendMem :: (Monoid a) => Mem s a -> Mem s a -> Mem s a
appendMem (Mem f) (Mem g) =
  Mem $ (\s -> go f g s)
  
go :: (Monoid a) => (s -> (a, s)) -> (s -> (a, s)) -> s -> (a, s)
go f g s = (apa, gs)
         where apa = (fst $ f s) <> (fst $ g s)
               (fa, fs) = f s
               (ga, gs) = g fs


type MemAssoc s a = Mem s a -> Mem s a -> Mem s a -> Bool 

f' = (Mem $ \s -> ("hi", s + 1)) 

main :: IO ()
main = do
  print $ runMem (f' <> mempty) 0
  print $ runMem (mempty <> f') 0
  print $ (runMem mempty 0 :: (String, Int))
  print $ runMem (f' <> mempty) 0 == runMem f' 0
  print $ runMem (mempty <> f') 0 == runMem f' 0
