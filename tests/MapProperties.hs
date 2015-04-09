{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Concurrent.STM
import Data.Hashable
import Data.Function (on)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Control.Concurrent.STM.Map as TTrie
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- most of this based on the unordered-containers tests

-----------------------------------------------------------------------

main = defaultMain [ testGroup "basic interface"
                        [ testProperty "lookup" pLookup
                        , testProperty "insert" pInsert
                        , testProperty "delete" pDelete
                        ]
                    , testGroup "unsafe functions"
                        [ testProperty "phantomLookup" pPhantomLookup
                        , testProperty "unsafeDelete" pUnsafeDelete
                        ]
                    , testGroup "conversions"
                        [ testProperty "fromList" pFromList
                        , testProperty "unsafeToList" pUnsafeToList
                        ]
                   ]

-----------------------------------------------------------------------

type Model k v = M.Map k v

eq :: (Eq a, Eq k, Hashable k, Ord k)
   => (Model k v -> a) -> (TTrie.Map k v -> IO a) -> [(k, v)] -> Property
eq f g xs = monadicIO $ do
    let a = f (M.fromList xs)
    b <- run $ g =<< TTrie.fromList xs
    assert $ a == b

eq_ :: (Eq k, Eq v, Hashable k, Ord k)
    => (Model k v -> Model k v) -> (TTrie.Map k v -> IO ()) -> [(k, v)] -> Property
eq_ f g xs = monadicIO $ do
    let a = M.toAscList $ f $ M.fromList xs
    m <- run $ TTrie.fromList xs
    run $ g m
    b <- run $ unsafeToAscList m
    assert $ a == b

unsafeToAscList :: Ord k => TTrie.Map k v -> IO [(k, v)]
unsafeToAscList m = do
    xs <- TTrie.unsafeToList m
    return $ L.sortBy (compare `on` fst) xs

-----------------------------------------------------------------------

-- key type that generates more hash collisions

newtype Key = K { unK :: Int }
    deriving (Arbitrary, Eq, Ord)

instance Show Key where
    show = show . unK

instance Hashable Key where
    hashWithSalt salt k = hashWithSalt salt (unK k) `mod` 20

-----------------------------------------------------------------------

pLookup :: Key -> [(Key,Int)] -> Property
pLookup k = M.lookup k `eq` (atomically . TTrie.lookup k)

pPhantomLookup :: Key -> [(Key,Int)] -> Property
pPhantomLookup k = M.lookup k `eq` (atomically . TTrie.phantomLookup k)

pInsert :: Key -> Int -> [(Key,Int)] -> Property
pInsert k v = M.insert k v `eq_` (atomically . TTrie.insert k v)

pDelete :: Key -> [(Key,Int)] -> Property
pDelete k = M.delete k `eq_` (atomically . TTrie.delete k)

pUnsafeDelete :: Key -> [(Key,Int)] -> Property
pUnsafeDelete k = M.delete k `eq_` TTrie.unsafeDelete k

pFromList :: [(Key,Int)] -> Property
pFromList = id `eq_` (\_ -> return ())

pUnsafeToList :: [(Key,Int)] -> Property
pUnsafeToList = M.toAscList `eq` unsafeToAscList
