{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BenchGen where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Bifunctor
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import System.Random.MWC
import System.Random.MWC.CondensedTable

------------------------------------------------------------------------------

newtype Generator m k a = Generator { unG :: StateT (Gen (PrimState m), Set k) m a }
  deriving (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (Generator m k) where
    liftIO = Generator . liftIO

runGenerator :: PrimMonad m => Gen (PrimState m) -> Generator m k a -> m a
runGenerator gen (Generator m) = evalStateT m (gen, Set.empty)

liftGen :: PrimMonad m => (Gen (PrimState m) -> m a) -> Generator m k a
liftGen f = Generator $ lift . f =<< gets fst

reuse :: (PrimMonad m, Ord k) => Generator m k k -> Generator m k k
reuse keygen = Generator $ do
    (gen,ks) <- get
    if Set.null ks
        then unG keygen
        else do
            i <- lift $ uniformR (0, Set.size ks - 1) gen
            return $ Set.elemAt i ks

remember :: (PrimMonad m, Ord k) => Generator m k k -> Generator m k k
remember keygen = Generator $ do
    k <- unG keygen
    modify (second $ Set.insert k)
    return k

forget :: (PrimMonad m, Ord k) => Generator m k k -> Generator m k k
forget keygen = Generator $ do
    k <- unG keygen
    modify (second $ Set.delete k)
    return k

------------------------------------------------------------------------------

data Config m op k = Config { operations :: CondensedTableV op
                            , keys :: op -> CondensedTableV (Generator m k k)
                            }

generateOperations :: (PrimMonad m, Ord k)
                   => Config m op k
                   -> Int
                   -> Generator m k [(op,k)]
generateOperations config n =
    replicateM n $ do
        op <- liftGen $ genFromTable (operations config)
        k <- join $ liftGen $ genFromTable $ (keys config) op
        return (op,k)

------------------------------------------------------------------------------

mkTable :: [(a,Double)] -> CondensedTableV a
mkTable = tableFromWeights . V.fromList
