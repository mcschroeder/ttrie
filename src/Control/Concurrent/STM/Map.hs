{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Control.Concurrent.STM.Map
    ( Map
    , empty
    , insert
    , lookup
    , delete
    , phantomLookup
    , unsafeDelete
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Control.Monad
import Data.Atomics
import Data.IORef
import Data.Maybe
import GHC.Conc.Sync (unsafeIOToSTM)
import Prelude hiding (lookup)

import Data.SparseArray

-----------------------------------------------------------------------

-- | A map from keys @k@ to values @v@.
newtype Map k v = Map (INode k v)

type INode k v = IORef (Node k v)

data Node k v = Array !(SparseArray (Branch k v))
              | List  ![Leaf k v]
              | Tomb  !(Leaf k v)

data Branch k v = I !(INode k v)
                | L !(Leaf k v)

data Leaf k v = Leaf !k !(TVar (Maybe v))

-----------------------------------------------------------------------

-- | /O(1)/. The empty map.
empty :: STM (Map k v)
empty = unsafeIOToSTM $ Map <$> newIORef (Array emptyArray)
{-# INLINE empty #-}

insert :: (Eq k, Hashable k) => k -> v -> Map k v -> STM ()
insert k v m = do var <- getTVar k m
                  writeTVar var (Just v)

{-# INLINABLE insert #-}

lookup :: (Eq k, Hashable k) => k -> Map k v -> STM (Maybe v)
lookup k m = do var <- getTVar k m
                readTVar var

{-# INLINABLE lookup #-}

delete :: (Eq k, Hashable k) => k -> Map k v -> STM ()
delete k m = do var <- getTVar k m
                writeTVar var Nothing

{-# INLINABLE delete #-}

-----------------------------------------------------------------------

getTVar :: (Eq k, Hashable k) => k -> Map k v -> STM (TVar (Maybe v))
getTVar k (Map root) = go root 0 undefined
  where
    h = hash k

    go inode level parent = do
        ticket <- unsafeIOToSTM $ readForCAS inode
        case peekTicket ticket of
            Array a -> case arrayLookup level h a of
                Just (I inode2) -> go inode2 (down level) inode
                Just (L leaf2@(Leaf k2 var))
                    | k == k2   -> return var
                    | otherwise -> cas inode ticket (growTrie level a (hash k2) leaf2)
                Nothing         -> cas inode ticket (insertLeaf level a)
            List xs -> case listLookup k xs of
                Just var        -> return var
                Nothing         -> cas inode ticket (return . List . (:xs))
            Tomb _  -> unsafeIOToSTM (clean parent (up level)) >> go root 0 undefined

    cas inode ticket f = do
        var <- newTVar Nothing
        node <- f (Leaf k var)
        (ok,_) <- unsafeIOToSTM $ casIORef inode ticket node
        if ok then return var
              else go root 0 undefined

    insertLeaf level a leaf = do
        let a' = arrayInsert level h (L leaf) a
        return (Array a')

    growTrie level a h2 leaf2 leaf1 = do
        inode2 <- unsafeIOToSTM $ combineLeaves (down level) h leaf1 h2 leaf2
        let a' = arrayUpdate level h (I inode2) a
        return (Array a')

    combineLeaves level h1 leaf1 h2 leaf2
        | level >= lastLevel = newIORef (List [leaf1, leaf2])
        | otherwise =
            case mkPair level h (L leaf1) h2 (L leaf2) of
                Just pair -> newIORef (Array pair)
                Nothing -> do
                    inode <- combineLeaves (down level) h1 leaf1 h2 leaf2
                    let a = mkSingleton level h (I inode)
                    newIORef (Array a)

{-# INLINE getTVar #-}


phantomLookup :: (Eq k, Hashable k) => k -> Map k v -> STM (Maybe v)
phantomLookup k (Map root) = go root 0 undefined
  where
    h = hash k
    go inode level parent = do
        node <- unsafeIOToSTM $ readIORef inode
        case node of
            Array a -> case arrayLookup level h a of
                Just (I inode2) -> go inode2 (down level) inode
                Just (L (Leaf k2 var))
                    | k == k2   -> readTVar var
                    | otherwise -> return Nothing
                Nothing         -> return Nothing
            List xs -> case listLookup k xs of
                Just var -> readTVar var
                Nothing  -> return Nothing
            Tomb _  -> unsafeIOToSTM (clean parent (up level)) >> go root 0 undefined

{-# INLINABLE phantomLookup #-}


unsafeDelete :: (Eq k, Hashable k) => k -> Map k v -> IO ()
unsafeDelete k m@(Map root) = do
    ok <- go root 0 undefined
    unless ok (unsafeDelete k m)
  where
    h = hash k
    go inode level parent = do
        ticket <- readForCAS inode
        case peekTicket ticket of
            Array a -> do
                ok <- case arrayLookup level h a of
                    Just (I inode2) -> go inode2 (down level) inode
                    Just (L (Leaf k2 _))
                        | k == k2   -> casArrayDelete inode ticket level a
                        | otherwise -> return True
                    Nothing         -> return True
                when ok (compressIfPossible level inode parent)
                return ok
            List xs -> casListDelete inode ticket xs
            Tomb _  -> clean parent (up level) >> return False

    compressIfPossible level inode parent = do
        n <- readIORef inode
        case n of
            Tomb _ -> cleanParent parent inode h (up level)
            _      -> return ()

    casArrayDelete inode ticket level a = do
        let a' = arrayDelete level h a
            n  = contract level (Array a')
        (ok,_) <- casIORef inode ticket n
        return ok

    casListDelete inode ticket xs = do
        let xs' = listDelete k xs
            n | [l] <- xs' = Tomb l
              | otherwise  = List xs'
        (ok,_) <- casIORef inode ticket n
        return ok

{-# INLINABLE unsafeDelete #-}

-----------------------------------------------------------------------

clean :: INode k v -> Level -> IO ()
clean inode level = do
    ticket <- readForCAS inode
    case peekTicket ticket of
        n@(Array _) -> do
            n' <- compress level n
            void $ casIORef inode ticket n'
        _ -> return ()
{-# INLINE clean #-}

cleanParent :: INode k v -> INode k v -> Hash -> Level -> IO ()
cleanParent parent inode h level = do
    ticket <- readForCAS parent
    case peekTicket ticket of
        n@(Array a) -> case arrayLookup level h a of
            Just (I inode2) | inode2 == inode -> do
                n2 <- readIORef inode
                case n2 of
                    Tomb _ -> do
                        n' <- compress level n
                        (ok,_) <- casIORef parent ticket n'
                        unless ok $ cleanParent parent inode h level
                    _ -> return ()
            _ -> return ()
        _ -> return ()

compress :: Level -> Node k v -> IO (Node k v)
compress level (Array a) = contract level . Array <$> arrayMapM resurrect a
compress _     n         = return n
{-# INLINE compress #-}

resurrect :: Branch k v -> IO (Branch k v)
resurrect b@(I inode) = do n <- readIORef inode
                           case n of
                               Tomb leaf -> return (L leaf)
                               _         -> return b
resurrect b           = return b
{-# INLINE resurrect #-}

contract :: Level -> Node k v -> Node k v
contract level (Array a) | level > 0
                         , Just (L leaf) <- arrayToMaybe a
                         = Tomb leaf
contract _     n         = n
{-# INLINE contract #-}

-----------------------------------------------------------------------

listLookup :: Eq k => k -> [Leaf k v] -> Maybe (TVar (Maybe v))
listLookup k1 = go
  where
    go []                             = Nothing
    go (Leaf k2 var : xs) | k1 == k2  = Just var
                          | otherwise = go xs

listDelete :: Eq k => k -> [Leaf k v] -> [Leaf k v]
listDelete k1 = go
  where
    go [] = []
    go (x@(Leaf k2 _):xs) | k1 == k2  = xs
                          | otherwise = x : go xs
