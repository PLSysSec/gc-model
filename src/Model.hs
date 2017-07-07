{-# LANGUAGE
ExistentialQuantification,
GADTs,
DataKinds,KindSignatures,TypeFamilies, RankNTypes,
ConstraintKinds, UndecidableInstances,TypeSynonymInstances, FlexibleInstances
#-}

-- prototype implementation of rc-gc
module Model where

import Control.Monad.Trans
import Control.Monad.Except
import Control.Monad.State.Lazy
import Control.Monad
import System.IO
import Data.IORef
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc
import Data.Array.MArray
import Data.Array.IO
import Data.Word
import Data.Proxy
import Data.Char
import Data.Binary

-- other structures to consider:
--   rose tree of IORefs / Ptr Word32

data Heap = Heap
  { heapMem :: IOUArray Int Word32 -- this is NOT block structured!!!
  , heapBounds :: (Int,Int)     -- array bounds
  , heapFree :: Int             -- next free index
  }

-- TODO: throw memory exhaustion exception
-- GC = catch exception, run gc, etc
data Exn = OutOfMemory
         | InvalidPtr -- if not tagged as a metadata field

type GC a = ExceptT Exn (StateT Heap IO) a

getAllocPtr :: GC Int
getAllocPtr = gets (^.heapFree)

bumpAllocPtr :: Int -> GC ()
bumpAllocPtr i =
  void $ modify (\h -> h { heapFree = heapFree h + i })
  --void $ heapFree <+= i

writeWords :: [Word32] -> GC Int
writeWords ws = do
  i <- getAllocPtr
  bumpAllocPtr (length ws)
  mem <- gets heapMem
  liftIO $ zipWithM_ (writeArray mem) [i..] ws
  return i

-- given an int index, get its metadata
getType :: Int -> GC (Proxy x)
getType i = do
  heap <- gets heapMem
  meta <- liftIO . flip readArray i
  checkIsMeta meta
  return $ metaType meta

-- TODO
checkIsMeta :: Word32 -> GC ()
checkIsMeta w = throwError InvalidPtr

metaType :: Word32 -> Proxy x
metaType = undefined


-- | DATA REPRESENTATION
-- right now only consider primitive data that fit into a single word32.
-- heap looks like this:
--      [meta | data | meta | data ... ]
--
-- tagging: use LSB for tagging:
-- 0 for metadata
-- 1 for data
--
-- NB: as this gets fleshed out it will be time to switch to a better repr like
-- the GHC representation: https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects
-- or the Scheme one: http://www.more-magic.net/posts/internals-data-representation.html


-- proxy for metadata serialisation
data Meta

-- representable data
data Value x where
  Int  :: Word32 -> Value (Proxy Int)
  Chr  :: Word32 -> Value (Proxy Char)
  Bool :: Word32 -> Value (Proxy Bool)
--  Meta :: Word32 -> Value (Proxy Meta)
  Pair :: Value a -> Value b -> Value x

-- serialisation
class Repr x where
  repr :: x -> [Word32]

  meta :: x -> Word32

-- TODO
instance Repr (Value x) where
  repr (Int i) = [0]

  meta (Int i) = 0

serialise :: Repr a => a -> [Word32]
serialise x = meta x : repr x

{-@ reify :: {b:[Word32]| len b >= 2} -> Proxy x -> Value x @-}
reify :: [Word32] -> Proxy x -> Value x
reify (m:bs) _ = undefined

alloc :: Repr a => a -> GC a
alloc x = do
  let r = meta x : repr x
  return x


-- compile arbitrary haskell progs
-- class Embed a where
--   embed :: a -> Value
-- instance Embed Int where
--   embed i = Int i
-- instance Embed String where
--   embed s = Str s
-- instance Embed Bool where
--   embed b = Bool b
-- instance (Embed a, Embed b) => Embed (a,b) where
--   embed (a,b) = Pair (embed a) (embed b)

-- class CoEmbed a where
--   embedFun :: a -> (Value -> Value)

-- instance (Embed a, Embed b) => CoEmbed (a -> b) where
--   embedFun f = \x -> embed (f (unembed x))

-- instance (Embed a, CoEmbed b, b ~ (c -> d)) => CoEmbed (a -> b) where
--   embedFun f = \x -> embed (f (unembed x))
--     where
--       unembed :: Value -> a
--       unembed (Int i) = i
--       unembed (Str s) = s
--       unembed (Bool b) = b


-- gc semantics




-- main :: IO ()
-- main = do
--   return ()
