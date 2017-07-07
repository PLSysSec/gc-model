{-# LANGUAGE
ExistentialQuantification,
GADTs,
DataKinds,KindSignatures,TypeFamilies, RankNTypes,
ConstraintKinds, UndecidableInstances,TypeSynonymInstances, FlexibleInstances,
TemplateHaskell
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
import Control.Lens.TH (makeLenses)
import Control.Lens.Operators ((^.), (%~), (<+=), (<%=))
import Data.Binary


data Heap = Heap
  { _heapMem :: IOUArray Int Word32 -- this is NOT block structured!!!
  , _heapBounds :: (Int,Int)     -- array bounds
  , _heapFree :: Int             -- next free index
  }

makeLenses ''Heap

data OOM = OutOfMemory

type GC a = ExceptT OOM (StateT Heap IO) a

getAllocPtr :: GC Int
getAllocPtr = gets (^.heapFree)

bumpAllocPtr :: Int -> GC ()
bumpAllocPtr i = void $ heapFree <+= i

writeWords :: [Word32] -> GC Int
writeWords ws = do
  i <- getAllocPtr
  bumpAllocPtr (length ws)
  mem <- gets (^.heapMem)
  liftIO $ mapM_ (writeArray mem i) ws
  return i

getType :: Int -> GC (Proxy x)
getType i = do

-- getType :: Int -> GC Proxy

-- proxy for metadata serialisation
data Meta

-- representable data
data Value x where
  Int  :: Word32 -> Value (Proxy Int)
  Chr  :: Word32 -> Value (Proxy Char)
  Bool :: Word32 -> Value (Proxy Bool)
  Meta :: Word32 -> Value (Proxy Meta)
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

reify :: [Word32] -> Proxy x -> Value x
reify bs _ = undefined

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
