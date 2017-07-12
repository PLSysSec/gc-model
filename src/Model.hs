{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Primitive
import Data.Primitive.ByteArray

import Data.Word
import Data.Int
import Data.Bits
import Data.Kind
import Data.List
import Data.Ord
import Data.Function
import Control.Monad.State
import Control.Monad.Fix

newtype Ptr = Ptr { getPtr :: Word16 } deriving (Num, Eq, Ord)

data TermType where
  T_Int  :: TermType
  T_Ptr  :: TermType
  T_Bool :: TermType
  T_Fun  :: TermType -> TermType -> TermType

data Var (a :: TermType) where
  Var :: String -> Var a

data Exp (a :: TermType) where
  E_Int   :: Int32      -> Exp 'T_Int
  E_Ptr   :: Word16     -> Exp 'T_Ptr
  E_Bool  :: Bool       -> Exp 'T_Bool
  E_Fun   :: Var a -> Exp b -> Exp ('T_Fun a b)

  -- E_Plus  :: Exp 'T_Int -> Exp 'T_Int -> Exp 'T_Int
  -- E_Mult  :: Exp 'T_Int -> Exp 'T_Int -> Exp 'T_Int
  -- E_Eq    :: Exp 'T_Int -> Exp 'T_Int -> Exp 'T_Bool
  E_If    :: Exp 'T_Bool -> Exp a -> Exp a -> Exp a
--  E_Fix   :: Exp ('T_Fun a a) -> Exp a
  E_App   :: Exp ('T_Fun a b) -> Exp a -> Exp b

-- fact :: Exp ('T_Fun 'T_Int 'T_Int)
-- fact = E_Fix $ E_Fun $ \fact -> E_Fun $ \n ->
--   E_If (E_Eq n (E_Int 0))
--        (E_Int 1)
--        (E_Mult (E_App fact (E_Plus n (E_Int (-1)))) n)


--fact = \f n -> if n == 0 then 1 else n * (f (n-1))

type family HSType (t :: TermType) :: Type where
  HSType T_Int       = Int32
  HSType T_Ptr       = Word16
  HSType T_Bool      = Bool
  HSType (T_Fun a b) = Exp a -> Exp b

-- eval_hs :: Exp t -> HSType t
-- eval_hs (E_Int i)  = i
-- eval_hs (E_Ptr p)  = p
-- eval_hs (E_Bool b) = b
-- -- eval_hs (E_Plus e1 e2) = eval_hs e1 + eval_hs e2
-- -- eval_hs (E_Mult e1 e2) = eval_hs e1 * eval_hs e2
-- -- eval_hs (E_Eq e1 e2) = eval_hs e1 == eval_hs e2
-- eval_hs (E_If c t e) = if eval_hs c then eval_hs t else eval_hs e
-- eval_hs (E_Fun f) = f
-- --eval_hs (E_Fix f) = eval_hs (fix (eval_hs f))
-- eval_hs (E_App f a) = eval_hs (eval_hs f a)

data AnyFunction = forall a b. AnyFunction { applyFunction :: Exp a -> Exp b }

data Interval = Interval { iLow :: Ptr, iHigh :: Ptr }
newtype FreeList = FreeList { unFreeList :: [Interval] }

data Thread m = Thread
  -- this is NOT block structured!!!
  { heapMem     :: MutableByteArray (PrimState m)
  , heapRootSet :: [Ptr]
  , freeList    :: FreeList
  , entryPoint  :: Ptr
  , codeMem     :: [AnyFunction]
  }

type RTS m = StateT (Thread m) m

fullHeap :: RTS IO Bool
fullHeap = null . unFreeList <$> gets freeList

splitLowestByte :: FreeList -> (Ptr, FreeList)
splitLowestByte (FreeList f) = (el, FreeList new)
  where (Interval el eh:es) = sortBy (compare `on` iLow) $ f
        el' = el + 1
        new | el' == eh = es
            | otherwise = Interval el' eh:es

allocWord :: Monad m => RTS m Ptr
allocWord = do
  (p, f') <- splitLowestByte <$> gets freeList
  modify (\h -> h { freeList = f' })
  pure p

heapWrite :: PrimMonad m => Ptr -> Word32 -> RTS m ()
heapWrite (Ptr addr) word =
  gets heapMem >>= flip (flip writeByteArray (fromIntegral addr)) word

-- type tag in lowest 3 bits of the right sort of header thingy
--
-- int
-- bool
-- intrinsic: plus <-- needs some pointers
-- intrinsic: mult <-- needs some pointers
-- intrinsic: eq   <-- needs some pointers
-- intrinsic: if   <-- needs some pointers
-- function
-- fixpoint
-- application     <-- needs some pointers
--
-- here's an idea:
--   00: int
--   01: bool
--   10: function
--   11: application
--
-- now there is an easy one bit test to decide whether or not there
-- are any more pointers that we need to follow.
--
-- intrinsics are just the first few code pointers.
compile :: PrimMonad m => Exp a -> Ptr -> RTS m ()
compile (E_Int i       ) p = heapWrite p $ shift (metadata 0b00) 32 .|. fromIntegral i
compile (E_Bool  b     ) p = heapWrite p $ shift (metadata 0b01) 32 .|. fromIntegral (fromEnum b)
-- TODO: code pointer
--compile (E_Fun (Var b) f   ) p = heapWrite p $ shift (metadata 0b10) 32 .|. fromIntegral f
compile (E_App   f e   ) p = do
  fp <- allocWord
  ep <- allocWord
  compile f fp
  compile e ep
  heapWrite p $  shift (metadata 0b11) 32
             .|. shift (fromIntegral (getPtr fp)) 16
             .|. fromIntegral (getPtr ep)

metadata :: Word32 -> Word32
metadata x = shift x 1




-- 64 bits is 8 bytes = 16 hexits
--serialize :: Exp a -> Word32
--serialize (E_Int i) = 0x0000000000000000 .|. fromIntegral i
--enacode :: Exp a -> RTS m ()

{-unrelated to the above:

data Foo = Foo { x :: Maybe Int, y :: Maybe Int, z :: [Int] } deriving (Show)

main = do
  let foo1 = Foo  { x = Nothing, y = Just 5, z = [1, 2, 3, 4, 8, 9] }
  let foo2 = foo1 { x = Just 10 }
  putStrLn $ show foo2
-}
