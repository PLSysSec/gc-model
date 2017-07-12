{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS -Wall #-}

import Control.Monad.Primitive
import Data.Primitive.ByteArray

import Data.Word
import Data.Int
--import Data.Bits
import Data.Kind
import Data.List
--import Data.Ord
import Data.Function
import Control.Monad.State

newtype Ptr = Ptr { getPtr :: Word16 } deriving (Num, Eq, Ord)

data Kind = Literal | Application
data TermType where
  T_Int  :: TermType
  T_Ptr  :: TermType
  T_Bool :: TermType
  T_Fun  :: TermType -> TermType -> TermType
data Exp (k :: Kind) (a :: TermType) where
  E_Int   :: Int32      -> Exp 'Literal 'T_Int
  E_Bool  :: Bool       -> Exp 'Literal 'T_Bool
  E_Fun   :: (Exp k a -> Exp k' b) -> Exp 'Literal ('T_Fun a b)
  E_App   :: Exp k ('T_Fun a b) -> Exp k' a -> Exp 'Application b

e_plus :: Exp ('T_Fun 'Literal ('T_Int 'Literal) ('T_Fun 'Literal ('T_Int 'Literal) ('T_Int 'Literal)))
e_plus = E_Fun $ \case
  E_Int i -> E_Fun $ \case
    E_Int j -> E_Int (i + j)
e_mult :: Exp ('T_Fun 'Literal ('T_Int 'Literal) ('T_Fun 'Literal ('T_Int 'Literal) ('T_Int 'Literal)))
e_mult = E_Fun $ \case
  E_Int i -> E_Fun $ \case
    E_Int j -> E_Int (i * j)
e_eq :: Exp ('T_Fun 'Literal ('T_Int 'Literal) ('T_Fun 'Literal ('T_Int 'Literal) ('T_Bool 'Literal)))
e_eq = E_Fun $ \case
  E_Int i -> E_Fun $ \case
    E_Int j -> E_Bool (i == j)
e_if :: Exp ('T_Fun 'Literal ('T_Bool 'Literal) ('T_Fun 'Literal a ('T_Fun 'Literal a a)))
e_if = E_Fun $ \case
  E_Bool c -> E_Fun $ \t -> E_Fun $ \e -> if c then t else e
e_fix :: Exp ('T_Fun 'Literal ('T_Fun 'Literal a a) a)
e_fix = E_Fun $ \case
  E_Fun f -> fix f

fact :: Exp ('T_Fun k ('T_Int k') ('T_Int k''))
fact = E_App e_fix $ E_Fun $ \fact' -> E_Fun $ \n ->
  (E_App (E_App (E_App e_if (E_App (E_App e_eq n) (E_Int 0)))
       (E_Int 1))
       (E_App (E_App e_mult (E_App fact' (E_App (E_App e_plus n) (E_Int (-1))))) n))

type family HSType (t :: TermType) :: Type where
  HSType ('T_Int _)     = Int32
  HSType ('T_Ptr _)     = Word16
  HSType ('T_Bool _)    = Bool
  HSType ('T_Fun _ a b) = Exp a -> Exp b

eval_hs :: Exp t -> HSType t
eval_hs (E_Int i)  = i
eval_hs (E_Ptr p)  = p
eval_hs (E_Bool b) = b
--eval_hs (E_Plus e1 e2) = eval_hs e1 + eval_hs e2
--neval_hs (E_Mult e1 e2) = eval_hs e1 * eval_hs e2
--eval_hs (E_Eq e1 e2) = eval_hs e1 == eval_hs e2
--eval_hs (E_If c t e) = if eval_hs c then eval_hs t else eval_hs e
eval_hs (E_Fun f) = f
--eval_hs (E_Fix f) = eval_hs (fix (eval_hs f))
eval_hs (E_App f a) = eval_hs (eval_hs f a)

data AnyFunction = forall a b. AnyFunction { applyFunction :: Exp a -> Exp b }

data Interval = Interval { iLow :: Ptr, iHigh :: Ptr }
newtype FreeList = FreeList { unFreeList :: [Interval] }

data Thread m = Thread
  -- this is NOT block structured!!!
  { heapMem      :: MutableByteArray (PrimState m)
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
--   000: int
--   001: bool
--   010: function
--   100: fixpoint
--   101: application
--
-- now there is an easy one bit test to decide whether or not there
-- are any more pointers that we need to follow.
--
-- intrinsics are just the first few code pointers.
{-encode :: PrimMonad m => Exp a -> Ptr -> RTS m ()
encode (E_Int i) p = heapWrite p $ 0x0000000000000000 .|. fromIntegral i-}

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
