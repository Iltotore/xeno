{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeOperators #-}

-- | Shared types.

module Xeno.Types where

import Control.DeepSeq
import Control.Exception
import Data.ByteString.Char8 (ByteString, pack)
import Data.Data
import GHC.Generics

#if MIN_VERSION_base(4,9,0)
import Control.Monad.Fail

-- It is recommended to use more specific `failHere` instead
instance MonadFail (Either Xeno.Types.XenoException) where
  fail = Left . XenoParseError 0 . pack
#endif

data XenoException
  = XenoStringIndexProblem { stringIndex :: Int, inputString :: ByteString }
  | XenoParseError         { inputIndex  :: Int, message     :: ByteString }
  | XenoExpectRootNode
  deriving (Show, Data, Typeable, NFData, Generic)

instance Exception XenoException where displayException = show

-- | ByteString wich guaranted have '\NUL' at the end
newtype ByteStringZeroTerminated = BSZT ByteString deriving (Generic, NFData)

-- |HList/"Flat tuple" used to easily manipulate products, defined as a head with a tail.
-- See https://www.scala-lang.org/api/3.6.4/scala/$times$colon$.html.
data h `HCons` t = h `HCons` t
  deriving (Eq, Show)
infixr `HCons`