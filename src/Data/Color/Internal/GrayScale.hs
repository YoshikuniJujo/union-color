{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Color.Internal.GrayScale where

import Data.Bits
import Data.Word
import Data.Int

import Data.Color.Internal

data Gray d
	= GrayWord8_ Word8 | GrayWord16_ Word16 | GrayWord32_ Word32
	| GrayInt32_ Int32 | GrayDouble_ d
	deriving Show

{-# COMPLETE GrayWord8 #-}

pattern GrayWord8 :: RealFrac d => Word8 -> Gray d
pattern GrayWord8 x <- (fromGrayWord8 -> x) where
	GrayWord8 = GrayWord8_

fromGrayWord8 :: RealFrac d => Gray d -> Word8
fromGrayWord8 = \case
	GrayWord8_ x -> x
	GrayWord16_ x -> fromIntegral $ x `shiftR` 8
	GrayWord32_ x -> fromIntegral $ x `shiftR` 24
	GrayInt32_ x -> fromIntegral $ x `shiftR` 23
	GrayDouble_ x -> fracToWord8 x

fracToWord8 :: RealFrac d => d -> Word8
fracToWord8 = round . (* 0xff)
