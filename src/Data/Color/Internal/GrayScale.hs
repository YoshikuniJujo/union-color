{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Color.Internal.GrayScale where

import Data.Bits
import Data.Word
import Data.Int

import Data.Color.Internal

data Gray d
	= GrayWord1_ Word8 | GrayWord2_ Word8 | GrayWord4_ Word8
	| GrayWord8_ Word8 | GrayWord16_ Word16 | GrayWord32_ Word32
	| GrayInt32_ Int32 | GrayDouble_ d
	deriving Show

{-# COMPLETE GrayWord1 #-}

pattern GrayWord1 :: RealFrac d => Word8 -> Gray d
pattern GrayWord1 g <- (grayToWord1 -> g)

grayWord1 :: Word8 -> Maybe (Gray d)
grayWord1 w
	| 0 <= w && w < 2 = Just $ GrayWord1_ w
	| otherwise = Nothing

grayToWord1 :: RealFrac d => Gray d -> Word8
grayToWord1 (GrayWord1_ g) = g
grayToWord1 (GrayWord2_ g) = g `shiftR` 1
grayToWord1 (GrayWord4_ g) = g `shiftR` 3
grayToWord1 (GrayWord8_ g) = g `shiftR` 7
grayToWord1 (GrayWord16_ g) = fromIntegral $ g `shiftR` 15
grayToWord1 (GrayWord32_ g) = fromIntegral $ g `shiftR` 31
grayToWord1 (GrayInt32_ g) = fromIntegral $ g `shiftR` 30
grayToWord1 (GrayDouble_ g) = fracToWord8 g `shiftR` 7

{-# COMPLETE GrayWord2 #-}

pattern GrayWord2 :: RealFrac d => Word8 -> Gray d
pattern GrayWord2 g <- (grayToWord2 -> g)

grayWord2 :: Word8 -> Maybe (Gray d)
grayWord2 g
	| 0 <= g && g < 4 = Just $ GrayWord2_ g
	| otherwise = Nothing

grayToWord2 :: RealFrac d => Gray d -> Word8
grayToWord2 (GrayWord1_ g) = g `shiftL` 1
grayToWord2 (GrayWord2_ g) = g
grayToWord2 (GrayWord4_ g) = g `shiftR` 2
grayToWord2 (GrayWord8_ g) = g `shiftR` 6
grayToWord2 (GrayWord16_ g) = fromIntegral $ g `shiftR` 14
grayToWord2 (GrayWord32_ g) = fromIntegral $ g `shiftR` 30
grayToWord2 (GrayInt32_ g) = fromIntegral $ g `shiftR` 29
grayToWord2 (GrayDouble_ g) = fracToWord8 g `shiftR` 6

{-# COMPLETE GrayWord4 #-}

pattern GrayWord4 :: RealFrac d => Word8 -> Gray d
pattern GrayWord4 g <- (grayToWord4 -> g)

grayWord4 :: Word8 -> Maybe (Gray d)
grayWord4 g
	| 0 <= g && g < 16 = Just $ GrayWord4_ g
	| otherwise = Nothing

grayToWord4 :: RealFrac d => Gray d -> Word8
grayToWord4 (GrayWord1_ g) = g `shiftL` 3
grayToWord4 (GrayWord2_ g) = g `shiftL` 2
grayToWord4 (GrayWord4_ g) = g
grayToWord4 (GrayWord8_ g) = g `shiftR` 4
grayToWord4 (GrayWord16_ g) = fromIntegral $ g `shiftR` 12
grayToWord4 (GrayWord32_ g) = fromIntegral $ g `shiftR` 28
grayToWord4 (GrayInt32_ g) = fromIntegral $ g `shiftR` 27
grayToWord4 (GrayDouble_ g) = fracToWord8 g `shiftR` 4

{-# COMPLETE GrayWord8 #-}

pattern GrayWord8 :: RealFrac d => Word8 -> Gray d
pattern GrayWord8 x <- (fromGrayWord8 -> x) where
	GrayWord8 = GrayWord8_

fromGrayWord8 :: RealFrac d => Gray d -> Word8
fromGrayWord8 = \case
	GrayWord1_ x -> x `shiftL` 7
	GrayWord2_ x -> x `shiftL` 6
	GrayWord4_ x -> x `shiftL` 4
	GrayWord8_ x -> x
	GrayWord16_ x -> fromIntegral $ x `shiftR` 8
	GrayWord32_ x -> fromIntegral $ x `shiftR` 24
	GrayInt32_ x -> fromIntegral $ x `shiftR` 23
	GrayDouble_ x -> fracToWord8 x

fracToWord8 :: RealFrac d => d -> Word8
fracToWord8 = round . (* 0xff)

{-# COMPLETE GrayWord16 #-}

pattern GrayWord16 :: RealFrac d => Word16 -> Gray d
pattern GrayWord16 x <- (grayToWord16 -> x) where
	GrayWord16 = GrayWord16_

grayToWord16 :: RealFrac d => Gray d -> Word16
grayToWord16 = \case
	GrayWord1_ x -> fromIntegral x `shiftL` 15
	GrayWord2_ x -> fromIntegral x `shiftL` 14
	GrayWord4_ x -> fromIntegral x `shiftL` 12
	GrayWord8_ x -> fromIntegral x `shiftL` 8
	GrayWord16_ x -> x
	GrayWord32_ x -> fromIntegral $ x `shiftR` 16
	GrayInt32_ x -> fromIntegral $ x `shiftR` 15
	GrayDouble_ x -> fracToWord16 x

fracToWord16 :: RealFrac d => d -> Word16
fracToWord16 = round . (* 0xffff)
