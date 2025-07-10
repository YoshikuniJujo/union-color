{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Color.Internal.GrayScale where

import Data.Bits
import Data.Word
import Data.Int

import Data.Color.Internal

-- GRAY

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
grayToWord2 (GrayWord1_ g) = g `shiftL` 1 .|. g
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
grayToWord4 (GrayWord1_ g) =
	g `shiftL` 3 .|. g `shiftL` 2 .|. g `shiftL` 1 .|. g
grayToWord4 (GrayWord2_ g) = g `shiftL` 2 .|. g
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
	GrayWord1_ x ->
		x `shiftL` 7 .|. x `shiftL` 6 .|.
		x `shiftL` 5 .|. x `shiftL` 4 .|.
		x `shiftL` 3 .|. x `shiftL` 2 .|.
		x `shiftL` 1 .|. x
	GrayWord2_ x ->
		x `shiftL` 6 .|. x `shiftL` 4 .|.
		x `shiftL` 2 .|. x
	GrayWord4_ x -> x `shiftL` 4 .|. x
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
	GrayWord1_ x ->
		x' `shiftL` 15 .|. x' `shiftL` 14 .|.
		x' `shiftL` 13 .|. x' `shiftL` 12 .|.
		x' `shiftL` 11 .|. x' `shiftL` 10 .|.
		x' `shiftL` 9 .|. x' `shiftL` 8 .|.
		x' `shiftL` 7 .|. x' `shiftL` 6 .|.
		x' `shiftL` 5 .|. x' `shiftL` 4 .|.
		x' `shiftL` 3 .|. x' `shiftL` 2 .|.
		x' `shiftL` 1 .|. x'
		where x' = fromIntegral x
	GrayWord2_ x ->
		x' `shiftL` 14 .|. x' `shiftL` 12 .|.
		x' `shiftL` 10 .|. x' `shiftL` 8 .|.
		x' `shiftL` 6 .|. x' `shiftL` 4 .|.
		x' `shiftL` 2 .|. x'
		where x' = fromIntegral x
	GrayWord4_ x ->
		x' `shiftL` 12 .|. x' `shiftL` 8 .|. x' `shiftL` 4 .|. x'
		where x' = fromIntegral x
	GrayWord8_ x -> x' `shiftL` 8 .|. x' where x' = fromIntegral x
	GrayWord16_ x -> x
	GrayWord32_ x -> fromIntegral $ x `shiftR` 16
	GrayInt32_ x -> fromIntegral $ x `shiftR` 15
	GrayDouble_ x -> fracToWord16 x

fracToWord16 :: RealFrac d => d -> Word16
fracToWord16 = round . (* 0xffff)

{-# COMPLETE GrayWord32 #-}

pattern GrayWord32 :: RealFrac d => Word32 -> Gray d
pattern GrayWord32 x <- (grayToWord32 -> x) where
	GrayWord32 = GrayWord32_

grayToWord32 :: RealFrac d => Gray d -> Word32
grayToWord32 = \case
	GrayWord1_ x ->
		x' `shiftL` 31 .|. x' `shiftL` 30 .|.
		x' `shiftL` 29 .|. x' `shiftL` 28 .|.
		x' `shiftL` 27 .|. x' `shiftL` 26 .|.
		x' `shiftL` 25 .|. x' `shiftL` 24 .|.
		x' `shiftL` 23 .|. x' `shiftL` 22 .|.
		x' `shiftL` 21 .|. x' `shiftL` 20 .|.
		x' `shiftL` 19 .|. x' `shiftL` 18 .|.
		x' `shiftL` 17 .|. x' `shiftL` 16 .|.
		x' `shiftL` 15 .|. x' `shiftL` 14 .|.
		x' `shiftL` 13 .|. x' `shiftL` 12 .|.
		x' `shiftL` 11 .|. x' `shiftL` 10 .|.
		x' `shiftL` 9 .|. x' `shiftL` 8 .|.
		x' `shiftL` 7 .|. x' `shiftL` 6 .|.
		x' `shiftL` 5 .|. x' `shiftL` 4 .|.
		x' `shiftL` 3 .|. x' `shiftL` 2 .|.
		x' `shiftL` 1 .|. x'
		where x' = fromIntegral x
	GrayWord2_ x ->
		x' `shiftL` 30 .|. x' `shiftL` 28 .|.
		x' `shiftL` 26 .|. x' `shiftL` 24 .|.
		x' `shiftL` 22 .|. x' `shiftL` 20 .|.
		x' `shiftL` 18 .|. x' `shiftL` 16 .|.
		x' `shiftL` 14 .|. x' `shiftL` 12 .|.
		x' `shiftL` 10 .|. x' `shiftL` 8 .|.
		x' `shiftL` 6 .|. x' `shiftL` 4 .|.
		x' `shiftL` 2 .|. x'
		where x' = fromIntegral x
	GrayWord4_ x ->
		x' `shiftL` 28 .|. x' `shiftL` 24 .|.
		x' `shiftL` 20 .|. x' `shiftL` 16 .|.
		x' `shiftL` 12 .|. x' `shiftL` 8 .|.
		x' `shiftL` 4 .|. x'
		where x' = fromIntegral x
	GrayWord8_ x ->
		x' `shiftL` 24 .|. x' `shiftL` 16 .|. x' `shiftL` 8 .|. x'
		where x' = fromIntegral x
	GrayWord16_ x -> x' `shiftL` 16 .|. x' where x' = fromIntegral x
	GrayWord32_ x -> x
	GrayInt32_ x -> fromIntegral x `shiftL` 1
	GrayDouble_ x -> fracToWord32 x

fracToWord32 :: RealFrac d => d -> Word32
fracToWord32 = round . (* 0xffffffff)

{-# COMPLETE GrayInt32 #-}

pattern GrayInt32 :: RealFrac d => Int32 -> Gray d
pattern GrayInt32 x <- (grayToInt32 -> x) where
	GrayInt32 = GrayInt32_

grayToInt32 :: RealFrac d => Gray d -> Int32
grayToInt32 = \case
	GrayWord1_ x ->
		x' `shiftL` 30 .|. x' `shiftL` 29 .|.
		x' `shiftL` 28 .|. x' `shiftL` 27 .|.
		x' `shiftL` 26 .|. x' `shiftL` 25 .|.
		x' `shiftL` 24 .|. x' `shiftL` 23 .|.
		x' `shiftL` 22 .|. x' `shiftL` 21 .|.
		x' `shiftL` 20 .|. x' `shiftL` 19 .|.
		x' `shiftL` 18 .|. x' `shiftL` 17 .|.
		x' `shiftL` 16 .|. x' `shiftL` 15 .|.
		x' `shiftL` 14 .|. x' `shiftL` 13 .|.
		x' `shiftL` 12 .|. x' `shiftL` 11 .|.
		x' `shiftL` 10 .|. x' `shiftL` 9 .|.
		x' `shiftL` 8 .|. x' `shiftL` 7 .|.
		x' `shiftL` 6 .|. x' `shiftL` 5 .|.
		x' `shiftL` 4 .|. x' `shiftL` 3 .|.
		x' `shiftL` 2 .|. x' `shiftL` 1 .|. x'
		where x' = fromIntegral x
	GrayWord2_ x ->
		x' `shiftL` 29 .|. x' `shiftL` 27 .|.
		x' `shiftL` 25 .|. x' `shiftL` 23 .|.
		x' `shiftL` 21 .|. x' `shiftL` 19 .|.
		x' `shiftL` 17 .|. x' `shiftL` 15 .|.
		x' `shiftL` 13 .|. x' `shiftL` 11 .|.
		x' `shiftL` 9 .|. x' `shiftL` 7 .|.
		x' `shiftL` 5 .|. x' `shiftL` 3 .|.
		x' `shiftL` 1 .|. x' `shiftR` 1
		where x' = fromIntegral x
	GrayWord4_ x ->
		x' `shiftL` 27 .|. x' `shiftL` 23 .|.
		x' `shiftL` 19 .|. x' `shiftL` 15 .|.
		x' `shiftL` 11 .|. x' `shiftL` 7 .|.
		x' `shiftL` 3 .|. x' `shiftR` 1
		where x' = fromIntegral x
	GrayWord8_ x ->
		x' `shiftL` 23 .|. x' `shiftL` 15 .|.
		x' `shiftL` 7 .|. x' `shiftR` 1
		where x' = fromIntegral x
	GrayWord16_ x -> x' `shiftL` 15 .|. x' `shiftR` 1
		where x' = fromIntegral x
	GrayWord32_ x -> fromIntegral $ x `shiftR` 1
	GrayInt32_ x -> x
	GrayDouble_ x -> fracToInt32 x

fracToInt32 :: RealFrac d => d -> Int32
fracToInt32 = round . (* 0x7fffffff)

{-# COMPLETE GrayDouble #-}

pattern GrayDouble :: RealFrac d => d -> Gray d
pattern GrayDouble x <- (grayToFrac -> x)

grayDouble :: (Ord d, Num d) => d -> Maybe (Gray d)
grayDouble x
	| 0 <= x && x <= 1 = Just $ GrayDouble_ x
	| otherwise = Nothing

grayToFrac :: (Eq d, Fractional d) => Gray d -> d
grayToFrac = \case
	GrayWord1_ x -> word1ToFrac x
	GrayWord2_ x -> word2ToFrac x
	GrayWord4_ x -> word4ToFrac x
	GrayWord8_ x -> word8ToFrac x
	GrayWord16_ x -> word16ToFrac x
	GrayWord32_ x -> word32ToFrac x
	GrayInt32_ x -> int32ToFrac x
	GrayDouble_ x -> x

word1ToFrac :: Fractional d => Word8 -> d
word1ToFrac = fromIntegral

word2ToFrac :: Fractional d => Word8 -> d
word2ToFrac = (/ 0x3) . fromIntegral

word4ToFrac :: Fractional d => Word8 -> d
word4ToFrac = (/ 0xf) . fromIntegral

word8ToFrac :: Fractional d => Word8 -> d
word8ToFrac = (/ 0xff) . fromIntegral

word16ToFrac :: Fractional d => Word16 -> d
word16ToFrac = (/ 0xffff) . fromIntegral

word32ToFrac :: Fractional d => Word32 -> d
word32ToFrac = (/ 0xffffffff) . fromIntegral

int32ToFrac :: Fractional d => Int32 -> d
int32ToFrac = (/ 0x7fffffff) . fromIntegral

-- GRAY ALPHA

data GrayAlpha d
	= GrayAlphaWord8_ Word8 Word8
	| GrayAlphaWord16_ Word16 Word16
	| GrayAlphaWord32_ Word32 Word32
	| GrayAlphaInt32_ Int32 Int32
	| GrayAlphaDouble_ d d
	deriving Show
