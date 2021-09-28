{-# LANGUAGE LambdaCase, ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Color (
	-- * Alpha
	Alpha, pattern AlphaWord8, pattern AlphaWord16, pattern AlphaDouble,
	alphaDouble, alphaRealToFrac,
	-- * RGB
	Rgb, pattern RgbWord8, pattern RgbWord16, pattern RgbDouble,
	rgbDouble, rgbRealToFrac,
	-- * RGBA
	-- ** Straight
	Rgba, pattern RgbaWord8, pattern RgbaWord16, pattern RgbaDouble,
	rgbaDouble,
	-- ** Premultiplied
	pattern RgbaPremultipliedWord8, rgbaPremultipliedWord8,
	pattern RgbaPremultipliedWord16, rgbaPremultipliedWord16,
	pattern RgbaPremultipliedDouble, rgbaPremultipliedDouble,
	-- ** From and To Rgb and Alpha
--	fromRgba,
	-- ** Convert Fractional
	rgbaRealToFrac ) where

import Data.Bits
import Data.Word

data Alpha d = AlphaWord8_ Word8 | AlphaWord16_ Word16 | AlphaDouble_ d
	deriving Show

{-# COMPLETE AlphaWord8 #-}

pattern AlphaWord8 :: RealFrac d => Word8 -> Alpha d
pattern AlphaWord8 a <- (fromAlphaWord8 -> a)
	where AlphaWord8 = AlphaWord8_

fromAlphaWord8 :: RealFrac d => Alpha d -> Word8
fromAlphaWord8 = \case
	AlphaWord8_ a -> a
	AlphaWord16_ a -> fromIntegral $ a `shiftR` 8
	AlphaDouble_ a -> cDoubleToWord8 a

{-# COMPLETE AlphaWord16 #-}

pattern AlphaWord16 :: RealFrac d => Word16 -> Alpha d
pattern AlphaWord16 a <- (fromAlphaWord16 -> a)
	where AlphaWord16 = AlphaWord16_

fromAlphaWord16 :: RealFrac d => Alpha d -> Word16
fromAlphaWord16 = \case
	AlphaWord8_ (fromIntegral -> a) -> a `shiftL` 8 .|. a
	AlphaWord16_ a -> a
	AlphaDouble_ a -> cDoubleToWord16 a

{-# COMPLETE AlphaDouble #-}

pattern AlphaDouble :: Fractional d => d -> (Alpha d)
pattern AlphaDouble a <- (fromAlphaDouble -> a)

fromAlphaDouble :: Fractional d => Alpha d -> d
fromAlphaDouble = \case
	AlphaWord8_ a -> word8ToCDouble a
	AlphaWord16_ a -> word16ToCDouble a
	AlphaDouble_ a -> a

alphaDouble :: (Ord d, Num d) => d -> Maybe (Alpha d)
alphaDouble a
	| from0to1 a = Just $ AlphaDouble_ a
	| otherwise = Nothing

alphaRealToFrac :: (Real d, Fractional d') => Alpha d -> Alpha d'
alphaRealToFrac = \case
	AlphaWord8_ a -> AlphaWord8_ a
	AlphaWord16_ a -> AlphaWord16_ a
	AlphaDouble_ a -> AlphaDouble_ $ realToFrac a

data Rgb d
	= RgbWord8_ Word8 Word8 Word8
	| RgbWord16_ Word16 Word16 Word16
	| RgbDouble_ d d d
	deriving Show

{-# COMPLETE RgbWord8 #-}

pattern RgbWord8 :: RealFrac d => Word8 -> Word8 -> Word8 -> Rgb d
pattern RgbWord8 r g b <- (fromRgbWord8 -> (r, g, b))
	where RgbWord8 = RgbWord8_

fromRgbWord8 :: RealFrac d => Rgb d -> (Word8, Word8, Word8)
fromRgbWord8 = \case
	RgbWord8_ r g b -> (r, g, b)
	RgbWord16_ r g b -> (
		fromIntegral $ r `shiftR` 8,
		fromIntegral $ g `shiftR` 8,
		fromIntegral $ b `shiftR` 8 )
	RgbDouble_ r g b ->
		let [r', g', b'] = cDoubleToWord8 <$> [r, g, b] in (r', g', b')

{-# COMPLETE RgbWord16 #-}

pattern RgbWord16 :: RealFrac d => Word16 -> Word16 -> Word16 -> Rgb d
pattern RgbWord16 r g b <- (fromRgbWord16 -> (r, g, b))
	where RgbWord16 = RgbWord16_

fromRgbWord16 :: RealFrac d => Rgb d -> (Word16, Word16, Word16)
fromRgbWord16 = \case
	RgbWord8_ (fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b) ->
		(r `shiftL` 8 .|. r, g `shiftL` 8 .|. g, b `shiftL` 8 .|. b)
	RgbWord16_ r g b -> (r, g, b)
	RgbDouble_ r g b ->
		let [r', g', b'] = cDoubleToWord16 <$> [r, g, b] in (r', g', b')

{-# COMPLETE RgbDouble #-}

pattern RgbDouble :: Fractional d => d -> d -> d -> (Rgb d)
pattern RgbDouble r g b <- (fromRgbDouble -> (r, g, b))

fromRgbDouble :: Fractional d => Rgb d -> (d, d, d)
fromRgbDouble = \case
	RgbWord8_ r g b ->
		let [r', g', b'] = word8ToCDouble <$> [r, g, b] in (r', g', b')
	RgbWord16_ r g b ->
		let [r', g', b'] = word16ToCDouble <$> [r, g, b] in (r', g', b')
	RgbDouble_ r g b -> (r, g, b)

rgbDouble :: (Ord d, Num d) => d -> d -> d -> Maybe (Rgb d)
rgbDouble r g b
	| from0to1 r && from0to1 g && from0to1 b = Just $ RgbDouble_ r g b
	| otherwise = Nothing

rgbRealToFrac :: (Real d, Fractional d') => Rgb d -> Rgb d'
rgbRealToFrac = \case
	RgbWord8_ r g b -> RgbWord8_ r g b
	RgbWord16_ r g b -> RgbWord16_ r g b
	RgbDouble_ r g b -> RgbDouble_ r' g' b'
		where [r', g', b'] = realToFrac <$> [r, g, b]

data Rgba d
	= RgbaWord8_ Word8 Word8 Word8 Word8
	| RgbaWord16_ Word16 Word16 Word16 Word16
	| RgbaDouble_ d d d d
	| RgbaPremultipliedWord8_ Word8 Word8 Word8 Word8
	| RgbaPremultipliedWord16_ Word16 Word16 Word16 Word16
	| RgbaPremultipliedDouble_ d d d d
	deriving Show

{-# COMPLETE RgbaWord8 #-}

pattern RgbaWord8 :: RealFrac d => Word8 -> Word8 -> Word8 -> Word8 -> Rgba d
pattern RgbaWord8 r g b a <- (fromRgbaWord8 -> (r, g, b, a))
	where RgbaWord8 = RgbaWord8_

fromRgbaWord8 :: RealFrac d => Rgba d -> (Word8, Word8, Word8, Word8)
fromRgbaWord8 = \case
	RgbaWord8_ r g b a -> (r, g, b, a)
	RgbaWord16_ r g b a -> (
		fromIntegral $ r `shiftR` 8,
		fromIntegral $ g `shiftR` 8,
		fromIntegral $ b `shiftR` 8,
		fromIntegral $ a `shiftR` 8 )
	RgbaDouble_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = cDoubleToWord8 <$> [r, g, b, a]
	RgbaPremultipliedWord8_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = unPremultipliedWord8 (r, g, b, a)
	RgbaPremultipliedWord16_ r g b a -> (
		fromIntegral $ r' `shiftR` 8,
		fromIntegral $ g' `shiftR` 8,
		fromIntegral $ b' `shiftR` 8,
		fromIntegral $ a' `shiftR` 8 )
		where [r', g', b', a'] = unPremultipliedWord16 (r, g, b, a)
	RgbaPremultipliedDouble_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] =
			cDoubleToWord8 <$> unPremultipliedDouble (r, g, b, a)

{-# COMPLETE RgbaWord16 #-}

pattern RgbaWord16 :: RealFrac d => Word16 -> Word16 -> Word16 -> Word16 -> Rgba d
pattern RgbaWord16 r g b a <- (fromRgbaWord16 -> (r, g, b, a))
	where RgbaWord16 = RgbaWord16_

fromRgbaWord16 :: RealFrac d => Rgba d -> (Word16, Word16, Word16, Word16)
fromRgbaWord16 = \case
	RgbaWord8_
		(fromIntegral -> r) (fromIntegral -> g)
		(fromIntegral -> b) (fromIntegral -> a) -> (
		r `shiftL` 8 .|. r, g `shiftL` 8 .|. g,
		b `shiftL` 8 .|. b, a `shiftL` 8 .|. a)
	RgbaWord16_ r g b a -> (r, g, b, a)
	RgbaDouble_ r g b a ->
		let [r', g', b', a'] = cDoubleToWord16 <$> [r, g, b, a] in (r', g', b', a')
	RgbaPremultipliedWord8_ r g b a -> (
		r' `shiftL` 8 .|. r', g' `shiftL` 8 .|. g',
		b' `shiftL` 8 .|. b', a' `shiftL` 8 .|. a')
		where [r', g', b', a'] =
			fromIntegral <$> unPremultipliedWord8 (r, g, b, a)
	RgbaPremultipliedWord16_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = unPremultipliedWord16 (r, g, b, a)
	RgbaPremultipliedDouble_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] =
			cDoubleToWord16 <$> unPremultipliedDouble (r, g, b, a)

{-# COMPLETE RgbaDouble #-}

pattern RgbaDouble :: (Eq d, Fractional d) => d -> d -> d -> d -> Rgba d
pattern RgbaDouble r g b a <- (fromRgbaDouble -> (r, g, b, a))

fromRgbaDouble :: (Eq d, Fractional d) => Rgba d -> (d, d, d, d)
fromRgbaDouble = \case
	RgbaWord8_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = word8ToCDouble <$> [r, g, b, a]
	RgbaWord16_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = word16ToCDouble <$> [r, g, b, a]
	RgbaDouble_ r g b a -> (r, g, b, a)
	RgbaPremultipliedWord8_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] =
			word8ToCDouble <$> unPremultipliedWord8 (r, g, b, a)
	RgbaPremultipliedWord16_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] =
			word16ToCDouble <$> unPremultipliedWord16 (r, g, b, a)
	RgbaPremultipliedDouble_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = unPremultipliedDouble (r, g, b, a)

rgbaDouble :: (Ord d, Num d) => d -> d -> d -> d -> Maybe (Rgba d)
rgbaDouble r g b a
	| from0to1 r && from0to1 g && from0to1 b && from0to1 a =
		Just $ RgbaDouble_ r g b a
	| otherwise = Nothing

{-
fromRgba :: Rgba d -> (Rgb d, Alpha d)
fromRgba = \case
	RgbaWord8_ r g b a -> (RgbWord8_ r g b, AlphaWord8_ a)
	RgbaWord16_ r g b a -> (RgbWord16_ r g b, AlphaWord16_ a)
	RgbaDouble_ r g b a -> (RgbDouble_ r g b, AlphaDouble_ a)
	RgbPremultipliedWord8_ 
	-}

rgbaRealToFrac :: (Real d, Fractional d') => Rgba d -> Rgba d'
rgbaRealToFrac = \case
	RgbaWord8_ r g b a -> RgbaWord8_ r g b a
	RgbaWord16_ r g b a -> RgbaWord16_ r g b a
	RgbaDouble_ r g b a -> RgbaDouble_ r' g' b' a'
		where [r', g', b', a'] = realToFrac <$> [r, g, b, a]
	RgbaPremultipliedWord8_ r g b a -> RgbaPremultipliedWord8_ r g b a
	RgbaPremultipliedWord16_ r g b a -> RgbaPremultipliedWord16_ r g b a
	RgbaPremultipliedDouble_ r g b a -> RgbaPremultipliedDouble_ r' g' b' a'
		where [r', g', b', a'] = realToFrac <$> [r, g, b, a]

cDoubleToWord8 :: RealFrac d => d -> Word8
cDoubleToWord8 = round . (* 0xff)

cDoubleToWord16 :: RealFrac d => d -> Word16
cDoubleToWord16 = round . (* 0xffff)

word8ToCDouble :: Fractional d => Word8 -> d
word8ToCDouble = (/ 0xff) . fromIntegral

word16ToCDouble :: Fractional d => Word16 -> d
word16ToCDouble = (/ 0xffff) . fromIntegral

from0to1 :: (Ord d, Num d) => d -> Bool
from0to1 n = 0 <= n && n <= 1

{-# COMPLETE RgbaPremultipliedWord8 #-}

pattern RgbaPremultipliedWord8 ::
	RealFrac d => Word8 -> Word8 -> Word8 -> Word8 -> Rgba d
pattern RgbaPremultipliedWord8 r g b a <-
	(fromRgbaPremultipliedWord8 -> (r, g, b, a))

rgbaPremultipliedWord8 :: Word8 -> Word8 -> Word8 -> Word8 -> Maybe (Rgba d)
rgbaPremultipliedWord8 r g b a
	| r <= a && g <= a && b <= a = Just $ RgbaPremultipliedWord8_ r g b a
	| otherwise = Nothing

fromRgbaPremultipliedWord8 :: RealFrac d => Rgba d -> (Word8, Word8, Word8, Word8)
fromRgbaPremultipliedWord8 = toPremultipliedWord8 . fromRgbaWord8

toPremultipliedWord8 ::
	(Word8, Word8, Word8, Word8) -> (Word8, Word8, Word8, Word8)
toPremultipliedWord8 (
	fromIntegral -> r, fromIntegral -> g,
	fromIntegral -> b, fromIntegral -> a) = (r', g', b', a')
	where
	[r', g', b', a'] = fromIntegral <$> [
		r * a `div` 0xff, g * a `div` 0xff, b * a `div` 0xff,
		a :: Word16 ]

unPremultipliedWord8 :: (Word8, Word8, Word8, Word8) -> [Word8]
unPremultipliedWord8 (
	fromIntegral -> r, fromIntegral -> g,
	fromIntegral -> b, fromIntegral -> a ) = fromIntegral <$> [
		r * 0xff `div'` a, g * 0xff `div'` a, b * 0xff `div'` a,
		a :: Word16 ]

{-# COMPLETE RgbaPremultipliedWord16 #-}

pattern RgbaPremultipliedWord16 ::
	RealFrac d => Word16 -> Word16 -> Word16 -> Word16 -> Rgba d
pattern RgbaPremultipliedWord16 r g b a <-
	(fromRgbaPremultipliedWord16 -> (r, g, b, a))

rgbaPremultipliedWord16 :: Word16 -> Word16 -> Word16 -> Word16 -> Maybe (Rgba d)
rgbaPremultipliedWord16 r g b a
	| r <= a && g <= a && b <= a = Just $ RgbaPremultipliedWord16_ r g b a
	| otherwise = Nothing

fromRgbaPremultipliedWord16 ::
	RealFrac d => Rgba d -> (Word16, Word16, Word16, Word16)
fromRgbaPremultipliedWord16 = toPremultipliedWord16 . fromRgbaWord16

toPremultipliedWord16 ::
	(Word16, Word16, Word16, Word16) -> (Word16, Word16, Word16, Word16)
toPremultipliedWord16 (
	fromIntegral -> r, fromIntegral -> g,
	fromIntegral -> b, fromIntegral -> a ) = (r', g', b', a')
	where
	[r', g', b', a'] = fromIntegral <$> [
		r * a `div` 0xffff, g * a `div` 0xffff, b * a `div` 0xffff,
		a :: Word32 ]

unPremultipliedWord16 :: (Word16, Word16, Word16, Word16) -> [Word16]
unPremultipliedWord16 (
	fromIntegral -> r, fromIntegral -> g,
	fromIntegral -> b, fromIntegral -> a ) = fromIntegral <$> [
		r * 0xffff `div'` a, g * 0xffff `div'` a, b * 0xff `div'` a,
		a :: Word32 ]

pattern RgbaPremultipliedDouble :: (Eq d, Fractional d) => d -> d -> d -> d -> Rgba d
pattern RgbaPremultipliedDouble r g b a <-
	(fromRgbaPremultipliedDouble -> (r, g, b, a))

rgbaPremultipliedDouble :: (Ord d, Num d) => d -> d -> d -> d -> Maybe (Rgba d)
rgbaPremultipliedDouble r g b a
	| 0 <= r && r <= a, 0 <= g && g <= a, 0 <= b && b <= a,
		0 <= a && a <= 1 = Just $ RgbaPremultipliedDouble_ r g b a
	| otherwise = Nothing

fromRgbaPremultipliedDouble :: (Eq d, Fractional d) => Rgba d -> (d, d, d, d)
fromRgbaPremultipliedDouble = toPremultipliedDouble . fromRgbaDouble

toPremultipliedDouble :: Fractional d => (d, d, d, d) -> (d, d, d, d)
toPremultipliedDouble (r, g, b, a) = (r * a, g * a, b * a, a)

unPremultipliedDouble :: (Eq d, Fractional d) => (d, d, d, d) -> [d]
unPremultipliedDouble (r, g, b, a) = [r ./. a, g ./. a, b ./. a, a]

div' :: Integral n => n -> n -> n
0 `div'` 0 = 0
a `div'` b = a `div` b

(./.) :: (Eq a, Fractional a) => a -> a -> a
0 ./. 0 = 0
a ./. b = a / b
