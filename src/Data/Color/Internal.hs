{-# LANGUAGE LambdaCase, ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Color.Internal (
	-- * Alpha
	Alpha(..), pattern AlphaWord8, pattern AlphaWord16, pattern AlphaWord32,
	pattern AlphaDouble, alphaDouble, alphaRealToFrac,
	-- * RGB
	Rgb(..), pattern RgbWord8, pattern RgbWord16, pattern RgbWord32,
	pattern RgbDouble, rgbDouble, rgbRealToFrac,
	-- * RGBA
	-- ** Straight
	Rgba(..), pattern RgbaWord8, pattern RgbaWord16, pattern RgbaWord32,
	pattern RgbaDouble, rgbaDouble,
	-- ** Premultiplied
	pattern RgbaPremultipliedWord8, rgbaPremultipliedWord8,
	pattern RgbaPremultipliedWord16, rgbaPremultipliedWord16,
	pattern RgbaPremultipliedDouble, rgbaPremultipliedDouble,
	-- ** From and To Rgb and Alpha
	toRgba, fromRgba,
	-- ** Convert Fractional
	rgbaRealToFrac ) where

import Data.Bits
import Data.Word

data Alpha d = AlphaWord8_ Word8 | AlphaWord16_ Word16 | AlphaWord32_ Word32 | AlphaDouble_ d
	deriving Show

{-# COMPLETE AlphaWord8 #-}

pattern AlphaWord8 :: RealFrac d => Word8 -> Alpha d
pattern AlphaWord8 a <- (fromAlphaWord8 -> a)
	where AlphaWord8 = AlphaWord8_

fromAlphaWord8 :: RealFrac d => Alpha d -> Word8
fromAlphaWord8 = \case
	AlphaWord8_ a -> a
	AlphaWord16_ a -> fromIntegral $ a `shiftR` 8
	AlphaWord32_ a -> fromIntegral $ a `shiftR` 24
	AlphaDouble_ a -> cDoubleToWord8 a

{-# COMPLETE AlphaWord16 #-}

pattern AlphaWord16 :: RealFrac d => Word16 -> Alpha d
pattern AlphaWord16 a <- (fromAlphaWord16 -> a)
	where AlphaWord16 = AlphaWord16_

fromAlphaWord16 :: RealFrac d => Alpha d -> Word16
fromAlphaWord16 = \case
	AlphaWord8_ (fromIntegral -> a) -> a `shiftL` 8 .|. a
	AlphaWord16_ a -> a
	AlphaWord32_ a -> fromIntegral $ a `shiftR` 16
	AlphaDouble_ a -> cDoubleToWord16 a

{-# COMPLETE AlphaWord32 #-}

pattern AlphaWord32 :: RealFrac d => Word32 -> Alpha d
pattern AlphaWord32 a <- (fromAlphaWord32 -> a)
	where AlphaWord32 = AlphaWord32_

fromAlphaWord32 :: RealFrac d => Alpha d -> Word32
fromAlphaWord32 = \case
	AlphaWord8_ (fromIntegral -> a) ->
		a `shiftL` 24 .|. a `shiftL` 16 .|. a `shiftL` 8 .|. a
	AlphaWord16_ (fromIntegral -> a) -> a `shiftL` 16 .|. a
	AlphaWord32_ a -> a
	AlphaDouble_ a -> cDoubleToWord32 a

{-# COMPLETE AlphaDouble #-}

pattern AlphaDouble :: Fractional d => d -> (Alpha d)
pattern AlphaDouble a <- (fromAlphaDouble -> a)

fromAlphaDouble :: Fractional d => Alpha d -> d
fromAlphaDouble = \case
	AlphaWord8_ a -> word8ToCDouble a
	AlphaWord16_ a -> word16ToCDouble a
	AlphaWord32_ a -> word32ToCDouble a
	AlphaDouble_ a -> a

alphaDouble :: (Ord d, Num d) => d -> Maybe (Alpha d)
alphaDouble a
	| from0to1 a = Just $ AlphaDouble_ a
	| otherwise = Nothing

alphaRealToFrac :: (Real d, Fractional d') => Alpha d -> Alpha d'
alphaRealToFrac = \case
	AlphaWord8_ a -> AlphaWord8_ a
	AlphaWord16_ a -> AlphaWord16_ a
	AlphaWord32_ a -> AlphaWord32_ a
	AlphaDouble_ a -> AlphaDouble_ $ realToFrac a

data Rgb d
	= RgbWord8_ Word8 Word8 Word8
	| RgbWord16_ Word16 Word16 Word16
	| RgbWord32_ Word32 Word32 Word32
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
	RgbWord32_ r g b -> (
		fromIntegral $ r `shiftR` 24,
		fromIntegral $ g `shiftR` 24,
		fromIntegral $ b `shiftR` 24 )
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
	RgbWord32_ r g b -> (
		fromIntegral $ r `shiftR` 16,
		fromIntegral $ g `shiftR` 16,
		fromIntegral $ b `shiftR` 16 )
	RgbDouble_ r g b ->
		let [r', g', b'] = cDoubleToWord16 <$> [r, g, b] in (r', g', b')

{-# COMPLETE RgbWord32 #-}

pattern RgbWord32 :: RealFrac d => Word32 -> Word32 -> Word32 -> Rgb d
pattern RgbWord32 r g b <- (fromRgbWord32 -> (r, g, b))
	where RgbWord32 = RgbWord32_

fromRgbWord32 :: RealFrac d => Rgb d -> (Word32, Word32, Word32)
fromRgbWord32 = \case
	RgbWord8_ (fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b) ->
		(	r `shiftL` 24 .|. r `shiftL` 16 .|. r `shiftL` 8 .|. r,
			g `shiftL` 24 .|. g `shiftL` 16 .|. g `shiftL` 8 .|. g,
			b `shiftL` 24 .|. b `shiftL` 16 .|. b `shiftL` 8 .|. b )
	RgbWord16_ (fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b)
		-> (	r `shiftL` 16 .|. r, g `shiftL` 16 .|. g,
			b `shiftL` 16 .|. b )
	RgbWord32_ r g b -> (r, g, b)
	RgbDouble_ r g b ->
		let [r', g', b'] = cDoubleToWord32 <$> [r, g, b] in (r', g', b')

{-# COMPLETE RgbDouble #-}

pattern RgbDouble :: Fractional d => d -> d -> d -> (Rgb d)
pattern RgbDouble r g b <- (fromRgbDouble -> (r, g, b))

fromRgbDouble :: Fractional d => Rgb d -> (d, d, d)
fromRgbDouble = \case
	RgbWord8_ r g b ->
		let [r', g', b'] = word8ToCDouble <$> [r, g, b] in (r', g', b')
	RgbWord16_ r g b ->
		let [r', g', b'] = word16ToCDouble <$> [r, g, b] in (r', g', b')
	RgbWord32_ r g b ->
		let [r', g', b'] = word32ToCDouble <$> [r, g, b] in (r', g', b')
	RgbDouble_ r g b -> (r, g, b)

rgbDouble :: (Ord d, Num d) => d -> d -> d -> Maybe (Rgb d)
rgbDouble r g b
	| from0to1 r && from0to1 g && from0to1 b = Just $ RgbDouble_ r g b
	| otherwise = Nothing

rgbRealToFrac :: (Real d, Fractional d') => Rgb d -> Rgb d'
rgbRealToFrac = \case
	RgbWord8_ r g b -> RgbWord8_ r g b
	RgbWord16_ r g b -> RgbWord16_ r g b
	RgbWord32_ r g b -> RgbWord32_ r g b
	RgbDouble_ r g b -> RgbDouble_ r' g' b'
		where [r', g', b'] = realToFrac <$> [r, g, b]

data Rgba d
	= RgbaWord8_ Word8 Word8 Word8 Word8
	| RgbaWord16_ Word16 Word16 Word16 Word16
	| RgbaWord32_ Word32 Word32 Word32 Word32
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
	RgbaWord32_ r g b a -> (
		fromIntegral $ r `shiftR` 24,
		fromIntegral $ g `shiftR` 24,
		fromIntegral $ b `shiftR` 24,
		fromIntegral $ a `shiftR` 24 )
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
	RgbaWord32_ r g b a -> (
		fromIntegral $ r `shiftR` 16, fromIntegral $ g `shiftR` 16,
		fromIntegral $ b `shiftR` 16, fromIntegral $ a `shiftR` 16 )
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

{-# COMPLETE RgbaWord32 #-}

pattern RgbaWord32 :: RealFrac d =>
	Word32 -> Word32 -> Word32 -> Word32 -> Rgba d
pattern RgbaWord32 r g b a <- (fromRgbaWord32 -> (r, g, b, a))
	where RgbaWord32 = RgbaWord32_

fromRgbaWord32 :: RealFrac d => Rgba d -> (Word32, Word32, Word32, Word32)
fromRgbaWord32 = \case
	RgbaWord8_
		(fromIntegral -> r) (fromIntegral -> g)
		(fromIntegral -> b) (fromIntegral -> a) -> (
		r `shiftL` 24 .|. r `shiftL` 16 .|. r `shiftL` 8 .|. r,
		g `shiftL` 24 .|. g `shiftL` 16 .|. g `shiftL` 8 .|. g,
		b `shiftL` 24 .|. b `shiftL` 16 .|. b `shiftL` 8 .|. b,
		a `shiftL` 24 .|. a `shiftL` 16 .|. a `shiftL` 8 .|. a )
	RgbaWord16_
		(fromIntegral -> r) (fromIntegral -> g)
		(fromIntegral -> b) (fromIntegral -> a) -> (
		r `shiftL` 16 .|. r, g `shiftL` 16 .|. g,
		b `shiftL` 16 .|. b, a `shiftL` 16 .|. a )
	RgbaWord32_ r g b a -> (r, g, b, a)
	RgbaDouble_ r g b a -> let
		[r', g', b', a'] = cDoubleToWord32 <$> [r, g, b, a] in
		(r', g', b', a')
	RgbaPremultipliedWord8_ r g b a -> (
		r' `shiftL`  24 .|. r' `shiftL` 16 .|. r' `shiftL` 8 .|. r',
		g' `shiftL`  24 .|. g' `shiftL` 16 .|. g' `shiftL` 8 .|. g',
		b' `shiftL`  24 .|. b' `shiftL` 16 .|. b' `shiftL` 8 .|. b',
		a' `shiftL`  24 .|. a' `shiftL` 16 .|. a' `shiftL` 8 .|. a' )
		where [r', g', b', a'] =
			fromIntegral <$> unPremultipliedWord8 (r, g, b, a)
	RgbaPremultipliedWord16_ r g b a -> (
		r' `shiftL` 16 .|. r', g' `shiftL` 16 .|. g',
		b' `shiftL` 16 .|. b', a' `shiftL` 16 .|. a' )
		where [r', g', b', a'] =
			fromIntegral <$> unPremultipliedWord16 (r, g, b, a)
	RgbaPremultipliedDouble_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] =
			cDoubleToWord32 <$> unPremultipliedDouble (r, g, b, a)

{-# COMPLETE RgbaDouble #-}

pattern RgbaDouble :: (Eq d, Fractional d) => d -> d -> d -> d -> Rgba d
pattern RgbaDouble r g b a <- (fromRgbaDouble -> (r, g, b, a))

fromRgbaDouble :: (Eq d, Fractional d) => Rgba d -> (d, d, d, d)
fromRgbaDouble = \case
	RgbaWord8_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = word8ToCDouble <$> [r, g, b, a]
	RgbaWord16_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = word16ToCDouble <$> [r, g, b, a]
	RgbaWord32_ r g b a -> (r', g', b', a')
		where [r', g', b', a'] = word32ToCDouble <$> [r, g, b, a]
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

fromRgba :: (Eq d, Fractional d) => Rgba d -> (Rgb d, Alpha d)
fromRgba = \case
	RgbaWord8_ r g b a -> (RgbWord8_ r g b, AlphaWord8_ a)
	RgbaWord16_ r g b a -> (RgbWord16_ r g b, AlphaWord16_ a)
	RgbaWord32_ r g b a -> (RgbWord32_ r g b, AlphaWord32_ a)
	RgbaDouble_ r g b a -> (RgbDouble_ r g b, AlphaDouble_ a)
	RgbaPremultipliedWord8_ r g b a -> (RgbWord8_ r' g' b', AlphaWord8_ a')
		where [r', g', b', a'] = unPremultipliedWord8 (r, g, b, a)
	RgbaPremultipliedWord16_ r g b a -> (RgbWord16_ r' g' b', AlphaWord16_ a')
		where [r', g', b', a'] = unPremultipliedWord16 (r, g, b, a)
	RgbaPremultipliedDouble_ r g b a -> (RgbDouble_ r' g' b', AlphaDouble_ a')
		where [r', g', b', a'] = unPremultipliedDouble (r, g, b, a)

toRgba :: RealFrac d => Rgb d -> Alpha d -> Rgba d
toRgba (RgbWord8_ r g b) (AlphaWord8 a) = RgbaWord8 r g b a
toRgba (RgbWord16_ r g b) (AlphaWord16 a) = RgbaWord16 r g b a
toRgba (RgbWord32_ r g b) (AlphaWord32 a) = RgbaWord32 r g b a
toRgba (RgbDouble_ r g b) (AlphaDouble a) = RgbaDouble_ r g b a

rgbaRealToFrac :: (Real d, Fractional d') => Rgba d -> Rgba d'
rgbaRealToFrac = \case
	RgbaWord8_ r g b a -> RgbaWord8_ r g b a
	RgbaWord16_ r g b a -> RgbaWord16_ r g b a
	RgbaWord32_ r g b a -> RgbaWord32_ r g b a
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

cDoubleToWord32 :: RealFrac d => d -> Word32
cDoubleToWord32 = round . (* 0xffffffff)

word8ToCDouble :: Fractional d => Word8 -> d
word8ToCDouble = (/ 0xff) . fromIntegral

word16ToCDouble :: Fractional d => Word16 -> d
word16ToCDouble = (/ 0xffff) . fromIntegral

word32ToCDouble :: Fractional d => Word32 -> d
word32ToCDouble = (/ 0xffffffff) . fromIntegral

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
