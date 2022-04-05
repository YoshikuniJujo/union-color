{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Color (
	-- * Alpha
	Alpha, pattern AlphaWord8, pattern AlphaWord16, pattern AlphaWord32,
	pattern AlphaDouble, alphaDouble, alphaRealToFrac,
	-- * RGB
	Rgb, pattern RgbWord8, pattern RgbWord16, pattern RgbWord32,
	pattern RgbDouble, rgbDouble, rgbRealToFrac,
	-- * RGBA
	-- ** Straight
	Rgba, pattern RgbaWord8, pattern RgbaWord16, pattern RgbaWord32,
	pattern RgbaDouble, rgbaDouble,
	-- ** Premultiplied
	pattern RgbaPremultipliedWord8, rgbaPremultipliedWord8,
	pattern RgbaPremultipliedWord16, rgbaPremultipliedWord16,
	pattern RgbaPremultipliedDouble, rgbaPremultipliedDouble,
	-- ** From and To Rgb and Alpha
	toRgba, fromRgba,
	-- ** Convert Fractional
	rgbaRealToFrac ) where

import Data.Color.Internal
