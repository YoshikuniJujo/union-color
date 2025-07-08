{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Color (

	-- * ALPHA

	Alpha, pattern AlphaWord8, pattern AlphaWord16, pattern AlphaWord32,
	pattern AlphaInt32, alphaInt32,
	pattern AlphaDouble, alphaDouble, alphaRealToFrac,

	-- * RGB

	Rgb, pattern RgbWord8, pattern RgbWord16, pattern RgbWord32,
	pattern RgbInt32, rgbInt32,
	pattern RgbDouble, rgbDouble, rgbRealToFrac,

	-- * RGBA

	-- ** Straight

	Rgba, pattern RgbaWord8, pattern RgbaWord16, pattern RgbaWord32,
	pattern RgbaInt32, rgbaInt32,
	pattern RgbaDouble, rgbaDouble,

	-- ** Premultiplied

	pattern RgbaPremultipliedWord8, rgbaPremultipliedWord8,
	pattern RgbaPremultipliedWord16, rgbaPremultipliedWord16,
	pattern RgbaPremultipliedDouble, rgbaPremultipliedDouble,

	-- ** From and To Rgb and Alpha

	toRgba, fromRgba,

	-- ** Convert Fractional

	rgbaRealToFrac

	-- * GRAY SCALE

	-- * GRAY SCALE WITH ALPHA

	) where

import Data.Color.Internal
