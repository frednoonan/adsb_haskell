module Adsb where

import Numeric
import Data.Bits
import Data.Char

type DownlinkFormat = Integer
type SubType        = Integer
type Identification = String
type IcaoId         = String
type Time           = String -- FIXME
type Capability     = Integer
type RawFrame       = String

type Lat            = Double
type Lon            = Double
type Position       = (Lat, Lon)

-- FIXME record syntax
data AdsbFrame = AdsbFrameAllCallReply RawFrame IcaoId Capability
	| AdsbFrameIdentificationReport    RawFrame IcaoId Capability Identification
	| AdsbFramePositionReport          RawFrame IcaoId Capability Position
	| AdsbFrameTrackReport             RawFrame IcaoId Capability Position
	| AdsbFrameOther                   RawFrame IcaoId Capability DownlinkFormat SubType

-- parse a hex-string of a frame into an AdsbFrame
instance Read AdsbFrame where
	readsPrec _ hex = [(parseFrame hex, "")]
		where parseFrame hex =
			-- FIXME error handling if frame has unexpected size
			case downlinkFormat of
				11 -> AdsbFrameAllCallReply hex icaoId capability
				17 -> case subType of
					4 -> AdsbFrameIdentificationReport hex icaoId capability (parseId hex)
					_ -> error "Unsupported subtype format"
					where subType = parseSubType hex
				_ -> error "Unsupported downlink format"
			where
				icaoId        = parseIcaoId hex
				capability    = parseCapability hex
				downlinkFormat = parseDownlinkFormat hex

instance Show AdsbFrame where
	show (AdsbFrameAllCallReply         rawFrame _ _    ) = rawFrame
	show (AdsbFrameIdentificationReport rawFrame _ _ _  ) = rawFrame
	show (AdsbFramePositionReport       rawFrame _ _ _  ) = rawFrame
	show (AdsbFrameTrackReport          rawFrame _ _ _  ) = rawFrame
	show (AdsbFrameOther                rawFrame _ _ _ _) = rawFrame

substr :: Int -> Int -> String -> String
substr start length = (take length) . (drop start)

-- ICAO ID is substring from 2 to 6 in the frame
parseIcaoId :: String -> IcaoId
parseIcaoId = substr 2 6

-- capability is lower three bytes (and with 7) of first byte
parseCapability :: String -> Capability
parseCapability hex = byteFromHex (substr 0 2 hex) .&. 7

-- downlink format is upper 5 bits (right shift 3) of first byte
parseDownlinkFormat :: String -> DownlinkFormat
parseDownlinkFormat hex = byteFromHex (substr 0 2 hex) `shiftR` 3

-- subtype is upper 5 bits of first message byte (starts at offset 8)
parseSubType :: String -> SubType
parseSubType hex = byteFromHex (substr 8 2 hex) `shiftR` 3

byteFromHex :: String -> Integer
byteFromHex = fst . head . readHex

parseId :: String -> Identification
parseId = map icaoCharCodeToChar . idCharcodes . byteFromHex . substr 10 12

-- decoding identification frames,
-- maps a hex-decoded integer to ICAO charcodes
-- splits 48bit int into 6bit chunks
idCharcodes :: Integer -> [Integer]
idCharcodes = map extractSixBits . zip ([42, 36..0]) . repeat
	where extractSixBits (shiftWidth, num) = num `shiftR` shiftWidth .&. 0x3f

-- decoding identification frames,
-- map a char code to its corresponding character
icaoCharCodeToChar :: Integer -> Char
icaoCharCodeToChar code =
	if code <= 26 then -- A-Z, cf. table on page 75/3-62
		chr (64 + fromIntegral code)
	else -- SPC and 0-9
		chr $ fromIntegral code
