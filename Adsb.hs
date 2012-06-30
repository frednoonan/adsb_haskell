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

type Lat            = Double
type Lon            = Double
type Position       = (Lat, Lon)

data AdsbFrame = AdsbFrameAllCallReply IcaoId Capability
	| AdsbFrameIdentificationReport IcaoId Capability Identification
	| AdsbFramePositionReport       IcaoId Capability Position
	| AdsbFrameTrackReport          IcaoId Capability Position
	| AdsbFrameOther                IcaoId Capability DownlinkFormat SubType
	deriving (Eq, Ord, Show)

-- parse a hex-string of a frame into an AdsbFrame
parseFrame :: String -> AdsbFrame
parseFrame hex =
	-- FIXME error handling if frame has unexpected size
	case downlinkFormat of
		11 -> AdsbFrameAllCallReply icaoId capability
		17 -> case subType of
			4 -> AdsbFrameIdentificationReport icaoId capability (parseId hex)
			_ -> error "Unsupported subtype format"
			where subType = parseSubType hex
		_ -> error "Unsupported downlink format"
	where
		icaoId        = parseIcaoId hex
		capability    = parseCapability hex
		downlinkFormat = parseDownlinkFormat hex

substr :: Int -> Int -> String -> String
substr start length = (take length) . (drop start)

-- ICAO ID is substring from 2 to 6 in the frame
parseIcaoId :: String -> IcaoId
parseIcaoId hex = substr 2 6 hex

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
parseId hex = map icaoCharCodeToChar (idCharcodes $ byteFromHex (substr 10 12 hex))

-- decoding identification frames,
-- maps a hex-decoded integer to ICAO charcodes
-- splits 48bit int into 6bit chunks
idCharcodes :: Integer -> [Integer]
idCharcodes int = map (\e -> (snd e `shiftR` ((fst e)*6)) .&. 0x3f) (reverse [0..7] `zip` repeat int)

-- decoding identification frames,
-- map a char code to its corresponding character
icaoCharCodeToChar :: Integer -> Char
icaoCharCodeToChar code =
	if code <= 26 then -- A-Z, cf. table on page 75/3-62
		chr (64 + fromIntegral code)
	else -- SPC and 0-9
		chr $ fromIntegral code
