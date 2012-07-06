module Adsb.Frame where

import Numeric
import Data.Bits
import Data.Char

type DownlinkFormat = Integer
type SubType        = Integer
type Identification = String
type IcaoId         = String
type Time           = String -- FIXME
type Capability     = Integer

type RawLat         = Integer
type RawLon         = Integer
type RawAltitude    = Integer
type RawPosition    = (RawLat, RawLon, RawAltitude)

type Lat            = Double
type Lon            = Double
type Altitude       = Integer
type Position       = (Lat, Lon, Altitude)

data FrameType = Even | Odd
	deriving (Eq, Ord, Show)

data AdsbFrame = AdsbFrameAllCallReply IcaoId Capability
	| AdsbFrameIdentificationReport IcaoId Capability Identification
	| AdsbFramePositionReport       IcaoId Capability RawPosition FrameType
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
			11 -> AdsbFramePositionReport      icaoId capability (parseRawPosition hex) (frameType hex) -- from the example, not sure if this works exactly for other cases
			12 -> AdsbFramePositionReport      icaoId capability (parseRawPosition hex) (frameType hex)
			18 -> AdsbFramePositionReport      icaoId capability (parseRawPosition hex) (frameType hex)
			_ -> AdsbFrameOther                icaoId capability 17 subType
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

parseRawPosition :: String -> RawPosition
parseRawPosition hex =  (rawLat, rawLon, rawAlt)
	where rawAlt = bitsFromHex hex 40 12
	      rawLat = bitsFromHex hex 54 17
	      rawLon = bitsFromHex hex 71 17

bitsFromHex :: String -> Int -> Int -> Integer
bitsFromHex hex start len =
	byteFromHex hex `shiftR` (bitLength hex - (start + len)) .&. (2^len-1)
	where bitLength hex = 4 * length hex

frameType :: String -> FrameType
frameType hex =
	case bitsFromHex hex 53 1 of
		0 -> Even
		_ -> Odd

-- compute positions from two frames (even and odd),
positionsFromTwoFrames :: AdsbFrame -> AdsbFrame -> (Position, Position)
positionsFromTwoFrames (AdsbFramePositionReport icaoId1 cap1 rawPos1 ft1) (AdsbFramePositionReport icaoId2 cap2 rawPos2 ft2)
	| icaoId1 /= icaoId2         = error "different ICAO-IDs for frames"
	| ft1 == Even && ft2 == Odd  = computePositions rawPos1 rawPos2
	| ft1 == Odd  && ft2 == Even = computePositions rawPos2 rawPos1
	| otherwise                  = error "incorrect frame formats (even/odd position reports required)"

-- decode compact position reporting format, see
-- http://www.lll.lu/~edward/edward/adsb/DecodingADSBposition.html
-- FIXME: work on altitude decding, for now raw altitude is passed on
computePositions :: RawPosition -> RawPosition -> (Position, Position)
computePositions (rawLat0, rawLon0, rawAlt0) (rawLat1, rawLon1, rawAlt1)
	| nl0 /= nl1 = error "NL(0) != NL(1)"
	| otherwise  = ((lat0, lon, rawAlt0), (lat1, lon, rawAlt1))
	where lat0  = rlat0 li rawLat0
	      lat1  = rlat1 li rawLat1
	      li    = latIndex rawLat0 rawLat1
	      nl0   = nL lat0
	      nl1   = nL lat1
	      ni1   = nI nl1
	      m     = longitudeIndex rawLon0 rawLon1 nl1
	      lon   = globalLon rawLon1 m nl1

latIndex :: RawLat -> RawLat -> Integer
latIndex rawLat0 rawLat1 =
	floor((fromIntegral(59*rawLat0 - 60*rawLat1) / 131072) + 0.5)

rlat0 :: Integer -> RawLat -> Double
rlat0 latIndex lat = 6.0 * (fromIntegral(latIndex `mod` 60) + fromIntegral(lat)/131072)

rlat1 :: Integer -> RawLat -> Double
rlat1 latIndex lat = (360.0/59.0) * (fromIntegral(latIndex `mod` 59) + fromIntegral(lat)/131072)

-- map result of rlat to number of longitude zones, NL
nL :: Double -> Integer
nL rlat =
	case smallerNL of
		[] -> 1
		_  -> snd $ head smallerNL
	where smallerNL = filter (\e -> rlat < fst e) nlLimits

nI :: Integer -> Integer
nI nl = max 1 (nl - 1)

dLon1 :: Integer -> Double
dLon1 ni = 360 / fromIntegral(ni)

longitudeIndex :: RawLon -> RawLon -> Integer -> Integer
longitudeIndex rawLon0 rawLon1 nl1 = floor(fromIntegral(((rawLon0 * (nl1 - 1)) - (rawLon1 * nl1))) / 131072 + 0.5)

globalLon :: RawLon -> Integer -> Integer -> Double
globalLon rawLon1 lonIndex nl =
	(dLon1 ni) * (fromIntegral(lonIndex `mod` ni) + fromIntegral(rawLon1) / 131072)
	where ni = nI nl

nlLimits = [
	(10.47047130, 59),
	(14.82817437, 58),
	(18.18626357, 57),
	(21.02939493, 56),
	(23.54504487, 55),
	(25.82924707, 54),
	(27.93898710, 53),
	(29.91135686, 52),
	(31.77209708, 51),
	(33.53993436, 50),
	(35.22899598, 49),
	(36.85025108, 48),
	(38.41241892, 47),
	(39.92256684, 46),
	(41.38651832, 45),
	(42.80914012, 44),
	(44.19454951, 43),
	(45.54626723, 42),
	(46.86733252, 41),
	(48.16039128, 40),
	(49.42776439, 39),
	(50.67150166, 38),
	(51.89342469, 37),
	(53.09516153, 36),
	(54.27817472, 35),
	(55.44378444, 34),
	(56.59318756, 33),
	(57.72747354, 31),
	(58.84763776, 30),
	(59.95459277, 30),
	(61.04917774, 29),
	(62.13216659, 28),
	(63.20427479, 27),
	(64.26616523, 26),
	(65.31845310, 25),
	(66.36171008, 24),
	(67.39646774, 23),
	(68.42322022, 22),
	(69.44242631, 21),
	(70.45451075, 20),
	(71.45986473, 19),
	(72.45884545, 18),
	(73.45177442, 17),
	(74.43893416, 16),
	(75.42056257, 15),
	(76.39684391, 14),
	(77.36789461, 13),
	(78.33374083, 12),
	(79.29428225, 11),
	(80.24923213, 10),
	(81.19801349, 9),
	(82.13956981, 8),
	(83.07199445, 7),
	(83.99173563, 6),
	(84.89166191, 5),
	(85.75541621, 4),
	(86.53536998, 3),
	(87.00000000, 2)]
