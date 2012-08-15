import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Control.Exception
import Adsb.Frame
import Adsb.IcaoId (countryFromIcaoId)
import System.Environment (getArgs)
import Text.Regex.Posix
import System.IO
import Data.Maybe

client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
                addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
                let serverAddr = head addrInfo
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                sockHandle <- socketToHandle sock ReadMode
                messageLoop sockHandle  []
                sClose sock

frameRegex = "@[0-9A-F]{12}([0-9A-F]{28}|[0-9A-F]{14});" -- 112 bit ES or 56 bit frames with timestamp

messageLoop :: Handle -> [AdsbFrame] -> IO ()
messageLoop h frames = do
	line <- hGetLine h
	let matches = (line =~ frameRegex :: [[String]])
	let (maybeFrame, output) = case matches of
		[[_, rawFrame]] -> (Just $ parseFrame rawFrame, frameDetails rawFrame frames)
		_               -> (Nothing, "")
	putStr output
	if maybeFrame == Nothing then
		messageLoop h frames
	else
		messageLoop h (fromJust maybeFrame : take 10000 frames)

frameDetails :: String -> [AdsbFrame] -> String
frameDetails rawFrame frames =
	basics ++ maybePos ++ "\n"
		where parsedFrame = parseFrame rawFrame
		      basics = rawFrame ++ "\n" ++ (show parsedFrame) ++ "\n" ++ (show $ countryFromIcaoId $ icaoId parsedFrame)
		      position = case parsedFrame of
		      	(AdsbFramePositionReport icaoid cap rawpos Odd) -> getPosition parsedFrame frames
		      	_ -> Nothing
		      maybePos = fromMaybe "" $ fmap show position

getPosition :: AdsbFrame -> [AdsbFrame] -> Maybe (Position, Position)
getPosition (AdsbFramePositionReport icaoid cap rawpos ft) frames =
	case correspondingFrames of
		[] -> Nothing
		_  -> Just $ positionsFromTwoFrames (AdsbFramePositionReport icaoid cap rawpos ft) (head correspondingFrames)
	where
		correspondingFrames = filter (evenFrameForIcaoId icaoid) frames

getPosition _ _ = Nothing

evenFrameForIcaoId :: IcaoId -> AdsbFrame -> Bool
evenFrameForIcaoId icaoid (AdsbFramePositionReport icaoid2 cap rawpos Even) = (icaoid == icaoid2)
evenFrameForIcaoId _ _ = False

main = do
	args <- getArgs
	let hostname =
		case args of
			[] -> "192.168.0.11" -- default hostname
			_  -> head args
	client hostname 30005
