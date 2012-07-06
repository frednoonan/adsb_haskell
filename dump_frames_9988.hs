import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Control.Exception
import Adsb.Frame
import Adsb.IcaoId (countryFromIcaoId)
import System.Environment (getArgs)

client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
                addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
                let serverAddr = head addrInfo
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                messageLoop sock []
                sClose sock

messageLoop :: Socket -> [AdsbFrame] -> IO ()
messageLoop sock frames = do
	rMsg <- recv sock 65
	let rawFrame = B8.unpack $ foldr B8.append (B8.pack "") $ take 3 $ drop 1 $ B8.split ' ' rMsg
	putStrLn rawFrame
	let frame = parseFrame rawFrame
	putStrLn $ show frame
	putStrLn $ show $ countryFromIcaoId $ icaoId frame
	let position = case frame of
		(AdsbFramePositionReport icaoid cap rawpos Odd) -> getPosition frame frames
		_ -> Nothing
	if position /= Nothing then putStrLn $ show position else return ()
	messageLoop sock (frame : take 10000 frames)

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
			[] -> "localhost" -- default hostname
			_  -> head args
	client hostname 9988
