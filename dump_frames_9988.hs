import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString (send, recv)
import qualified Data.ByteString.Char8 as B8
import Adsb.Frame (parseFrame, icaoId)
import Adsb.IcaoId (countryFromIcaoId)
import Control.Exception

client :: String -> Int -> IO ()
client host port = withSocketsDo $ do
                addrInfo <- getAddrInfo Nothing (Just host) (Just $ show port)
                let serverAddr = head addrInfo
                sock <- socket (addrFamily serverAddr) Stream defaultProtocol
                connect sock (addrAddress serverAddr)
                messageLoop sock
                sClose sock

messageLoop :: Socket -> IO ()
messageLoop sock = do
  rMsg <- recv sock 65
  let rawFrame = B8.unpack $ foldr B8.append (B8.pack "") $ take 3 $ drop 1 $ B8.split ' ' rMsg
  putStrLn rawFrame
  let frame = parseFrame rawFrame
  putStrLn $ show frame
  putStrLn $ show $ countryFromIcaoId $ icaoId frame
  messageLoop sock

main = do
	client "mpd" 9988
