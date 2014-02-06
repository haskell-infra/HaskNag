module Main where

import Control.Concurrent
import Control.Monad
import Network
import qualified Network.Socket as S
import Network.IRC
import System.IO
import Text.Printf

server :: String
server = "irc.freenode.net"

port :: Int
port = 6667

nickname :: String
nickname = "HaskNag"

channels :: [String]
channels = ["#haskell-alerts"]

bot :: String -> IO Handle
bot nickname' = withSocketsDo $ do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  write h "NICK" nickname'
  write h "USER" (nickname ++ " 0 * :Haskell Nagios Bot")
  mapM_ (write h "JOIN") channels
  return h

write :: Handle -> String -> String -> IO ()
write h s t = do
  _ <- hPrintf h "%s %s\r\n" s t
  printf    "> %s %s\n" s t

botPrivmsg :: Handle -> String -> IO ()
botPrivmsg h msg = mapM_ (\c -> write h "PRIVMSG" $ c ++ " :" ++ msg) channels

botPrivmsgChannel :: Handle -> String -> String -> IO ()
botPrivmsgChannel h ch msg = write h "PRIVMSG" $ ch ++ " :" ++ msg

botListen :: Handle -> IO ()
botListen h = forever $ do
    s <- hGetLine h
    handleLine h $ decode s
    putStrLn s

handleLine :: Handle -> Maybe Message -> IO ()
handleLine _ Nothing = return ()
handleLine h (Just m) = reply $ msg_command m
  where
    params = msg_params m

    reply "PRIVMSG" = case response of
      Just s -> botPrivmsgChannel h (head params) s >> return ()
      _ -> return ()
      where
        response =
          case params !! 1 of
            "!hello" -> Just "hey there"
            _ -> Nothing
    reply "PING" = write h "PONG " $ ":" ++ params !! 0
    reply _ = return ()

udpServer :: Handle -> IO ()
udpServer h = withSocketsDo $ do
    sock <- S.socket S.AF_INET S.Datagram 0
    S.bindSocket sock (S.SockAddrInet 2000 S.iNADDR_ANY)
    forever $ do
      (mesg, _, client) <- S.recvFrom sock 1024
      _ <- S.sendTo sock mesg client
      botPrivmsg h mesg
      putStrLn mesg

main :: IO ()
main = do
  h <- bot nickname
  _ <- forkIO $ do
    botListen h
  udpServer h
