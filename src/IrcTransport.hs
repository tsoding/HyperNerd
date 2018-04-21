module IrcTransport where

import           Control.Concurrent.STM
import           Irc.Message (IrcMsg)
import           Irc.RawIrcMsg (RawIrcMsg)

type IncomingQueue = TQueue IrcMsg
type OutcomingQueue = TQueue Irc.RawIrcMsg.RawIrcMsg

-- TODO(#104): IrcTransport.ircTransportEntry is not implemented
ircTransportEntry :: IncomingQueue -> OutcomingQueue -> FilePath -> IO ()
ircTransportEntry _ _ _ = putStrLn "Not implemented yet. See https://github.com/tsoding/HyperNerd/issues/104"
