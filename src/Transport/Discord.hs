module Transport.Discord
  ( discordTransportEntry
  ) where

import Config
import Transport

discordTransportEntry ::
     IncomingQueue -> OutcomingQueue -> DiscordParams -> IO ()
-- TODO(#453): Discord transport is not implemented
discordTransportEntry _ _ _ = error "Discord transport is not implemented"
