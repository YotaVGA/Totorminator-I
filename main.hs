-- Copyright © 2013 Fulvio Satta

-- Totorminator is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- Totorminator is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

import Control.Concurrent
import Network.IRC.Bot.BotMonad
import Network
import qualified Network.IRC as IRC
import Network.IRC.Bot.Core
import Network.IRC.Bot.Log
import Network.IRC.Bot.Part.Ping
import Network.IRC.Bot.Part.NickUser
import Network.IRC.Bot.Commands
import Network.IRC.Bot.Parsec
import Data.Maybe
import Data.Char
import Control.Monad.Trans
import System.Random
import System.Process
import Text.Parsec
import Data.String.Utils
import qualified Data.Set as Set

polygenPath :: String
polygenPath = "/home/totoro/temp/polygen-1.0.6/polygen"

autoJoinChannels :: Set.Set String
--autoJoinChannels = Set.singleton "#proveTotorminator"
autoJoinChannels = Set.singleton "#100%cotton"

userconf :: User
userconf = User {username   = "Totorminator",
                 hostname   = "Totorminator",
                 servername = "irc.azzurra.org",
                 realname   = "Totorminator"}
botconf :: BotConf
botconf  = BotConf {channelLogger = Nothing,
                    logger        = stdoutLogger Normal,
                    host          = "irc.azzurra.org",
                    port          = PortNumber 6667,
                    nick          = "Totorminator",
                    commandPrefix = "%",
                    user          = userconf,
                    channels      = autoJoinChannels,
                    limits        = Just (128, 300)}

getTale :: BotMonad m => String -> [String] -> m [String]
getTale file s = do tale <- liftIO $ readProcess polygenPath [file] ""
                    return $ lines $ filterTale tale
  where filterTale tale = replace "E'" "È" $
                          replace "e'" "è" $
                          replace "o'" "ò" $
                          replace "<1>" (s !! 0) $
                          replace "<2>" (s !! 1) tale

joinChansPart :: BotMonad m => Set.Set String -> IO (m ())
joinChansPart chans = return $ do incomingMessage <- askMessage
                                  case IRC.msg_command incomingMessage of
                                    "005" -> mapM_ act $ Set.toList chans
                                    _     -> return ()
  where act channel = do sendMessage $ IRC.joinChan channel
                         logM Normal $ "Joining room " ++ channel
                         sendCommand $ PrivMsg Nothing [channel] $
                           "Ciao a tutti gli utenti di " ++ channel ++ "!"

callingPart :: BotMonad m => m ()
callingPart = do incomingMessage <- privMsg
                 logM Debug "callingPart"
                 sender          <- askSenderNickName
                 target          <- maybeZero =<< replyTo
                 choiceNumber    <- liftIO $ randomRIO (0, 5)
                 let nickname    = "totorminator"
                     messageText = msg $ incomingMessage
                     senderName  = fromMaybe "chiunque tu sia" sender
                     lowerWords  = map (map toLower) $ words $ messageText
                     responseMsg = response senderName choiceNumber
                 if not $ null $ filter (nickname ==) lowerWords
                   then sendCommand $ PrivMsg Nothing [target] responseMsg
                   else return ()
  where response :: String -> Integer -> String
        response name 0 = "Dimmi, " ++ name
        response name 1 = "Sono tutto orecchie, " ++ name
        response name 2 = "Dimmi tutto, " ++ name
        response _     3 = "Che c'è?"
        response name 4 = "Qualcosa non va, " ++ name ++ "?"
        response _    5 = "Chi mi chiama?"
        response _    _ = error "Undefined response"

storyPart :: BotMonad m => m ()
storyPart = parsecPart story
  where story = do _      <- try $ botPrefix >> string "racconta"
                   tale   <- getTale "story.grammar" []
                   target <- maybeZero =<< replyTo
                   mapM_ (sendCommand . (PrivMsg Nothing [target])) tale
                <|> return ()

sexPart :: BotMonad m => m ()
sexPart = parsecPart story
  where story = do (s1, s2)  <- try $ do _  <- botPrefix >> string "sex"
                                         v1 <- skipMany1 space >> many1 (noneOf " ")
                                         v2 <- skipMany1 space >> many1 (noneOf " ")
                                         return (v1, v2)
                   tale   <- getTale "sex.grammar" [s1, s2]
                   target <- maybeZero =<< replyTo
                   mapM_ (sendCommand . (PrivMsg Nothing [target])) tale
                <|> return ()

parts :: BotMonad m => IO [m ()]
parts = do chansPart <- joinChansPart autoJoinChannels
           return [pingPart, nickUserPart, chansPart, callingPart, storyPart,
                   sexPart]

wait :: IO ()
wait = do threadDelay 1000
          wait

copyright :: String
copyright = "Copyright © 2013 Fulvio Satta\n" ++ 
            "This program comes with ABSOLUTELY NO WARRANTY; " ++
            "for details read gpl.txt.\n" ++
            "This is free software, and you are welcome to redistribute it " ++
            "under certain conditions; read gpl.txt for details."

main :: IO ()
main = do putStrLn copyright
          partsToDo <- parts
          _ <- simpleBot botconf partsToDo
          wait
