module System.Expect.ExpectCombinators
       ( ExpectM (ExpectM), ExpectOption
       , spawn, send, switch, wait, check, readStr
       , mute, unmute
       , runExpect, runExpectIO )
       where

import Data.Maybe
import Control.Applicative
import Control.Monad
import System.Expect
import System.IO

data ExpectM a = ExpectM { expectMProc :: (Maybe ExpectProc -> IO (a, Maybe ExpectProc)) }
data ExpectOption a = ExpectOption { optionType :: ExpectType
                                   , optionPattern :: String
                                   , optionAction :: (ExpectM a) }

instance Monad ExpectM where
  return a = ExpectM (\x -> return (a, x))
  (ExpectM f) >>= g = ExpectM (\x -> do { (a, x') <- f x; expectMProc (g a) x'})

-- | Spawns the child process specified, and is used for all following actions.
spawn :: String -- ^ Command to spawn
      -> ExpectM ()
spawn cmd = ExpectM (\_ -> do { p <- spawnExpect cmd; return ((), Just p) })

-- | Send a line to the current process.
send :: String -- ^ Line to send to the process
     -> ExpectM ()
send line = ExpectM (\x -> if isJust x then do {sendLine (fromJust x) line; return ((), x) } else return ((), x))

-- | Read N characters from the terminal. Note that this includes characters echoed from send actions.
readStr :: Int -- ^ Number of characters to read from the terminal
     -> ExpectM String
readStr count = ExpectM (\x -> if isJust x then do {a <- replicateM count (hGetChar (expectHandle (fromJust x))); return (a, x) } else return ("", x))

-- | Take a list of cases and run the action of the case that matches,
-- or return a fail value in the case of no matches.
switch :: [ExpectOption a] -- ^ List of cases
       -> a -- ^ Resulting value in case of no case matches
       -> ExpectM a
switch options failVal =  ExpectM (switch')
  where switch' p@(Just proc) =
          do let numOptions = zip [1..] options
                 cases = map (\(n,o) -> ExpectCase (optionPattern o) (optionType o) n) numOptions
             result <- expectCases proc cases
             if isJust result 
                then let opt = lookup (fromJust result) numOptions in
                     if isJust opt
                        then (expectMProc $ optionAction $ fromJust opt) p
                        else return (failVal, p)
                else return (failVal, p)
        switch' p = return (failVal, p)

-- | Wait for a pattern to match.
wait :: ExpectType -- ^ How pattern is used.
     -> String -- ^ Pattern
     -> ExpectM ()
wait expType pattern = ExpectM (\x -> do { _ <- expectCases (fromJust x) [ExpectCase pattern expType 1]; return ((), x) })

-- | Construct an option for use with switch
check :: ExpectType  -- ^ How to interpret the pattern
      -> String -- ^ Pattern
      -> ExpectM a -- ^ action to take in case that the case matches
      -> ExpectOption a
check expType pattern act = ExpectOption expType pattern act

-- | Unmute expect.
unmute :: ExpectM ()
unmute = ExpectM (\x -> do { unmuteExpect; return ((),x) })

-- | Mute expect.
mute :: ExpectM ()
mute = ExpectM (\x -> do { muteExpect; return ((),x) })

-- | Run an ExpectM action.
runExpect :: ExpectM a -- ^ ExpectM action to be run
          -> IO a
runExpect expAction = fst <$> expectMProc expAction Nothing

-- | Run an ExpectM (IO a) action, and join the result,
-- moving it from IO (IO a) to IO a.
runExpectIO :: ExpectM (IO a) -- ^ ExpectM action to be run
            -> IO a
runExpectIO = join . runExpect
