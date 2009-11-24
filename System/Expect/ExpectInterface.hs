{-# LANGUAGE ForeignFunctionInterface #-}
-- | Provides a thin wrapper for the FFI bindings to libexpect contained in
-- System.Expect.ExpectBindings.
module System.Expect.ExpectInterface 
  ({-- The contents of the binding that are to be further exposed --}
   ExpectType(ExpExact,ExpRegex,ExpGlob,ExpNull)
   {-- The custom interface --}
  ,ExpectCase(ExpectCase,expectPattern,expectType,expectValue)
  ,ExpectProc(ExpectProc,expectHandle,expectFilePtr)
  ,muteExpect,unmuteExpect
  ,spawnExpect
  ,expectCases,expectExact,expectRegex,expectMultiple
  ,sendLine)
where

import System.Expect.ExpectBindings as EB
import System.Expect.ExpectBindings

import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Handle
import IO

foreign import ccall "stdio.h fileno" fileno :: Ptr CFile -> IO CInt

-- * Types

-- | Denotes how a case's pattern is treated.
data ExpectType = ExpExact  -- ^ Match against pattern exactly.
                | ExpRegex  -- ^ Compile the pattern string to a regex and match.
                | ExpGlob   -- ^ Pattern string is glob style.
                | ExpNull

-- | Defines a case to match against.
data ExpectCase = ExpectCase { -- | Pattern to match against.
                               expectPattern ::String 
                               -- | Type of pattern contained in the case.
     		  	     , expectType :: ExpectType 
                               -- | The value to return if the case is matched.
			     , expectValue :: Int     
                             }

-- | Proc created by spawnExpect. Contains both the
-- CFile pointer and a Haskell handle, so the
-- translation needs only be done once.
data ExpectProc = ExpectProc { -- | Gets the pointer to the expect process file handle.
                               expectFilePtr :: Ptr CFile
                               -- | Gets a Handle to the expect process.
     		  	     , expectHandle :: Handle }

-- | Child process does not echo output to stdout.
muteExpect :: IO ()
muteExpect = poke exp_loguser 0

-- | Child process echoes output to stdout.
unmuteExpect :: IO ()
unmuteExpect = poke exp_loguser 1

-- | Spawn a new expect process, running a specified program.
spawnExpect :: String -- ^ The command to be processed. eg. "adduser bob"
            -> IO ExpectProc -- ^ Expect process.
spawnExpect cmd = do
  cstr <- newCString cmd
  cfileptr <- EB.exp_popen cstr
  cfileno <- fileno cfileptr
  handle <- fdToHandle $ fromIntegral cfileno
  return $ ExpectProc cfileptr handle


expectCases :: ExpectProc -- ^ The process to expect on.
            -> [ExpectCase] -- ^ The cases to match against.
            -> IO (Maybe Int) -- ^ Nothing if there are no matches (timeout / EOF), the value field
                              -- of the case that matched.

-- Expect one of a list of cases
expectCases proc cases = do
  scases <- mapM toStorableCase cases
  sarray <- newArray (scases ++ [endStorableCase])
  cval <- EB.exp_fexpectv (expectFilePtr proc) sarray
  nlist <- peekArray (length scases + 1) sarray
  mapM freeStorableCase nlist
  if cval < 0 then return Nothing
     	      else return $ Just $ fromEnum cval

-- | Expect a single case with a given type.
expectSingle :: ExpectProc -- ^ The process to expect on.
             -> String     -- ^ The pattern.
             -> ExpectType -- ^ The type of the pattern.
             -> IO (Maybe Int) -- ^ See expectCases.
expectSingle proc str ec = expectCases proc [ExpectCase str ec 1]

-- | Expect a single case with a type of ExpExact.
expectExact :: ExpectProc -- ^ The process to expect on.
            -> String -- ^ The pattern.
            -> IO (Maybe Int) -- ^ See expectCases.
expectExact proc exact = expectSingle proc exact ExpExact

-- | Expect a single case with a type of ExpExact.
expectRegex :: ExpectProc -- ^ The process to expect on.
            -> String -- ^ The pattern.
            -> IO (Maybe Int) -- ^ See expectCases.
expectRegex proc reg = expectSingle proc reg ExpRegex

-- | Expect multiple cases of a given type.
expectMultiple :: ExpectProc -- ^ The process to expect on.
               -> [String] -- ^ The patterns.
               -> ExpectType -- ^ The type of the pattern.
               -> IO (Maybe Int) -- ^ See expectCases.
expectMultiple proc ss ec = expectCases proc cases
    where cases = map (\(x,y) -> ExpectCase x ec y) (zip ss [1..])

-- | Send a line of input to the process.
sendLine :: ExpectProc -- ^ The process.
         -> String -- ^ The line to send, without the '\n'
         -> IO ()
sendLine proc line = hPutStrLn (expectHandle proc) line

{-------------------------
 --- Private functions ---
 -------------------------}

toStorableCase :: ExpectCase
               -> IO ExpCase
toStorableCase cs = do
  cstr <- newCString $ expectPattern cs
  cval <- (return . toEnum . expectValue) cs
  return $ ExpCase cstr nullPtr (expectTypeToExpType $ expectType cs) cval

endStorableCase :: ExpCase
endStorableCase = ExpCase nullPtr nullPtr expEnd 0

freeStorableCase :: ExpCase
                 -> IO ()
freeStorableCase cs = do
  if (regexp cs) == nullPtr then free (regexp cs)
     	     	    	    else return ()

expectTypeToExpType :: ExpectType -> ExpType
expectTypeToExpType ExpRegex = expRegexp
expectTypeToExpType ExpExact = expExact
expectTypeToExpType ExpGlob = expGlob
expectTypeToExpType ExpNull = expNull
