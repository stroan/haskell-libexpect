{-# LANGUAGE ForeignFunctionInterface #-}
module System.Expect.ExpectInterface 
  ({-- The contents of the binding that are to be further exposed --}
   ExpCase,expExact,expRegexp,expGlob,expNull
   {-- The custom interface --}
  ,ExpectCase(ExpectCase)
  ,ExpectProc,expectHandle,expectFilePtr
  ,muteExpect,unmuteExpect
  ,spawnExpect
  ,expectCases,expectExact,expectRegex
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

data ExpectCase = ExpectCase { expectPattern ::String
     		  	     , expectType :: ExpType 
			     , expectValue :: Int }

data ExpectProc = ExpectProc { expectFilePtr :: Ptr CFile
     		  	     , expectHandle :: Handle }

muteExpect :: IO ()
muteExpect = poke exp_loguser 0

unmuteExpect :: IO ()
unmuteExpect = poke exp_loguser 1

-- Spawn a new expect process, running a specified program.
spawnExpect :: String
            -> IO ExpectProc
spawnExpect cmd = do
  cstr <- newCString cmd
  cfileptr <- EB.exp_popen cstr
  cfileno <- fileno cfileptr
  handle <- fdToHandle $ fromIntegral cfileno
  return $ ExpectProc cfileptr handle

-- Expect one of a list of cases
expectCases :: ExpectProc
            -> [ExpectCase]
            -> IO (Maybe Int)
expectCases proc cases = do
  scases <- mapM toStorableCase cases
  sarray <- newArray (scases ++ [endStorableCase])
  cval <- EB.exp_fexpectv (expectFilePtr proc) sarray
  nlist <- peekArray (length scases + 1) sarray
  mapM freeStorableCase nlist
  if cval < 0 then return Nothing
     	      else return $ Just $ fromEnum cval

expectExact :: ExpectProc
            -> String
            -> IO (Maybe Int)
expectExact proc exact = expectCases proc [ExpectCase exact expExact 1]

expectRegex :: ExpectProc
            -> String
            -> IO (Maybe Int)
expectRegex proc reg = expectCases proc [ExpectCase reg expRegexp 1]

sendLine :: ExpectProc
         -> String
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
  return $ ExpCase cstr nullPtr (expectType cs) cval

endStorableCase :: ExpCase
endStorableCase = ExpCase nullPtr nullPtr expEnd 0

freeStorableCase :: ExpCase
                 -> IO ()
freeStorableCase cs = do
  if (regexp cs) == nullPtr then free (regexp cs)
     	     	    	    else return ()
