module Webb.File.Internal.File where

import Prelude
import Webb.State.Prelude

import Control.Monad.State (StateT)
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.FS (FileFlags(..))
import Node.FS.Aff as FS
import Node.FS.Stats as Stat
import Webb.Directory.Data.Absolute as Abs
import Webb.File.Data.File as File
import Webb.Monad.Prelude (forceMaybe', notM)



{- Program for how the file works internally. All a file does is give enough 
  API exposure to think clearly about file operations.
-}


type State = 
  { file :: ShowRef File.File
  }
  
type Prog = StateT State Aff

path :: Prog Abs.AbsPath
path = do
  this <- mread
  File.path <: this.file

position :: Prog Int
position = do
  this <- mread
  File.position <: this.file

forceFd :: Prog File.Fd
forceFd = do
  this <- mread
  mfd <- File.fd <: this.file
  forceMaybe' "File was not open" mfd

isAppending :: Prog Boolean
isAppending = do
  this <- mread
  File.isAppending <: this.file

isOpen :: Prog Boolean
isOpen = do
  this <- mread
  File.isOpen <: this.file

isClosed :: Prog Boolean
isClosed = notM isOpen

-- If the file isn't open, open it, creating it if necessary, and truncating
-- it if it already exists.
openTruncated :: Prog Unit
openTruncated = do
  this <- mread
  abs <- path
  whenM isClosed do
    fd <- FS.fdOpen (Abs.unwrap abs) W_PLUS Nothing # liftAff
    File.loadFd fd >>> File.setPosition 0 >>>
      File.appending false :> this.file

-- If the file isn't open, open it and create it if necessary. Writes will
-- only append.
openAppending :: Prog Unit
openAppending = do
  this <- mread
  abs <- path
  whenM isClosed do
    fd <- FS.fdOpen (Abs.unwrap abs) A_PLUS Nothing # liftAff
    File.loadFd fd >>> File.setPosition 0 >>>
      File.appending true :> this.file
    
-- If the file isn't open, open it and create it if necessary, pointing to
-- the start of the file. But the write takes place at the current position.
open :: Prog Unit
open = do
  ensureExists

  this <- mread
  abs <- path
  whenM isClosed do
    fd <- FS.fdOpen (Abs.unwrap abs) R_PLUS Nothing # liftAff
    File.loadFd fd >>> File.setPosition 0 >>>
      File.appending false :> this.file
  where
  ensureExists = do openAppending *> close

-- Closes the current fd, if it exists.
close :: Prog Unit
close = do
  this <- mread
  mfd <- File.fd <: this.file
  for_ mfd \fd -> do
    File.unloadFd :> this.file 
    FS.fdClose fd # liftAff
    
-- Number of bytes in the file. If 0, the number could not
-- be determined.
size :: Prog Int
size = do
  abs <- path
  stat <- FS.stat (Abs.unwrap abs) # liftAff
  let int = Int.fromNumber $ Stat.size stat
  pure $ fromMaybe 0 int
  
-- If the file isn't open, truncate it to the desired length.
truncate :: Int -> Prog Unit
truncate n =  do 
  abs <- path
  whenM isClosed do
    FS.truncate (Abs.unwrap abs) n # liftAff
  
-- Seek the file's position to a particular place in the file. The File
-- always performs read/write operations from its current position.
-- cannot seek to a negative number.
seek :: Int -> Prog Unit
seek n = do
  this <- mread
  File.setPosition (max n 0) :> this.file

-- Change the file's position by a relative amount.
seekBy :: Int -> Prog Unit
seekBy n = do
  pos <- position
  let newPos = pos + n
  seek newPos
  
-- Writes the buffer to file at the current file position -- but if the file
-- was opened as an 'APPENDING' file, it will always write to the end.
-- We always write the ENTIRE buffer, for simplicity, since I'm not
-- doing much buffer management.
writeBuffer :: Buffer -> Prog Unit
writeBuffer buf = do
  pos <- position
  fd <- forceFd
  append <- isAppending
  length <- Buffer.size buf # liftEffect

  if append then do
    -- An append file _should_ write to the end, naturally.
    void (FS.fdWrite fd buf 0 length Nothing) # liftAff
  else do
    void (FS.fdWrite fd buf 0 length $ Just pos) # liftAff

  seekBy length -- We advance the file position.
  
  
-- Are we at the end? We're at the end if attempting to read does nothing.
isEof :: Prog Boolean
isEof = do
  _buffer /\ count <- peekBuffer 1
  pure $ count <= 0

-- Peek the next `n` notes at the current position.
peekBuffer :: Int -> Prog (Buffer /\ Int)
peekBuffer n = do
  pos <- position
  fd <- forceFd
  buffer <- Buffer.create n # liftEffect
  count <- FS.fdRead fd buffer 0 n (Just pos) # liftAff
  pure $ buffer /\ count
  
-- From the file position, attempt to read N bytes. Return a byte buffer and
-- the number of bytes that was read.
readBuffer :: Int -> Prog (Buffer /\ Int)
readBuffer n = do
  result@(_buffer /\ count) <- peekBuffer n
  seekBy count -- advance the recorded position after reading.
  pure result
  