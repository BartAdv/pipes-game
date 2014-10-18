import Control.Concurrent (threadDelay)
import Control.Monad
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import Data.Monoid

import System.Console.ANSI
import System.IO
  
import Debug.Trace

-- this is all very quick n' dirty, just to play with composing
-- pipes and forkIO
-- don't expect quality code
-- you have been warned


-- the flow of 'messages' is following:
-- * 'input' subsystems produce commands
-- * commands are processed by entities handlers
-- * handlers are producing events
-- * events are consumed by 'output' subsystems
-- * events can be transformed to commands


data Command = MoveLeft | MoveDown | MoveUp | MoveRight
             | MakeAttack     -- no info on target
             | MakeDamage Int -- hence we're gonna only damage ourselves
             | DisplayInfo
               deriving(Show)
               
data Event = Redraw Char Int Int Int Int
           | DrawInfo String
           | Attack Int Int
           | Damage Int

renderer :: Pipe Event Event IO ()
renderer = forever $ do
  ev <- await
  case ev of
    Redraw c ox oy nx ny -> lift $ do
        setCursorPosition ox oy
        putChar ' '
        setCursorPosition nx ny
        putChar c
    DrawInfo msg -> lift $ do
      setCursorPosition 20 0
      putStr msg
    Attack _ _ -> do
      lift $ do
        setCursorPosition 20 0
        putStr "Attack!"
      yield ev
    Damage _ -> do
      lift $ do
        setCursorPosition 20 0
        putStr "Splash!"
      yield ev

input :: Producer Command IO r
input = forever $ do
  k <- lift getChar
  case k of
    'h' -> yield MoveLeft
    'j' -> yield MoveDown
    'k' -> yield MoveUp
    'l' -> yield MoveRight
    'a' -> yield MakeAttack
    'i' -> yield DisplayInfo
    _ -> return ()

data Entity = Entity { posX   :: Int
                     , posY   :: Int
                     , avatar :: Char
                     , target :: (Int, Int)
                     , health :: Int }
              deriving(Show)

handler :: Entity -> Pipe Command Event IO ()
handler ent = loop ent
  where
    loop ent@Entity{posX=x, posY=y, avatar=av, target=(tx,ty), health=h} = do
      when (h > 0) $ do
        cmd <- await
        let (tx,ty) = case cmd of
              MoveLeft  -> (0, -1)
              MoveUp    -> (-1, 0)
              MoveDown  -> (1, 0)
              MoveRight -> (0, 1)
              _ -> (0,0)
            (nx,ny) = (x+tx, y+ty)
            nh = case cmd of
              MakeDamage x -> h - x
              _ -> h
        case cmd of
          MakeAttack -> yield $ Attack tx ty
          DisplayInfo -> yield $ DrawInfo $ (show ent)
          _ -> yield $ Redraw av x y nx ny
        loop $ Entity nx ny av (nx+tx, ny+ty) nh

-- an example of subsystem that just processes events
combat :: Pipe Event Event IO ()
combat = forever $ do
  cmd <- await
  case cmd of
    Attack x y -> do
      yield $ Damage 10
    _ -> yield cmd

-- not all 'output' subsystems are busy producing effects in IO
-- some just want to produce commands for further processing
-- hence loopback will be wired again to input
loopback :: Pipe Event Command IO ()
loopback = forever $ do
  ev <- await
  case ev of
    Damage x -> yield $ MakeDamage x
    _ -> return ()
    
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  let player = Entity 0 0 '@' (0,1) 100

  -- this is where commands are coming from
  (cmdOut, cmdIn) <- spawn Unbounded
  -- and this is where events are born
  (evOut, evIn) <- spawn Unbounded

  -- input subsystem - push the input commands
  forkIO $ do runEffect $ input >-> toOutput cmdOut
              performGC
  -- handling subsystem
  forkIO $ do runEffect $ fromInput cmdIn >-> handler player >-> toOutput evOut
              performGC
  -- main flow of messages
  runEffect $ fromInput evIn >-> combat >-> renderer >-> loopback >-> toOutput cmdOut
