import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (retry, atomically)
import Control.Monad
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import Data.Monoid

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Tuple (swap)

import System.Console.ANSI
import System.IO
  
import Debug.Trace

-- this is all very quick n' dirty, just to play with composing
-- pipes and forkIO
-- don't expect quality code
-- you have been warned


-- the flow of 'messages' is following:
-- * 'input' subsystems produce events
-- * events are processed by entities handlers
-- * handlers are producing more events
-- * events are consumed by 'output' subsystems
-- * events can be routed back as an input


type Ident = Int

type Coord = (Int,Int)

data EntityEvent = Damage Int
                 | Died
                 deriving(Show)
                       
data Event = MoveLeft
           | MoveDown
           | MoveUp
           | MoveRight
           | MakeAttack
           | DisplayInfo
             
           | EntityEvent Ident EntityEvent
             
           | Redraw Char Int Int Int Int
           | DrawInfo String
             
           | Attack Coord
           deriving(Show)

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
    Attack coord -> do
      lift $ do
        setCursorPosition 20 0
        putStr $ "Attacking: " ++ (show coord)
      yield ev
    EntityEvent _ (Damage _) -> do
      lift $ do
        setCursorPosition 20 0
        putStr "Splash!"
      yield ev
    _ -> yield ev

input :: Producer Event IO r
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

data Entity = Entity { ident  :: Ident
                     , pos    :: (Int, Int)
                     , avatar :: Char
                     , target :: (Int, Int)
                     , health :: Int }
              deriving(Show)

player :: Entity -> Pipe Event Event IO ()
player ent = loop ent
  where
    loop ent@Entity{ident=ident, pos=(x,y), avatar=av, target=(tx,ty), health=h} = do
      when (h > 0) $ do
        ev <- await
        let (dx,dy) = case ev of
              MoveLeft  -> (0, -1)
              MoveUp    -> (-1, 0)
              MoveDown  -> (1, 0)
              MoveRight -> (0, 1)
              _ -> (0,0)
            (nx,ny) = (x+dx, y+dy)
            ent' = ent{pos=(nx,ny), target=(tx+dx, ty+dy)}
        yield $ Redraw av x y nx ny -- detect move
        case ev of
          EntityEvent ident' ev' -> do
            let nh = case ev' of
                  Damage x -> h - x
                  _ -> h
            loop $ ent'{health=nh}
          DisplayInfo -> do
            yield $ DrawInfo (show ent')
            loop ent'
          MakeAttack -> do
            yield $ Attack (target ent')
            loop ent'
          _ -> loop ent'

monster :: Entity -> Pipe Event Event IO ()
monster ent = loop ent
  where
    loop ent@Entity{ident=ident, pos=(x,y), avatar=av, health=health} = do
      
      yield $ Redraw av x y x y
      ev <- await
      case ev of
        EntityEvent ident' ev -> do
          case ev of
            Damage dmg -> do
              let h' = health - dmg
              yield $ DrawInfo (show h')
              if h' > 0 then loop ent{health = h'}
                        else do
                               yield $ Redraw '+' x y x y
                               yield $ EntityEvent ident Died
                               return ()
            _ -> loop ent
        _ -> loop ent
          
-- an example of subsystem that just processes events
combat :: Map Coord Ident -> Pipe Event Event IO ()
combat ents = loop ents
  where
    loop ents = do
        ev <- await
        yield ev -- pass everything further down the chain
        case ev of
          Attack coord -> do
            case Map.lookup coord ents of
              Just e -> do yield $ EntityEvent e (Damage 10)
                           loop ents
              _ -> loop ents
          EntityEvent ident Died -> do
            -- ohhhh
            let kvs  = Map.assocs ents
                vks  = map swap kvs
                k    = lookup ident vks
                ents'= case k of Just k' -> Map.delete k' ents
                                 _ -> ents
            loop ents'
          _ -> loop ents

-- not all 'output' subsystems are busy producing effects in IO
-- some just want to produce commands for further processing
-- hence loopback will be wired again to input
loopback :: Pipe Event Event IO ()
loopback = forever $ do
  ev <- await
  case ev of
    EntityEvent _ (Damage _) -> yield ev
    _ -> return ()

fromEntityInput :: Input Event -> Ident -> Producer Event IO () 
fromEntityInput input ident = loop
  where
    loop = do
        ma <- liftIO $ atomically recvOnly
        case ma of
            Nothing -> return ()
            Just a  -> do
                yield a
                loop
    recvOnly :: STM (Maybe Event)
    recvOnly = do
        x <- recv input
        case x of
            Nothing                                  -> return Nothing
            Just ev@(EntityEvent i' _) | ident == i' -> return (Just ev)
                                       | otherwise   -> retry
            Just ev -> return $ Just ev
                                        
main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  clearScreen
  let playerE = Entity 1 (0,0) '@' (0,1) 100
      monsters = map (\(i, pos) -> Entity i pos 'x' (0,0) 20) [(10, (5,5)), (11, (2,3)), (12, (11,15)), (13, (4,4))]
      combatMonsters = Map.fromList $ map (\ent -> ((pos ent), (ident ent))) monsters
      
  -- this is where commands are coming from
  (inpOut, inpIn) <- spawn Unbounded
  (logicOut, logicIn) <- spawn Unbounded
  -- and this is where entities activities produce to
  (evOut, evIn) <- spawn Unbounded

  -- input subsystem - push the input commands
  forkIO $ do runEffect $ input >-> toOutput inpOut
              performGC
  -- handling entities
  -- player only reacts to input
  forkIO $ do runEffect $ fromInput inpIn >-> player playerE >-> toOutput evOut
              performGC
  -- monsters react to logic
  mapM (\e -> forkIO $ do runEffect $ fromEntityInput logicIn (ident e) >-> monster e >-> toOutput evOut) monsters
  -- main flow of messages
  runEffect $ fromInput evIn >-> combat combatMonsters >-> renderer >-> loopback >-> toOutput logicOut
