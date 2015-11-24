{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text as T
import Data.Function
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import qualified Data.List ()
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson as Aeson
import qualified Data.Set as S
import qualified Data.Map as M
--import qualified Data.Text.IO as TextIO
import Control.Monad
import Data.Maybe
--import Data.Either
import qualified Network.WebSockets as WS
import qualified Control.Concurrent as C-- (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)
import GHC.Generics
import Control.Exception as Exc
import qualified Text.Read as R
import qualified URI.ByteString as URI
import qualified Data.ByteString.Char8 as Char8

data Point = Point {pX :: Integer, pY :: Integer} deriving (Eq, Ord, Show, Generic)
data Command = SetAlive [Point] | SetDead [Point] deriving (Eq, Ord, Show, Generic)
type GameState = S.Set Point
type ConnectionList = S.Set UniqConn
data UniqConn = UniqConn { getUniqId :: UUID.UUID, getConnection :: WS.Connection }
type Universe = C.MVar (M.Map Integer (C.MVar World))
data World = World { wGameState :: GameState, wConnections :: ConnectionList }

instance Aeson.ToJSON Point
instance Aeson.FromJSON Point
instance Aeson.ToJSON Command
instance Aeson.FromJSON Command

instance Show UniqConn where
  show c = show (getUniqId c)

instance Ord UniqConn where
  compare = compare `on` getUniqId

instance Eq UniqConn where
  (==) = (==) `on` getUniqId

stepState :: GameState -> GameState
stepState s = S.union survivors births
  where survivors = S.filter isSurvivor s
        births = S.filter isBirth $ allNeighbors
        isBirth cell = (not (alive cell)) && (livingNeighborsCount cell == 3)
        allNeighbors = foldl S.union S.empty (S.map exclusiveNeighbors s)
        isSurvivor cell = (alive cell) && (livingNeighborsCount cell) `elem` [2,3] -- "alive" check is redundant
        livingNeighborsCount cell = length $ S.filter alive $ exclusiveNeighbors cell
        alive cell = S.member cell s
        exclusiveNeighbors cell = S.fromList [Point x y | x <- [(pX cell - 1)..(pX cell + 1)],
                                                          y <- [(pY cell - 1)..(pY cell + 1)],
                                                          Point x y /= cell]

updateWorld :: World -> World
updateWorld (World state connections) = World (stepState state) connections

-- TODO: put the threadDelay intervals in variables / reader monad
--       experiment with putting refs "world" and "connections" mvars into reader
--       get config detail from command line
main :: IO ()
main = do
  universe <- makeUniverse
  _ <- C.forkIO $ stepUniverse universe
  WS.runServer "0.0.0.0" 9160 (acceptConnection universe)

getWorldID :: WS.PendingConnection -> Maybe Integer
getWorldID pending = let getPath = WS.requestPath . WS.pendingRequest
                         parser = URI.parseRelativeRef URI.laxURIParserOptions
                         getQueryDict uriStructure = M.fromList $ URI.queryPairs $ URI.rrQuery uriStructure
                         eitherToMaybe (Left _) = Nothing
                         eitherToMaybe (Right thing) = Just thing
                      in do
                        uriStructure <- eitherToMaybe $ parser (getPath pending)
                        thedict <- return $ getQueryDict uriStructure
                        found <- M.lookup "worldID" thedict
                        R.readMaybe $ Char8.unpack found

makeUniqConn connection = do
  rando <- UUID.nextRandom
  return $ UniqConn rando connection

acceptConnection :: Universe -> WS.ServerApp
acceptConnection universe pending = do
  let worldID = getWorldID pending
  when (isJust worldID) $ do
    conn <- WS.acceptRequest pending
    uniqueConnection <- makeUniqConn conn
    wld <- addConnection (fromJust worldID) uniqueConnection universe -- fromJust is not a good pattern to use..
    pollMove conn wld

-- this will blow up when the client disconnects.
-- results in error messages printing to standard out
pollMove conn worldref = forever $ do
  msg :: T.Text <- WS.receiveData conn
  let asByteString = TextEncoding.encodeUtf8 msg
      maybeCommand :: Maybe Command
      maybeCommand = Aeson.decodeStrict asByteString
  runUserCommand maybeCommand worldref
  C.threadDelay 100000

stepUniverse :: Universe -> IO ()
stepUniverse universe = forever $ do
  asdf :: (M.Map Integer (C.MVar World)) <- C.readMVar universe
  forM_ (M.elems asdf) broadcastWorld
  C.threadDelay 1000000

broadcastWorld :: (C.MVar World) -> IO ()
broadcastWorld world = do
  newWorld <- C.modifyMVar world (return . tuplify . updateWorld)
  let connectionsNow = wConnections newWorld
  forM_ connectionsNow (\uniqueConn -> safeSend uniqueConn $ dumps $ wGameState newWorld)
  where safeSend uniqConnection message = Exc.catch (WS.sendTextData (getConnection uniqConnection) message) (handleSocketError uniqConnection)
        handleSocketError :: UniqConn -> WS.ConnectionException -> IO ()
        handleSocketError uniqConn _ = do
          pruneConnection world uniqConn
          return ()
        tuplify x = (x,x)

dumps :: (Aeson.ToJSON t) => t -> LBS.ByteString
dumps = Aeson.encode . Aeson.toJSON

makeWorld :: IO (C.MVar (S.Set Point))
makeWorld = C.newMVar $ S.fromList [Point (-1) 0, Point 0 0, Point 1 0]

makeConnectionList = C.newMVar $ S.empty

makeUniverse :: IO Universe
makeUniverse = C.newMVar M.empty

makeWorldRef = C.newMVar $ World S.empty S.empty

getSeedWorld connection = C.newMVar $ World S.empty S.empty

insertIfAbsent k v m = (withNew, withNew M.! k)
  where withNew = M.insertWith takeExisting k v m
        takeExisting _ already = already

addConnection :: Integer -> UniqConn -> Universe -> IO (C.MVar World)
addConnection worldID connection universe = do
    C.modifyMVar_ universe (\oldUniverse -> do
      blankWorld <- getSeedWorld connection
      let (withNewWorld, someWorld) = insertIfAbsent worldID blankWorld oldUniverse
      insertConnectionToWorld someWorld
      return withNewWorld)
    newUniverse <- C.readMVar universe
    return (newUniverse M.! worldID) -- unsafe map lookup
    where insertConnectionToWorld w = C.modifyMVar_ w (\(World g c) -> return $ World g (S.insert connection c))

pruneConnection world uniqConnection = C.modifyMVar_ world (return . removeConnection)
  where removeConnection (World state connections) = World state $ S.delete uniqConnection connections

runUserCommand (Just (SetAlive points)) world = C.modifyMVar_ world (return . addPoints)
  where addPoints (World gameState connections) = World (bulkInsert points gameState) connections

runUserCommand (Just (SetDead points)) world = C.modifyMVar_ world (return .  removePoints)
  where removePoints (World gameState connections) = World (bulkDelete points gameState) connections

runUserCommand Nothing _ = return ()

bulkInsert :: (Ord a) => [a] -> S.Set a -> S.Set a
bulkInsert elements target = foldr S.insert target elements

bulkDelete :: (Ord a) => [a] -> S.Set a -> S.Set a
bulkDelete elements target = foldr S.delete target elements

