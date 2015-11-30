{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Repository where

import Types

import Database.Persist hiding ((==.), delete)
import qualified Database.Persist as P
import Database.Persist.Postgresql hiding ((==.), delete)
import Database.Persist.TH
import Control.Monad.Trans.Resource
import Control.Monad.Logger
import Database.Esqueleto
import Control.Monad.IO.Class (liftIO)
import qualified Data.Set as S
import qualified Config as Config
import qualified Data.ByteString.Char8 as BS

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkDeleteCascade sqlSettings] [persistLowerCase|
AlivePoint
    pX Int
    pY Int
    gameId SavedGameId
    deriving Show
SavedGame
    title String
    UniqueTitle title
    deriving Show
|]

connStr :: ConnectionString
connStr = BS.pack $ "postgres://" ++ Config.dbUser ++ ":" ++ Config.dbPassword ++ "@" ++ Config.dbHost ++ "/" ++ Config.dbName

runquery :: SqlPersist (ResourceT (NoLoggingT IO)) a -> IO a
runquery query =  runNoLoggingT . runResourceT . withPostgresqlConn connStr $ runSqlConn query

saveGame gameState name = runquery saveQuery
  where saveQuery = do
          deleteCascadeWhere [SavedGameTitle P.==. name]
          gameId <- insert $ SavedGame name
          let points = fmap convert $ S.toList gameState
              convert (Point x y) = AlivePoint (fromIntegral x) (fromIntegral y) gameId
          insertMany_ points

fetchGame name = do
  entities <- runquery fetchQuery
  return $ map (mkPoint . entityVal) entities
  where fetchQuery = select $ from $ \(g `InnerJoin` p) -> do
          on (g ^. SavedGameId ==. p ^. AlivePointGameId)
          where_ (g ^. SavedGameTitle ==. val name)
          return p
        mkPoint p = Point (fromIntegral (alivePointPX p)) (fromIntegral (alivePointPY p))

