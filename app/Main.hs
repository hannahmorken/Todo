{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Servant
import Servant.Server.StaticFiles
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import Data.IORef
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)

import Control.Monad.IO.Class (liftIO)

-- Task data type
data Task = Task
  { taskId    :: Int
  , title     :: String
  , completed :: Bool
  } deriving (Eq, Show, Generic)

instance ToJSON Task
instance FromJSON Task

-- Input for adding a task
data TaskInput = TaskInput { titleInput :: String } deriving (Generic)
instance ToJSON TaskInput
instance FromJSON TaskInput

-- API definition
type TaskAPI =
       "tasks" :> Get '[JSON] [Task]                     -- GET /tasks
  :<|> "tasks" :> ReqBody '[JSON] TaskInput :> Post '[JSON] Task  -- POST /tasks
  :<|> "tasks" :> Capture "id" Int :> "complete" :> Post '[JSON] Task -- POST /tasks/:id/complete

type API = TaskAPI
      :<|> Raw  -- serve static frontend files

taskAPI :: Proxy TaskAPI
taskAPI = Proxy

api :: Proxy API
api = Proxy

-- Server implementation
taskServer :: IORef [Task] -> Server TaskAPI
taskServer tasksRef =
       getTasks
  :<|> addTask
  :<|> completeTask
  where
    getTasks :: Handler [Task]
    getTasks = liftIO $ readIORef tasksRef

    addTask :: TaskInput -> Handler Task
    addTask (TaskInput t) = liftIO $ do
      tasks <- readIORef tasksRef
      let newTask = Task (length tasks + 1) t False
      writeIORef tasksRef (tasks ++ [newTask])
      return newTask

    completeTask tid = liftIO $ do
      tasks <- readIORef tasksRef
      let updated = map (\task -> if taskId task == tid then task { completed = True } else task) tasks
      writeIORef tasksRef updated
      return $ head [task | task <- updated, taskId task == tid]

server :: IORef [Task] -> Server API
server tasksRef = taskServer tasksRef :<|> serveDirectoryFileServer "/Users/hannahmorken/Desktop/idk/todo/app/static"

main :: IO ()
main = do
  tasksRef <- newIORef []
  putStrLn "Server running on http://localhost:3000"
  run 3000 (serve api (server tasksRef))
