{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Task where

import Import
import Yesod.Form.Bootstrap3 (BootstrapGridOptions (..), BootstrapFormLayout (..), renderBootstrap3)
--import Text.Julius (RawJS (..))
import Data.Maybe (fromJust)

import Data.Time.Clock ()
import Data.Time.Calendar ()
import Data.Time.LocalTime ( LocalTime(localDay) )

getTaskR :: Handler Html
getTaskR = do
    (formWidget, formEnctype) <- generateFormPost taskForm
    -- let handlerName = "getTaskR" :: Text
    allTasks <- runDB getAllTasks
    (year, month, day) <- getCurrentDate
    let today = show year ++ show month ++ show day
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
--        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"
--        addScriptRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js"
--        addScriptRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.bundle.min.js"

        $(widgetFile "tasks")

postTaskR :: Handler Html
postTaskR = do
    ((result, formWidget), formEnctype) <- runFormPost taskForm
    -- let handlerName = "postTaskR" :: Text
    let allTasksw = case result of
            FormSuccess task -> do
                taskId <- runDB $ insert $ task
                print taskId
                runDB getAllTasks
            _ -> do
                runDB getAllTasks
    allTasks <- allTasksw
    (year, month, day) <- getCurrentDate
    let today = show year ++ show month ++ show day
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
        $(widgetFile "tasks")

patchTaskR :: Handler Html
patchTaskR = do
    ((result, formWidget), formEnctype) <- runFormPost taskForm
    print result
    --let handlerName = "patchTaskR" :: Text
    allTasks <- runDB getAllTasks
    (year, month, day) <- getCurrentDate
    let today = show year ++ show month ++ show day
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
        $(widgetFile "tasks")

deleteTaskR :: Handler Html
deleteTaskR = do
    ((result, formWidget), formEnctype) <- runFormPost taskForm
    print result
    (year, month, day) <- getCurrentDate
    let today = show year ++ show month ++ show day
    --let handlerName = "deleteTaskR" :: Text
    allTasks <- runDB getAllTasks
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
        $(widgetFile "tasks")

getTaskByIdR :: Key Task -> Handler Html
getTaskByIdR taskId = do
    --let handlerName = "getTaskByIdR" :: Text
    allTasks <- runDB $ selectList [TaskId ==. taskId] [Asc TaskId]
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
        $(widgetFile "tasksbyid")

getTaskByDayR :: Day -> Handler Html
getTaskByDayR day = do
    ((result, formWidget), formEnctype) <- runFormPost taskForm
    print result
    --let handlerName = "getTaskByDayR" :: Text
    allTasks <- runDB $ selectList [TaskBegin >=. day] [Asc TaskId]
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
        $(widgetFile "tasksbyday")

taskForm :: Form Task
taskForm = renderBootstrap3 (BootstrapHorizontalForm (ColSm 1) (ColSm 1) (ColSm 1) (ColSm 1)) $ Task
    <$> areq textField "Description" Nothing
    <*> areq dayField "Start" Nothing
    <*> aopt dayField "End" Nothing
    <*> areq textField "RepeatPattern" Nothing

getAllTasks :: DB [Entity Task]
getAllTasks = selectList [] [Asc TaskId]

--getTasksByDay :: Day -> DB [Entity Task]
--getTasksByDay d = selectList [TaskBeginDay >=. d, TaskEndDay <=. d] [Asc TaskId]

showTaskDay :: Maybe Day -> [Char]
showTaskDay d
  | isJust d = show $ fromJust d
  | otherwise = ""


getCurrentDate :: IO (Integer,Int,Int) -- :: (year,month,day)
getCurrentDate = getCurrentTime >>= return . toGregorian . localDay