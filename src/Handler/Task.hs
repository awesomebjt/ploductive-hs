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

getTaskR :: Handler Html
getTaskR = do
    (formWidget, formEnctype) <- generateFormPost taskForm
    let handlerName = "getTaskR" :: Text
    allTasks <- runDB getAllTasks

    defaultLayout $ do
        setTitle "Ploductive: Tasks"
--        addStylesheetRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css"
--        addScriptRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.min.js"
--        addScriptRemote "https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/js/bootstrap.bundle.min.js"

        $(widgetFile "tasks")

postTaskR :: Handler Html
postTaskR = do
    ((result, formWidget), formEnctype) <- runFormPost taskForm
    let handlerName = "postTaskR" :: Text
        allTasksw = case result of
            FormSuccess task -> do
                taskId <- runDB $ insert $ task
                runDB getAllTasks
            _ -> do
                runDB getAllTasks
    allTasks <- allTasksw
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
        $(widgetFile "tasks")

patchTaskR :: Handler Html
patchTaskR = do
    ((result, formWidget), formEnctype) <- runFormPost taskForm
    let handlerName = "patchTaskR" :: Text
    allTasks <- runDB getAllTasks
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
        $(widgetFile "tasks")

deleteTaskR :: Handler Html
deleteTaskR = do
    ((result, formWidget), formEnctype) <- runFormPost taskForm
    let handlerName = "deleteTaskR" :: Text
    allTasks <- runDB getAllTasks
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
        $(widgetFile "tasks")

getTaskByIdR :: Key Task -> Handler Html
getTaskByIdR taskId = do
    ((result, formWidget), formEnctype) <- runFormPost taskForm
    let handlerName = "getTaskByIdR" :: Text
    allTasks <- runDB getAllTasks
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
        $(widgetFile "tasks")

getTaskByDayR :: Day -> Handler Html
getTaskByDayR day = do
    ((result, formWidget), formEnctype) <- runFormPost taskForm
    let handlerName = "getTaskByDayR" :: Text
    allTasks <- runDB getAllTasks
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
        $(widgetFile "tasks")

taskForm :: Form Task
taskForm = renderBootstrap3 (BootstrapHorizontalForm (ColSm 1) (ColSm 1) (ColSm 1) (ColSm 1)) $ Task
    <$> areq textField "Description" Nothing
    <*> areq dayField "Start" Nothing
    <*> aopt dayField "End" Nothing
    <*> areq textField "RepeatPattern" Nothing

getAllTasks :: DB [Entity Task]
getAllTasks = selectList [] [Asc TaskId]

showTaskDay :: Maybe Day -> [Char]
showTaskDay d
  | isJust d = show $ fromJust d
  | otherwise = ""
