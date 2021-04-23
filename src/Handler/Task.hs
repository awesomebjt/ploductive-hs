{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Task where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
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
taskForm = renderDivs $ Task
    <$> areq textField "Description" Nothing
    -- <*> areq textField "Start" Nothing
    -- <*> areq textField "End" Nothing
    -- <*> areq textField "RepeatPattern" Nothing

getAllTasks :: DB [Entity Task]
getAllTasks = selectList [] [Asc TaskId]