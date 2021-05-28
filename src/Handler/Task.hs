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
--import Data.Time.LocalTime ( LocalTime(localDay) )
import Data.Time.Calendar.WeekDate ( toWeekDate )
import Data.Time.Calendar (addDays)

import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Database.Esqueleto      (rawSql)

getTaskR :: Handler Html
getTaskR = do
    (formWidget, formEnctype) <- generateFormPost taskForm
    -- let handlerName = "getTaskR" :: Text
    allTasks <- runDB getDefaultTasks
    print allTasks
    (year, month, day) <- liftIO getCurrentDate
    let today = show $ fromGregorian year month day
    let wday = getWeekDate year month day
    print $ numToDayOfWeek wday
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
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
    (year, month, day) <- liftIO getCurrentDate
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
    (year, month, day) <- liftIO getCurrentDate
    let today = show year ++ show month ++ show day
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
        $(widgetFile "tasks")

deleteTaskR :: Handler Html
deleteTaskR = do
    ((result, formWidget), formEnctype) <- runFormPost taskForm
    print result
    (year, month, day) <- liftIO getCurrentDate
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
    let nextDay = addDays 1 day
    let prevDay = addDays (-1) day
    let weekdayOfDay = fromJust $ numToDayOfWeek $ third $ toGregorian day
    --let handlerName = "getTaskByDayR" :: Text
    --let sql :: Text = "SELECT ?? FROM Task WHERE begin <= '" ++ show day ++ "' and end >= '" ++ show day ++ "' and repeatpattern like '%" ++ [weekdayOfDay] ++ "%'"
    -- TODO: use the sql query to actually pull 'allTasks'
    allTasks <- runDB $ selectList [TaskBegin <=. day, TaskEnd >=. Just day] [Asc TaskId]
    --allTasks2 <- runDB $ rawSql sql []
    defaultLayout $ do
        setTitle "Ploductive: Tasks"
        $(widgetFile "tasksbyday")

taskForm :: Form Task
taskForm = renderBootstrap3 (BootstrapHorizontalForm (ColSm 1) (ColSm 1) (ColSm 1) (ColSm 1)) $ Task
    <$> areq textField "Description" Nothing
    <*> areq dayField "Start" Nothing
    <*> aopt dayField "End" Nothing

getAllTasks :: DB [Entity Task]
getAllTasks = selectList [] [Asc TaskId]

getDefaultTasks :: DB [Entity Task]
getDefaultTasks = do
    d <- liftIO getCurrentDay
    selectList [TaskEnd >=. Just d] [Asc TaskId]

showTaskDay :: Maybe Day -> [Char]
showTaskDay d
  | isJust d = show $ fromJust d
  | otherwise = ""


getCurrentDate :: IO (Integer,Int,Int) -- :: (year,month,day)
getCurrentDate = getCurrentTime >>= return . toGregorian . utctDay

getCurrentDay :: IO Day
getCurrentDay = getCurrentTime >>= return . utctDay

getWeekDate :: Integer -> Int -> Int -> Int
getWeekDate y m d = third (toWeekDate $ fromGregorian y m d)

third :: (a,b,c) -> c
third (_, _, x) = x

numToDayOfWeek :: Int -> Maybe Char
numToDayOfWeek n
  | n==0 = Just 'S'
  | n==1 = Just 'M'
  | n==2 = Just 'T'
  | n==3 = Just 'W'
  | n==4 = Just 'H'
  | n==5 = Just 'F'
  | n==6 = Just 'A'
  | otherwise = Nothing

weekdayInPattern :: Int -> String -> Bool
weekdayInPattern d p = elem (fromJust $ numToDayOfWeek d) p
