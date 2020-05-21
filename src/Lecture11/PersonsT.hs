{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture11.PersonsT where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Prelude hiding (id)
import Lecture10.Reader (Person (..), PersonId, findById', persons, processSingle, processPair)


-- <Задачи для самостоятельного решения>

{-
  В этом задании нужно адаптировать код из 10 лекции к новым требованиям
  при помощи трансформеров.

  1. Необходимо собирать статистику найденных людей:
    - количество одиноких персон
    - количество замужних персон
  2. В функцию `findById` добавить логгирование с помощью монады Write.
    Нужно сообщать была ли найдена персона по данному id или нет.

  Вы можете переиспользовать функции, которые остались без изменения из Lecture10.Reader
-}

data PersonSearchStats = PersonSearchStats
  { marriedPersonsCount :: Integer
  , singlePersonsCount :: Integer
  } deriving (Show)

emptyStats :: PersonSearchStats
emptyStats = PersonSearchStats 0 0

newtype PersonsT a = PersonsT
  { runPersonsT ::  ReaderT [Person] (StateT PersonSearchStats (Writer [String] )) a}
  deriving
    ( Functor
    , Applicative
    , Monad
--  ...
    ,MonadWriter [[Char]]
    ,MonadReader [Person]
    ,MonadState PersonSearchStats
    )

-- personsFinderT :: ReaderT [Person] (StateT PersonSearchStats (Writer [String])) 

runPersons :: PersonsT a -> ((a, PersonSearchStats), [String])
runPersons p = runWriter $ flip runStateT emptyStats $ flip runReaderT persons $ runPersonsT $ p

findById :: PersonId -> PersonsT (Maybe Person)
findById pId = do 
  persons' <- ask
  let person' = findById' pId persons'
  case person' of 
    Just _        -> tell  ["Found person: " ++ (show pId)]
    Nothing       -> tell  ["Indalid id: "   ++ (show pId) ]
  return $ person' 

processPerson :: PersonId -> PersonsT (Maybe String)
processPerson pId = do
  searchStats <- get
  one <- findById pId
  two <- case one of
    Just (Person _ _ _ _ _ (Just tId)) -> findById tId
    _                                  -> return $ Nothing
  case (one, two) of
    (Just o, Just t) ->  do
      put (PersonSearchStats (marriedPersonsCount searchStats + 1) (singlePersonsCount searchStats))
      return $ Just $ processPair o t
    (Just o, _)      ->  do
      put (PersonSearchStats (marriedPersonsCount searchStats) (singlePersonsCount searchStats + 1))
      return $ Just $ processSingle o
    _                ->  return $ Nothing
{-
  Функция должна выводить на экран:
  - общую поисковую статистику по всем найденым персонам.
  - результат каждого поиска

  Записывать в "persons.log" общий лог всех поисков.
-}

type Result     = ((Maybe String, PersonSearchStats), [String])
type ResultList = [Result]

iterateOverIds :: [PersonId] -> ResultList
iterateOverIds personIds = do 
  pId <- personIds
  return $ runPersons $ processPerson pId

sumTwoStats :: PersonSearchStats -> PersonSearchStats -> PersonSearchStats
sumTwoStats s1 s2 = PersonSearchStats ((marriedPersonsCount s1) + (marriedPersonsCount s2)) ((singlePersonsCount s1) + (singlePersonsCount s2))

getStats :: ResultList -> PersonSearchStats
getStats list = foldr (\((_, ns), _) cs -> sumTwoStats cs ns) emptyStats list

getLogs  :: ResultList -> [String]
getLogs list = foldr (\(_, log) clog -> concat [log, clog]) [] list

processResultPair :: (Result, PersonId) -> String
processResultPair (((ms,_),_), pId) = case ms of 
    Just s   -> "found: " ++ show pId ++ ".\n" ++ s
    Nothing  -> "not found: " ++ show pId ++ "."

getProcessResults :: [(Result, PersonId)] -> [String]
getProcessResults paired = map processResultPair paired

processPersons :: [PersonId] -> IO ()
processPersons personIds = do
  let res     = iterateOverIds personIds
  let stats   = getStats res
  let logs    = getLogs res
  let procRes = getProcessResults $ zip res personIds

  putStrLn ("processResults:\n" ++ intercalate "\n" procRes)
  putStrLn ("\npersonStats:\n" ++ show stats)
  writeFile "persons.log" $ intercalate "\n" logs

-- </Задачи для самостоятельного решения>
