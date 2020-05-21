module Lecture10.Reader where

import Data.List
import Data.Maybe
import Control.Monad.Reader
import Prelude hiding (id)

-- <Задачи для самостоятельного решения>

{-
  Задача: по имеющейся базе данных сфомировать набор рекламных писем.
  Для неженатого(-ой) персоны письмо должно иметь вид:

  Для мужчины:

"""
  Уважаемый Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Для женщины:

"""
  Уважаемая Имя Отчество!
  Разрешите предложить Вам наши услуги.
"""

  Семейным парам шлется одно(?) письмо вида

"""
  Уважаемые Имя_мужа Отчество_мужа и Имя_жены Отчество_жены!
  Разрешите предложить вам наши услуги.
"""

-}

data Sex = Male | Female deriving (Show, Eq, Ord)

type PersonId = Int

data Person = Person
  { id :: Int
  , family :: String
  , name :: String
  , surname :: String
  , sex :: Sex
  , marriedBy :: Maybe Int
  } deriving (Show, Eq, Ord)

persons :: [Person]
persons =
  [ Person 1 "Иванов" "Иван" "Иванович" Male Nothing
  , Person 2 "Петров" "Петр" "Петрович" Male (Just 7)
  , Person 3 "Соловьева" "Алия" "Фаридовна" Female Nothing
  , Person 4 "Кузнецова" "Мария" "Ивановна" Female (Just 8)
  , Person 5 "Гринько" "Юлия" "Владимировна" Female Nothing
  , Person 6 "Кабанов" "Александр" "Романович" Male Nothing
  , Person 7 "Петрова" "Екатерина" "Алексеевна" Female (Just 2)
  , Person 8 "Кузнецов" "Евгений" "Семёнович" Male (Just 4)
  , Person 9 "Антонов" "Юрий" "Васильевич" Male Nothing
  ]

findById' :: PersonId -> [Person] -> Maybe Person
findById' pId [] = Nothing
findById' pId (p:ps) = if id p == pId 
  then Just p
  else findById' pId ps
 
-- Поиск персоны по номеру
findById :: PersonId -> Reader [Person] (Maybe Person)
findById pId = do 
  persons <- ask
  return $ findById' pId persons 

singleEnding  :: String
singleEnding  = "Разрешите предложить Вам наши услуги."

pairEnding    :: String
pairEnding    = "Разрешите предложить вам наши услуги."

processSingle :: Person -> String
processSingle p = intercalate "\n" [intro, singleEnding]
  where 
    names        = (name p) ++ " " ++ (surname p)
    introEnding  = if sex p == Male 
      then "ый"
      else "ая"
    intro        = "Уважаем" ++ introEnding ++ " " ++ names ++ "!"

processPair :: Person -> Person -> String
processPair h w = intercalate "\n" [intro, pairEnding]
  where 
    namesh       = (name h) ++ " " ++ (surname h)
    namesw       = (name w) ++ " " ++ (surname w)
    intro        = "Уважаемые " ++ namesh ++ " и " ++ namesw ++ "!"

processPerson :: PersonId -> Reader [Person] (Maybe String)
processPerson pId = do
  one <- findById pId
  two <- case one of
    Just (Person _ _ _ _ _ (Just tId)) -> findById tId
    _                                  -> return $ Nothing
  return $ case (one, two) of
    (Just o, Just t) -> Just $ processPair o t
    (Just o, _)      -> Just $processSingle o
    _                -> Nothing

processPersons :: [PersonId] -> [Maybe String]
processPersons personIds = map (\pId -> runReader (processPerson pId) persons) personIds

-- </Задачи для самостоятельного решения>
