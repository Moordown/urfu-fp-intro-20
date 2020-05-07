{-# LANGUAGE DuplicateRecordFields, ScopedTypeVariables, TypeApplications #-}

module Lecture09 where


import System.IO
import System.Random
import Data.List.Split
import System.FilePath.Posix
import System.Directory
import Control.DeepSeq
import Data.List

{-
  09: Монады IO и Random

  - overview and motivation
  - Pure computations a -> b
    - Lazy: very hard to do i/o => pure
    - effects and side-effects
      - java checked exceptions
        - throwable Exception
  - not a toy => I/O (before monads, haskell 1.0)
    - streams
    - continuations
  - Effects
    - a -> IO b -- i/o
    - a -> (b -> r) -> r -- continuations
    - a -> b + Ex -- exceptions
    - a -> [b]
    - a -> Maybe b
    - T = PR _, IO _, (_ -> r) -> r, _ + Ex, [_], Maybe _
      - T-effectfull computation a -> b is a pure computation a -> T b
      - why we want (show examples, continuations figure 4, arguments passing)
        - 1. want to embed pure data into effectful world
          a -> T a
        - 2. composition of effectful computation
          a -> T b
          b -> T c
          ---------
          a -> T c

          bind :: T b -> (b -> T c) -> T c
    - class T m where
        (>>=)  :: m a -> (  a -> m b) -> m b
        return ::   a                 -> m a
  - Monad
    - set of types: Int, Float, Maybe a, IO a, ...
    - laws
      Left identity: 	return a >>= f ≡ f a
      Right identity: m >>= return ≡ m
      Associativity: 	(m >>= f) >>= g	≡ m >>= (\x -> f x >>= g)
  - Higher-kinded polymorphism + type classes + monads
  - monadic I/O (haskell 1.3 1996)
    - standard functions
  - random
    - standard functions
  - algebraic effects
  - Functor => Applicative => Monad
    - semantics (behaviour)

  Подробнее:
  - Chapter 7
    https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/history.pdf
  - https://www.seas.upenn.edu/~cis194/fall16/lectures/06-io-and-monads.html
-}

-- <Задачи для самостоятельного решения>

{-
  TODO list

  Напишите программу для работы со списком задач.
  Хранить задачи нужно в виде файлов в определённой папке.
  Вы можете сами выбрать формат имени и содержимого файлов.

  Вам понадобятся
  - System.Directory
    https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html
  - Работа с файлами в IO
    http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-}

newtype TodoList = TodoList FilePath deriving (Eq, Show)

newtype Id = Id String deriving (Eq, Show)

newtype Title = Title String deriving (Eq, Show)

newtype Deadline = Deadline String deriving (Eq, Show, Ord)

newtype Content = Content String deriving (Eq, Show)

-- Тип для чтения Todo
data Todo = Todo
  { todoId :: Id
  , title :: Title
  , content :: Content
  , deadline :: Deadline
  , isDone :: Bool
  } deriving (Eq, Show)

instance Ord Todo where
  compare (Todo _ _ _ deadline1 _) (Todo _ _ _ deadline2 _) = compare deadline1 deadline2

-- Тип для редактирования Todo
data TodoEdit = TodoEdit
  { title :: Title
  , content :: Content
  , deadline :: Deadline
  } deriving (Eq, Show)

createTodoList :: FilePath -> IO TodoList
createTodoList rootFolder = do
  createDirectory rootFolder
  return $ TodoList rootFolder

delim :: [Char] 
delim = "\n"

writeTODO :: TodoList -> Todo -> IO ()
writeTODO (TodoList folder) (Todo (Id id) (Title title) (Content content) (Deadline deadline) isDone) =  
  writeFile fname $!! text
  where fname    = folder </>  id
        done    = if isDone == False then "f" else "t"
        text    = done ++ delim ++ title ++ delim ++ content ++ delim ++ deadline

addTodo :: TodoList -> Title -> Content -> Deadline -> IO Id
addTodo todoList title@(Title title') content deadline@(Deadline deadline') = do
  writeTODO todoList $ Todo (Id id) title content deadline False 
  return (Id id)
  where id = deadline' <.> title' <.> "txt"

getLines :: Handle -> IO [String]
getLines h = hGetContents h >>= return . lines  

readTodo :: TodoList -> Id -> IO Todo
readTodo (TodoList path) (Id id) = do
      content <- readFile fname
      let values    = splitOn delim content
      let isDone    = if values !! 0 == "f" then False else True
      let title     = values !! 1
      let content   = values !! 2
      let deadline  = values !! 3
      let res       = Todo (Id id) (Title title) (Content content) (Deadline deadline) isDone
      return res
  where fname   = path </> id

showTodo :: TodoList -> Id -> IO ()
showTodo todoList id = do
  todo <- readTodo todoList id
  putStrLn $ show todo 

removeTodo :: TodoList -> Id -> IO ()
removeTodo (TodoList path) (Id id) = do
  removeFile $ path </> id

editTodo :: TodoList -> Id -> TodoEdit -> IO ()
editTodo todoList id (TodoEdit title content deadline) = do
  writeTODO todoList $ Todo id title content deadline False

setTodoAsDone :: TodoList -> Id -> IO ()
setTodoAsDone todoList id = do
  Todo id title content deadline isDone <- readTodo todoList id
  writeTODO todoList $ Todo id title content deadline True

getByFilePath :: TodoList -> FilePath -> IO Todo
getByFilePath todoList filePath = readTodo todoList (Id filePath) :: IO Todo 

isFinished :: Todo -> Bool
isFinished (Todo _ _ _ _ isDone) = isDone


-- Todo должны быть упорядочены по возрастанию deadline'а
readAllTodo :: TodoList -> IO [Todo]
readAllTodo todoList@(TodoList todoList') = do
  filePaths <- listDirectory todoList'
  todo <- mapM (getByFilePath todoList) filePaths
  return $ sort todo

readUnfinishedTodo :: TodoList -> IO [Todo]
readUnfinishedTodo todoList = do
  todo   <- readAllTodo todoList
  return $ filter (not . isFinished) todo

showAll :: [Todo] -> IO ()
showAll list = mapM_ (putStrLn . show) list

showAllTodo :: TodoList -> IO ()
showAllTodo todoList = do
  todo    <- readAllTodo todoList
  showAll todo

showUnfinishedTodo :: TodoList -> IO ()
showUnfinishedTodo todoList = do
  todo    <- readUnfinishedTodo todoList
  showAll todo

{-
  Напишите игру для угадывания случайного числа.

  При старте, необходимо случайным образом выбрать число в
  отрезке [0..100] и предоставить пользователю возможность его угадать.

  Пример игровой сессии:

  > playGuessGame
  Your number: 50
  > Too big
  Your number: 25
  > Too small
  Your number: 37  
  > Yep, that's the number!
-}

guessGame :: Integer -> IO ()
guessGame number = do
  putStrLn "Your number: "
  input <- getLine
  let num = read input :: Integer
  if number < num then putStrLn "Too big" >> guessGame number
  else if number == num then putStrLn "Yep, that's the number!"
  else putStrLn "Too small" >> guessGame number
    

playGuessGame :: IO ()
playGuessGame = do
  putStrLn "> playGuessGame"
  number <- randomRIO (0, 100) :: IO Integer
  guessGame number


-- </Задачи для самостоятельного решения>