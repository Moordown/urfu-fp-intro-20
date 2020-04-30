{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications #-}

module Lecture08 where

import Data.Char
import Data.Array
import qualified Data.Set as Set
import qualified Data.IntMap as Map

{- 08: Структуры данных

  Под структурами данных понимаются типы, которые позволяют хранить данные и в том
  или ином смысле удобно и эффективно с ними обращаться. Поэтому эти типы также часто
  называют контейнерными. Конечно, чтобы ими было удобно пользоваться, они должны
  уметь хранить элементы любых типов, т.е. быть полиморфными.
  
  Обычно контейнерные структуры имеют следующие операции:
   - insert   - добавление элемента
   - remove   - удаление элемента
   - contains - проверка наличия элемента в структуре, доступ к элементу в случае его наличия
   
  Основное различие между структурами данных состоит в том, насколько эффективно выполняются
  эти операции. При этом эффективность операций можно рассматривать в худшем случае и в среднем.
  Для оценки в худшем случае нужно найти данные, на которых будет совершенно больше всего операций,
  А среднюю сложность получают, усредняя количество проделываемых операций в время работы программы.
  https://neerc.ifmo.ru/wiki/index.php?title=Амортизационный_анализ
-}

{- Stack
  
  Если вдруг вы забыли, что это https://neerc.ifmo.ru/wiki/index.php?title=Стек.
  Основные операции на стеке: Push, Pop, IsEmpty, иногда к ним добавляют Peek (посмотреть вершину, не удаляя).
  
  Стек — одна из самых простых для реализации в ФП структура данных. Её очень легко реализовать
  на списках. Именно это мы и предлагаем вам сделать.
-}

-- <Задачи для самостоятельного решения>

data Stack a = Stack [a] deriving (Eq, Show)

createStack :: Stack a
createStack = Stack []

-- Обратите внимание, что все структуры данных неизменяемые (immutable). Значит, если операция
-- предполагает изменение структуры, то она просто должна возвращать новую уже изменённую версию.
push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack ([x] ++ xs)

pop :: Stack a -> Maybe (Stack a)
pop (Stack [])        = Nothing
pop (Stack (x : xs)) = Just (Stack xs)

peek :: Stack a -> Maybe a
peek (Stack [])     = Nothing
peek (Stack (x:_))  = Just x

-- </Задачи для самостоятельного решения>

{- Важно, что такая очень прямолинейная реализация будет эффективной. Почему?
  
  Дело в том, что операция `:` в Haskell работает за O(1). И это несовсем очевидно.
  Рассмотрим такой пример:
  
    > a = [1,2,3]
    > b = 0 : a
    > a
    [1, 2, 3]
    > b
    [0, 1, 2, 3]

  Т.е. Нам доступны и `a` и `b`, но копирования не произошло, иначе это заняло бы O(n) времени.
  Всё дело в том, что в односвязнм списоке очень легко запоминать все предыдущие состояния.
  Нарисуем, как хранится пример выше:

              a
              |
              v
    b -> 0 -> 1 -> 2 -> 3

  При этом `a` и `b` не "мешают друг другу". Если бы мы получили `a` по-другому: `a = tail b`,
  картинка была бы точно такой же.
-}

{- О персистентности.
  https://neerc.ifmo.ru/wiki/index.php?title=Персистентные_структуры_данных

  Структуры данных, которые при внесении в них каких-то изменений сохраняют все свои предыдущие состояния
  и доступ к этим состояниям, называются персистентными (persistent). При этом различают разные "уровни" персистентности.
  Например, если в структуре вносить изменения можно только в последнюю версию, а старые версии только читать,
  то такая структура будет частично (partial) персистентной. А если менять можно любую версию,
  то — полностью (full) персистентной.
  Как мы увидели выше, односвязный список может хранить все свои версии, каждую можно изменять, т.е. он
  получился полностью персистентным.
  Ну и на самом деле, все чистые функциональные структуры данных, т.е. такие структуры данных, которые
  можно написать на чистом функциональном языке, будут автоматически полностью персистентными.

  Структуры же, которые предполагают доступ к единственной своей версии, называются эфемерными.
-}

{- Очередь

  Попробуем реализовать очередь так же прямолинейно. Будем брать элемент из головы с помощью
  pattern matching и дописывать элемент в хвост с помощью операции (++):
-}

enqueue' :: [a] -> a -> [a]
enqueue' queue x = queue ++ [x]

dequeue' :: [a] -> (a, [a])
dequeue' [] = error "Queue is empty"
dequeue' (q:qs) = (q, qs)             -- возвращаем (элемент, новая версия очереди)

{- Насколько эффективно это будет работать?
  
  Мы уже выяснили, что (:) работает быстро, а значит нас интересует эффективность конкатенации.
  Здесь может иметь значение то, лениво или строго она выполняется.
  
  Строгий случай.
  При дописывании нового элемента в связный список нельзя сделать тот же трюк, который мы описывали выше.
  Придётся скопировать всю структуру. Посмотрим на рисунок:

    > a = [1, 2]            -- a -> 1 -> 2 -> nil
    > b = [1, 2] ++ [3]     -- a -> 1 -> 2 -> nil  b -> 1 -> 2 -> 3 -> nil

  И мы уже не можем хранить и `a`, и `b`b в одном списке, т.к. непонятно, куда должна вести
  ссылка из 2:

  a,b -> 1 -> 2 -> nil  -- xтобы работали оба списка элемент 2 должен ссылаться и на nil, и на 3
              |   
              v   ????
              3
  
  А значит enqueue будет работать за O(n), что, конечно, очень медленно.

  Ленивый случай.
  При конкатенации новый список не создаётся, но создаётся thunk. Его можно представить вот таким деревом:

        (++)
       /    \
    [1,2]   [3]

  Тогда, получение головы списка, которое участвует в реализации очереди, можно представить вот так:
  
    head (xs ++ ys) = if null xs then head ys else head xs
  
  Выглядит так, как будто работать будет быстро. К сожалению, для большой очереди дерево будет высоким:

    [1] ++ [2] ++ [3] ++ ... ++ [n]                 (++)
                                                   /    \
                                                 ...    [n]
                                                /
                                             (++)
                                            /    \
                                          (++)   [3]
                                         /    \
                                       [1]    [2]

   А значит, вытащить `1` быстро у нас не получится. Надо либо за O(n) вычислить всю очередь,
   либо пройтись по высокому дерево, что так же долго.
-}

{- 
  Поэтому воспользуемся реализацией очереди на двух стеках. В такой реализации иногда операции
  требуют O(n) времени, но в среднем выполняются за O(1).
  https://neerc.ifmo.ru/wiki/index.php?title=Очередь -- #Реализация_на_двух_стеках
-}

-- <Задачи для самостоятельного решения>

data Queue a = Queue [a] [a] deriving (Eq, Show)

createQueue :: Queue a
createQueue = Queue [] []

enqueue :: Queue a -> a -> Queue a
enqueue (Queue enc dec) x = Queue (x:enc) dec 

-- если очередь пустая возвращает ошибку
dequeue :: Queue a -> (a, Queue a)
dequeue (Queue [] [])         = error "try dequeue from empty Queue"
dequeue (Queue enc [])        = dequeue (Queue [] (foldl (flip (:)) [] enc))
dequeue (Queue enc (xd:dec))  = (xd, Queue enc dec)

isEmpty :: Queue a -> Bool
isEmpty (Queue [] []) = True
isEmpty _             = False

-- </Задачи для самостоятельного решения>

{- Массив

  Самая тривиальная в императивном программировании структура здесь оказывается самой сложной.
  В императивных языках массив предоставляет очень быстрое чтение/изменение элементов по индексам
  через "прямой" доступ к памяти. В сплошном куске памяти очень просто найти место элемента,
  зная его индекс, размер одного элемента и начало массива.
  Чистая функция, конечно, не может обратиться к памяти по указателю, а значит непонятно, как быстро обратиться
  к элементу по индексу.

  Попробуем посмотреть, что можно использовать в Haskell вместо привычных массивов.
  Под "массивом" мы будем подразумевать структуру данных с быстрым, желательно контастным, чтением
  и записью по индексу.

  На самом деле, в Haskell тоже можно работать с памятью, ведь на Haskell пишут настоящие программы.
  Но ради этого придётся выйти за рамки чистых функций и воспользоваться монадами, о которых мы подробно поговорим
  на двух следующих занятиях. Пока только скажем, что настоящие, но монадные, массивы в Haskell'е есть.
  Это STArray https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array-ST.html
  и IOArray http://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array-IO.html.

  А теперь поговорим о "чистых" альтернативах.
-}

{- Data.Array

  https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array.html#t:Array
  Честный функциональный массив с доступом к элементам по индексу за O(1). Подвох в том, что
  "функциональность" влечёт за собой неизменяемость, а это значит, при каждом изменении
  будет создан новый массив за O(n).
-}

{-
  Создать такой массив можно с помощью функции array

    > :t array
    array :: Ix i => (i, i) -> [(i, e)] -> Array i e

  где Ix — класс типов для индексов

    > :i Ix
    class Ord a => Ix a where
    range :: (a, a) -> [a]
    index :: (a, a) -> a -> Int
    ...
  
  (i, i)    — максимальный и минимальный индексы массива
  [(i, e)]  — список пар (индекс, значение)
  Array i e — созданный массив

  или функции listArray

    > :t listArray
    listArray :: Ix i => (i, i) -> [e] -> Array i e
-}

x :: Array Int Int
x = array (0, 9) [(i, i) | i <- [0..9]]

-- Т.к. у Char есть instance класса Ix, то индексами можно задать и символы
-- все Instance можно посмотреть с помощью `:i Ix`
y :: Array Char Int
y = array ('a', 'c') [(chr i, i) | i <- [ord 'a'..ord 'c']]

-- Т.к. массив полиморфный, можно создать и двойной массив, т.е. массив массивов
matrix :: Array Int (Array Int Int)
matrix = array (0, 2) [(i, x) | i <- [0..2]]

{-
  Интересно посмотреть, что будет, если диапазон индексов будет больше, чем список элементов:

    > listArray (0, 5) [1,2,3]
    array (0,5) [(0,1),(1,2),(2,3),(3,*** Exception: (Array.!): undefined array element
    ^                                         ^
    Массив создался лениво, начал печататься и сломался
-}

{-
  Получить i-ый элемент массива можно с помощью функции (!)

    > :t (!)
    (!) :: Ix i => Array i e -> i -> e
-}

x0 :: Int
x0 = x ! 0 -- x0 = 0

matrix00 :: Int
matrix00 = matrix ! 0 ! 0

{-
  А изменить массив можно с помощью функции (//)

    > :t (//)
    (//) :: Ix i => Array i e -> [(i, e)] -> Array i e
-}

z :: Array Int Int
z = x // [(i, 0) | i <- [0..9], even i]  -- в `y` на всех чётных местах `x` стоит 0

{- Разные древовидные структуры

  Что делать, если нам всё-таки нужно часто менять массив и O(n) за операцию нас не устраивает.
  Здесь могут помочь любые известные вам сбалансированные деревья. Их много разных, выбирайте то,
  какое вам больше нравится.

  В результате получится структура с чтением и изменением элементов за O(log n). При этом по эффективности
  такие чистые структуры на Haskell будут несильно уступать аналогичным эфемерным реализациям.
  Здесь мы не будем вас просить их реализовывать, это вы сделаете в рамках других курсов.
  Проблемы могут возникнуть только с теми структурами, в которых нужны случайные числа.
  Например, в декартовом дереве (treap) значения неявного ключа выбирают случайно. Функция, которая
  по ничему возвращает число (каждый раз новое), конечно, не будет чистой. Это сново возвращает нас
  к теме следующих занятий — монадам.

  Отдельно хочется обсудить, как можно внести изменение в дерево, не копируя его полностью, ведь копирование
  займёт O(n) времени и памяти. Это можно сделать примерно так же, как мы делали с односвязным списком.
  Это называется методом копирования пути (Path Copying Method)

  https://neerc.ifmo.ru/wiki/index.php?title=Персистентные_структуры_данных  -- #Метод_копирование_пути
-}
{-
  В Haskell уже есть структуры данных, внутри которых скрываются сбалансированные деревья.
  При этом все эти структуры существуют в двух версия — строгая и ленивая.

  Data.Map.[Strict|Lazy] https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map.html
  Data.Set[.Lazy]        http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Set.html
  Data.IntMap[.Lazy]     http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-IntMap.html
  (строгая эффективная заточенная под ключи-int'ы реализация словаря)
-}

set :: Set.Set Integer
set = Set.fromList [1..100]

oddSet :: Set.Set Integer
oddSet = Set.fromList [i | i <- [1,3..99]]

evenSet :: Set.Set Integer
evenSet =  Set.difference set oddSet

emptySet :: Set.Set Integer
emptySet = Set.intersection evenSet oddSet

-- ...

{- Словарь за O(1)

  Ещё одна привычная структура данных, которая предоставляет очень быстрый доступ к элементам (а также
  поиск и изменение) — словарь. Один вариант словаря (такой же как, например, std::map в С++) мы уже видели,
  это словарь на сбалансированном дереве Data.Map, он работает за log(n). Теперь нас интересует словарь,
  основанный на хеш-таблице. https://neerc.ifmo.ru/wiki/index.php?title=Хеш-таблица

  Главное, что даёт скорость хеш-таблице — хороший хеш и массив. Как мы уже выяснили, чистого и массива
  с изменением элементов за O(1) у нас нет, а значит и хеш-таблицу построить не получится.
  Хотя есть монадная реализация
  Data.HashTable  http://hackage.haskell.org/package/base-4.2.0.0/docs/Data-HashTable.html
-}

{- Hash array mapped trie (HAMT)

  Познакомимся с HAMT — одним из чистых аналогов хеш-таблицы.
  Data.HashMap.[Lazy|Strict]  https://hackage.haskell.org/package/unordered-containers
-}

-- <Задачи для самостоятельного решения>

{-
    Вам нужно реализовать сортировку подсчётом http://neerc.ifmo.ru/wiki/index.php?title=Сортировка_подсчётом
    с использованием разных структур данных в качестве массива и измерить производительность.

    А именно:
    - создать класс типов IntArray, хранящих целые числа, и реализовать его инстансы
      для каждой структуры
      - [Int]
      - Array
      - Один из Map, IntMap или Data.HashMap
    - написать сортировку с использованияем этого класса типов

  Вы можете сравнить скорость работы разных реализаций с помощью функции computeTime, которая
  принимает на вход:
    - максимальное значение чисел для генерации
    - размер списка
    - список реализаций (имя, функция)
  и печатает время выполнения для каждой реализации в секундах.

    > :load Lecture08/Time.hs
    *Lecture08.Time> :set -XTypeApplications
    *Lecture08.Time> computeTime 20 (10^7) [("[Int]", countingSort @[Int])]
    [Int]: 2.325400635

  Мы уже рассказывали про расширения ScopedTypeVariables и TypeApplications.
  Помимо них в этой задаче используется расширение AllowAmbiguousTypes
  https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=ambiguous#extension-AllowAmbiguousTypes
-}



-- Названия методов можно менять
class IntArray a where
  fromList :: [(Int, Int)] -> a    -- создать из списка пар [(index, value)]
  toList :: a -> [(Int, Int)]      -- преобразовать в список пар [(index, value)]
  update :: a -> Int -> Int -> a   -- обновить элемент по индексу
  (#) :: a -> Int -> Int           -- получить элемент по индексу

zeroesArr :: Array Int Int -> Int -> Int
zeroesArr arr index = ans
  where
    f (a, b) = a
    s (a, b) = b
    ans = if index < (f (bounds arr)) || index > (s (bounds arr)) 
          then 0 
          else arr ! index

insertInArr :: Array Int Int -> Int -> Int -> Array Int Int
insertInArr arr index value = ans
  where
    f (a, b) = a
    s (a, b) = b
    ans = if index < (f (bounds arr)) || index > (s (bounds arr)) 
          then arr 
          else arr // [(index, value)]


instance IntArray (Array Int Int) where 
  fromList pairs           = let 
                                _min [] = 0
                                _min arr = minimum arr 
                                _max [] = -1
                                _max arr = maximum arr
                                mn = _min (map fst pairs) 
                                mx = _max (map fst pairs)
                             in array (mn, mx) pairs 
  toList array             = assocs array 
  update array index value = array // [(index, value)]
  (#) array index          = array ! index

zeroes :: Maybe Int -> Int
zeroes (Just a) = a
zeroes Nothing  = 0
  
instance IntArray (Map.IntMap Int) where 
  fromList values       = Map.fromList values 
  toList map            = Map.assocs map
  update map key value  = Map.insert key value map  
  (#) map key           = zeroes $ map Map.!? key

  
instance IntArray [Int] where 
  fromList pairs               = map snd pairs
  toList array                 = zip (range (0, length array)) array
  
  update [] _ _                = error "can't update empty list"
  update (x:array) 0 value     = value : array
  update (x:array) index value = x : (update array (index - 1) value)

  (#) [] _                     = error "can't get from empty list"
  (#) (x:array) 0              = x
  (#) (x:array) index          = array # (index - 1)   

-- Сортирует массив целых неотрицательных чисел по возрастанию
countingSort :: forall a. IntArray a => [Int] -> [Int]
countingSort [] = []
countingSort values = ans
  where 
    intArray = fromList [(i, 0) | i <- range (0, maximum values)] :: a
    inc array index = update array index $ 1 + array # index :: a
    countArray = foldl (\arr index -> inc arr index) intArray values

    repeat (v, 0) = []
    repeat (v, c) = v : (repeat (v, c - 1))
    ans = concat (map repeat (toList countArray)) 

{-
  Tак можно запустить функцию сортировки с использованием конкретной реализацией массива:

  *Lecture08> :set -XTypeApplications
  *Lecture08> countingSort @[Int] [2,2,2,3,3,3,1,1,1]
  [1,1,1,2,2,2,3,3,3]
  *Lecture08> countingSort @(Map.IntMap Int) [2,2,2,3,3,3,1,1,1]
  [1,1,1,2,2,2,3,3,3]
-}

sorted :: [Int]
sorted = countingSort @[Int] [2,2,2,3,3,3,1,1,1]
-- </Задачи для самостоятельного решения>

{- Сылки

  - "Purely Functional Data Structures"         Chris Okasaki https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf
  - "Functional Data Structures and Algorithms" Milan Straka  http://fox.ucw.cz/papers/thesis/thesis.pdf
  - "Ideal Hash Trees"                          Phil Bagwell  http://lampwww.epfl.ch/papers/idealhashtrees.pdf
  - "A Hash Array Mapped Trie for C++"          Phil Nash     https://www.youtube.com/watch?v=imrSQ82dYns
  - "Hash Array Mapped Trie"                    Андрей Гейн   https://www.youtube.com/watch?v=aERxzsp_49U
-}
