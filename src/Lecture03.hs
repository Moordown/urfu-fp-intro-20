module Lecture03 where

{-

  03: Лямбда исчисление

  1. Базовый синтаксис и семантика untyped lambda calculus
  2. Нормальная форма, стратегии вычисления
  3. Примеры на Church encoding (логика) + упражнения
  4. Рекурсия (Y combinator)
  5. Примеры на Church encoding (арифметика) + упражнения
  6. Пары
  7. О типах

-}

{-

  Нетипизированное лямбда-исчисление
    "Untyped lambda calculus"

  Синтаксис:

    term ::= x          -- переменная
           | λ x . term -- абстракция
           | term term  -- аппликация

  Примеры термов:
  
    (λ x . (λ y . (x y)))

    (λ y . (λ z . (λ x . x)))

    ((λ x . x) y)

  Приоритеты операций:

    x y z = ((x y) z)

    λ x . λ y . λ z . x y z = λ x . (λ y . (λ z . x y z))

    Терм выше ^ сокращают как λ x y z . x y z

  Связанные и свободные переменные:

    1. λ x . λ y . x z
         ^         ^
         |         |
         -----------

    x - связанная
    z - свободная

    2. λ x . y (λ y . y)
             ^    ^---^
             |    связанная
             |
          свободная 

    3. M ≡ (λ x . x y)(λ y . y z)
         ^
      равенство по определению    

    Можно определить функцию, которая принимает терм и
    возвращает множество свободных переменных:

      FV(x) = {x}
      FV(M N) = FV(M) ∪ FV (N)
      FV(λ x . M) = FV(M) \ {x}

    Тогда свободные переменные для M, определенного выше:

    FV(M) = FV((λ x . x y)(λ y . y z)) =
      = FV(λ x . x y) ∪ FV(λ y . y z) =
      = (FV(x y) \ {x}) ∪ (FV(y z) \ {y}) =
      = (FV(x) ∪ FV(y) \ {x}) ∪ (FV(y) ∪ FV(z) \ {y}) =
      = ({x, y} \ {x}) ∪ ({y, z} \ {y})
      = {y} ∪ {z} = {y, z}

    FV — функция, рекурсивно определенная на структуре терма.

  Операция переименовывания переменной:
    
    Пусть есть терм M, в котором мы хотим переименовать переменную x на переменную y,
    тогда операция переименования обозначается как M{y/x} и определяется как:

      x{y/x}        ≡ y
      z{y/x}        ≡ z                 (если x ≠ z)
      (M N){y/x}    ≡ (M{y/x})(N{y/x})
      (λx . M){y/x} ≡ λy . (M{y/x})
      (λz . M){y/x} ≡ λz . (M{y/x})     (если x ≠ z) 

  α-эквивалентность:
  
    Термы, в которых можно переименовать связанную переменную,
    а они по сути не поменяются, называются альфа-эквивалентными:

      λ x . x   =α   λ y . y

      λ x . y x   ≠α   λ y . y y

      λ x . y x   =α   λ z . y z

    α-эквивалентность — это наименьшее конгруэнтное отношение эквивалентности на лямбда-термах.
    Такое, что для любых термов M и переменных y, которые не входят в M:

      λ x . M   =α   λ y . (M{y/x})

    Так как это отношение эквивалентности, то оно удовлетворяет
    правилам (refl), (symm) и (trans). Для конгруэнтности (cong) и (ξ).

    Правила:

      (refl)     --------
                  M =α M

                  M =α N
      (symm)     --------
                  N =α M

                  M =α N  N =α P
      (trans)    ----------------
                       M =α P

                   M =α M'  N =α N'
      (cong-app) -------------------
                     M N =α M' N'

                      M =α M'
      (cong-abs) -----------------
                   λx.M =α λx.M'

                          y ∉ M
      (α)        ------------------------
                    λx.M =α λy.(M{y/x})

  Подстановка:

    Пусть M и N термы, тогда подстановка терма N вместо переменной X в терме M обозначается как:
  
      M[N/x]

    Это сложная операция и редко когда определяется корректно. Проблемы:

      1. Нужно заменять только свободные переменные. То есть:

        (x(λx.λy.x))[N/x] = N(λx.λy.x), а не N(λx.λy.N)

      2. Нужно избежать ненамеренного связывания свободных переменных:

        M ≡ λx. y x
        N ≡ λz. x z

      Если делать подстановку наивно:

        M[N/y] = (λx. y x)[N/y] = λx. N x = λx. (λz. x z) x
                                                     ^
                                                    была свободной, а стала связанной 

      Значит нужно переименовывать связанную переменную перед подстановкой:

        M[N/y] = (λx'. y x')[N/y] = λx'. N x' = λx'. (λz. x z) x'

      Переменная x' — называется fresh variable, потому что не используется в термах
      и ее можно использовать. Поэтому удобно, когда множество переменных V — бесконечное.
      Это позволяет получить fresh variable в любой момент.

    Определение подстановки:

      x[N/x] ≡ N
      y[N/x] ≡ y                        (если x ≠ y)
      (M P)[N/x] ≡ (M[N/x])(P[N/x])
      (λx.M)[N/x] ≡ λx.M
      (λy.M)[N/x] ≡ λy.(M[N/x])         (если x ≠ y и y ∉ FV(N))
      (λy.M)[N/x] ≡ λy'.(M{y'/y}[N/x])  (если x ≠ y, y ∈ FV(N), и y' — fresh)

  β-редукция:

    До этого момента с выражениеями, которые мы ввели, нельзя было делать ничего полезного.
    Мы могли переименовать переменные внутри терма, но это не добавляло синтаксису никакого смысла.
    Пора перейти к собственно вычислениям.

    Процесс "подстановки" аргументов в функцию называется β-редукция.
    Определим её формально с помощью подстановки:

    (β)          --------------------------
                   (λx.M) N   ->β   M[N/x]

                         M ->β M'
    (cong-app1)     ----------------
                      M N ->β M' N

                        N ->β N'
    (cong-app2)     ----------------
                      M N ->β M N'

                          M ->β M'
    (cong-abs)      ------------------
                      λx.M ->β λx.M'

    Рассмотрим в качестве примера терм
    
      (λx.y) ((λz.z z) (λw.w)) -- выполним β-редукцию внутри вторых скобок, получим:
  ->β (λx.y) ((λw.w) (λw.w))   -- можем продолжить, снова выполнив β-редукцию внутри вторых скобок:
  ->β (λx.y) (λw.w)            -- и, применив β-редукцию ещё раз, получим:
  ->β y
  
    Говорят, что `y` вычисляется из `(λx.y) ((λz.z z) (λw.w))`, а для обозначения используют ->>β:
    
      (λx.y) ((λz.z z) (λw.w))  ->>β  y
    
    У нас остался терм, в котором нет ни одного выражения, которое можно было бы редуцировать.
    Такой терм называется β-нормальной формой.
    
    Заметим, что мы могли начать с редукции первого терма и сразу получить:
    
      (λx.y) ((λz.z z) (λw.w))  ->β  y
     
    Интересно, что получившаяся нормальная форма не зависила от порядка редукции.
    Сразу возникает вопрос: верно ли это для всех термов?

    Ответ на этот вопрос даёт одно из центральных утверждений нетипизированного лямбда-исчисления,
    которое носит имя создателя лямбда-исчисления [Аланзо Чёрча](https://en.wikipedia.org/wiki/Alonzo_Church)
    и его ученика [Джона Россера](https://en.wikipedia.org/wiki/J._Barkley_Rosser).

    Теорема Чёрча-Россера
    Пусть существуют термы M, N и P такие, что N и P вычисляются из M.
    Тогда существуе терм Z, который вычисляется из N и P.

      M ->> N ->> Z
      M ->> P ->> Z

    Доказательство можно посмотреть в заметках Селинжера, ссылка на них есть в конце лекции.

    Теперь рассмотрим терм:
    
        (λx.x x) (λy.y y y)     -- проредуцируем его несколько раз:
    ->β (λy.y y y) (λy.y y y) 
    ->β (λy.y y y) (λy.y y y) (λy.y y y)
    ->β ...
  
    Из этого примера видно, что редукция необязательно уменьшает размер лямбда-выражения.
    Значит не все лямбда-выражения имеют нормальную форму.
    
    Следствие теоремы Чёрча-Россера
    Если у лямбда-терма есть нормальная форма, то она единственна.
    
    При этом, от порядка редукции может зависеть то, получится ли привести терм к нормальной форме.
    Если добавить терм (λx.y) к предыдущему примеру, то станет ясно,
    что редукция вторых скобок никогда не даст нам нормальную форму:
    
      (λx.y) ((λx.x x) (λy.y y y))

    Существуют разные порядки редукции:
    - call by value
      Вычислить аргумент функции перед тем, как её выполнить
    - call by name
      Не вычислять аргумент функции перед тем, как её выполнить, а подставлять аргумент как есть
    - сall by need
      Как call by name, но, если аргумент функции всё-таки был вычислен, результат вычисления запоминается
    - normal order
      Начинать редукции с самого левого терма, который ещё можно редуцировать.
    - ...
      
    Подробнее: https://en.wikipedia.org/wiki/Evaluation_strategy
-}

{-
  Чтобы запустить лямбда калькулятор, откройте отдельное окно терминала и введите:

    cabal new-run lambda

  Посмотрим, как можно выразить логические операции с помощью лямбда-термов.

  true ≡ λa.λb.a

  false ≡ λa.λb.b

  ifelse ≡ λx.x

    ifelse true M N ->> M    
    ifelse false M N ->> N
-}

-- <Задачи для самостоятельного решения>
{-
  WARNING

  Все лямбды в этом файле нужно записывать, экранируя `\`, потому что так работают строки.

  Т.е. \a.\b.\a будет выглядеть как true = "\\a.\\b.a"
-}

{-
  Напишите терм `not`:

    - not true -> not (λa.λb.a) ->> λa.λb.b
    - not false ->> true
-}

not :: String
not = "\\x.x false true"

{-
  Напишите терм `and`:

    - and true true ->> true
    - and true false ->> false
    - and false true ->> false
    - and false false ->> false
-}
and :: String
and = "\\x.\\y.x y false"

{-
  Напишите терм `or`:

    - or true true ->> true
    - or true false ->> true
    - or false true ->> true
    - or false false ->> false
-}
or :: String
or = "\\x.\\y.x true y"
-- </Задачи для самостоятельного решения>

{-
  Нумералы Чёрча

  В лямбда-термах можно выразить и арифметику. Для каждого натурального числа (с нулём) введём терм:
  
    n ≡ λf x.f^n x

  Такой терм и называют n-ым нумералом Чёрча. Если в такой нумерал передать функцию (+1) и 0,
  то в результате как раз получится нужное число.
  Например, так выглядят первые три нумерала:
  
    0 ≡ λf x.x
    1 ≡ λf x.f x
    2 ≡ λf x.f (f x)

  Чтобы из n-ого нумерала получить n+1 введём функцию succ:
  
    succ = λn f x.f (n f x)
   
  Убедимся, что `succ n = n+1`:
  
    succ n ≡  (λn f x.f (n f x)) (λf x.f^n x)
          →β  f x.f ((λf x.f^n x) f x)
        ->>β  f x.f (f^n x)
           ≡  (n+1)
   
   Так определяется функции сложения и умножения:
   
     add ≡ λn m f x.n f (m f x)
     mult ≡ λn m f x.n (m f) x
   
   Заметим, что у функции mult можно убрать аргумент `x`, т.к. он просто дописывает его в конец.
   Это называется η-редукция:
    
      mult ≡ λn m f.n (m f)
   
   Самостоятельно убедитесь, что
   
    (plus 3 3) succ 0 ≡ 6
    (mult 3 3) succ 0 ≡ 9
-}

    
-- <Задачи для самостоятельного решения>

{-
  isZero 0 = True
  isZero n = False
-}
isZero :: String
isZero = "\\n. n (\\x. false) true"

-- </Задачи для самостоятельного решения>

{-
  Рекурсивные функции
  
  Для того, чтобы понять, как выразить рекурсивную функцию в лямбда-терм, введём понятие
  фиксированной точки.
  
  Будем писать M =β M', если M можно привести к M' за ноль или больше β-редукций.
  Пусть F и N лямбда-термы. Тогда N называется фиксированной точкой F, если F N =β N.

  Теорема
  В нетипизированном лямбда-исчислении для каждого терма F существует фиксированная точка.

  Доказательство
  Обозначим A ≡ λx y.y (x x y) и определим терм Y ≡ A A.
  Пусть F — лямбда-терм. Докажем, что N ≡ Y F является фиксированной точкой для F.
  
    N  ≡  Y F
       ≡  A A F
       ≡  (λx y.y (x x y)) A F
    ->>β  F (A A F)
       ≡  F (Y F)
       ≡  F N
                                                  ч.т.д.

  Y называют комбинатором неподвижной точки Тьюринга (Turing’s fixed point combinator) (y-combinator).

  Зачем нам понадобилась неподвижная точка? Дело в том, что фиксированные точки позволяют нам
  решать уравнения. Поиск N в F = F N не отличается от решения уравнения x = f(x).
  Благодаря теореме выше, мы знаем, что всегда можем решить такое уравнение.

  Поймём, как это делать на примере. Напишем функцию вычисления факторила, используя термы
  ifelse, iszero, mult и pred (функция обратная succ, (-1)) и нумералы Чёрча.

    fact n = ifelse (iszero n) (1) (mult n (fact (pred n)))

  В этом выражении fact встречается и в левой, и в правой части? значит, чтобы найти fact,
  нужно решить уравнение. Для этого, сначала немного перепишем его.

    fact = λn.ifelse (iszero n) (1) (mult n (fact (pred n)))
    fact = (λf.λn.ifelse (iszero n) (1) (mult n (f (pred n)))) fact

  Обозначим за F терм, стоящий в правой части перед fact. Получим:

    fact = F fact  -- уравнение, в котором нужно найти фиксированную точку

    fact = Y F
         = Y (λf.λn.ifelse (iszero n) (1) (mult n (f (pred n))))

  Правая часть получившегося выражения -- замкнутый лямбда-терм. Терм называется замкнутым,
  если в нём нет свободных переменных). Посмотрим, как он работает:

  fact 2
        ->>β  F fact 2
           ≡  (λf.λn.ifelse (iszero n) (1) (mult n (f (pred n)))) fact 2
        ->>β  ifelse (iszero 2) (1) (mult 2 (fact (pred 2))))
        ->>β  ifelse (False) (1) (mult 2 (fact (pred 2))))
        ->>β  (mult 2 (fact (pred 2))))
        ->>β  (mult 2 (fact 1)))
        ->>β  (mult 2 (F fact 1)))
        ->>β  (mult 2 ((λf.λn.ifelse (iszero n) (1) (mult n (f (pred n)))) fact 1))
        ->>β  (mult 2 ((ifelse (iszero 1) (1) (mult 1 (fact (pred 1))))))
        ->>β  (mult 2 ((ifelse (iszero False) (1) (mult 1 (fact (pred 1))))))
        ->>β  (mult 2 (mult 1 (fact (pred 1))))
        ->>β  (mult 2 (mult 1 (fact 0)))
        ->>β  (mult 2 (mult 1 (F fact 0)))
        ->>β  (mult 2 (mult 1 ((λf.λn.ifelse (iszero n) (1) (mult n (f (pred n)))) fact 0)))
        ->>β  (mult 2 (mult 1 (ifelse (iszero 0) (1) (mult 0 (fact (pred 0))))))
        ->>β  (mult 2 (mult 1 (ifelse (True) (1) (mult 0 (fact (pred 0))))))
        ->>β  (mult 2 (mult 1 (1)))
        ->>β  (mult 2 (mult 1 1))
        ->>β  (mult 2 1)
        ->>β  (2)
        ->>β  2

  Рекомендуем довести до конца самостоятельно.
-}

-- <Задачи для самостоятельного решения>

{-
  pred = λa b c. ((a (λd e.e (d b))) (λd.c)) (λd.d)

  Используя Y-комбинатор напишите функцию для вычисления чисел Фибоначчи: 
    
    f(n) = f(n-1) + f(n-2)

  Начальные значения определяют по-разному, в нашем случае:

    - fib 0 ->>β 1
    - fib 1 ->>β 2
    - fib 5 ->>β 13

  Для решения вам понадобятся уже известные функции succ, plus, iszero, а так же функция новая функция pred.
  Мы не будем приводить её определение. Попробуйте запустить её для нескольких нумералов.
  Что она возвращает? Чему равен pred pred 1?

  pred = λn s z. snd (n (λp . pair (s (fst p)) (fst p)) (pair z z))
-}
fib :: String
fib = "\\n.ifelse (iszero n) (1) (ifelse (iszero (pred n)) (2) (plus (fib (pred (pred n))) (fib (pred n))))"

-- </Задачи для самостоятельного решения>

{-
  Кодирование функций и данных с помощью функций в лямбда-исчислении называется Church encoding.  

  В лямбда-исчислении можно выразить и структуры данных. Например, введём пару из двух элементов
  и функции для получения первого и второго элемента соответственно:
    
    tuple M N ≡ λz.z M N
    first ≡ λp.p (λx y.x)
    second ≡ λp.p (λx y.y)
  
  Попробуйте поиспользовать её в лямбда-калькуляторе.
-}

{-
   О типах
  
   Обратите внимание, функции first и second для пары можно переписать как
 
    first ≡ λp.p true
    second ≡ λp.p false

   Что выглядит довольно странным. Более того, термы 0 и False равны,
   поэтому нам нужно знать, как именно интепретировать результат вычисления.
   Проверка типов на этапе компиляции позволяет избежать множество ошибок, но об этом мы говорим позже.
-}

{-

  Почитать:

  - https://neerc.ifmo.ru/wiki/index.php?title=%D0%9B%D1%8F%D0%BC%D0%B1%D0%B4%D0%B0-%D0%B8%D1%81%D1%87%D0%B8%D1%81%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5
  - https://www.irif.fr/~mellies/mpri/mpri-ens/biblio/Selinger-Lambda-Calculus-Notes.pdf
-}
