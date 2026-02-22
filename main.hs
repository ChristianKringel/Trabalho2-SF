
-- Definição das árvore sintática para representação dos programas:

data E = Num Int
      |Var String
      |Soma E E
      |Sub E E
      |Mult E E
   deriving(Eq,Show)

data B = TRUE
      | FALSE
      | Not B
      | And B B
      | Or  B B
      | Leq E E
      | Igual E E  -- verifica se duas expressões aritméticas são iguais
   deriving(Eq,Show)

data C = While B C
    | If B C C
    | Seq C C
    | Atrib E E
    | Skip
    | TenTimes C   ---- Executa o comando C 10 vezes
    | Repeat C B --- Repeat C until B: executa C enquanto B é falso
    | Loop E E C      ---- Loop e1 e2 c: executa (e2 - e1) vezes o comando C
    | DuplaATrib E E E E -- recebe 2 variáveis e 2 expressões (DuplaATrib (Var v1) (Var v2) e1 e2) e faz v1:=e1 e v2:=e2
    | AtribCond B E E E --- AtribCond b (Var v1) e1 e2: se b for verdade, então faz v1:e1, se B for falso faz v1:=e2
    | Swap E E -- swap(x,y): troca o conteúdo das variáveis x e y
   deriving(Eq,Show)

-----------------------------------------------------
-----
----- As próximas funções, servem para manipular a memória (sigma)
-----
------------------------------------------------


--- A próxima linha de código diz que o tipo memória é equivalente a uma lista de tuplas, onde o
--- primeiro elemento da tupla é uma String (nome da variável) e o segundo um Inteiro
--- (conteúdo da variável):


type Memoria = [(String,Int)]

exSigma :: Memoria
exSigma = [ ("x", 10), ("temp",0), ("y",0)]


--- A função procuraVar recebe uma memória, o nome de uma variável e retorna o conteúdo
--- dessa variável na memória. Exemplo:
---
--- *Main> procuraVar exSigma "x"
--- 10


procuraVar :: Memoria -> String -> Int
procuraVar [] s = error ("Variavel " ++ s ++ " nao definida no estado")
procuraVar ((s,i):xs) v
  | s == v     = i
  | otherwise  = procuraVar xs v


--- A função mudaVar, recebe uma memória, o nome de uma variável e um novo conteúdo para essa
--- variável e devolve uma nova memória modificada com a varíável contendo o novo conteúdo. A
--- chamada
---
--- *Main> mudaVar exSigma "temp" 20
--- [("x",10),("temp",20),("y",0)]
---
---
--- essa chamada é equivalente a operação exSigma[temp->20]

mudaVar :: Memoria -> String -> Int -> Memoria
mudaVar [] v n = error ("Variavel " ++ v ++ " nao definida no estado")
mudaVar ((s,i):xs) v n
  | s == v     = ((s,n):xs)
  | otherwise  = (s,i): mudaVar xs v n


-------------------------------------
---
--- Completar os casos comentados das seguintes funções:
---
---------------------------------

smallStepE :: (E, Memoria) -> (E, Memoria)
smallStepE (Var x, s)                  = (Num (procuraVar s x), s)

-- Soma
smallStepE (Soma (Num n1) (Num n2), s) = (Num (n1 + n2), s)
smallStepE (Soma (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Soma (Num n) el, sl)
smallStepE (Soma e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Soma el e2,sl)

-- Mult
smallStepE (Mult (Num n1) (Num n2), s) = (Num (n1 * n2), s)
smallStepE (Mult (Num n) e, s)         = let (el,sl) = smallStepE (e,s)
                                         in (Mult (Num n) el, sl)
smallStepE (Mult e1 e2,s)              = let (el,sl) = smallStepE (e1,s)
                                         in (Mult el e2,sl)

-- Sub
smallStepE (Sub (Num n1) (Num n2),s)  = (Num (n1 - n2), s)
smallStepE (Sub (Num n) e,s)          = let (el, sl) = smallStepE (e, s)
                                        in (Sub (Num n) el, sl)
smallStepE (Sub e1 e2,s)              = let (el, sl) = smallStepE (e1, s)
                                        in (Sub el e2, sl)

-- Booleans
smallStepB :: (B,Memoria) -> (B, Memoria)

-- Not (NOT1, NOT2, NOT3)
smallStepB (Not TRUE, s)  = (FALSE, s)                         -- NOT2: ¬True  → False
smallStepB (Not FALSE, s) = (TRUE, s)                          -- NOT3: ¬False → True
smallStepB (Not b, s)     = let (b1, s1) = smallStepB (b, s)  -- NOT1: B → B' / ¬B → ¬B'
                             in (Not b1, s1)

-- AND (AND1, AND2, AND3)
smallStepB (And TRUE TRUE, s)   = (TRUE, s)
smallStepB (And TRUE FALSE, s)  = (FALSE, s)
smallStepB (And FALSE TRUE, s)  = (FALSE, s)
smallStepB (And FALSE FALSE, s) = (FALSE, s)
smallStepB (And TRUE b, s)      = let (b1, s1) = smallStepB (b, s)
                                  in (And TRUE b1, s1)
smallStepB (And b1 b2, s)       = let (b1', s1) = smallStepB (b1, s)
                                  in (And b1' b2, s1)

-- OR (OR1, OR2, OR3)
smallStepB (Or TRUE TRUE, s)   = (TRUE, s)
smallStepB (Or TRUE FALSE, s)  = (TRUE, s)
smallStepB (Or FALSE TRUE, s)  = (TRUE, s)
smallStepB (Or FALSE FALSE, s) = (FALSE, s)
smallStepB (Or TRUE b, s)      = let (b1, s1) = smallStepB (b, s)
                                  in (Or TRUE b1, s1)
smallStepB (Or b1 b2, s)       = let (b1', s1) = smallStepB (b1, s)
                                  in (Or b1' b2, s1)

-- Leq (LEQ1, LEQ2, LEQ3)
smallStepB (Leq (Num n1) (Num n2), s) = if n1 <= n2 then (TRUE, s) else (FALSE, s)  -- LEQ3: n1≤n2 → b
smallStepB (Leq (Num n) e, s)         = let (e1, s1) = smallStepE (e, s)             -- LEQ2: n≤E → n≤E'
                                         in (Leq (Num n) e1, s1)
smallStepB (Leq e1 e2, s)             = let (e1', s1) = smallStepE (e1, s)           -- LEQ1: E1→E1' / E1≤E2→E1'≤E2
                                         in (Leq e1' e2, s1)

-- Igual (análogo ao Leq, mas com igualdade)
smallStepB (Igual (Num n1) (Num n2), s) = if n1 == n2 then (TRUE, s) else (FALSE, s)
smallStepB (Igual (Num n) e, s)         = let (e1, s1) = smallStepE (e, s)
                                           in (Igual (Num n) e1, s1)
smallStepB (Igual e1 e2, s)             = let (e1', s1) = smallStepE (e1, s)
                                           in (Igual e1' e2, s1)

-- Comandos
smallStepC :: (C,Memoria) -> (C,Memoria)

-- Atrib (ATRIB1, ATRIB2)
smallStepC (Atrib (Var x) (Num n), s) = (Skip, mudaVar s x n)           -- ATRIB2: x:=n → Skip, σ[x↦n]
smallStepC (Atrib (Var x) e, s)       = let (e', s') = smallStepE (e, s) -- ATRIB1: E→E' / x:=E→x:=E'
                                         in (Atrib (Var x) e', s')

-- Seq (SEQ1, SEQ2)
smallStepC (Seq Skip c2, s) = (c2, s)                                    -- SEQ2: Skip;C2 → C2
smallStepC (Seq c1 c2, s)   = let (c1', s') = smallStepC (c1, s)         -- SEQ1: C1→C1' / C1;C2→C1';C2
                               in (Seq c1' c2, s')

-- If (IF1, IF2, IF3)
smallStepC (If TRUE  c1 c2, s) = (c1, s)                                 -- IF2: if True  then C1 else C2 → C1
smallStepC (If FALSE c1 c2, s) = (c2, s)                                 -- IF3: if False then C1 else C2 → C2
smallStepC (If b c1 c2, s)     = let (b', s') = smallStepB (b, s)        -- IF1: B→B' / if B then C1 else C2 → if B' then C1 else C2
                                  in (If b' c1 c2, s')

-- While (WHILE)
-- While B do C → if B then (C ; While B do C) else Skip
smallStepC (While b c, s) = (If b (Seq c (While b c)) Skip, s)

-- TenTimes C: executa C exatamente 10 vezes
-- TenTimes C → C;C;C;C;C;C;C;C;C;C
smallStepC (TenTimes c, s) =
  (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c (Seq c Skip))))))))), s)

-- Repeat C B: executa C enquanto B for falso (repeat-until)
-- Repeat C B → C ; if B then Skip else (Repeat C B)
smallStepC (Repeat c b, s) = (Seq c (If b Skip (Repeat c b)), s)

-- Loop e1 e2 c: executa C (e2 - e1) vezes usando e1 como contador crescente
-- Se e1 não é valor, reduz e1 primeiro
-- Se e2 não é valor, reduz e2 primeiro
-- Se n1 >= n2, termina (Skip)
-- Se n1 <  n2, executa C e incrementa o contador: C ; Loop (n1+1) n2 c
smallStepC (Loop (Num n1) (Num n2) c, s)
  | n1 >= n2  = (Skip, s)
  | otherwise = (Seq c (Loop (Num (n1 + 1)) (Num n2) c), s)
smallStepC (Loop (Num n) e c, s) = let (e', s') = smallStepE (e, s)
                                    in (Loop (Num n) e' c, s')
smallStepC (Loop e1 e2 c, s)     = let (e1', s') = smallStepE (e1, s)
                                    in (Loop e1' e2 c, s')

-- DuplaATrib (Var v1) (Var v2) e1 e2: faz v1:=e1 e v2:=e2 sequencialmente
-- DuplaATrib x1 x2 e1 e2 → (x1:=e1) ; (x2:=e2)
smallStepC (DuplaATrib x1 x2 e1 e2, s) = (Seq (Atrib x1 e1) (Atrib x2 e2), s)

-- AtribCond b (Var v) e1 e2: se b for verdade faz v:=e1, senão faz v:=e2
-- AtribCond b x e1 e2 → if b then (x:=e1) else (x:=e2)
smallStepC (AtribCond b x e1 e2, s) = (If b (Atrib x e1) (Atrib x e2), s)

-- Swap (Var x) (Var y): troca o conteúdo das variáveis x e y atomicamente
-- Lê os valores de x e y no estado atual e produz Skip com memória atualizada
smallStepC (Swap (Var x) (Var y), s) =
  let vx = procuraVar s x
      vy = procuraVar s y
  in (Skip, mudaVar (mudaVar s x vy) y vx)

----------------------
--  INTERPRETADORES
----------------------


--- Interpretador para Expressões Aritméticas:
isFinalE :: E -> Bool
isFinalE (Num n) = True
isFinalE _       = False


interpretadorE :: (E,Memoria) -> (E, Memoria)
interpretadorE (e,s) = if (isFinalE e) then (e,s) else interpretadorE (smallStepE (e,s))

--- Interpretador para expressões booleanas

isFinalB :: B -> Bool
isFinalB TRUE    = True
isFinalB FALSE   = True
isFinalB _       = False

interpretadorB :: (B,Memoria) -> (B, Memoria)
interpretadorB (b,s) = if (isFinalB b) then (b,s) else interpretadorB (smallStepB (b,s))


-- Interpretador da Linguagem Imperativa

isFinalC :: C -> Bool
isFinalC Skip    = True
isFinalC _       = False

interpretadorC :: (C,Memoria) -> (C, Memoria)
interpretadorC (c,s) = if (isFinalC c) then (c,s) else interpretadorC (smallStepC (c,s))


--------------------------------------
---
--- Exemplos de programas para teste
---

exSigma2 :: Memoria
exSigma2 = [("x",3), ("y",0), ("z",0)]


---
--- O progExp1 é um programa que usa apenas a semântica das expressões aritméticas. Esse
--- programa já é possível rodar com a implementação que fornecida:

progExp1 :: E
progExp1 = Soma (Num 3) (Soma (Var "x") (Var "y"))

---
--- para rodar:
-- A função smallStepE anda apenas um passo na avaliação da Expressão

-- *Main> smallStepE (progExp1, exSigma)
-- (Soma (Num 3) (Soma (Num 10) (Var "y")),[("x",10),("temp",0),("y",0)])

-- Note que no exemplo anterior, o (Var "x") foi substituido por (Num 10)

-- Para avaliar a expressão até o final, deve-se usar o interpretadorE:

-- *Main> interpretadorE (progExp1 , exSigma)
-- (Num 13,[("x",10),("temp",0),("y",0)])

-- *Main> interpretadorE (progExp1 , exSigma2)
-- (Num 6,[("x",3),("y",0),("z",0)])


---
--- Exemplos de expressões booleanas:


teste1 :: B
teste1 = (Leq (Soma (Num 3) (Num 3))  (Mult (Num 2) (Num 3)))

teste2 :: B
teste2 = (Leq (Soma (Var "x") (Num 3))  (Mult (Num 2) (Num 3)))


---
-- Exemplos de Programas Imperativos:

testec1 :: C
testec1 = (Seq (Seq (Atrib (Var "z") (Var "x")) (Atrib (Var "x") (Var "y")))
               (Atrib (Var "y") (Var "z")))

fatorial :: C
fatorial = (Seq (Atrib (Var "y") (Num 1))
                (While (Not (Igual (Var "x") (Num 1)))
                       (Seq (Atrib (Var "y") (Mult (Var "y") (Var "x")))
                            (Atrib (Var "x") (Sub (Var "x") (Num 1))))))


--------------------------------------
--- Exemplos dos novos comandos:
---
--- exSigma3: memória para os exemplos abaixo
--- x=5, y=0, z=0

exSigma3 :: Memoria
exSigma3 = [("x",5), ("y",0), ("z",0)]

---
--- TenTimes: soma 1 a x, 10 vezes (resultado: x = 15)
---
--- *Main> interpretadorC (exTenTimes, exSigma3)
--- (Skip,[("x",15),("y",0),("z",0)])

exTenTimes :: C
exTenTimes = TenTimes (Atrib (Var "x") (Soma (Var "x") (Num 1)))

---
--- Repeat: incrementa x até que x seja igual a 10
--- (repeat { x:=x+1 } until x==10)
---
--- *Main> interpretadorC (exRepeat, exSigma3)
--- (Skip,[("x",10),("y",0),("z",0)])

exRepeat :: C
exRepeat = Repeat (Atrib (Var "x") (Soma (Var "x") (Num 1)))
                  (Igual (Var "x") (Num 10))

---
--- Loop: executa y:=y+x exatamente (10-5)=5 vezes (resultado: y = 5*5 = 25)
---
--- *Main> interpretadorC (exLoop, exSigma3)
--- (Skip,[("x",5),("y",25),("z",0)])

exLoop :: C
exLoop = Loop (Num 5) (Num 10) (Atrib (Var "y") (Soma (Var "y") (Var "x")))

---
--- DuplaATrib: atribui x:=10 e y:=20 simultaneamente
---
--- *Main> interpretadorC (exDuplaAtrib, exSigma3)
--- (Skip,[("x",10),("y",20),("z",0)])

exDuplaAtrib :: C
exDuplaAtrib = DuplaATrib (Var "x") (Var "y") (Num 10) (Num 20)

---
--- AtribCond: se x<=5 então z:=1, senão z:=2
--- Como x=5, a condição é verdadeira e z recebe 1
---
--- *Main> interpretadorC (exAtribCond, exSigma3)
--- (Skip,[("x",5),("y",0),("z",1)])

exAtribCond :: C
exAtribCond = AtribCond (Leq (Var "x") (Num 5)) (Var "z") (Num 1) (Num 2)

---
--- Swap: troca o conteúdo de x e y
--- x=5, y=0 → após swap: x=0, y=5
---
--- *Main> interpretadorC (exSwap, exSigma3)
--- (Skip,[("x",0),("y",5),("z",0)])

exSwap :: C
exSwap = Swap (Var "x") (Var "y")


---
---   Exemplos ---
---

fibonacci :: Int -> C
fibonacci val =
  Seq
    (Atrib (Var "x") (Num 0))
    (Seq
      (Atrib (Var "y") (Num 1))
      (Seq
        (Atrib (Var "n") (Num val))
        (While (Not (Igual (Var "n") (Num 1)))
          (Seq
            (Atrib (Var "temp") (Var "x"))
            (Seq           -- x aqui recebe y
               (DuplaATrib (Var "x") (Var "y") (Var "y")
                           -- y aqui vai receber temp (Valor original de x) + y
                           (Soma (Var "temp") (Var "y")))
               (Atrib (Var "n") (Sub (Var "n") (Num 1)))
            )
          )
        )
      )
    )

--- Para rodar:
--- *Main> runFibonacci 5
--- Fibonacci(5) = 5
---
--- Após o loop: x = F(n-1), y = F(n). Logo lemos "y".
--- A memória precisa ter todas as variáveis usadas pelo programa.

runFibonacci :: Int -> IO ()
runFibonacci n = do
    let mem = [("x",0), ("y",0), ("n",0), ("temp",0)]
    let (_, memFinal) = interpretadorC (fibonacci n, mem)
    let resultado = procuraVar memFinal "y"
    putStrLn $ "Fibonacci(" ++ show n ++ ") = " ++ show resultado ++ "\n"


calculaLinha :: C
calculaLinha =
  Seq
    (Atrib (Var "spaces") (Sub (Var "h") (Var "i")))
    (Atrib (Var "stars") (Sub (Mult (Num 2) (Var "i")) (Num 1)))

desenhaLinha :: Int -> Int -> IO ()
desenhaLinha altura linhaAtual = do
    let memInicial = [("h", altura), ("i", linhaAtual), ("spaces", 0), ("stars", 0)]
    let (_, memFinal) = interpretadorC (calculaLinha, memInicial)
    let espacos = procuraVar memFinal "spaces"
    let estrelas = procuraVar memFinal "stars"
    putStrLn (replicate espacos ' ' ++ replicate estrelas '*')

executaLoopVisual :: Int -> Int -> IO ()
executaLoopVisual altura i = do
    if fst (interpretadorB (Leq (Num i) (Num altura), [])) == FALSE
       then return ()
       else do
           desenhaLinha altura i
           executaLoopVisual altura (i + 1)

printArvore :: Int -> IO ()
printArvore altura = do
   executaLoopVisual altura 1
   putStrLn (replicate (altura - 1) ' ' ++ "|")
   putStrLn (replicate (altura - 1) ' ' ++ "|")


sort3Vars :: Int -> Int -> Int -> C
sort3Vars a b c =
  Seq
    (DuplaATrib (Var "x") (Var "y") (Num a) (Num b))
    (Seq
      (Atrib (Var "z") (Num c))
      (Seq
        (If (Leq (Var "x") (Var "y"))
            Skip
            (Seq
               (DuplaATrib (Var "temp") (Var "x") (Var "x") (Var "y"))
               (Atrib (Var "y") (Var "temp"))
            )
        )
        (Seq
          (If (Leq (Var "y") (Var "z"))
              Skip
              (Seq
                 (DuplaATrib (Var "temp") (Var "y") (Var "y") (Var "z"))
                 (Atrib (Var "z") (Var "temp"))
              )
          )
          (If (Leq (Var "x") (Var "y"))
              Skip
              (Seq
                 (DuplaATrib (Var "temp") (Var "x") (Var "x") (Var "y"))
                 (Atrib (Var "y") (Var "temp"))
              )
          )
        )
      )
    )

--- Para rodar:
--- *Main> runSort3Vars 3 1 2
--- Sorted values: 1, 2, 3

runSort3Vars :: Int -> Int -> Int -> IO ()
runSort3Vars a b c = do
    let mem = [("x",0), ("y",0), ("z",0), ("temp",0)]
    let (_, memFinal) = interpretadorC (sort3Vars a b c, mem)
    let xSorted = procuraVar memFinal "x"
    let ySorted = procuraVar memFinal "y"
    let zSorted = procuraVar memFinal "z"
    putStrLn $ "Sorted values: " ++ show xSorted ++ ", " ++ show ySorted ++ ", " ++ show zSorted ++ "\n"
