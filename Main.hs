-- Marco Antônio Barbosa Zulian

{-- 1. Usando List Comprehension escreva uma função, chamada divisoresden, que devolva uma lista dos divisores de um número dado. --}
divisoresden :: Int -> [Int]
divisoresden x = [ a | a <- [1..(x-1)], x `mod` a == 0]

{-- 2. Usando  List Comprehension  escreva  uma  função,  chamada  contaCaractere,  que  conte  a  ocorrência de um caractere específico, em uma string dada. --}
contaCaractere :: String -> Char -> Int
contaCaractere s ch = length [ a | a <- s, a == ch]

{-- 3. Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve o dobro dos valores dos elementos não negativos da lista de inteiros dada. --}
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo x = [ 2*a | a <- x, a >= 0]

{-- 4. Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis de serem construídos por inteiros entre 1 e um número inteiro dado. --}
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras x = [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x], a + b > c && a + c > b && b + c > a]

{-- 5. Números  perfeitos  são  aqueles  cuja  soma  dos  seus  divisores  é  igual  ao  próprio  número. Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se que você já tem uma função que devolve uma lista dos divisores de um número dado. --}
somaDigitos :: [Int] -> Int
somaDigitos [] = 0
somaDigitos (h:t) = h + somaDigitos t

numerosPerfeitos :: Int -> [Int]
numerosPerfeitos x = [a | a <- [1..x], (somaDigitos (divisoresden a)) == a]

{-- 6. Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e zip no 
prelude que podem ser úteis. --}
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar x y = somaDigitos [ fst a * snd a  | a <- zip x y ]


{-- 7. Usando  List Comprehension  escreva  uma  função,  chamada  primeirosPrimos,  que  devolva uma lista contendo os n primeiros números primos a partir do número 2. --}
primo :: Int -> Bool
primo x = (length (divisoresden x)) == 1

primeirosPrimos :: Int -> [Int]
primeirosPrimos x = take x [ a | a <- [1..], primo a]

{-- 8. Usando  List Comprehension  escreva  uma  função,  chamada  paresOrdenados,  que  devolva uma  lista  de  par  ordenados  contendo  uma  potência  de  2  e  uma  potência  de  3  até  um determinado número dado. Observe que estes números podem ser bem grandes. --}
paresOrdenados :: Double -> [(Double, Double)]
paresOrdenados x = [(a * a, a * a * a) | a <- [1..x]]

main = do
  putStrLn ("Func. 1: entrada: 15; resultado: " ++ show(divisoresden 15))
  putStrLn ("Func. 2: entrada: 'banana' 'b'; resultado: " ++ show(contaCaractere "banana" 'b'))
  putStrLn ("Func. 2: entrada: 'banana' 'n'; resultado: " ++ show(contaCaractere "banana" 'n'))
  putStrLn ("Func. 2: entrada: 'banana' 'a'; resultado: " ++ show(contaCaractere "banana" 'a'))
  putStrLn ("Func. 2: entrada: 'banana' 'x'; resultado: " ++ show(contaCaractere "banana" 'x'))
  putStrLn ("Func. 3: entrada: [-4,1,0,-1,4]; resultado: " ++ show(dobroNaoNegativo [(-4), 1, 0, (-1), 4]))
  putStrLn ("Func. 4: entrada: 3; resultado: " ++ show(pitagoras 3))
  putStrLn ("Func. 5: entrada: 500; resultado: " ++ show(numerosPerfeitos 500))  
  putStrLn ("Func. 6: entrada: (1,2,3) (2,3,-4); resultado: " ++ show(produtoEscalar [1,2,3] [2,3,(-4)]))
  putStrLn ("Func. 7: entrada: 5 resultado: " ++ show(primeirosPrimos 10))
  putStrLn ("Func. 8: entrada: 3 resultado: " ++ show(paresOrdenados 3))