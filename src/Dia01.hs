{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Dia01 where

import Prelude hiding ((++))
import Data.Maybe

-- Representa uma Pilha utilizando uma lista ligada

data Pilha a = Vazia | Cons a (Pilha a)

nova :: Pilha a
nova = Vazia

vazia :: Pilha a -> Bool
vazia Vazia = True
vazia _     = False

empilha :: a -> Pilha a -> Pilha a
empilha  = Cons

topo :: Pilha a -> a
topo Vazia = error "Pilha vazia"
topo (Cons v _) = v

desempilha :: Pilha a -> Pilha a
desempilha Vazia = error "Pilha vazia"
desempilha (Cons _ p) = p

-- Também podemos representar uma pilha usando listas de Haskell

type Pilha' a = [a]

nova' :: Pilha' a
nova' = []

vazia' :: Pilha' a -> Bool
vazia' = null

empilha' :: a -> Pilha' a -> Pilha' a
empilha' = (:)

topo' :: Pilha' a -> a
topo' = head

desempilha' :: Pilha' a -> Pilha' a
desempilha' = tail

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

update :: [a] -> Int -> a -> [a]
update [] _ _ = error "Tentando atualizar lista vazia"
update (_:xs) 0 v = v:xs
update (x:xs) i v = x : update xs (i - 1) v

-- -------------
-- Exercício
-- -------------
sufixos :: [a]-> [[a]]
sufixos _xs = undefined

--                             esquerda  valor direita
data Lista2 a = Vazia2 | Cons2 (Lista2 a) a (Lista2 a)

null2 :: Lista2 a -> Bool
null2 Vazia2 = True
null2 _     = False

head2 :: Lista2 a -> a
head2 Vazia2 = error "Lista vazia"
head2 (Cons2 _ x _) = x

tail2 :: Lista2 a -> Lista2 a
tail2 Vazia2 = Vazia2
tail2 (Cons2 _ _ d) = d

cons2 :: a -> Lista2 a -> Lista2 a
cons2 = undefined -- ??

-- A primeira lista representa os elementos à esquerda
-- do foco (na ordem inversa). A segunda lista
-- os elementos à sua direita

type ListZipper a =  ([a], [a])

toListZipper :: [a] -> ListZipper a
toListZipper xs = ([], xs)

fromListZipper :: ListZipper a -> [a]
fromListZipper (es, ds) = reverse es ++ ds

lzFoco :: ListZipper a -> Maybe a
lzFoco (_, []) = Nothing
lzFoco (_, x:_) = Just x

lzDir :: ListZipper a -> Maybe (ListZipper a)
lzDir (_, []) = Nothing
lzDir (es, d:ds) = Just (d:es, ds)

lzEsq :: ListZipper a -> Maybe (ListZipper a)
lzEsq ([], _) = Nothing
lzEsq (e:es, ds) = Just (es, e:ds)

lzTrocaFoco :: a -> ListZipper a -> Maybe (ListZipper a)
lzTrocaFoco _ (_, []) = Nothing
lzTrocaFoco y (es, _:ds) = Just (es, y:ds)

trocaCincos :: [Int] -> [Int]
trocaCincos [] = []
trocaCincos xs =
  fromListZipper $ trocaZip $ toListZipper xs
  where
    trocaZip z@(_, []) = z
    trocaZip z@(_, d:_) =
      let nz = if d == 5
          then fromJust $ lzTrocaFoco 42 z
          else z in
      maybe nz trocaZip (lzDir nz)

-- Mas pra que tudo isso?
trocaCincos' :: [Int] -> [Int]
trocaCincos' xs = [if x == 5 then 42 else x | x <- xs]

-- Podemos representar uma árvore binária como

data Arv a = ArvVazia | No (Arv a) a (Arv a)

deriving instance Show a => Show (Arv a)

class Conjunto t where
   membro :: Ord a => a -> t a -> Bool
   insere :: Ord a => a -> t a -> t a

instance Conjunto Arv where

    membro _ ArvVazia = False
    membro x (No e v d)
        | x < v     = membro x e
        | x > v     = membro x d
        | otherwise = True

    insere x ArvVazia = No ArvVazia x ArvVazia
    insere x t@(No e v d)
       | x < v     = No (insere x e) v d
       | x > v     = No e v (insere x d)
       | otherwise = t

preOrdem, inOrdem, posOrdem :: Show a => Arv a -> IO ()
preOrdem ArvVazia = return ()
preOrdem (No e x d) = do
  print x
  preOrdem e
  preOrdem d
inOrdem ArvVazia = return ()
inOrdem (No e x d) = do
  inOrdem e
  print x
  inOrdem d
posOrdem ArvVazia = return ()
posOrdem (No e x d) = do
  posOrdem e
  posOrdem d
  print x

-- Fila "fuleira"®
type Queue a = [a]

-- O(1)
empty :: Queue a
empty = []

-- O(1)
isEmpty :: Queue a -> Bool
isEmpty = null

-- O(1)
enq :: Queue a -> a -> Queue a
enq = flip (:)

-- O(n)
deq :: Queue a -> (a, Queue a)
deq xs = (last xs, init xs)

larguraQ :: Show a => Arv a -> IO ()
larguraQ arv =
  larguraFila $ enq empty arv
  where
  larguraFila q =
    if isEmpty q then return ()
    else case deq q of
      (ArvVazia, q') -> larguraFila q'
      (No e x d, q') -> do
        print x
        larguraFila $ enq (enq q' e) d

larguraNivel :: Show a => Arv a -> IO ()
larguraNivel arv =
  larguraNivel' [arv]
  where
    printArv ArvVazia = return ()
    printArv (No _ x _) = print x

    filhos ArvVazia = []
    filhos (No e _ d) = [e, d]

    larguraNivel' [] = return ()
    larguraNivel' lvl = do
      mapM_ printArv lvl
      larguraNivel' $ concatMap filhos lvl

numeraPreOrdem :: Arv a -> Arv (a, Int)
numeraPreOrdem arv =
  snd $ numeraPreOrdem' 1 arv
  where
    numeraPreOrdem' i ArvVazia = (i, ArvVazia)
    numeraPreOrdem' i (No e x d) =
      (i3, No e' (x, i) d')
      where
        (i2, e') = numeraPreOrdem' (i + 1) e
        (i3, d') = numeraPreOrdem' i2 d

numeraInOrdem :: Arv a -> Arv (a, Int)
numeraInOrdem arv =
   snd $ numeraInOrdem' 1 arv
  where
    numeraInOrdem' i ArvVazia = (i, ArvVazia)
    numeraInOrdem' i (No e x d) =
      (i3, No e' (x, i2) d')
      where
        (i2, e') = numeraInOrdem' i e
        (i3, d') = numeraInOrdem' (i2 + 1) d

numeraPosOrdem :: Arv a -> Arv (a, Int)
numeraPosOrdem arv =
   snd $ numeraPosOrdem' 1 arv
  where
    numeraPosOrdem' i ArvVazia = (i, ArvVazia)
    numeraPosOrdem' i (No e x d) =
      (i3 + 1, No e' (x, i3)  d')
      where
        (i2, e') = numeraPosOrdem' i e
        (i3, d') = numeraPosOrdem' i2 d

bfn1 :: Arv a -> Arv (a, Int)

bfn1 t =
  fst $ deq $ bfn' 1 (enq empty t)
  where
    bfn' i inQ
      | isEmpty inQ = empty
      | otherwise =
        case deq inQ of
          (ArvVazia, inQ1) -> enq (bfn' i inQ1) ArvVazia
          (No e x d, inQ1) ->
            let
              inQ2 = enq (enq inQ1 e) d
              outQ0 = bfn' (i + 1) inQ2
              (d', outQ1) = deq outQ0
              (e', outQ2) = deq outQ1 in
              enq outQ2 (No e' (x, i) d')

bfn2 :: Arv a -> Arv (a, Int)

bfn2 arv = head $ bfn' 1 [arv]
  where
    filhos ArvVazia = []
    filhos (No a _ b) = [a, b]

    recons _ [] [] = []
    recons i (ArvVazia:ts) cs= ArvVazia : recons i ts cs
    recons i ~(No _ x _ : ts) ~(a : b : cs) =
      No a (x, i) b : recons (i + 1) ts cs

    bfn' _ [] = []
    bfn' i lvl =
      let
        proxNivel = concatMap filhos lvl
        j = i + length proxNivel `div` 2
        proxNivelNum = bfn' j proxNivel in
        recons i lvl proxNivelNum

--   Foco   Lista com o caminho percorrido
--          (Left/Right indica por que lado veio)

type ZipperArv a =
    (Arv a, [Either (a, Arv a) (a, Arv a)])

toArvZipper :: Arv a -> ZipperArv a
toArvZipper arv = (arv, [])

fromArvZipper :: ZipperArv a -> Arv a
fromArvZipper (arv, []) = arv
fromArvZipper z = fromJust $ fromArvZipper <$> arvCima z

arvFoco :: ZipperArv a -> Maybe a
arvFoco (ArvVazia, _) = Nothing
arvFoco (No _ x _, _) = Just x

arvTrocaFoco :: a -> ZipperArv a -> Maybe (ZipperArv a)
arvTrocaFoco _ (ArvVazia, _) = Nothing
arvTrocaFoco x (No a _ b, rastro) =
    Just (No a x b, rastro)

arvDir :: ZipperArv a -> Maybe (ZipperArv a)
arvDir (ArvVazia, _) = Nothing
arvDir (No e x d, rastro) =
    Just (d, Right (x, e):rastro)

arvEsq :: ZipperArv a -> Maybe (ZipperArv a)
arvEsq (ArvVazia, _) = Nothing
arvEsq (No e x d, rastro) =
    Just (e, Left (x, d):rastro)

arvCima :: ZipperArv a -> Maybe (ZipperArv a)
arvCima (_, []) = Nothing
arvCima (arv, Left (x, d):rastro)  =
    Just (No arv x d, rastro)
arvCima (arv, Right (x, e):rastro) =
    Just (No e x arv, rastro)

trocaArv :: Ord a => a -> a -> Arv a -> Arv a
trocaArv _ _ ArvVazia = ArvVazia
trocaArv velho novo (No e x d)
  | x < velho = No e x $ trocaArv velho novo d
  | x > velho = No (trocaArv velho novo e) x d
  | otherwise = No e novo d

trocaArvZ :: Ord a => a -> a -> Arv a -> Arv a
trocaArvZ velho novo arv =
  fromArvZipper $ trocaArvZ' $ toArvZipper arv
  where
    trocaArvZ' z@(ArvVazia, _) = z
    trocaArvZ' z
      | f < velho = go arvDir
      | f > velho = go arvEsq
      | otherwise = fromJust $ arvTrocaFoco novo z
      where
        f = fromJust $ arvFoco z
        go d = maybe z trocaArvZ' (d z)

data Emptiness = Empty | NonEmpty

data RoseTree (e :: Emptiness) a where
  EmptyRose :: RoseTree Empty a
  RoseTree  :: a -> [RoseTree NonEmpty a] -> RoseTree NonEmpty a

data RoseTreeZipper a = RoseTreeZipper
  a -- Valor do foco
  (ListZipper (RoseTree NonEmpty a)) -- Subzipper da lista de filhos
  [(a, ListZipper (RoseTree NonEmpty a))] -- Rastro

toRoseZipper :: RoseTree e a -> Maybe (RoseTreeZipper a)
toRoseZipper EmptyRose = Nothing
toRoseZipper (RoseTree v bs) =
  Just $ RoseTreeZipper v (toListZipper bs) []

roseFoco :: RoseTreeZipper a -> a
roseFoco (RoseTreeZipper v _ _) = v

roseTrocaFoco :: RoseTreeZipper a-> a-> RoseTreeZipper a
roseTrocaFoco (RoseTreeZipper _ lz ps) x =
  RoseTreeZipper x lz ps

roseDir :: RoseTreeZipper a -> Maybe (RoseTreeZipper a)
roseDir (RoseTreeZipper v lz ps) = do
  lz' <- lzDir lz
  Just $ RoseTreeZipper v lz' ps

roseEsq :: RoseTreeZipper a -> Maybe (RoseTreeZipper a)
roseEsq (RoseTreeZipper v lz ps) = do
  lz' <- lzEsq lz
  Just $ RoseTreeZipper v lz' ps

roseBaixo:: RoseTreeZipper a -> Maybe (RoseTreeZipper a)
roseBaixo (RoseTreeZipper v lz ps) = do
  (RoseTree v' bs') <- lzFoco lz
  Just $ RoseTreeZipper v' (toListZipper bs') ((v, lz) : ps)

roseCima :: RoseTreeZipper a -> Maybe (RoseTreeZipper a)
roseCima (RoseTreeZipper _ _ []) = Nothing
roseCima (RoseTreeZipper _ _ ((v',lz'):ps)) =
  Just $ RoseTreeZipper v' lz' ps

-- Exercício
-- A combinação das implementações das funções trocaFoco e
-- roseCima, tal como está, descarta eventuais alterações feitas no
-- valor em foco. Corrija a implementação.

-- -------------------------------------------------------
-- ------------------------------------------------------
-- Exercícios para casa
-- ------------------------------------------------------

-- ------------------------------------------------------
--
-- Exercício 01
--
-- ------------------------------------------------------
--
-- A função membro (definida para árvores binárias de busca) faz, no
-- pior caso, aproximadamente 2d comparações, onde d é a profundidade
-- da árvore. Altere a função membro (renomeada abaixo como membroOpt)
-- para que ela não faça mais do que d + 1 comparações no pior
-- caso. Dica: a cada passo mantenha guardado o elemento candidato que
-- pode ser igual ao elemento buscado (por exemplo, o último elemento
-- para o qual a comparação < devolveu falso ou <= devolveu
-- verdadeiro) e verifique a igualdade apenas quando alcançar o final
-- da árvore.

membroOpt :: Ord a => a -> Arv a -> Bool
membroOpt _ ArvVazia = False
membroOpt x (No e v d)
    | x < v     = membroOpt x e
    | x > v     = membroOpt x d
    | otherwise = True


-- ------------------------------------------------------
--
-- Exercício 02
--
-- ------------------------------------------------------
--
-- A inserção em uma árvore binária copia completamente o caminho de
-- busca. Ainda que os nós substitutos sejam idênticos aos originais,
-- isto acaba gerando lixo que precisará ser limpo posteriormente pelo
-- coletor de lixo caso o valor a ser inserido já estiver presente na
-- árvore. Altere a função insere (renomeada abaixo como insereOpt)
-- para evitar a cópia. Dica: insere troca os nós conforme desce na
-- árvore. Faça com que a troca seja condicionada à efetiva inserção
-- do elemento na árvore e que, portanto, efetue a troca apenas na
-- volta da chamada recursiva.

insereOpt :: Ord a => a -> Arv a -> Arv a
insereOpt x ArvVazia = No ArvVazia x ArvVazia
insereOpt x t@(No e v d)
   | x < v     = No (insereOpt x e) v d
   | x > v     = No e v (insereOpt x d)
   | otherwise = t


-- ------------------------------------------------------
--
-- Exercício 03
--
-- ------------------------------------------------------
--
-- Combine O resultado dos exercícios 1 e 2 para fazer uma versão de
-- insere (chamada abaixo de superInsere) que não faça cópias
-- desnecessárias e que não faça mais do que d + 1 comparações.

superInsere :: Ord a => a -> Arv a -> Arv a
superInsere = undefined


-- ------------------------------------------------------
--
-- Exercício 04
--
-- ------------------------------------------------------
--
-- Neste exercício considere apenas o caso de árvores binárias
-- perfeitas. Uma árvore binária é considerada perfeita se além de
-- completa todas as folhas se encontram no mesmo nível. Utilizando
-- zippers, reimplemente a percurso em largura imprimindo a cada nó
-- visitado o seu valor. Dica: reveja a implementação das funções
-- larguraNivel e larguraQ.

larguraZipper :: Show a => Arv a -> IO ()
larguraZipper = undefined

-- ------------------------------------------------------
--
-- Exercício 05
--
-- ------------------------------------------------------
--
-- A função apresentada como resposta no exercício 04 funciona para
-- árvores não perfeitas?
