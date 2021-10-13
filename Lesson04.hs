{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -Wall -Wno-missing-signatures #-}

module Lesson04 where


data D4 = D1 | D2 | D3 | D4

instance Eq D4 where
    D1 == D1 = True
    D2 == D2 = True
    D3 == D3 = True
    D4 == D4 = True
    _  == _  = False

-- Инстанс класса Ord для нашего типа D4
-- instance Ord D4 where
--     compare a b
--         | a == b = EQ
--         | otherwise = case (a, b) of
--             (_, D4) -> GT
--             (D4, _) -> LT
--             (_, D3) -> GT
--             (D3, _) -> LT
--             (_, D2) -> GT
--             (D2, _) -> LT
--             _ -> error "impossible"

-- Более красивая -- плоская реализация
instance Ord D4 where
    compare a b = case (a, b) of
        _ | a == b -> EQ -- Захватывает вариант с равентсвом любых D4
        (_, D4)    -> GT
        (D4, _)    -> LT
        (_, D3)    -> GT
        (D3, _)    -> LT
        (_, D2)    -> GT
        (D2, _)    -> LT
        _          -> error "Imposible"

-- Может быть полезно в том случае если необходимо применить
-- сортировку.

-- Например, функция sort является полиморфной, но требует
-- от типов передаваемых значений:

-- ghci> import Data.List (sort)
-- ghci> :t sort
-- sort :: Ord a => [a] -> [a]

-- Т.е. функция sort требует от типа реализацию класса Ord
areEqual = D4 >= D4

-- Реализация сравнения для V4
data V4 a = V4 a a a a

-- Здесь первый "Eq a" про требование к "a" реализовывать
-- класс Eq что бы можно было сравнить такое составное значение как V4
instance Eq a => Eq (V4 a) where
    V4 a1 b1 c1 d1 == V4 a2 b2 c2 d2 =
        (a1 == a2)
        && (b1 == b2)
        && (c1 == c2)
        && (d1 == d2)

class HasSize a where
    sizeOf :: Int


instance HasSize D4 where
    sizeOf = 4

-- Требуется реализация HasSize от a и b
instance (HasSize a, HasSize b) => HasSize (a, b) where
    sizeOf = sizeOf @a * sizeOf @b


instance HasSize Bool where
    sizeOf = 2

instance HasSize () where
    sizeOf = 1

-- Количество возможных значений всех значений переданных
-- во вложенных кортежах
-- sizeOf ((D4, ()), (True, (D4, D4))) 
-- 128
-- main = print (sizeOf ((D4, ()), (True, (D4, D4))))
