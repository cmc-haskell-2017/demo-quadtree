module QuadTree where

import Graphics.Gloss.Interface.Pure.Game

-- | Запустить визуализацию дерева квадрантов.
runQuadTreeDemo :: IO ()
runQuadTreeDemo = do
  play display bgColor fps initDemo drawDemo handleDemo updateDemo
  where
    display = InWindow "Дерево квадрантов" (screenWidth, screenHeight) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

-- | Состояние визуализации.
data Demo = Demo

-- | Прямоугольная область, заданная левой нижней
-- и правым верхней вершинами.
type Rect = (Point, Point)

-- | Вектор длины четыре.
data Quad a = Quad a a a a

-- | Квадрант.
data Quadrant a
  = Empty                     -- ^ Пустой квадрант.
  | Bucket a Point            -- ^ Квадрант, содержащий один объект.
  | Split (Quad (QuadTree a)) -- ^ Квадрант, разбитый на четыре подквадранта.

-- | Дерево квадрантов.
data QuadTree a = QuadTree Rect (Quadrant a)

-- | Вставить новый объект в дерево квадрантов.
insert :: a -> Point -> QuadTree a -> QuadTree a
insert x p (QuadTree rect Empty) = QuadTree rect (Bucket x p)
insert x p (QuadTree rect (Bucket y q))
  = insert x p (insert y q (QuadTree rect (Split (splitRect rect))))
insert x p (QuadTree rect (Split (Quad lb lt rt rb)))
  = QuadTree rect (Split quad)
  where
    quad
      | leftBottom = Quad (f lb) lt rt rb
      | leftTop    = Quad lb (f lt) rt rb
      | rightTop   = Quad lb lt (f rt) rb
      | otherwise  = Quad lb lt rt rb

    f = insert x p
    leftBottom = left  p rect && bottom p rect
    leftTop    = left  p rect && top p rect
    rightTop   = right p rect && top p rect

-- | Разбить область на четыре пустых квадранта.
splitRect :: Rect -> Quad (QuadTree a)
splitRect ((l, b), (r, t)) = Quad
  (QuadTree ((l, b), (x, y)) Empty)   -- левый нижний
  (QuadTree ((l, y), (x, t)) Empty)   -- левый верхний
  (QuadTree ((x, y), (r, t)) Empty)   -- правый верхний
  (QuadTree ((x, b), (r, y)) Empty)   -- правый нижний
  where
    x = (r + l) / 2
    y = (b + t) / 2

-- | Находится ли точка в левой части прямоугольной области?
left :: Point -> Rect -> Bool
left (x, _) ((l, _), (r, _)) = x < (l + r) / 2

-- | Находится ли точка в правой части прямоугольной области?
right :: Point -> Rect -> Bool
right p = not . left p

-- | Находится ли точка в нижней части прямоугольной области?
bottom :: Point -> Rect -> Bool
bottom (_, y) ((_, b), (_, t)) = y < (b + t) / 2

-- | Находится ли точка в верхней части прямоугольной области?
top :: Point -> Rect -> Bool
top p = not . bottom p

-- | Инициализировать демо.
initDemo :: Demo
initDemo = Demo

-- | Отобразить демо.
drawDemo :: Demo -> Picture
drawDemo _ = blank

-- | Обработка событий.
handleDemo :: Event -> Demo -> Demo
handleDemo _ = id

-- | Обновление визуализации.
-- Все изменения в этой демонстрационной программе происходят
-- по событиям, поэтому эта функция ничего не делает.
updateDemo :: Float -> Demo -> Demo
updateDemo _ = id

-- | Ширина экрана в пикселях.
screenWidth :: Int
screenWidth = 500

-- | Высота экрана в пикселях.
screenHeight :: Int
screenHeight = 500
