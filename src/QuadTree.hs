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
  { quadTree :: QuadTree Point }

-- | Прямоугольная область, заданная левой нижней
-- и правым верхней вершинами.
type Rect = (Point, Point)

-- | Вектор длины четыре.
data Quad a = Quad a a a a
  deriving (Show)

-- | Квадрант.
data Quadrant a
  = Empty                     -- ^ Пустой квадрант.
  | Bucket a Point            -- ^ Квадрант, содержащий один объект.
  | Split (Quad (QuadTree a)) -- ^ Квадрант, разбитый на четыре подквадранта.
  deriving (Show)

-- | Дерево квадрантов.
data QuadTree a = QuadTree Rect (Quadrant a)
  deriving (Show)

-- | Вставить новый объект в дерево квадрантов.
insert :: a -> Point -> QuadTree a -> QuadTree a
insert x p (QuadTree rect Empty) = QuadTree rect (Bucket x p)
insert x p (QuadTree rect (Bucket y q))
  | p == q    = QuadTree rect (Bucket x p)
  | otherwise = insert x p (insert y q (QuadTree rect (Split (splitRect rect))))
insert x p (QuadTree rect (Split (Quad lb lt rt rb)))
  = QuadTree rect (Split quad)
  where
    quad
      | leftBottom = Quad (f lb) lt rt rb
      | leftTop    = Quad lb (f lt) rt rb
      | rightTop   = Quad lb lt (f rt) rb
      | otherwise  = Quad lb lt rt (f rb)

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

-- | Получить список всех объектов в дереве квадрантов.
toList :: QuadTree a -> [a]
toList (QuadTree _ Empty) = []
toList (QuadTree _ (Bucket x _)) = [x]
toList (QuadTree _ (Split (Quad a b c d)))
  = toList a ++ toList b ++ toList c ++ toList d

-- | Инициализировать демо.
initDemo :: Demo
initDemo = Demo
  { quadTree = initQuadTree }

-- | Инициализировать начальное дерево квадрантов.
initQuadTree :: QuadTree Point
initQuadTree = QuadTree ((-w, -h), (w, h)) Empty
  where
    w = fromIntegral screenWidth / 2
    h = fromIntegral screenHeight / 2

-- | Отобразить демо.
drawDemo :: Demo -> Picture
drawDemo demo = pictures 
  [ pictures (map drawPoint (toList (quadTree demo)))
  , drawQuadrants (quadTree demo) ]

-- | Отобразить разбиение пространства на квадранты.
drawQuadrants :: QuadTree a -> Picture
drawQuadrants (QuadTree rect q) = pictures
  [ drawRect rect
  , drawQuadrant q ]

-- | Отобразить границы квадранта.
drawRect :: Rect -> Picture
drawRect ((l, b), (r, t)) = color orange (line
  [(l, b), (l, t), (r, t), (r, b), (l, b)])

-- | Отобразить разбиение квадранта.
drawQuadrant :: Quadrant a -> Picture
drawQuadrant Empty = blank
drawQuadrant (Bucket _ _) = blank
drawQuadrant (Split (Quad a b c d)) = pictures (map drawQuadrants [a, b, c, d])

-- | Отобразить одну точку.
drawPoint :: Point -> Picture
drawPoint (x, y) = color white (translate x y (circle 3))

-- | Обработка событий.
handleDemo :: Event -> Demo -> Demo
handleDemo (EventKey (MouseButton LeftButton) _ _ mouse) = addPoint mouse
handleDemo _ = id

-- | Добавить объект в заданной точке.
addPoint :: Point -> Demo -> Demo
addPoint p demo = demo { quadTree = insert p p (quadTree demo) }

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
