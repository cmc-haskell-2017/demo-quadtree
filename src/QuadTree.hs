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
