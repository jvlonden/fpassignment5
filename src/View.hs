{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import Model

-- | Drawing

data Resolution = Resolution {
    horizontal :: Float,
    vertical   :: Float
}

data Coordinates = Coordinates {
    xCoordinate :: Float,
    yCoordinate :: Float
}

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution ( World{..} ) = 
    pictures [
        drawGameObjects (drawEnemy resolution) enemies,
        drawPlayer resolution player
    ]
    where resolution = Resolution horizontalResolution verticalResolution

drawGameObjects :: (GameObject -> Picture) -> [GameObject] -> Picture
drawGameObjects drawFunction enemies = pictures (drawFunction <$> enemies)

drawEnemy :: Resolution -> GameObject -> Picture
drawEnemy resolution enemy 
    = positionGameObject resolution enemy
    $ color red 
    $ circle 20.0

drawPlayer :: Resolution -> GameObject -> Picture
drawPlayer resolution player 
    = positionGameObject resolution player
    $ rotateGameObject player
    $ color green 
    $ line [
        (-10, -10),
        ( 10, -10),
        (  0,  10),
        (-10, -10)
    ]

rotateGameObject :: GameObject -> Picture -> Picture
rotateGameObject (GameObject {rotation}) = rotate rotation

positionGameObject :: Resolution -> GameObject -> Picture -> Picture
positionGameObject (Resolution {..}) (GameObject {..}) =
    translate 
        (horizontal * xCoordinate) 
        (vertical   * yCoordinate)
        where (Coordinates {..}) = locationToCoordinates location

locationToCoordinates :: Location -> Coordinates
locationToCoordinates (Location {..}) =
    let x = locationX - 0.5
        y = 0.5 - locationY
    in Coordinates x y