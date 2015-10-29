{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))

import Data.List

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import System.Random

import Model

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) 
    = handlePlayer
    $ moveEnemies world

moveEnemies :: World -> World
moveEnemies world@(World{..}) = world { enemies = moveGameObject <$> enemies }

handlePlayer :: World -> World
handlePlayer world 
    = handlePlayerThrust
    $ handlePlayerRotation world




handlePlayerRotation :: World -> World
handlePlayerRotation world @ ( World {rotateAction} ) =
    case rotateAction of RotateLeft  -> rotatePlayer (-10) world
                         RotateRight -> rotatePlayer   10  world
                         _           -> world

rotatePlayer :: Float -> World -> World
rotatePlayer rotation world @ ( World {..} ) = 
    world { player = rotateGameObject player rotation }

rotateGameObject :: GameObject -> Float -> GameObject
rotateGameObject object @ ( GameObject {..} ) degrees = 
    object { 
        rotation = rotation + degrees,
        velocity = rotateVelocity velocity degrees
    }

rotateVelocity :: Velocity -> Float -> Velocity
rotateVelocity (Velocity {..}) degrees =
    let radians = degreesToRadians degrees
        x = velocityX * cos radians - velocityY * sin radians
        y = velocityX * sin radians + velocityY * cos radians
    in Velocity x y

degreesToRadians :: Float -> Float
degreesToRadians degrees = degrees * pi / 180


handlePlayerThrust :: World -> World
handlePlayerThrust world @ ( World {movementAction} ) =
    case movementAction of Thrust -> thrustPlayer world
                           _      -> world

thrustPlayer :: World -> World
thrustPlayer world @ ( World {..} ) = world { player = moveGameObject player }

moveGameObject :: GameObject -> GameObject
moveGameObject object @ ( GameObject {..} ) = 
    let newX = locationX location + velocityX velocity
        newY = locationY location + velocityY velocity
    in object { location = Location newX newY }
