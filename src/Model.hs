{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen,
        -- Event queue
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
        -- TODO: add more fields here!
        player           :: GameObject,
        enemies          :: [GameObject]
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot

data Location = Location {
    locationX :: Float,
    locationY :: Float
}

data Velocity = Velocity {
    velocityX :: Float,
    velocityY :: Float
}

data GameObject = GameObject { 
    location :: Location,
    velocity :: Velocity,
    rotation :: Float
}

createPlayer :: GameObject
createPlayer = GameObject ( Location 0.25 0.5 ) ( Velocity 0.0 (-0.002) ) 0

initial :: Int -> World
initial seed = 
    World 
        (mkStdGen seed) 
        NoRotation 
        NoMovement 
        DontShoot 
        createPlayer
        [
            GameObject (Location 0.25 0.75) (Velocity 0.0 0.0) 0,
            GameObject (Location 0.75 0.25) (Velocity 0.0 0.0) 0,
            GameObject (Location 0.50 0.75) (Velocity 0.0 0.0) 0
        ]