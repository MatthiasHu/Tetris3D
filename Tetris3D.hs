-- still missing:  show holes


import Graphics.UI.GLUT
import Data.IORef
import Data.Array
import Data.Maybe (isJust, isNothing)
import System.Random
import Control.Concurrent (threadDelay)



main:: IO ()
main = do
  (progName, args) <- getArgsAndInitialize
  putStrLn "Tetris3D"
  putStrLn "move: up/down/left/right  rotate tetromino: W/S  rotate view: A/D  down: space"
  putStrLn "Have fun!"
  rng <- newStdGen
  stateRef <- newIORef (state0 rng)
  initialDisplayMode $= [WithDepthBuffer]
  initialWindowSize $= Size 512 512
  window <- createWindow "Tetris 3D"
  matrixMode $= Projection
  frustum (-0.05) (0.05) (-0.05) (0.05) (0.1) (100.0)
  matrixMode $= Modelview 0
  depthFunc $= Just Lequal
  clearColor $= Color4 0.0 0.0 0.0 1.0
  -- setup lighting
  lighting $= Enabled
  lightModelLocalViewer $= Enabled
  materialEmission FrontAndBack $= Color4 0.0 0.0 0.0 1.0
  materialSpecular FrontAndBack $= Color4 1.0 1.0 1.0 1.0
  colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
  light (Light 0) $= Enabled
  light (Light 1) $= Enabled
  ambient (Light 0) $= Color4 0.1 0.1 0.1 1
  diffuse (Light 0) $= Color4 0.9 1.0 0.8 1
  ambient (Light 1) $= Color4 0.1 0.1 0.1 1
  diffuse (Light 1) $= Color4 1.0 0.9 1.0 1
  specular (Light 0) $= Color4 0.2 0.2 0.2 1
  specular (Light 1) $= Color4 0.2 0.2 0.2 1
  attenuation (Light 0) $= (0 , 0.3, 0.05)
  attenuation (Light 1) $= (0 , 0.3, 0.05)
  -- set callbacks
  reshapeCallback $= Just reshape
  displayCallback $= display stateRef
  keyboardMouseCallback $= Just (keyboardMouse stateRef)
  addTimerCallback initialTimeout (timer stateRef)
  mainLoop



reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)


display:: (IORef State) -> DisplayCallback
display stateRef = do
  state <- get stateRef
  loadIdentity
  clear [ColorBuffer, DepthBuffer]
  rotate (-20) $ Vector3 1 0 (0::GLfloat)  -- Rotate down and ...
  translate $ Vector3 0 3.0 (-6::GLfloat)  -- ... move to the falling tetromino ...
  rotate (fromIntegral (90*(viewRotation state))) $ Vector3 0 0 (1::GLfloat)  -- ... rotate the view as the player likes it ...
  applyAnimationViewRotation $ animationState state
  translate $ fromIntegralV (negateV (pos state))  -- ... and then move to the origin of the scene.
  position (Light 0) $= Vertex4 0 0 (fromIntegral zMax) 1  -- Place two lights ...
  position (Light 1) $= Vertex4 (fromIntegral (xMax+1)) (fromIntegral (yMax+1)) (fromIntegral (div zMax 2)) 1
  preservingMatrix $ do  -- ... render the falling tetromino ...
    translate $ fromIntegralV $ pos state
    applyAnimationTetrominoRotation (viewRotation state) (animationState state)
    color $ Color3 0.5 1.0 (0.8::GLfloat)
    renderCubes (tetromino state)
  color $ Color3 1.0 1.0 (1.0::GLfloat)  -- ... render a ground plane ...
  normal (Normal3 0 0 (1::GLfloat))
  renderPrimitive Quads $
    mapM_ (\(x, y) -> vertex (Vertex3 x y (0.5::GLfloat)))
      [(0.4, 0.4), (0.4, (fromIntegral yMax)+0.6), ((fromIntegral xMax)+0.6, (fromIntegral yMax)+0.6), ((fromIntegral xMax)+0.6, 0.4)]
  color $ Color3 0.0 0.7 (1.0::GLfloat)  -- ... and all the static cubes.
  renderCubes (allCubes state)
  flush


applyAnimationViewRotation:: (Maybe AnimationState) -> IO ()
applyAnimationViewRotation (Just (ViewRotation dir, n)) =
  rotate (((fromIntegral(dir*(n-animationFrames)*90))/(fromIntegral animationFrames))::GLfloat) $ Vector3 0 0 (1::GLfloat)
applyAnimationViewRotation _ = return ()

applyAnimationTetrominoRotation:: Int -> (Maybe AnimationState) -> IO ()
applyAnimationTetrominoRotation viewRotation (Just (TetrominoRotation dir, n)) =
  rotate (((fromIntegral(dir*(n-animationFrames)*90))/(fromIntegral animationFrames))::GLfloat) (fromIntegralV axis)
  where axis = applyViewRotation viewRotation (v3 1 0 0)
applyAnimationTetrominoRotation _ _ = return ()


renderCubes:: [(V3, CubeColor)] -> IO ()
renderCubes [] = return ()
renderCubes ((v, cc):tail) = preservingMatrix (translate (fromIntegralV v)
                             >> color (cubeColor cc) >> renderCubeWithChamfer 0.1)
                             >> renderCubes tail

renderCubeWithChamfer:: GLfloat -> IO ()
renderCubeWithChamfer p =
  mapM_ (\rotation -> preservingMatrix (rotation >> renderCubeFaceWithChamfer p))
  ([rotate angle (Vector3 1 0 (0::GLfloat)) | angle <- [0, 90, 180, 270]]
  ++ [rotate angle (Vector3 0 1 (0::GLfloat)) | angle <- [90, 270]])

renderCubeFaceWithChamfer:: GLfloat -> IO ()
renderCubeFaceWithChamfer p = do
  normal (Normal3 0 0 (1::GLfloat))
  renderPrimitive Quads $ mapM_ (\(x, y) -> vertex (Vertex3 x y (c::GLfloat))) [(-a, -a), (-a, a), (a, a), (a, -a)]
  (\chamferPart -> chamferPart >> chamferPart >> chamferPart >> chamferPart) $ do
  rotate 90 (Vector3 0 0 (1::GLfloat))
  normal (Normal3 1.414 0 (1.414::GLfloat))
  renderPrimitive Quads $ mapM_ (\(x, y, z) -> vertex (Vertex3 x y (z::GLfloat))) [(b, b, b), (b, -b, b), (a, -a, c), (a, a, c)]
  where a = 0.5*(1-p)
        b = 0.5*(1-0.5*p)
        c = 0.5



keyboardMouse:: (IORef State) -> KeyboardMouseCallback
keyboardMouse stateRef (Char ' ') Down _ _ = changeState stateRef tickDown
keyboardMouse stateRef (SpecialKey KeyLeft) Down _ _ = changeState stateRef (tryMoveConsideringViewRotation (v3 (-1) 0 0))
keyboardMouse stateRef (SpecialKey KeyRight) Down _ _ = changeState stateRef (tryMoveConsideringViewRotation (v3 1 0 0))
keyboardMouse stateRef (SpecialKey KeyDown) Down _ _ = changeState stateRef (tryMoveConsideringViewRotation (v3 0 (-1) 0))
keyboardMouse stateRef (SpecialKey KeyUp) Down _ _ = changeState stateRef (tryMoveConsideringViewRotation (v3 0 1 0))
keyboardMouse stateRef (Char 'a') Down _ _ = changeState stateRef (rotateView (-1))
keyboardMouse stateRef (Char 'd') Down _ _ = changeState stateRef (rotateView 1)
keyboardMouse stateRef (Char 'w') Down _ _ = changeState stateRef (tryRotateTetromino (-1))
keyboardMouse stateRef (Char 's') Down _ _ = changeState stateRef (tryRotateTetromino 1)
keyboardMouse _ _ _ _ _ = return ()


timer:: (IORef State) -> TimerCallback
timer stateRef = do
  oldState <- get stateRef
  addTimerCallback (div (initialTimeout*5) ((score oldState)+5)) (timer stateRef)
  changeState stateRef tickDown


changeState:: (IORef State) -> (State -> State) -> IO ()
changeState stateRef f = do
  oldState <- get stateRef
  let newState = f oldState
  if score oldState /= score newState then putStrLn ("SCORE: "++(show (score newState))) else return ()
  if isJust (animationState newState) then idleCallback $= Just (idleAnimation stateRef) else return ()
  stateRef $= newState
  postRedisplay Nothing
  

initialTimeout:: Timeout
initialTimeout = 2000


idleAnimation:: IORef State -> IdleCallback
idleAnimation stateRef = do
  threadDelay 20
  oldState <- get stateRef
  let newState = oldState {animationState = proceedAnimation (animationState oldState)}
  if isNothing (animationState newState) then idleCallback $= Nothing else return()
  stateRef $= newState
  postRedisplay Nothing




-- from above:
-- y
-- 4 O O O O
-- 3 O O O O
-- 2 O O O O
-- 1 O O O O
--   1 2 3 4 x

xMax = 4
yMax = 4
zMax = 12


data State = State {pos :: V3  -- the falling tetrominos Position
                   ,tetromino :: Tetromino
                   ,cubes :: Array V3 (Maybe CubeColor)
                   ,score :: Int
				   ,viewRotation :: Int
                   ,rng :: StdGen
                   ,animationState :: Maybe AnimationState
                   }

pos0:: V3
pos0 = v3 (div xMax 2) (div yMax 2) (zMax-1)

state0:: StdGen -> State
state0 g = State {pos = pos0, tetromino = tetrominos!!(mod randomNumber (length tetrominos))
                 ,cubes = array ((v3 1 1 1), (v3 xMax yMax zMax)) [((v3 x y z), Nothing) | x<-[1..xMax], y<-[1..yMax], z<-[1..zMax]]
                 ,score = 0
			     ,viewRotation = 0
                 ,rng = g'
                 ,animationState = Nothing}
                 where (randomNumber, g') = next g

cubeAt:: State -> V3 -> Bool
cubeAt s (Vector3 x y z)  | x<=0 || x>xMax  = True
                          | y<=0 || y>yMax  = True
                          | z<=0  = True
						  | z>zMax  = False
                          | otherwise  = isJust ((cubes s)!(v3 x y z))

allCubes::State -> [(V3, CubeColor)]
allCubes s = map (\v -> (v,(\(Just cc) -> cc) ((cubes s)!v))) $ filter (cubeAt s) [(v3 x y z) | x<-[1..xMax], y<-[1..yMax], z<-[1..zMax]]


type V3 = Vector3 Int
v3 :: Int -> Int -> Int -> V3
v3 = Vector3

fromIntegralV:: V3 -> Vector3 GLfloat
fromIntegralV (Vector3 x y z) = Vector3 (fromIntegral x) (fromIntegral y) (fromIntegral z)


addV:: V3 -> V3 -> V3
addV (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = (Vector3 (x1+x2) (y1+y2) (z1+z2))

negateV:: V3 -> V3
negateV (Vector3 x y z) = (Vector3 (-x) (-y) (-z))

getXV:: V3 -> Int
getXV (Vector3 x _ _) = x
getYV:: V3 -> Int
getYV (Vector3 _ y _) = y
getZV:: V3 -> Int
getZV (Vector3 _ _ z) = z

type CubeColor = Int

cubeColor:: CubeColor -> Color3 GLfloat
cubeColor 0 = Color3 0.2 0.2 0.2
cubeColor 1 = Color3 0.7 0.2 0.1
cubeColor 2 = Color3 0.9 0.8 0.1
cubeColor 3 = Color3 0.2 0.3 0.8
cubeColor 4 = Color3 0.7 0.1 0.6
cubeColor 5 = Color3 0.3 0.8 0.2
cubeColor 6 = Color3 0.9 0.8 0.7
cubeColor 7 = Color3 0.6 0.9 0.9

brighter:: Color3 GLfloat -> Color3 GLfloat
brighter (Color3 r g b) = Color3 (min 1 (r*1.5)) (min 1 (g*1.5)) (min 1 (b*1.5))



type Tetromino = [(V3, CubeColor)]

oneCube:: Tetromino
oneCube = [(v3 0 0 0, 0)]

testingColors:: Tetromino
testingColors = zip [v3 (-1) 0 0, v3 0 (-1) 0, v3 0 0 0, v3 (-1) 1 0, v3 1 (-1) 0, v3 0 1 0, v3 1 0 0] [1..]

tetrominos:: [Tetromino]
tetrominos = [zip [(v3 (-1) 0 0), (v3 0 0 0), (v3 1 0 0), (v3 1 1 0)] (repeat 1)  -- L
             ,zip [(v3 (-1) 0 0), (v3 0 0 0), (v3 1 0 0), (v3 0 1 0)] (repeat 2)  -- T
             ,zip [(v3 (-1) 0 0), (v3 0 0 0), (v3 0 1 0), (v3 1 1 0)] (repeat 3)  -- S
             ,zip [(v3 0 0 0), (v3 1 0 0), (v3 0 1 0), (v3 1 1 0)] (repeat 4)  -- O
             ,zip [(v3 0 0 0), (v3 1 0 0), (v3 0 1 0), (v3 0 0 1)] (repeat 5)  -- Y
             ,zip [(v3 (-1) 0 0), (v3 0 0 0), (v3 0 1 0), (v3 0 1 1)] (repeat 6)
             ,zip [(v3 (-1) 0 0), (v3 0 0 0), (v3 0 0 1), (v3 0 1 1)] (repeat 7)]




---      ACTION         ---
--- modifying the state ---


tickDown:: State -> State
tickDown oldState = if collision (oneDown oldState) then newTetromino oldState else oneDown oldState

oneDown:: State -> State
oneDown oldState = oldState {pos = v3 (getXV $ pos oldState) (getYV $ pos oldState) ((getZV $ pos oldState)-1)}

tryMove:: V3 -> State -> State
tryMove v oldState = if collision (move v oldState) then oldState else move v oldState

move:: V3 -> State -> State
move v oldState = oldState {pos = addV v (pos oldState)}

newTetromino:: State -> State
newTetromino oldState = testGameOver $ eraseCompletedLayersFrom 1 $
  (insertTetrominoCubes oldState) {pos=pos0, tetromino=tetrominos!!(mod randomNumber (length tetrominos)), rng = g'}
  where (randomNumber, g') = next (rng oldState)

testGameOver:: State -> State
testGameOver state = if collision state then state0 (rng state) else state

insertTetrominoCubes:: State -> State
insertTetrominoCubes oldState = oldState {cubes = (cubes oldState)//(map (\(v, cc) -> (addV (pos oldState) v, Just cc)) (tetromino oldState))}

eraseCompletedLayersFrom:: Int -> State -> State
eraseCompletedLayersFrom layer state =
  if layer > zMax then state else
    if layerCompleted layer state then eraseCompletedLayersFrom layer (eraseLayer layer (state {score = (score state)+1})) else
      eraseCompletedLayersFrom (layer+1) state

layerCompleted:: Int -> State -> Bool
layerCompleted layer state = all isJust [(cubes state)!(v3 x y layer) | x <- [1..xMax], y <- [1..yMax]]

eraseLayer:: Int -> State -> State
eraseLayer layer state =
 if layer == zMax then state {cubes = (cubes state)//[(v3 x y zMax, Nothing) | x <- [1..xMax], y <- [1..yMax]]}
   else eraseLayer (layer+1) (state {cubes = (cubes state)//[(v3 x y layer, (cubes state)!(v3 x y (layer+1))) | x <- [1..xMax], y <- [1..yMax]]})

collision:: State -> Bool
collision state = any (cubeAt state) $ map (\v -> addV v (pos state)) $ map fst (tetromino state)


--- rotating everything ---


rotateView:: Int -> State -> State
rotateView n oldState = oldState {viewRotation = (viewRotation oldState)+n, animationState = Just (ViewRotation n, 0)}

tryMoveConsideringViewRotation:: V3 -> State -> State
tryMoveConsideringViewRotation v state = tryMove (applyViewRotation (viewRotation state) v) state

tryRotateTetromino:: Int -> State -> State
tryRotateTetromino n state = if collision (rotateTetromino n state) then state else rotateTetromino n state

rotateTetromino:: Int -> State -> State
rotateTetromino n oldState = oldState {tetromino = map (\(v, cc) -> (transform v, cc)) (tetromino oldState)
  ,animationState = Just (TetrominoRotation n, 0)}
  where transform = (applyViewRotation (viewRotation oldState)) . (applyTetrominoRotation n) . (applyViewRotation (-(viewRotation oldState)))


applyViewRotation:: Int -> V3 -> V3
applyViewRotation nRaw (Vector3 x y z) = Vector3 (x*self+y*other) (y*self-x*other) z
  where self = (mod (n+1) 2)*((div n 2)*(-2)+1)  -- 1 0 -1 0 
        other = (mod n 2)*((div (n-1) 2)*(-2)+1)  -- 0 1 0 -1
        n = mod nRaw 4

applyTetrominoRotation:: Int -> V3 -> V3
applyTetrominoRotation nRaw (Vector3 x y z) = Vector3 x (y*self-z*other) (z*self+y*other)
 where self = (mod (n+1) 2)*((div n 2)*(-2)+1)  -- 1 0 -1 0 
       other = (mod n 2)*((div (n-1) 2)*(-2)+1)  -- 0 1 0 -1
       n = mod nRaw 4


--- animations ---

animationFrames = 10


type AnimationState = (AnimationType, Int)

data AnimationType =
  ViewRotation Int -- +/-1, the direction
 |TetrominoRotation Int

proceedAnimation:: Maybe AnimationState -> Maybe AnimationState
proceedAnimation Nothing = Nothing
proceedAnimation (Just (aType, n)) = if n>=animationFrames then Nothing else Just (aType, n+1)