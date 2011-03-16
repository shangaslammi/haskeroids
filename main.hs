
import Haskeroids.Initialize
import Graphics.UI.GLUT (mainLoop)

main = do
    initializeWindow
    initializeOpenGL
    initializeCallbacks
    
    mainLoop
