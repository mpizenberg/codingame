import System.IO
import Control.Monad

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- DO NOT REMOVE

    -- Auto-generated code below aims at helping you parse
    -- the standard input according to the problem statement.
    input_line <- getLine
    let input = words input_line
    let width = read (input!!0) :: Int -- width of the building.
    let height = read (input!!1) :: Int -- height of the building.
    input_line <- getLine
    let n = read input_line :: Int -- maximum number of turns before game over.
    input_line <- getLine
    let input = words input_line
    let x0 = read (input!!0) :: Int
    let y0 = read (input!!1) :: Int

    -- game loop
    let model = Model ( x0, y0 ) (Surface { hInt = (0,width-1), vInt = (0,height-1) })
    loop model

loop model = do
    -- hPutStrLn stderr "Debug messages..."
    -- bomb direction (U, UR, R, DR, D, DL, L or UL)
    input_line <- getLine
    let bombdir = dirFromString input_line
    let newSurface = reduceSurface (position model) bombdir (surface model)
    let (x, y) = pickMiddle newSurface
    let newPosition = (x, y)

    -- the location of the next window Batman should jump to.
    putStrLn (show x ++ " " ++ show y)
    loop (Model newPosition newSurface)


data Model = Model { position :: ( Int, Int ), surface :: Surface }

dirFromString :: String -> Direction
dirFromString "U" = Direction CenterH Up
dirFromString "UR" = Direction Main.Right Up
dirFromString "R" = Direction Main.Right CenterV
dirFromString "DR" = Direction Main.Right Down
dirFromString "D" = Direction CenterH Down
dirFromString "DL" = Direction Main.Left Down
dirFromString "L" = Direction Main.Left CenterV
dirFromString "UL" = Direction Main.Left Up

data Direction = Direction LeftRight UpDown deriving Show
data UpDown = Up | CenterV | Down deriving Show
data LeftRight = Left | CenterH | Right deriving Show

data Surface = Surface
    { hInt :: Interval
    , vInt :: Interval
    } deriving Show

type Interval = ( Int, Int )

middle :: Int -> Int -> Int
middle a b =
    quot (a + b) 2

pickMiddle :: Surface -> ( Int, Int )
pickMiddle Surface { hInt = (h1, h2), vInt = (v1, v2) } =
    ( middle h1 h2, middle v1 v2 )

reduceSurface :: ( Int, Int ) -> Direction -> Surface -> Surface
reduceSurface (x,y) (Direction hDir vDir) Surface { hInt = (h1,h2), vInt = (v1,v2) }=
    let
        newHorizontalInterval =
            case hDir of
                Main.Left -> (h1, x-1)
                CenterH -> (x,x)
                Main.Right -> (x+1, h2)

        newVerticalInterval =
            case vDir of
                Up -> (v1, y-1)
                CenterV -> (y, y)
                Down -> (y+1, v2)
    in
    Surface { hInt = newHorizontalInterval, vInt = newVerticalInterval }
