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
    input_line <- getLine -- throw the first temperature (unknown)

    -- game loop
    hPutStrLn stderr "Game loop starts"
    hPutStrLn stderr ("width: " ++ show width)
    hPutStrLn stderr ("height: " ++ show height)
    horizontalSearch (width-1, height-1) (x0,y0) (0, width-1)

horizontalSearch (xMax, yMax) (x, y) interval@(h1,h2) =
    if h1 == h2 then do
        hPutStrLn stderr ("xBomb: " ++ show h1)
        verticalSearch h1 yMax (x, y) (0, yMax)
    else do
        hPutStrLn stderr ("horizontal interval: " ++ show interval)
        let xNextIdeal = nextIdealPos x interval
        if xNextIdeal < 0 then
            searchLeft (xMax, yMax) (x, y) interval
        else if xNextIdeal > xMax then
            searchRight (xMax, yMax) (x, y) interval
        else do
            putStrLn (show xNextIdeal ++ " " ++ show y)
            input_line <- getLine
            let temperature = readTemperature input_line
            case temperature of
                Warmer -> horizontalSearch (xMax, yMax) (xNextIdeal, y) (halfSide x xNextIdeal interval)
                Colder -> horizontalSearch (xMax, yMax) (xNextIdeal, y) (halfSide xNextIdeal x interval)
                Same -> do
                    hPutStrLn stderr ("xBomb: " ++ show (middle interval))
                    verticalSearch (middle interval) yMax (xNextIdeal, y) (0, yMax)

searchLeft (xMax, yMax) (x, y) (l,r) =
    if l == r then do
        hPutStrLn stderr ("xBomb: " ++ show l)
        verticalSearch l yMax (x, y) (0, yMax)
    else do
        let nextLeft = min r (middle (l, x))
        putStrLn (show nextLeft ++ " " ++ show y)
        input_line <- getLine
        let temperature = readTemperature input_line
        case temperature of
            Warmer -> searchLeft (xMax, yMax) (nextLeft, y) (halfSide x nextLeft (l,r))
            Colder -> horizontalSearch (xMax, yMax) (nextLeft, y) (halfSide nextLeft x (l,r))
            Same -> do
                let xBomb = middle (x, nextLeft)
                hPutStrLn stderr ("xBomb: " ++ show xBomb)
                verticalSearch xBomb yMax (nextLeft, y) (0, yMax)

searchRight (xMax, yMax) (x, y) (l,r) =
    if l == r then do
        hPutStrLn stderr ("xBomb: " ++ show l)
        verticalSearch l yMax (x, y) (0, yMax)
    else do
        let nextRight = max l (middle (r, x + 1)) -- can be slightly improved
        putStrLn (show nextRight ++ " " ++ show y)
        input_line <- getLine
        let temperature = readTemperature input_line
        case temperature of
            Warmer -> searchRight (xMax, yMax) (nextRight, y) (halfSide x nextRight (l,r))
            Colder -> horizontalSearch (xMax, yMax) (nextRight, y) (halfSide nextRight x (l,r))
            Same -> do
                let xBomb = middle (x, nextRight)
                hPutStrLn stderr ("xBomb: " ++ show xBomb )
                verticalSearch xBomb yMax (nextRight, y) (0, yMax)


verticalSearch xBomb yMax (x, y) interval@(v1,v2) =
    if v1 == v2 then do
        hPutStrLn stderr ("yBomb: " ++ show v1)
        putStrLn (show xBomb ++ " " ++ show v1)
    else do
        hPutStrLn stderr ("vertical interval: " ++ show interval)
        let yNextIdeal = nextIdealPos y interval
        if yNextIdeal < 0 then
            searchTop xBomb yMax (x, y) interval
        else if yNextIdeal > yMax then
            searchBottom xBomb yMax (x, y) interval
        else do
            putStrLn (show x ++ " " ++ show yNextIdeal)
            input_line <- getLine
            let temperature = readTemperature input_line
            case temperature of
                Warmer -> verticalSearch xBomb yMax (x, yNextIdeal) (halfSide y yNextIdeal interval)
                Colder -> verticalSearch xBomb yMax (x, yNextIdeal) (halfSide yNextIdeal y interval)
                Same -> do
                    hPutStrLn stderr ("yBomb: " ++ show (middle interval))
                    putStrLn (show xBomb ++ " " ++ show (middle interval))

searchTop xBomb yMax (x, y) (t, b) =
    if t == b then do
        hPutStrLn stderr ("yBomb: " ++ show t)
        putStrLn (show xBomb ++ " " ++ show t)
    else do
        let nextTop = min b (middle (t, y))
        putStrLn (show x ++ " " ++ show nextTop)
        input_line <- getLine
        let temperature = readTemperature input_line
        case temperature of
            Warmer -> searchTop xBomb yMax (x, nextTop) (halfSide y nextTop (t,b))
            Colder -> verticalSearch xBomb yMax (x, nextTop) (halfSide nextTop y (t,b))
            Same -> do
                let yBomb = middle (y, nextTop)
                hPutStrLn stderr ("yBomb: " ++ show yBomb)
                putStrLn (show xBomb ++ " " ++ show yBomb)

searchBottom xBomb yMax (x, y) (t, b) =
    if t == b then do
        hPutStrLn stderr ("yBomb: " ++ show t)
        putStrLn (show xBomb ++ " " ++ show t)
    else do
        let nextBottom = max t (middle (b, y + 1))
        putStrLn (show x ++ " " ++ show nextBottom)
        input_line <- getLine
        let temperature = readTemperature input_line
        case temperature of
            Warmer -> searchBottom xBomb yMax (x, nextBottom) (halfSide y nextBottom (t,b))
            Colder -> verticalSearch xBomb yMax (x, nextBottom) (halfSide nextBottom y (t,b))
            Same -> do
                let yBomb = middle (y, nextBottom)
                hPutStrLn stderr ("yBomb: " ++ show yBomb)
                putStrLn (show xBomb ++ " " ++ show yBomb)

halfSide :: Int -> Int -> Interval -> Interval
halfSide far near ( a, b ) =
    let
        intervalSplit = middle ( far, near )
    in
    if near <= intervalSplit then
        ( a, min b (quot (far + near - 1) 2) )
    else
        ( max a (intervalSplit + 1), b )

type Interval = ( Int, Int )
data Temperature = Same | Warmer | Colder deriving Show

readTemperature :: String -> Temperature
readTemperature "SAME" = Same
readTemperature "WARMER" = Warmer
readTemperature "COLDER" = Colder

middle :: Interval -> Int
middle ( a, b ) = quot (a + b) 2

nextIdealPos :: Int -> Interval -> Int
nextIdealPos current (a, b) =
    a + b - current
