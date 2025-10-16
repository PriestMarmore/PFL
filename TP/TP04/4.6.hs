-- a
type AWord = String
type Line = [AWord]
type Paragraph = [Line]

fillWords :: Int -> [AWord] -> Paragraph
fillWords _ [] = []
fillWords width ws = currentLine : fillWords width restWords
    where
        (currentLine, restWords) = fillLine width ws

fillLine :: Int -> [AWord] -> (Line, [AWord])
fillLine _ [] = ([], [])
fillLine width (w:ws)
    | length w >= width = ([w], ws)
    | otherwise = go [w] (length w) ws
    where
        go line currentLength [] = (line, [])
        go line currentLength (x:xs)
            | currentLength + 1 + length x <= width = go (line ++ [x]) (currentLength + 1 + length x) xs
            | otherwise = (line, x:xs)

-- b
