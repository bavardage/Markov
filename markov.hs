import qualified Data.Map as Map
import Data.Char
import System.Random


type MarkovMap = Map.Map [String] [String]

makeMarkovMap :: Int -> [String] -> MarkovMap
makeMarkovMap n ws = makeMap n ws Map.empty
    where
      makeMap :: Int -> [String] -> MarkovMap -> MarkovMap
      makeMap n [] map = map
      makeMap n ws map = makeMap n (tail ws) map'
          where
            map' = Map.alter (adjust value) key map
            key = take n ws
            value = let ws' = drop n ws
                    in
                      if (null ws') then "" else head ws'
            adjust val Nothing = Just [val]
            adjust val (Just xs) = Just (val:xs)


randomElement :: [a] -> IO a
randomElement xs = do
  index <- randomRIO (0, length xs - 1)
  return $ xs !! index
  

markovWords :: MarkovMap -> Int -> [String] -> IO [String]
markovWords map n xs = do
  let key = take n xs
--  print $ "key is " ++ show key
  case (Map.lookup (reverse key) map) of
    Nothing -> do
      return xs
    Just ws -> do
--      print $ "--> " ++ show ws
      word <- randomElement ws
      markovWords map n $ word : xs
    
outputFiles :: [(String, String)] -> IO ()
outputFiles fs = outputFiles' 0 fs
    where
      outputFiles' _ [] = return ()
      outputFiles' n ((f,_):fs) = (putStrLn $ show n ++ ": " ++ f)
                                  >> (outputFiles' (n+1) fs)

getFilename :: [(String, String)] -> IO String
getFilename fs = do
  let stories = length fs - 1
      readm s = case reads s of [(x, "")] -> Just x; _ -> Nothing
      failure = putStrLn "That's not an option" >> getFilename fs
  putStr "Story> "
  input <- getLine 
  let story = readm input :: Maybe Int
  case story of
    Just s -> if (s > stories || s < 0)
             then failure
             else return $ snd (fs !! s)
    Nothing -> failure

doOutput :: MarkovMap -> Int -> IO ()
doOutput markovmap level = do
  output <- markovWords markovmap level ["a", "was", "it"]
  toDrop <- randomRIO (50, 200)
  putStrLn $ unwords $ reverse $ take 100 $ drop toDrop output
  putStr "\n"
  choice <- getChar
  if choice /= 'q'
     then doOutput markovmap level
     else main


main = do
  let files = [("Alice in Wonderland", "alice.txt"),
               ("Dracula", "dracula.txt"),
               ("Accelerando (cyberpunk/sci-fi)", "accelerando.txt"),
               ("Fairy Tales of the Brothers Grimm", "fairy.txt"),
               ("Pride and Prejudice", "prideandprejudice.txt"),
               ("1984", "1984.txt"),
               ("Tale of Two Cities", "tale2cities.txt"),
               ("Frankenstein", "frankenstein.txt"),
               ("Down and Out in the Magic Kingdom", "daoitmk.txt")]
  putStrLn "Please choose a story to seed with: "
  outputFiles files
  filename <- getFilename files
  file <- readFile filename
  putStrLn "Reading file..."
  let level = 2
      ws = words $ map toLower file
      markovmap = makeMarkovMap level ws
  doOutput markovmap level

