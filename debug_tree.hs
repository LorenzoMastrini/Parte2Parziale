import LexLinguaggio

main :: IO ()
main = do
  putStrLn "Testing tree search for ':*='"
  debugTreeFind ":*=" resWords
  
  putStrLn "\nTesting tree search for ':+='"
  debugTreeFind ":+=" resWords
  
  putStrLn "\nTesting tree search for ':-='"
  debugTreeFind ":-=" resWords 