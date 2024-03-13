module LogParser
  ( parseMessage,
    insert,
    build,
    inOrder,
    parse,
    whatWentWrong,
  )
where

import Log (LogMessage (..), MessageTree (..), MessageType (..))

--  1
parseMessage :: String -> LogMessage
parseMessage s = case words s of
  "I" : ts : msg -> LogMessage Info (read ts) (unwords msg)
  "W" : ts : msg -> LogMessage Warning (read ts) (unwords msg)
  "E" : sev : ts : msg -> LogMessage (Error (read sev)) (read ts) (unwords msg)
  _ -> Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg@(LogMessage _ ts _) (Node leftTree nodeMsg@(LogMessage _ nodeTS _) rightTree)
  | ts < nodeTS = Node (insert logMsg leftTree) nodeMsg rightTree
  | otherwise = Node leftTree nodeMsg (insert logMsg rightTree)
insert _ tree = tree

-- 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = [] -- Base case: If the tree is a Leaf (empty), return an empty list
inOrder (Node leftTree logMsg rightTree) =
  inOrder leftTree ++ [logMsg] ++ inOrder rightTree

-- 5

-- Function to extract relevant error messages from a sorted list of LogMessage strings
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMessages =
  let relevantErrors = filter isRelevantError logMessages
   in map extractMessage relevantErrors
  where
    isRelevantError (LogMessage (Error severity) _ _) = severity >= 50
    isRelevantError _ = False
    extractMessage (LogMessage _ _ msg) = msg
    extractMessage (Unknown msg) = msg
