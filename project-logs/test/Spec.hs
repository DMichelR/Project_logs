import Log (LogMessage, MessageTree)
import LogParser (build, inOrder, parse, parseMessage, whatWentWrong)

testParse ::
  (String -> [LogMessage]) ->
  Int ->
  FilePath ->
  IO [LogMessage]
testParse parser n file = take n . parser <$> readFile file

testWhatWentWrong ::
  (String -> LogMessage) ->
  ([LogMessage] -> MessageTree) ->
  (MessageTree -> [LogMessage]) ->
  ([LogMessage] -> [String]) ->
  FilePath ->
  IO [String]
testWhatWentWrong parser builder getMessage checkWhatWentWrong file =
  checkWhatWentWrong . getMessage . builder . map parser . lines <$> readFile file

main :: IO ()
main = do
  testWhatWentWrong parseMessage build inOrder whatWentWrong "samples/sample.log" >>= print
  testWhatWentWrong parseMessage build inOrder whatWentWrong "samples/log_file.log" >>= print

  testParse LogParser.parse 10 "samples/sample.log" >>= print
  testParse LogParser.parse 10 "samples/log_file.log" >>= print
