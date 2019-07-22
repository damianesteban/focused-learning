messyMain :: IO()

-- Original:
messyMain = do
  print "Recipient?"
  recipient <- getLine
  print "Subject?"
  subject <- getLine
  print "Body?"
  body <- getLine
  print ("Recipient: " ++ recipient ++ "\n" ++
          "Subject: " ++ subject ++ "\n" ++
          "Body: " ++ body ++ "\n")

-- Refactored:
recipientPart :: [Char] -> [Char]
recipientPart recipient = "Dear " ++ recipient ++ ",\n"

subjectPart :: [Char] -> [Char]
subjectPart subject = subject

bodyPart :: [Char] -> [Char]
bodyPart body = body

createEmail :: [Char] -> [Char] -> [Char] -> [Char]
createEmail recipient subject body = recipientPart recipient ++
                                     subjectPart subject ++
                                     bodyPart body
-- Final function
main :: IO()
main = do
  print "Recipient?"
  recipient <- getLine
  print "Subject?"
  subject <- getLine
  print "Body?"
  body <- getLine
  print (createEmail recipient subject body)