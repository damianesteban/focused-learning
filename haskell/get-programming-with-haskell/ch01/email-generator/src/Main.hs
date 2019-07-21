messyMain :: IO()


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
