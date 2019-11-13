

data Maybe a where
  Nothing
  Just a


d = Just (Just 6)
e = Just (Just 6)

z = d == e
