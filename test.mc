
fact n =
  case n of
    0 => 1
    _ => n*fact(n-1)

data Maybe a where
  Nothing
  Just a

main args = block
  let a = Just (Ref 5)
  case a of
    Nothing => 0
    Just b => !b

z = main 0

