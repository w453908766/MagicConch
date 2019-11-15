
fact n =
  case n of
    0 => 1
    _ => n*fact(n-1)

data Maybe a where
  Nothing
  Just a

main args = block
  let a = Ref 5
      b = Just a
      d = a
     
  case b of
    Nothing => 0
    Just &d => 666
    Just c => !c

z = main 0

