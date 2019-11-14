
fact n =
  case n of
    0 => 1
    _ => n*fact(n-1)


main args = block
  print (fact 5)

z = main 0

