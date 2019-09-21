
fact :: Int -> Int
fact 0 = 1
fact n = mul n (fact (sub n 1))
