-- projecteuler 21

d n = sum . filter ((== 0) . (n `mod`)) $ [1..pred n]

main = print . sum . filter ((&&) <$> ((/=) =<< d) <*> ((==) =<< d . d)) $ [1..pred 10000]
