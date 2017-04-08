:l Cauchy.hs CalkinWilf.hs

1 + 1 :: Cauchy

let det (a, b, c, d) = a * d - b * c

det (64919121, -159018721, 41869520.5, -102558961) :: Double

det (64919121, -159018721, 41869520.5, -102558961) :: Cauchy

expC 1

exp 1

printToPrecision 100 $ expC 1

printToPrecision 100 $ expC (-7) / sinC 9 + cosC (1/3) * 42.42

cantor $ realToFrac . genericIndex calkinWilf'
