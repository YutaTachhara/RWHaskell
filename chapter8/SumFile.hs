main = do
    contens <- getContents
    print (sumFile contens)
        where sumFile = sum . map read . words
