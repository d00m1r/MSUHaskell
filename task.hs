arrRunPlus [] = []
arrRunPlus [x] = []
arrRunPlus (x:xs:xss) = [x+xs] ++ arrRunPlus(xs:xss)

arrRunComp [] = []
arrRunComp [x] = []
arrRunComp (x:xs:xss) = [x*xs] ++ arrRunComp(xs:xss)

arrRun [] flag = []
arrRun [x] flag = [x]
arrRun x flag
    | flag == True = arrRun (arrRunPlus x) False
    | otherwise = arrRun (arrRunComp x) True