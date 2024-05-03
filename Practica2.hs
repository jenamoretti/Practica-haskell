data Color = Color {rojo :: Int, verde :: Int, blue :: Int} deriving Show

mezclar :: Color -> Color -> Color
mezclar x y = Color { rojo = (rojo x + rojo y) `div` 2,
                      verde = (verde x + verde y) `div` 2,
                      blue = (blue x + blue y) `div` 2
                    }
                 
data Linea = Linea {caracteres :: String, cursor :: Int} deriving Show

vacia :: Linea
vacia = Linea {caracteres = "", cursor = 0}

moverIzq :: Linea -> Linea
moverIzq linea
    | cursor linea > 0 = linea {cursor = cursor linea - 1}
    | otherwise = linea
    
moverDer :: Linea -> Linea
moverDer linea
    | cursor linea < length (caracteres linea) = linea { cursor = cursor linea + 1 }
    | otherwise = linea 
    
moverIni :: Linea -> Linea
moverIni linea = linea {cursor = 0}

moverFin :: Linea -> Linea
moverFin linea = linea {cursor = length (caracteres linea)}

insertarEn :: Char -> String -> Int -> String
insertarEn char linea pos = take pos linea ++ [char] ++ drop pos linea

insertar :: Char -> Linea -> Linea
insertar c linea = linea {caracteres = insertarEn c (caracteres linea) (cursor linea), cursor = cursor linea + 1}

borrarEn :: String -> Int -> String
borrarEn linea pos
    | pos >= 0 && pos < length linea = take pos linea ++ drop (pos + 1) linea
    | otherwise = linea  
 
borrar :: Linea -> Linea
borrar linea = linea {caracteres = borrarEn (caracteres linea) (cursor linea)}

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show, Eq)

headCl :: CList a -> a
headCl (CUnit a) = a
headCl (Consnoc a b c) = a

snocCL :: CList a -> a -> CList a
snocCL EmptyCL a = CUnit a
snocCL (CUnit a) b = Consnoc a EmptyCL b
snocCL (Consnoc a b c) d = Consnoc a (snocCL b c) d

tailCl :: CList a -> CList a
tailCl (CUnit a) = EmptyCL
tailCl (Consnoc a b c) = snocCL b c

isEmptyCl :: CList a -> Bool
isEmptyCl EmptyCL = True
isEmptyCl _ = False

isCUnitCL :: CList a -> Bool
isCUnitCL (CUnit a) = True
isCUnitCL _ = False
