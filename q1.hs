data Arvore a = Vazia
              | No a (Arvore a) (Arvore a)
              deriving (Show)

type Coordenada = (Int, Int)
calcularCoord :: Arvore a -> Arvore (a, Coordenada)
calcularCoord arvore = fst (auxiliar arvore 1 0)
  where
    auxiliar :: Arvore a -> Int -> Int -> (Arvore (a, Coordenada), Int)
    auxiliar Vazia x y = (Vazia, x)
    auxiliar (No valor esq dir) x y =
      let (arvoreEsq, x1) = auxiliar esq x (y + 1)    
          coordenadaAtual  = (valor, (x1, y))          
          (arvoreDir, x2)  = auxiliar dir (x1 + 1) (y + 1)  
      in (No coordenadaAtual arvoreEsq arvoreDir, x2)  


imprimirCoord :: Show a => Arvore (a, Coordenada) -> IO ()
imprimirCoord Vazia = return ()  
imprimirCoord (No (valor, coordenada) esq dir) = do
  print (valor, coordenada)  
  imprimirCoord esq        
  imprimirCoord dir        

-- exemplo ilustrativo
arvoreExemplo :: Arvore Char
arvoreExemplo = No 'a'
                (No 'b'
                  (No 'd' 
                    Vazia 
                    (No 'e' Vazia Vazia))
                  (No 'f' Vazia Vazia))
                (No 'c'
                  (No 'g' Vazia Vazia)
                  (No 'h' Vazia Vazia))


-- função que roda o programa
main :: IO ()
main = do
  let resultado = calcularCoord arvoreExemplo 
  imprimirCoord resultado  