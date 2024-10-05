data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0
longitud (Node x lista) = 1 + longitud lista

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void x = False
estaContenido (Node x lista) y = if x == y
                                    then True 
                                else estaContenido lista y 

convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node x lista) = x : convertirALista lista 

conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node x lista) = if estaContenido lista x
                            then conjunto lista 
                          else Node x (conjunto lista)

eliminarIndice :: List a -> Int -> List a
eliminarIndice Void n = error "Lista vacía"
eliminarIndice (Node x lista) n = if n < 0 || n >= longitud (Node x lista)
                                    then error "Indice invalido"
                                    else if n == 0
                                        then lista 
                                        else Node x (eliminarIndice lista (n-1))

insertarIndice :: List a -> Int -> a -> List a
insertarIndice Void n a = if n /= 0
                            then error "Indice invalido (unico indice valido en lista vacia es 0)"
                            else (Node a Void)
insertarIndice (Node x lista) n a = if n < 0 || n > longitud (Node x lista)
                                        then error "Indice invalido"
                                        else if n == 0
                                        then Node a (Node x lista)    
                                        else Node x (insertarIndice lista (n-1) a)           

recorrerLista :: List a -> Int -> List a
recorrerLista lista 0 = lista
recorrerLista (Node x lista) n = recorrerLista (insertarIndice lista (longitud lista) x) (n-1)


