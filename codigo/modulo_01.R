# Insumo-Producto Ambientalmente Extendido
# Facilitado por Renato Vargas
# Modulo 01 - Los elementos básicos del análisis de Insumo-Producto

# Una matriz es un conjunto de elementos ordenados en forma de una rejilla
# de filas y columnas. Las matrices se definien rectangularmente de manera 
# que puedan ser utilizadas para representar sistemas de relaciones lineares 
# entre variables. Eso es la estructura básica de un sistema de insumo-producto.

# El caso general es una matriz de "m" filas y "n" columnas. Si m=2 y n=3, usando
# notación de doble subíndice a_ij para denotar el elemento in la fila i y la
# columna j de la matriz, tendremos:

#    A =           [,1] [,2] [,3]
#             [1,] a_11 a_12 a_13
#             [2,] a_21 a_22 a_23

# Nota: en notación escrita, se utiliza paréntesis cuadrados [ ] alrededor de todos
# los elementos de la matriz.

# Ahora creemos una matriz de ejemplo.

#   M =            [,1] [,2] [,3]
#             [1,]    2    1    3
#             [2,]    4    6   12      

# Una forma de crear una matriz es primero creando un vector con los números
# y luego forzarlo a conformarse en forma de matriz.

# Busquemos en la ayuda la función "combine" o c()
help("c")

c <- c(2, 1, 3, 4, 6, 12)
c

# A veces una instrucción es muy larga. Esto a R no le importa, pero 
# a los humanos nos dificulta la lectura. Entonces podemos omitir
# el "retorno" (o enter) con el símbolo de numeral.

c <- c(2, 1,  3, #
       4, 6, 12)

# Después "llenamos" nuestra matriz con el vector
help("matrix")

# Nótese el parámetro "byrow" muy importante en este caso
M <- matrix(c, nrow = 2, ncol = 3, byrow = TRUE)

# Veamos qué pasa si no lo ponemos
matrix( c, nrow = 2,ncol = 3, byrow = FALSE ) 

# Veamos nuestra matriz
M

# Para acceder a cualquier elemento de nuestra matriz, utilizamos "indexación"
M[2,3]
M[2,2]

# También lo podemos hacer con vectores, con la posición en la lista
c[5]

# Veamos la ayuda de la función as.matrix( )
help("as.matrix")

# Otro método es leer del portapapeles
read.table("clipboard", sep="\t")

# Y otro es usar una librería para leer un archivo de Excel.
# Lo cual veremos algunos módulos adelante.

# =============================================================================
# Operaciones Matriciales
# 1. Adición y sustracción

# Adición

# La adición de matrices se logra sumando elementos de cada matriz en las
# posiciónes correspondientes. Solo las matrices de las mismas dimensiones
# pueden ser sumadas.

# Agreguemos una segunda matriz

N <- matrix(    c(1,2,3,3,2,1)    ,   nrow = 2   , ncol = 3 , byrow = TRUE)
N

# Y agreguémoslas. Pero primero chequeemos que las dimensiones de las matrices
# coinciden.

help("dim")

dim(M)

dim(N)

dim(M) == dim(N)

S <- M + N

S

# ¿Qué pasa si no tienen las mismas dimensiones?

O <- matrix( c(2,2,3,4), ncol = 2, nrow = 2, byrow = TRUE)

O

M+O

# Sustracción

# La sustracción se define como la resta de elementos en posiciones correspon-
# dientes. Solo las matrices de exactamente las mismas dimensiones pueden ser
# restadas.

D <- M - N

D

# Igualdad

# Las matrices son iguales si tienen las mismas dimensiones y si los elemen-
# tos en posiciones correspondientes son iguales.

# Verificar si las dimensiones de las matrices son iguales.
dim(M) == dim(N)

# Verificar si los elementos individuales son iguales.
M == N

# Verificar igualdad.

help("isTRUE")
isTRUE(   M == N   )


# La Matriz Nula o Matriz Cero

# El equivalente del cero en algebra ordinario. Es un número que cuando es
# sumado o restado de otro número, deja ese número sin cambiar. En álgebra
# matricial es una matriz que contiene solamente ceros.

Z <- matrix(     c(0,0,0,  0,0,0)      , nrow = 2, ncol = 3, byrow = TRUE)

Z

# Utilizando el primer método que usamos para crear una matriz, en vez de
# pasarle un vector a la función, podemos pasarle un solo número.

Z <- matrix(    0   , nrow = 2, ncol = 3, byrow = TRUE)

# =============================================================================

# 2. Multiplicación

# Multiplicación de una matriz por un solo número (un escalar). Cada elemento 
# de la matriz se multiplica por ese valor. ¿Ejemplo de para qué sirve esto?

M

Mx2 <- M * 2

Mx2

# ¿Cómo quitamos un elemento de nuestro ambiente de trabajo?
rm(Mx2)

# Multiplicación de una matriz por otra matriz
# (No es intuitivo pero de acuerdo a la notación, hace sentido).

# Si tenemos que     P     =    M    x    Q
#                 (m x n)    (m x r)   (r x n)

# Para que sean conformables para multiplicación, el número de columnas en la
# matriz de la izquierda debe ser igual al número de filas en la matriz de la
# derecha. 

# Esta definición también incluye el hecho que el producto de las dos matrices
# "P" tendrá el mismo número de filas que "M" y el mismo número de columnas
# que "Q". 

#       [,1] [,2] [,3]             [,1] [,2] [,3]
#  [1,]    2    1    3        [1,]    2    0    4
#  [2,]    4    6   12        [2,]    1    1    2
#                             [3,]    3    4    5

# En álgebra matricial, el orden de los factores sí importa. En este ejemplo,
# por la regla de filas y columnas el producto de Q x M no puede ser calculado
# pues hay tres columnas en Q pero solo dos filas en M.

# Construyamos una matriz 3 x 3 denominada Q
Q <- matrix(c(2,0,4,  1,1,2,  3,4,5), nrow = 3, ncol = 3, byrow = TRUE)

# Veamos cómo sería la multiplicación a mano:

c( (2*2 + 1*1 + 3*3 ), (2*0 + 1*1 + 3*4 ), (2*4 + 1*2 + 3*5 ), #
   (4*2 + 6*1 + 12*3), (4*0 + 6*1 + 12*4), (4*4 + 6*2 + 12*5))

# Para la multiplicación de matrices utilizamos el símbolo %*%

P <- M %*% Q

P

# La Matriz Identidad

# De manera similar al álgebra ordinario una matriz identidad es una que
# deja una matriz sin alteraciones si se multiplica por ella.

# Si tenemos
# M =          [,1] [,2] [,3]
#         [1,]    2    1    3
#         [2,]    4    6   12

# Instintivamente pensaríamos que deberemos tener una matriz que solamente tenga
# 1s en todas las posiciones y que tenga las dimensiones adecuadas para multipli-
# cación (número de filas y columnas), pero esto sería incorrecto:
M %*% matrix( 1, nrow = 3, ncol = 3)

# ¿Por qué matriz podríamos posmultiplicar M para que no sufra cambios?
# La respuesta es una matriz con 1s en su diagonal con 0s fuera de la diagonal.

I <- matrix( c(1, 0, 0,   0, 1, 0,  0, 0, 1), nrow = 3, ncol = 3, byrow = TRUE)

# Recordemos que M es:
M

# Y posmultiplicamos M por I (recordemos la multiplicación de matrices en R)
M %*% I

# El resultado es M. ¿Qué pasa si M tiene dimensiones de miles de filas por
# miles de columnas? No tenemos que escribirlas a mano. Primero obtenemos las
# dimensiones deseadas (recordemos el concepto de indexación):

dim(M)

dim(M)[1]

dim(M)[2]

# > dim(M)
# [1] 2 3

# Sabemos que nuestra matriz debe tener dimensiones 3 x 3.
# Y ahora usamos la función diag()

help("diag")

I3 <-  diag(3)

# ¿Qué tal una más grande?
I10 <- diag(10)
I10


# Podemos ahorrarnos un paso y obtener la matriz identidad de dimensiones
# adecuadas de manera automágica:
diag(   dim(M)[2]   )

# ¿Por qué matriz debemos premultiplicar M para que quede igual?
# (recordemos la regla de multiplicación)

diag( dim(M)[1] ) %*% M

# =============================================================================
# 3. Transposición de matrices

# Esta operación no tiene paralelos en el álgebra matricial, pero es importante
# en ciertas operaciones de insumo-producto.
# M' (M prima) o M^t (M transpuesta) es una matriz n x m en que la fila i de M
# se convierte en la columna i de M^t

Mt <- t(M)
t(M)

# En álgebra matricial, la notación exige que un vector "horizontal" se deno-
# mine como un vector transpuesto o "prima", pues ha sido manipulado para po-
# der ser multiplicado con una matriz que tiene ciertos requerimientos de nú-
# mero de columnas o filas. 

# En R, esto es irrelevante, pues los vectores son agnósticos con respecto a 
# las dimensiones y se pueden utilizar de manera indistinta en pre o posmul-
# tiplicación, siempre y cuando tenga el largo adecuado respecto de las di-
# mensiones de la matriz con la que se quiere operar. R hará el trabajo. 
# Incluso es posible usar una multiplicación simple para multiplicar por el 
# vector, entendiendo la regla del reciclaje.

# Al usar la función de transposición, R transforma un vector en matriz.
t(c)
t( t(c) )

# =============================================================================
# 4. Sistemas de ecuaciones lineares

# Si tenemos dos ecuaciones lineares con dos incógnitas x_1 and x_2

#     2x_1 +  x_2 = 10
#     5x_1 + 3x_2 = 26

# Podemos definir A =      [,1] [,2]
#                     [1,]    2    1
#                     [2,]    5    3

# Definir un vector columna x que contiene las incógnitas

#               x =       [,1] 
#                   [1,] "x_1"
#                   [2,] "x_2"

# Y otro vector que tiene las soluciones del lado derecho

#              b =      [,1]
#                   [1,]   10
#                   [2,]   26

# Por la forma en que definimos la multiplicación de matrices y la igualdad de
# matrices, el sistema de ecuaciones puede ser representado:

#               Ax = b

# ¿Cómo encontramos la respuesta a las incógnitas? División de matrices...

# Llevemos nuestro ejercicio a R
A <- matrix(c(2,1,5,3), nrow = 2, ncol = 2, byrow = TRUE)
b <- matrix(c(10, 26), nrow = 2, ncol = 1, byrow = TRUE)
A
b

# recordemos también que podemos leer del portapapeles:
# A <- read.table("clipboard", sep="\t")

# En álgebra ordinario, si tenemos:

#                      3x = 12

# Encontramos la solución dividiendo ambos lados entre 3.
# La división en álgebra matricial no está definida y no podemos usar % / %

# Pero en álgebra ordinario también se puede multiplicar ambos lados por el
# recíproco de 3, que es (1/3) o 3^(-1), así que:

#                   3x = 12  
#             (1/3) 3x = (1/3)  12
#       [  o 3^(-1) 3x = 3^(-1) 12 ]
#                 (1)x = 4
#                    x = 4

# Podemos usar estos mismos conceptos con álgebra matricial.


#                  Ax = b
#            (A^-1)Ax = (A^-1)b
#                  Ix = (A^-1)b    (I = Matriz Identidad)
#                   x = A^-1 b 

# Pueden recordar el proceso tedioso de encontrar la determinante de una 
# matriz 2 x 2 sustrayendo los productos cruzados por algún método.
# Nos ahorraremos este cálculo manual y dejaremos que la computadora lo haga

# La función solve() nos permite obtener la inversa de una matriz.

solve(A)

# Hay matrices en que los productos cruzados son ceros no tienen inversas como:
C <- matrix(c(2,4,6,12), nrow = 2, ncol = 2)
solve(C)

# Y para estas no hay solución, pues es el equivalente a tratar de dividir por 
# cero en álgebra ordinario.


# Y regresando a nuestro ejemplo: 
#                   x = A^-1 b 

x <- solve(A) %*% b
x

# =============================================================================
# Diagonal matrices

# En notación encontraremos repetidamente un mecanismo útil para crear matrices
# diagonales de un vector. La versión "sombrero" de un vector "x" es una matriz
# diagonal con los elementos de "x" ubicados en su diagonal principal.

# Para         x  =       [,1]
#                    [1,] "x1"
#                    [2,] "x2"
#                    [3,] "x3"

#   ^
#   x  [o x sombrero]  =      [,1] [,2] [,3]
#                        [1,] "x1" "0"  "0" 
#                        [2,] "0"  "x2" "0" 
#                        [3,] "0"  "0"  "x3"

# También se puede usar esta notación para denominar a una matriz formada con
# los elementos de la diagonal principal de una matriz existente, convirtiendo
# el resto de elementos fuera de la diagonal en ceros. Una matriz con un som-
# brero invertido denota una matriz a la que se le convirtieron en ceros los
# elementos de la diagonal y el resto se han dejado intactos.

# Para nuestra matriz Q

#     Q =      [,1] [,2] [,3]
#         [1,]    2    0    4
#         [2,]    1    1    2
#         [3,]    3    4    5

#  Qhat =      [,1] [,2] [,3]
#         [1,]    2    0    0
#         [2,]    0    1    0
#         [3,]    0    0    5

Qhat <- diag(diag(Q))

# Quhat =      [,1] [,2] [,3]
#         [1,]    0    0    4
#         [2,]    1    0    2
#         [3,]    3    4    0

Quhat       <- Q
diag(Quhat) <- 0

# La inversa de una matriz diagonal es otra matriz diagonal cuyos elementos
# no son más que el recíproco del elemento original. Para Qhat:

invQhat <- solve(Qhat)

# Para verificar que este es el caso, el resultado de la multiplicación de 
# ambas matrices debe ser una matriz identidad.

invQhat %*% Qhat

# Cuando tenemos una matriz diagonal D que posmultiplica a otra matriz M, 
# el elemento j de D, dj, multiplica todos los elementos de la columna j
# de M y cuando una matriz diagonal premultiplica M, dj multiplica todos
# los eleemntos en la fila j de M.

# Útil cuando se tienen coeficientes de toneladas de contaminación por
# dólar de producción por cada industria. 


# Vectores sumatorios

# Si se posmultiplica a M con un vector columna de 1s con dimensiones (n)
# el resultado será un vector columna de dimensiones (m) con las sumas
# de las filas de M.

# Si se premultiplica a M con un vector fila de 1s con dimensiones (m) 
# el resultado será un vector fila de dimensiones (n) con las sumas de las
# columnas de M.

#   M =      [,1] [,2] [,3] Total
#       [1,]    2    1    3     6
#       [2,]    4    6   12    22
#            --------------
#     Total     6    7   15

# Suma de filas
M %*% matrix(1, nrow = 3, ncol = 1)

M %*% c(1,1,1)

# Suma de columnas
matrix(1, ncol = 2, nrow = 1) %*% M

c(1,1) %*% M

# Haciendo trampa con R
colSums(M)
rowSums(M)

# Esto es útil cuando se tiene miles de columnas y filas.
