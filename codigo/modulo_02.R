# Insumo-Producto Ambientalmente Extendido
# Facilitado por Renato Vargas
# Modulo 02 - El Modelo de Insumo Producto

# 1. El modelo básico


#           Hacia   Sectores
#                  Agr   Manuf  FinDem  Total Producto
# Desde
#   Agricultura    150     500     350          1000
#   Manufacturas   200     100    1700          2000
# 
# Pagos a factores 650    1400    1100          3150
# Total insumos   1000    2000    3150          6150 


# Coeficientes técnicos (la matriz A) 

#           Hacia    Sectores
#                   Agr   Manuf  
# Desde
#   Agricultura    0.15    0.25
#   Manufacturas   0.20    0.05

# Primero creamos nuestro cuadro de flujos entre industrias

Z <- matrix( c(150,500,200,100), nrow = 2, ncol = 2, byrow = TRUE)

# Es importante identificar los sectores en nuestra matriz

ind <- c("Agricultura", "Manufacturas")
colnames(Z) <- ind 
rownames(Z) <- ind

# Ingresamos nuestra demanda final (según el diagrama)
f <- c(350, 1700)
f

# Nuestro vector de producto total
x <- c(1000, 2000)
x

# Una matriz diagonal con la producción total por sector
xhat <- diag(x)
xhat

#  Nuestra matriz de coeficientes técnicos.
A <- Z %*% solve( xhat )
A

# Ahora necesitamos conocer las dimensiones de A.
dim(A)

# Para poder crear nuestra matriz identidad de tamaño apropiado

I <- diag(   dim(A)[1]    )
I

# Y así estimar nuestra matriz de Leontief
L <- solve( I - A )
L

# Verificamos que nuestro modelo calcula x = Lf (¡Estamos calibrados!) 
L %*% f

# ¿Ahora qué pasaría si cambiamos nuestra demanda final?

# Si la demanda final de los productos agrícolas se incrementa a $600
# el siguiente año y la de las manufacturas bajara a $1500,
# ¿cuánto producto de los dos sectores sería necesario para satisfacer
# esta nueva demanda?

fnueva <- c(600, 1500)

xnueva <- L %*% fnueva
xnueva

# De nuestra definición de coeficientes Z= A %*% xhat encontramos Znueva

Znueva <- A %*% diag(xnueva)  # error

# Nos da un error porque xnueva no es entendido como vector por R, es una
# matriz y diag() funciona de manera distinta si se le pasa una matriz.

help("diag")
help("as.vector")

Znueva <- A %*% diag(c(xnueva))
Znueva <- A %*% diag(as.vector(xnueva))
Znueva

# Nuestro nuevo cuadro

#             Hacia   Sectores
#                    Agr    Manuf  FinDem  Total Producto
# Desde
#  Agricultura    187.13   460.40     600       1247.52
#  Manufacturas   249.51    92.08    1500       1841.58  
# 
# Pago a factores 810.89  1289.11    1100       3200.00
# Total insumos  1247.53  1841.58    3200       6289.10

# A veces queremos saber cuáles son los cambios, en lugar
# de los nuevos niveles.

# Cambios en demanda
deltaf <- fnueva - f

# Cambios en producto
deltax <- xnueva - x

# Cambios a toda la tabla Z
deltaZ <- Znueva - Z

# En moneda local. Tipo de cambio 7.70 ML por 1 USD

tc <- 7.70

deltaf * tc
deltax * tc
deltaZ * tc

# =============================================================================

# 2. La extensión ambiental básica 

# Otros impactos pueden ser obtenidos a través de coeficientes

# Imaginemos que nuestro uso de agua está dado por
eprima <- c(300, 500)
rownames(eprima) <- ind # c("Agricultura", "Manufacturas")
colnames(eprima) <- "Metros Cúbicos de Agua"
eprima

# In matrix algebra you would have to transpose e and 
# postmultiply it by a diagonal version of x
epsilon <- t(as.matrix(eprima)) %*% solve(diag(x))

# También se puede utilizar la regla de reciclaje para usar la división normal
# epsilon <- eprima / x

# Epsilon es entonces nuestro vector de coeficientes técnicos ambientales
epsilon

# Y enueva su nuevo vector de impactos ambientales
enueva <- diag(c(epsilon)) %*% L %*% fnueva
rownames(enueva) <- ind # c("Agricultura", "Manufacturas")
colnames(enueva) <- "Metros Cúbicos de Agua"

# Y vemos nuestro resultado enueva
enueva

# El cambio en uso de agua como resultado de la política
deltae <- enueva - eprima

# El cambio porcentual (por regla de reciclaje)
((enueva - eprima) / eprima) * 100

# =============================================================================

# 3. Multiplicadores

# El multiplicador de producto para el sector j esta definido como
# el valor de producción en todos los sectores de la economía que es
# necesario para satisfacer una unidad monetaria de la demanda de los 
# productos de ese sector.

# Multiplicadores simples

# Para el multiplicador simple esta producción total se define como
# la unidad monetaria adicional de la producción del sector j 
# necesaria para satisfacer la demanda final adicional.
# El multiplicador es la razón del cambio indirecto al cambio directo.

# De nuestro ejemplo:

A
L

# A=

#               [,1] [,2]
# Agricultura   0.15 0.25
# Manufacturas  0.20 0.05

# L=

#      Agricultura  Manufacturas
# [1,]   1.2541254      0.330033
# [2,]   0.2640264      1.122112

# Primero veamos la demanda adicional solo para el primer sector
deltaf1 <- c(1,0)
L %*% deltaf1

# o el segundo sector
deltaf2 <- c(0,1)
L %*% deltaf2

# En el primer caso, vemos una producción adicional de USD 1.254
# del sector 1 y USD 0.264 del sector 2. El 1.254 es lo que se necesita
# para satisfacer la demanda más USD 0.254 para que se puedan mover los
# insumos entre sectores.
# El USD 0.254 is para compras entre sectores exclusivamente.

# El multiplicador  del sector 1 está definido como la suma de los ele-
# mentos en la columna deltax(1) (USD 1.518) dividido por USD 1 (adicional)
# m(o)1= $1.518/$1= 1.518 el cual no tiene unidades de medida.

# ¿Cómo hacemos esto para todos los sectores? Con vectores sumatorios.

# Obtenemos las dimensiones de L
dim(L)

# Y creamos un vector sumatorio de largo apropiado.
help("rep")
i <- rep(1, dim(L)[1])

# Nuestros multiplicadores son:
mo <- i%*%L

# Estos son muy útiles porque nos dicen en qué sectores nuestras inversiones
# tienen mayores impactos en la economía.

