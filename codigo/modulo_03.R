# Insumo-Producto Ambientalmente Extendido
# Facilitado por Renato Vargas
# Modulo 03 - Matriz de Insumo Producto y 
# Cuentas Ambientales de Costa Rica
# Publicadas por el Banco Central de Costa Rica
# Año 2017

# Preámbulo
library(openxlsx)
rm(  list = ls()  )


# Directorio de trabajo (ruta a los datos con "/" en vez de "\")
wd <- c("D:/github/insumo-producto-ambiental/datos")
setwd(wd)
getwd()

# Consumo intermedio
Z_cruda <- as.matrix(read.xlsx("MIP-AE-AE-017-CR.xlsx", 
                               sheet = "MIP 2017", 
                               rows= c(12:264), 
                               cols = c(4:256), 
                               skipEmptyRows = FALSE, 
                               colNames = FALSE, 
                               rowNames = FALSE)
                     )  # <-- Fin del paréntesis

# componente doméstico
local <- c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30, 
           32,34,36,38,40,42,44,46,48,50,52,54,56,58,60, 
           62,64,66,68,70,71,73,75,77,78,80,81,83,85,87,88, 
           90,92,94,95,97,98,99,100,101,103,105,107,108,110, 
           112,114,115,117,119,120,121,123,125,127,129,131, 
           133,135,137,139,141,143,145,147,149,151,153,155, 
           157,159,161,162,164,166,167,169,171,173,174,176, 
           178,180,182,184,186,188,190,192,194,196,197,199, 
           200,202,204,206,208,210,212,214,216,218,219,221, 
           223,225,227,229,231,233,235,237,239,241,243,245, 
           247,249,251,253)

# Si el BCR hubiera dejado (aunque en ceros) el componente importado
# de los sectores que solamente tienen componente local, los indices
# serían más fáciles de construir con:
# seq( from=2, to= 254, by=2 ) para lo doméstico 

# Aquí utilizamos el método del índice en R para obtener nuestros elementos

# Compras entre actividades económicas domésticas (consumo intermedio)
Z <- Z_cruda[  local  ,  local   ]

# Compras de producto importado de las actividades domésticas (importaciones)
M <- Z_cruda[-local,local]

# Compras de Actividades económicas no-domésticas a las actividades locales
# (exportaciones)
X <- Z_cruda[local,-local]

# Compras de Actividades no-domésticas a no-domésticas?
XX <- Z_cruda[-local,-local]

# Demanda Final
# Consumo intermedio
DF_cruda <- as.matrix(read.xlsx("MIP-AE-AE-017-CR.xlsx", 
                               sheet = "MIP 2017", 
                               rows= c(12:264), 
                               cols = c(258:262), 
                               skipEmptyRows = FALSE, 
                               colNames = FALSE, 
                               rowNames = FALSE)
                     )  # <-- Fin del paréntesis

# Componente doméstico de la demanda final
DF <- DF_cruda[ local ,  ]

# Obtenemos nuestros códigos de actividad y nombres
cods <- as.data.frame(
    read.xlsx(
      "MIP-AE-AE-017-CR.xlsx",
      sheet = "clasificaciones", 
      rows= c(5:141), 
      cols = c(1:5), 
      skipEmptyRows = FALSE, 
      colNames = TRUE, 
      rowNames = FALSE
      )
  )# <-- Fin del paréntesis


# Lo mismo para la demanda final
codsDF <- as.data.frame(
  t(
    read.xlsx(
      "MIP-AE-AE-017-CR.xlsx",
      sheet = "MIP 2017", 
      rows= c(9:10), 
      cols = c(258:262), 
      skipEmptyRows = FALSE,
      colNames = FALSE, 
      rowNames = FALSE
  ))
)# <-- Fin del paréntesis

# Y nombramos nuestra matriz Z
colnames(Z) <- as.vector(cods$codAECR)
rownames(Z) <- as.vector(cods$codAECR)

# Y nuestra matriz de demanda final DF
colnames(DF) <- as.vector(codsDF$"1")
rownames(DF) <- as.vector(cods$codAECR)

# =============================================================================
# Cuenta de energía

# Importamos los datos crudos
E_cruda <- as.matrix(read.xlsx("COUF-2017.xlsx", 
                               sheet = "COUF-E 2017", 
                               rows= c(127:146), 
                               cols = c(3:169), 
                               skipEmptyRows = FALSE, 
                               colNames = FALSE, 
                               rowNames = TRUE)
)  # <-- Fin del paréntesis

# Extraemos los valores excepto los grupos

posGruposEnergia <- c(
  2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,
  24,25,26,27,28,29,30,32,33,34,36,37,38,39,40,41,42,43,44,45,
  46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,
  66,67,68,69,70,71,72,73,74,75,76,77,79,80,81,83,86,89,91,93,
  95,96,98,99,100,101,102,103,104,105,107,108,109,111,112,114,
  115,116,117,118,120,123,124,125,126,127,130,131,132,134,135,
  136,137,138,139,140,141,142,144,145,146,147,150,154,155,156,
  157,159,161,162,163,164,165,166
)

# Extraemos las que nos interesan
E <- E_cruda[ , posGruposEnergia]

# Y nombramos las columnas
colnames(E) <- as.vector(cods$codAECR)

# =============================================================================
# Modelo de insumo producto

# Producción
x <- rowSums(Z) + rowSums(DF)

# Demanda final
f <- as.vector(rowSums(DF))

# x sombrero
xhat <- diag(x)

# Matriz de coeficientes técnicos
A <- Z %*% solve( xhat )

# Matriz identidad
I <- diag( dim(A)[1])

# Matriz de Leontief
L <- solve(I - A )

# Coeficientes de uso de cada energético por unidad de producto
EC <- E %*% solve(xhat)

# Cálculo de nuevas demandas de energía por energético
EC %*% L %*% f


