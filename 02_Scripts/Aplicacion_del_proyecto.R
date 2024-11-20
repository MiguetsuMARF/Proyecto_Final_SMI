
###############################################
######## Ejemplo de Uso de las Funciones ######
###############################################

# Los paquetes necesarios de instalar para este proyecto son:
install.packages("qtl")
install.packages("ape")

# Primero debemos cargar las funciones especificas por medio de la funcion source:

source("02_Scripts/Funciones.R")

# Despues vamos a cargar la base de datos que vamos a usar para la prueba de 3 puntos:

Datos_3_puntos <- read.csv("01_Data/Ejercicio3_3_puntos.csv")

# Hay dos funciones para calcular la distancia en prueba de 3 puntos, una necesita de
# una base de datos con las frecuencias y otra solamente necesita que ingresemos los datos.

## 1.- Para la funcion que no necesita la base de datos necesitamos:
### a) Los fenotipos parentales.
### b) Los fenotipos y frecuencias de la progiene luego de cruza de prueba.

Datos_3_puntos$Genotipo # En esta base de datos aquellos genes con - son los que tienen 
# expresion fenotipica ya que significa que son recesivos.

Datos_3_puntos$Frecuencias # Y aqui tenemos las frecuencias. Por lo que ahora si podemos 
# ingresar que, aquellos con mayor frecuencia (1761, 1773) son los parentales.
# Por lo que se ingresa que, gen1 = k, gen2 = e, gen3 = cd. Entonces se ingresa:
## Parental no tiene expresion fenotipica de K, si tiene de e y no tiene de cd.
## a partir de aqui solo tenemos que ingresar las frecuencias y obtendremos las distancias.
View(Datos_3_puntos)

Ejemplo1_3puntos_no_base_datos <- prueba3puntos.no("k","e","cd") # debemos ingresar los genes entre comillas.
# Si queremos observar el resultado de la funcion podemos imprimir el objeto que guardo la funcion.
Ejemplo1_3puntos_no_base_datos

# Para obtener entonces las distancias para pasos posteriores podemos hacer:

Distancias_Ejemplo1 <- c(Ejemplo1_3puntos_no_base_datos$Distancias_Zona_1,
                         Ejemplo1_3puntos_no_base_datos$Distancias_Zona_2)
Distancias_Ejemplo1

## 2.- Para la funcion que necesita la base de datos necesitamos obligatoriamente:
### a) Una base de datos que contenga una columna con las frecuencias de cada fenotipo.
## De forma opcional se puede obtener mas informacion con:
### a) Saber que genes tiene el fenotipo wt y con que frecuencia se presenta este en la progiene.

Ejemplo1_3puntos_base_datos <- prueba3puntos(Datos_3_puntos,"Frecuencias",c("k","e","cd"),fr.wt = 6)
Ejemplo1_3puntos_base_datos

# Para obtener entonces las distancias para pasos posteriores podemos hacer:

Distancias_Ejemplo2 <- c(Ejemplo1_3puntos_base_datos$Distancias_Zona_1,
                         Ejemplo1_3puntos_base_datos$Distancias_Zona_2)
Distancias_Ejemplo2

# Y para terminar las funciones de 3 puntos podemos realizarla a la inversa, para esto necesitamos:
### a) Coeficiente de coincidencia
### b) Distancia zona 1
### c) Distnacia zona 2
## Ademas podemos agregar el total de la progiene y podemos agregar los genes y la inferencia de cuanta 
## progiene esperada se puede obtener.

Ejemplo1_3puntos_inversa <- prueba3puntos.inv(Ejemplo1_3puntos_base_datos$Coeficiente_coincidencia,
                                              Ejemplo1_3puntos_base_datos$Distancias_Zona_1,
                                              Ejemplo1_3puntos_base_datos$Distancias_Zona_2,
                                              c("k","e","CD"),
                                              total = 4000, inf.observados = TRUE)
Ejemplo1_3puntos_inversa

# Si vemos la tabla de resultados podemos obtener las frecuencias y los observados esperados:

Ejemplo1_3puntos_inversa$Resultados_estimacion

# En este caso el signo negativo representa que no hay la misma expresion al parental1, osea:
## K = wt, no se expresa, si es K- entonces si hay expresion.
## e = mutante, se expresa, si es e- entonces no hay expresion.
## CD = wt, no se expresa, si es CD- entonces si hay expresion.


                    ### Mapa Genetico ###
Genes <- c ("K", "e", "CD") # Para utilizar esta función se recomiend crear un vector
# que contenga los genes para los que se realizará la prueba. La posición del gen
# dentro del vector, denota cuales genes estan a las orillas y cual esta en el centro.
# En caso de no crear un vector, la funcion nombrara a los genes por default A, B y C


as.mapa.genetico(Ejemplo1_3puntos_inversa, Genes) # Para utilizar la función solo hay
# que introducir el resultado de las funciones: prueba3puntos.inv, prueba3puntos.no o
# prueba3puntos.
# En el seugndo espacio se puede o no introducir un vector que cuente con los genes de 
# la prueba




                            ### Mapa Genetico ###
Genes <- c ("K", "e", "CD") # Para utilizar esta función se recomiend crear un vector
# que contenga los genes para los que se realizará la prueba. La posición del gen
# dentro del vector, denota cuales genes estan a las orillas y cual esta en el centro.
# En caso de no crear un vector, la funcion nombrara a los genes por default A, B y C


Mapa_genetico_p3p <- as.mapa.genetico(Ejemplo1_3puntos_inversa, Genes) # Para utilizar la función solo hay
# que introducir el resultado de las funciones: prueba3puntos.inv, prueba3puntos.no o
# prueba3puntos.
# En el seugndo espacio se puede o no introducir un vector que cuente con los genes de 
# la prueba


as.mapa.genetico(Ejemplo1_3puntos_inversa) # Así se obseraría la grafica si no utilizamos
# un vectorque contenga los genes.

png("03_Results/Mapa_genetico.png", width = 400, height = 600)
as.mapa.genetico(Ejemplo1_3puntos_inversa, Genes)
dev.off()

# La función arroja un mapa genetico con la distancia en centimorgans que hay entre cada gen.
# A su vez, esta función esta enriquecida por una función creada por Junli Zhang
# cuyo github es el siguiente: https://github.com/pinbo



# La función arroja un mapa genetico con la distancia en centimorgans que hay entre cada gen.
# A su vez, esta función esta enriquecida por una función creada por Junli Zhang
# cuyo github es el siguiente: https://github.com/pinbo


######CREACIÓN DE UN MAPA GENÉTICO A PARTIR DE DISTANCIAS#########

#Esta función nos va a servir para generar un mapa genético a partir de una matriz
#de distancias entre un número determinado de genes, sin importar si estos
#estan ordenados o no. 

#Lo que tenemos que darle a la matriz es una matriz cuadrada de n dimensiones 
#como la siguiente, que está inspirada en las otras funciones
matriz_mapa_genetico<- matrix( c(0, 17, 12, 2, 17, 4, 4, 19, 1, 17, 
                                 17, 0, 5, 17, 18, 20, 19, 10, 10, 5, 
                                 12, 5, 0, 9, 6, 20, 2, 2, 20, 19, 
                                 2, 17, 9, 0, 20, 1, 7, 3, 1, 6, 
                                 17, 18, 6, 20, 0, 7, 2, 14, 3, 9, 
                                 4, 20, 20, 1, 5, 0, 5, 3, 15, 6, 
                                 4, 19, 2, 7, 2, 5, 0, 7, 12, 1, 
                                 19, 10, 2, 3, 14, 3, 7, 0, 4, 11, 
                                 1, 10, 20, 1, 3, 15, 12, 4, 0, 6, 
                                 17, 5, 19, 6, 9, 6, 1, 11, 6, 0), ncol = 10, byrow = TRUE) 
#Y ahora creamos un vector para nombrar los genes

genes_mapa<- c("G1","G2","G3","G4","K","E","Cd","G5","G6","G7")

#Ahora, al correr la función con esta matriz y este vector de valores, veremos 
#nuestro mapa genético
as.mapa.genetico.matrix(matriz_mapa_genetico, genes_mapa)

png("03_Results/Mapa_genetico_matrix.png", width = 400, height = 600)
as.mapa.genetico.matrix(matriz_mapa_genetico, genes_mapa)
dev.off()
