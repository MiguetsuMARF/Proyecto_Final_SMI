# Creando las bases de datos.Vamos unicamente a guardar 1 columna importante, frecuencia observada.

Ejercicio1 <- data.frame(
  Fenotipo = c("Vermillion, Crossveinless",
               "Cut",
               "Vermillion, Cut",
               "Crossveinless",
               "Vermillion",
               "Crossveinless, Cut",
               "WT",
               "Vermillion, Crossveinless, Cut"),
  Frecuencias = c(766,759,140,158,73,85,3,2)
)
Ejercicio2 <- data.frame(
  Genotipo = c("ec cv- ct",
               "ec- cv ct-",
               "ec- cv- ct",
               "ec cv ct-",
               "ec- cv ct",
               "ec cv- ct-",
               "ec cv ct",
               "ec- cv- ct-"),
  Frecuencias = c(2207,2125,273,265,223,217,5,3)
)
Ejercicio3 <- data.frame(
  Genotipo = c("k e- cd",
               "k- e cd-",
               "k e cd-",
               "k- e- cd",
               "k e- cd-",
               "k- e cd",
               "k e cd",
               "k- e- cd-"),
  Frecuencias = c(1761,1773,128,138,97,89,6,8)
)
Ejercicio4 <- data.frame(
  Genotipo = c("v- cv ct",
               "v cv- ct-",
               "v- cv- ct",
               "v cv ct-",
               "v- cv- ct-",
               "v cv ct",
               "v- cv ct-",
               "v cv- ct"),
  Frecuencias = c(1580,592,45,40,89,94,3,5)
)
Ejercicio5 <- data.frame(
  Genotipo = c("sc- ec- vg-",
               "sc ec vg",
               "sc- ec- vg",
               "sc ec vg-",
               "sc- ec vg-",
               "sc ec- vg",
               "sc- ec vg",
               "sc ec- vg-"),
  Frecuencias = c(235,241,243,233,12,14,14,16)
)

write.csv(Ejercicio1, "01_Data/Ejercicio1_3_puntos.csv")
write.csv(Ejercicio2, "01_Data/Ejercicio2_3_puntos.csv")
write.csv(Ejercicio3, "01_Data/Ejercicio3_3_puntos.csv")
write.csv(Ejercicio4, "01_Data/Ejercicio4_3_puntos.csv")
write.csv(Ejercicio5, "01_Data/Ejercicio5_3_puntos.csv")

# Funcion de la prueba con base de datos

prueba3puntos <- function(data, frecuencias, wt = FALSE, fr.wt = FALSE){
  orden <- c ()
  vector <- data[[frecuencias]]
  for (i in 1:(length(vector))){
    orden <- c(orden, vector[which(vector == sort(vector)[length(vector)])])
    vector <- vector[-which(vector == sort(vector)[length(vector)])]
  }
  frecuencias_fenotipicas <- data.frame(
    parentales = c (orden[1],orden[2]),
    recombinantes_simples_1 = c (orden[3], orden[4]),
    recombinantes_simples_2 = c(orden[5], orden[6]),
    recombinantes_dobles = c(orden[7], orden[8])
  )
  dz1 <- ((sum(frecuencias_fenotipicas$recombinantes_simples_1)/sum(frecuencias_fenotipicas))+
            (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas)))*100
  dz2 <- ((sum(frecuencias_fenotipicas$recombinantes_simples_2)/sum(frecuencias_fenotipicas))+
            (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas)))*100
  cc <- (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas))/
    ((dz1/100)*(dz2/100))
  ci <- abs(1-cc)
  if (all(c(wt != FALSE, fr.wt != FALSE)) == TRUE){
    if (length(wt) == 3){
      gen1 <- wt[1]
      gen2 <- wt[2]
      gen3 <- wt[3]
      if (fr.wt == orden[1]){
        genotipos <- data.frame(
          genotipo = c(
            Parental_1 = paste(gen1,gen2,gen3),
            Parental_2 = paste(gen1,"-",gen2,"-",gen3,"-"),
            Recombinante_simple_zona_1_1 = paste(gen1,gen2,"-",gen3,"-"),
            Recombinante_simple_zona_1_2 = paste(gen1,"-",gen2,gen3),
            Recombinante_simple_zona_2_1 = paste(gen1,gen2,gen3,"-"),
            Recombinante_simple_zona_2_2 = paste(gen1,"-",gen2,"-",gen3),
            Recombinante_doble_1 = paste(gen1,gen2,"-",gen3),
            Recombinante_doble_2 = paste(gen1,"-",gen2,gen3,"-")
          ), frecuencias = c(
            orden[1],orden[2],orden[3],orden[4],orden[5],orden[6],orden[7],orden[8]
          )
        )
      }
      if (fr.wt == orden[2]){
        genotipos <- data.frame(
          genotipo = c(
            Parental_1 = paste(gen1,"-",gen2,"-",gen3,"-"),
            Parental_2 = paste(gen1,gen2,gen3),
            Recombinante_simple_zona_1_1 = paste(gen1,"-",gen2,gen3),
            Recombinante_simple_zona_1_2 = paste(gen1,gen2,"-",gen3,"-"),
            Recombinante_simple_zona_2_1 = paste(gen1,"-",gen2,"-",gen3),
            Recombinante_simple_zona_2_2 = paste(gen1,gen2,gen3,"-"),
            Recombinante_doble_1 = paste(gen1,"-",gen2,gen3,"-"),
            Recombinante_doble_2 = paste(gen1,gen2,"-",gen3)
          ), frecuencias = c(
            orden[1],orden[2],orden[3],orden[4],orden[5],orden[6],orden[7],orden[8]
          )
        )
      }
      if (fr.wt == orden[3]){
        genotipos <- data.frame(
          genotipo = c(
            Parental_1 = paste(gen1,gen2,"-",gen3,"-"),
            Parental_2 = paste(gen1,"-",gen2,gen3),
            Recombinante_simple_zona_1_1 = paste(gen1,gen2,gen3),
            Recombinante_simple_zona_1_2 = paste(gen1,"-",gen2,"-",gen3,"-"),
            Recombinante_simple_zona_2_1 = paste(gen1,gen2,"-",gen3),
            Recombinante_simple_zona_2_2 = paste(gen1,"-",gen2,gen3,"-"),
            Recombinante_doble_1 = paste(gen1,gen2,gen3,"-"),
            Recombinante_doble_2 = paste(gen1,"-",gen2,"-",gen3)
          ), frecuencias = c(
            orden[1],orden[2],orden[3],orden[4],orden[5],orden[6],orden[7],orden[8]
          )
        )
      }
      if (fr.wt == orden[4]){
        genotipos <- data.frame(
          genotipo = c(
            Parental_1 = paste(gen1,"-",gen2,gen3),
            Parental_2 = paste(gen1,gen2,"-",gen3,"-"),
            Recombinante_simple_zona_1_1 = paste(gen1,"-",gen2,"-",gen3,"-"),
            Recombinante_simple_zona_1_2 = paste(gen1,gen2,gen3),
            Recombinante_simple_zona_2_1 = paste(gen1,"-",gen2,gen3,"-"),
            Recombinante_simple_zona_2_2 = paste(gen1,gen2,"-",gen3),
            Recombinante_doble_1 = paste(gen1,gen2,gen3,"-"),
            Recombinante_doble_2 = paste(gen1,"-",gen2,"-",gen3)
          ), frecuencias = c(
            orden[1],orden[2],orden[3],orden[4],orden[5],orden[6],orden[7],orden[8]
          )
        )
      }
      if (fr.wt == orden[5]){
        genotipos <- data.frame(
          genotipo = c(
            Parental_1 = paste(gen1,gen2,gen3,"-"),
            Parental_2 = paste(gen1,"-",gen2,"-",gen3),
            Recombinante_simple_zona_1_1 = paste(gen1,gen2,"-",gen3),
            Recombinante_simple_zona_1_2 = paste(gen1,"-",gen2,gen3,"-"),
            Recombinante_simple_zona_2_1 = paste(gen1,gen2,gen3),
            Recombinante_simple_zona_2_2 = paste(gen1,"-",gen2,"-",gen3,"-"),
            Recombinante_doble_1 = paste(gen1,gen2,"-",gen3,"-"),
            Recombinante_doble_2 = paste(gen1,"-",gen2,gen3)
          ), frecuencias = c(
            orden[1],orden[2],orden[3],orden[4],orden[5],orden[6],orden[7],orden[8]
          )
        )
      }
      if (fr.wt == orden[6]){
        genotipos <- data.frame(
          genotipo = c(
            Parental_1 = paste(gen1,"-",gen2,"-",gen3),
            Parental_2 = paste(gen1,gen2,gen3,"-"),
            Recombinante_simple_zona_1_1 = paste(gen1,"-",gen2,gen3,"-"),
            Recombinante_simple_zona_1_2 = paste(gen1,gen2,"-",gen3),
            Recombinante_simple_zona_2_1 = paste(gen1,"-",gen2,"-",gen3,"-"),
            Recombinante_simple_zona_2_2 = paste(gen1,gen2,gen3),
            Recombinante_doble_1 = paste(gen1,"-",gen2,gen3),
            Recombinante_doble_2 = paste(gen1,gen2,"-",gen3,"-")
          ), frecuencias = c(
            orden[1],orden[2],orden[3],orden[4],orden[5],orden[6],orden[7],orden[8]
          )
        )
      }
      if (fr.wt == orden[7]){
        genotipos <- data.frame(
          genotipo = c(
            Parental_1 = paste(gen1,gen2,"-",gen3),
            Parental_2 = paste(gen1,"-",gen2,gen3,"-"),
            Recombinante_simple_zona_1_1 = paste(gen1,gen2,gen3,"-"),
            Recombinante_simple_zona_1_2 = paste(gen1,"-",gen2,"-",gen3),
            Recombinante_simple_zona_2_1 = paste(gen1,gen2,"-",gen3,"-"),
            Recombinante_simple_zona_2_2 = paste(gen1,"-",gen2,gen3),
            Recombinante_doble_1 = paste(gen1,gen2,gen3),
            Recombinante_doble_2 = paste(gen1,"-",gen2,"-",gen3,"-")
          ), frecuencias = c(
            orden[1],orden[2],orden[3],orden[4],orden[5],orden[6],orden[7],orden[8]
          )
        )
      }
      if (fr.wt == orden[8]){
        genotipos <- data.frame(
          genotipo = c(
            Parental_1 = paste(gen1,"-",gen2,gen3,"-"),
            Parental_2 = paste(gen1,gen2,"-",gen3),
            Recombinante_simple_zona_1_1 = paste(gen1,"-",gen2,"-",gen3),
            Recombinante_simple_zona_1_2 = paste(gen1,gen2,gen3,"-"),
            Recombinante_simple_zona_2_1 = paste(gen1,"-",gen2,gen3),
            Recombinante_simple_zona_2_2 = paste(gen1,gen2,"-",gen3,"-"),
            Recombinante_doble_1 = paste(gen1,"-",gen2,"-",gen3,"-"),
            Recombinante_doble_2 = paste(gen1,gen2,gen3)
          ), frecuencias = c(
            orden[1],orden[2],orden[3],orden[4],orden[5],orden[6],orden[7],orden[8]
          )
        )
      }
      resultados <- list(
        Frecuencias = print(genotipos),
        Distancias_Zona_1 = print(dz1),
        Distancias_Zona_2 = print(dz2),
        Coeficiente_coincidencia = print(cc),
        Coeficiente_interferencia = print(ci))
    }
  }else{
    resultados <- list(
      Frecuencias = print(frecuencias_fenotipicas),
      Distancias_Zona_1 = print(dz1),
      Distancias_Zona_2 = print(dz2),
      Coeficiente_coincidencia = print(cc),
      Coeficiente_interferencia = print(ci))
  }
}


# Pruebas para practicas como funcionan las cosas

which(Ejercicio5$Frecuencias == sort(Ejercicio5$Frecuencias)[length(Ejercicio5$Frecuencias)])
orden <- c ()
vector <- Ejercicio5$Frecuencias

for (i in 1:(length(vector))){
  orden <- c(orden, vector[which(vector == sort(vector)[length(vector)])])
  vector <- vector[-which(vector == sort(vector)[length(vector)])]
}
orden
vector
Ejercicio1
ejer <- prueba3puntos(Ejercicio1,"Frecuencias")
ejer
ejer2 <- prueba3puntos(Ejercicio1,"Frecuencias", c ("Vermillion","Crossveinless", "Cut"), 3)
ejer2 <- prueba3puntos(Ejercicio1,"Frecuencias")
ejer2$Distancias_Zona_1
x <- c ("Vermillion","Crossveinless", "Cut")
ejer2
ejer3 <- prueba3puntos(Ejercicio5, "Frecuencias", c("sc","ec","vg"), 241)
ejer3
orden <- c ()
vector <- Ejercicio5$Frecuencias
for (i in 1:(length(vector))){
  orden <- c(orden, vector[which(vector == sort(vector)[length(vector)])])
  vector <- vector[-which(vector == sort(vector)[length(vector)])]
}
frecuencias_fenotipicas <- data.frame(
  parentales = c (orden[1],orden[2]),
  recombinantes_simples_1 = c (orden[3], orden[4]),
  recombinantes_simples_2 = c(orden[5], orden[6]),
  recombinantes_dobles = c(orden[7], orden[8])
)
frecuencias_fenotipicas

gen1 <- "cd"
gen2 <- "ct"
gen3 <- "v"

genotipos <- data.frame(
  genotipo = c(
    Parental_1 = paste(gen1,gen2,gen3),
    Parental_2 = paste(gen1,"-",gen2,"-",gen3,"-"),
    Recombinante_simple_zona_1_1 = paste(gen1,gen2,"-",gen3,"-"),
    Recombinante_simple_zona_1_2 = paste(gen1,"-",gen2,gen3),
    Recombinante_simple_zona_2_1 = paste(gen1,gen2,gen3,"-"),
    Recombinante_simple_zona_2_2 = paste(gen1,"-",gen2,"-",gen3),
    Recombinante_doble_1 = paste(gen1,gen2,"-",gen3),
    Recombinante_doble_2 = paste(gen1,"-",gen2,gen3,"-")
  ), frecuencias = c(
    orden[1],orden[2],orden[3],orden[4],orden[5],orden[6],orden[7],orden[8]
  )
)

genotipos


