
## Necesitamos los datos de frecuencias.

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

# primer intento de funcion para prueba de 3 puntos
# Para empezar vamos a trabajar una funcion. Cosas que necesita la funcion
## Ingresar cuales son los alelos
## ingresar las frecuencias de cada genotipo de las pruebas de cruza

prueba3puntos.no <- function(gen1,gen2,gen3, dominancia = FALSE){
  posibles_combinaciones <- list( # creamos un objeto que contenga todas las posibles combinaciones de fenotipo
    c("s","s","s"),
    c("n","s","s"),
    c("n","s","n"),
    c("n","n","s"),
    c("n","n","n"),
    c("s","n","s"),
    c("s","n","n"),
    c("s","s","n")
  )
  a <- 0 # creamos objetos con valor 0 que son necesarios para los ciclos while durante los readlines posteriores
  b <- 0
  c <- 0
  d <- 0
  e <- 0
  f <- 0
  if (dominancia == FALSE){ # evaluamos que estos genes no se expresen en dominancia, sino en recesivo
    while (a == 0){ # hacemos un ciclo que unicamente reciba s o n como respuesta, y preguntamos si hay expresion de cada gen para ambos parentales
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 1 (Aquel fenotipo con mayor frecuencia en la progiene)? s/n")) -> parental1_1
      if (parental1_1 == "s" | parental1_1 == "n"){
        a <- a + 1
      }else{
        print ("Por favor ingresa un caracter valido (s/n)")
      }
    }
    while (b == 0){
      readline(prompt = cat("Hay expresion fenotipica de", gen2,"en el progenitor 1(Aquel fenotipo con mayor frecuencia en la progiene)? s/n")) -> parental1_2
      if (parental1_2 == "s" | parental1_2 == "n"){
        b <- b + 1
      }else{
        print ("Por favor ingresa un caracter valido (s/n)")
      }
    }
    while (c == 0){
      readline(prompt = cat("Hay expresion fenotipica de", gen3,"en el progenitor 1(Aquel fenotipo con mayor frecuencia en la progiene)? s/n")) -> parental1_3
      if (parental1_3 == "s" | parental1_3 == "n"){
        c <- c + 1
      }else{
        print ("Por favor ingresa un caracter valido (s/n)")
      }
    }
    expresion_parental1 <- c( # asignamos las respuestas de cada gen a un vector que contenga las 3 respuestas para cada parental
      parental1_1,
      parental1_2,
      parental1_3)
    while (d == 0){
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 2(Aquel fenotipo con la segunda mayor frecuencia en la progiene)? s/n")) -> parental2_1
      if (parental2_1 == "s" | parental2_1 == "n"){
        d <- d + 1
      }else{
        print ("Por favor ingresa un caracter valido (s/n)")
      }
    }
    while (e == 0){
      readline(prompt = cat("Hay expresion fenotipica de", gen2,"en el progenitor 2(Aquel fenotipo con la segunda mayor frecuencia en la progiene)? s/n")) -> parental2_2
      if (parental2_2 == "s" | parental2_2 == "n"){
        e <- e + 1
      }else{
        print ("Por favor ingresa un caracter valido (s/n)")
      }
    }
    while (f == 0){
      readline(prompt = cat("Hay expresion fenotipica de", gen3,"en el progenitor 2(Aquel fenotipo con la segunda mayor frecuencia en la progiene)? s/n")) -> parental2_3
      if (parental2_3 == "s" | parental2_3 == "n"){
        f <- f + 1
      }else{
        print ("Por favor ingresa un caracter valido (s/n)")
      }
    }
    expresion_parental2 <- c(
      parental2_1,
      parental2_2,
      parental2_3
    )
    if (all((expresion_parental1 != expresion_parental2))){ # aqui evaluamos que los fenotipos parentales sean opuestos, algo obligatorio para la prueba
      if (all (posibles_combinaciones[[1]] == expresion_parental1)){ # A partir de aqui comparamos el fenotipo parental con mayor frecuencia con las posibles combinaciones antes formadas, de tal forma podemos categorizar las frecuencias para los calculos posteriores. 
        frecuencias_fenotipicas <- data.frame( # creamos un data frame que categorice las frecuencias en los 4 subgrupos de progiene posibles a partir de las combinaciones antes establecidas.
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen1, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen2, "y", gen3, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen2, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen3, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen3, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen2, "?: "))))
        )
      }else if(all (posibles_combinaciones[[2]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen2, "y",gen3, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen1, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen2, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1, "y",gen3, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen3, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen2, "?: "))))
        )
      }else if(all (posibles_combinaciones[[3]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen2, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen3, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen2, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen2, "y",gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen1, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: "))))
        )
      }else if(all (posibles_combinaciones[[4]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen3, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen2, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen2, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen3, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen2, "y",gen3, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen1, "?: "))))
        )
      }else if(all (posibles_combinaciones[[5]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen2, "y",gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen1, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen2, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen2, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen3, "?: "))))
        )
      }else if(all (posibles_combinaciones[[6]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen3, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen2, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen2, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen3, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen1, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen2,  "y",gen3, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: "))))
        )
      }else if(all (posibles_combinaciones[[7]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen1, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen2,  "y",gen3, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen2, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen2, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen3, "?: "))))
        )
      }else if(all (posibles_combinaciones[[8]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1, "y",gen2, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen3, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de",gen1, "y",gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen2, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen1, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica unicamente de", gen2, "y", gen3, "?: "))))
        )
      } # Finalmente calculamos las distancias y los coeficientes utilizando las frecuencias ya categorizadas en el dataframe frecuencias_fenotipicas
      dz1 <- ((sum(frecuencias_fenotipicas$recombinantes_simples_1)/sum(frecuencias_fenotipicas))+
                (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas)))*100
      dz2 <- ((sum(frecuencias_fenotipicas$recombinantes_simples_2)/sum(frecuencias_fenotipicas))+
                (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas)))*100
      cc <- (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas))/
        ((dz1/100)*(dz2/100))
      ci <- abs(1-cc)
      resultados <- list( # y creamos una lista que contenga todos los resultados para que sea el resultado de la funcion.
        Frecuencias = print(frecuencias_fenotipicas),
        Distancias_Zona_1 = print(paste("La distancia en zona 1 es:", dz1)),
        Distancias_Zona_2 = print(paste("La distancia en zona 2 es:", dz2)),
        Coeficiente_coincidencia = print(paste("El coeficiente de coincidencia es:", cc)),
        Coeficiente_interferencia = print(paste("El coeficiente de interferencia es:", ci)))
    }else{
      cat("ERROR", "\n",
          "Tu prueba de 3 puntos está mal planeada ya que los individuos parentales tienen genotipos similares")
    }
  }else{
    cat("ERROR", "\n",
        "Tu prueba de 3 puntos está mal planeada ya que los genes a evaluar deben de tener expresion recesiva, no dominante")## Aqui va lo que pasa si el fenotipo no es recesivo
  }
}

# Practicando con la funcion sin base de datos

prueba3puntos.no("EC","CV","CT") -> eccvct
eccvct

# Funcion de la prueba con base de datos

prueba3puntos <- function(data, frecuencias, wt = FALSE, fr.wt = FALSE){
  orden <- c ()
  vector <- data[[frecuencias]] # Extraemos las frecuencias de la base de datos y las asignamos como vector.
  orden <- sort(vector, decreasing = TRUE) # reordenamos el vector de orden ascendente a descendente 
  frecuencias_fenotipicas <- data.frame( # debido a las caracteristicas de las frecuencias cuando hay ligamiento podemos categorizar a partir del orden.
    parentales = c (orden[1],orden[2]),
    recombinantes_simples_1 = c (orden[3], orden[4]),
    recombinantes_simples_2 = c(orden[5], orden[6]),
    recombinantes_dobles = c(orden[7], orden[8])
  ) # Finalmente se calculan las distancias y coeficientes
  dz1 <- ((sum(frecuencias_fenotipicas$recombinantes_simples_1)/sum(frecuencias_fenotipicas))+
            (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas)))*100
  dz2 <- ((sum(frecuencias_fenotipicas$recombinantes_simples_2)/sum(frecuencias_fenotipicas))+
            (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas)))*100
  cc <- (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas))/
    ((dz1/100)*(dz2/100))
  ci <- abs(1-cc)
  if (all(c(wt != FALSE, fr.wt != FALSE)) == TRUE){ # en caso de que se ingrese el genotipo del wt y la frecuencia 
    if (length(wt) == 3){ # se comprueba que el vector ingresado si tenga 3 elementos, si no entonces no se ejecuta el codigo adicional
      gen1 <- wt[1]
      gen2 <- wt[2]
      gen3 <- wt[3] # se extraen los nombres de los genes
      if (fr.wt == orden[1]){ # se busca en que posicion del orden esta el wt y se asigna un tipo de combinacion a partir de esa posicion para construir los posibles genotipos
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
      resultados <- list( # finalmente se asignan los resultados para que sea el resultado de la funcion
        Frecuencias = print(genotipos),
        Distancias_Zona_1 = print(dz1),
        Distancias_Zona_2 = print(dz2),
        Coeficiente_coincidencia = print(cc),
        Coeficiente_interferencia = print(ci))
    }
  }else{ # si no se ingresaron los argumentos adicionales entonces la data.frame sera la de frecuencias fenotipicas y ya.
    resultados <- list(
      Frecuencias = print(frecuencias_fenotipicas),
      Distancias_Zona_1 = print(dz1),
      Distancias_Zona_2 = print(dz2),
      Coeficiente_coincidencia = print(cc),
      Coeficiente_interferencia = print(ci))
  }
}


# Practicando con la funcion de prueba3puntos

prueba3puntos(Ejercicio2, "Frecuencias")
prueba3puntos(Ejercicio1, "Frecuencias", c("v", "ct","cv"), 3)

# haciendo la funcion inversa

prueba3puntos.inv <- function(Coeficiente_consistencia, Distancia_zona1, Distancia_zona2, 
                              gen_parental = c("A","B","C"), 
                              total = FALSE, inf.observados = FALSE){
  frec_dobles_recombinantes <- ((Distancia_zona1/100)*(Distancia_zona2/100))*Coeficiente_consistencia
  frec_simples_zona1 <- ((Distancia_zona1/100)-frec_dobles_recombinantes)
  frec_simples_zona2 <- ((Distancia_zona2/100)-frec_dobles_recombinantes)
  frec_parentales <- 1 - (frec_simples_zona2+frec_simples_zona1)
  gen1_parental <- gen_parental[1]
  gen2_parental <- gen_parental[2]
  gen3_parental <- gen_parental[3]
  if (total != FALSE){ # Aqui vamos a poner lo que pasa si total = a un numero
    num_dobles_recombinantes <- frec_dobles_recombinantes*total
    num_simples_zona1 <- frec_simples_zona1*total
    num_simples_zona2 <- frec_simples_zona2*total
    num_parentales <- frec_parentales*total
    if (inf.observados == TRUE){ # Aqui vamos a poner lo que pasa si total != FALSE e inf.observados != FALSE
      Data.frame_resultados <- data.frame(
        Tipo_cruza = c("Parental","Parental","Recombinante simple de zona 1", "Recombinante simple de zona 1",
                       "Recombinante simple de zona 2","Recombinante simple de zona 2",
                       "Recombinante doble","Recombinante doble"),
        Genotipo = c(paste(gen1_parental,gen2_parental,gen3_parental),
                     paste(gen1_parental,"-",gen2_parental,"-",gen3_parental,"-"),
                     paste(gen1_parental,gen2_parental,"-",gen3_parental,"-"),
                     paste(gen1_parental,"-",gen2_parental,gen3_parental),
                     paste(gen1_parental,gen2_parental,gen3_parental,"-"),
                     paste(gen1_parental,"-",gen2_parental,"-",gen3_parental),
                     paste(gen1_parental,gen2_parental,"-",gen3_parental),
                     paste(gen1_parental,"-",gen2_parental,gen3_parental,"-")),
        Esperados = c(num_parentales/2,num_parentales/2,num_simples_zona1/2,num_simples_zona1/2,
                      num_simples_zona2/2,num_simples_zona2/2,num_dobles_recombinantes/2,num_dobles_recombinantes/2),
        Frecuencia_esperada = c(frec_parentales/2, frec_parentales/2,frec_simples_zona1/2,frec_simples_zona1/2,
                                frec_simples_zona2/2,frec_simples_zona2/2,frec_dobles_recombinantes/2,frec_dobles_recombinantes/2)
      )
      resultados <- list(
        Resultados_estimacion = Data.frame_resultados,
        Frecuencia_parental = frec_parentales,
        Frecuencia_simple_zona1 = frec_simples_zona1,
        Frecuencia_simple_zona2 = frec_simples_zona2,
        Frecuencia_dobles_Recombinantes = frec_dobles_recombinantes
        
      )
    } else{ # Aqui vamos a poner lo que pasa si total != FALSE pero inf.observados == FALSE
      Data.frame_resultados <- data.frame(
        Tipo_cruza = c("Parental","Recombinante simple de zona 1", "Recombinante simple de zona 2",
                       "Recombinante doble"),
        Genotipo = c(c(paste(gen1_parental,gen2_parental,gen3_parental,"and",gen1_parental,"-",gen2_parental,"-",gen3_parental,"-")),
                     c(paste(gen1_parental,gen2_parental,"-",gen3_parental,"-","and",gen1_parental,"-",gen2_parental,gen3_parental)),
                     c(paste(gen1_parental,gen2_parental,gen3_parental,"-","and",gen1_parental,"-",gen2_parental,"-",gen3_parental)),
                     c(paste(gen1_parental,gen2_parental,"-",gen3_parental,"and",gen1_parental,"-",gen2_parental,gen3_parental,"-"))),
        Esperados = c(num_parentales,num_simples_zona1,num_simples_zona2,num_dobles_recombinantes),
        Frecuencia = c(frec_parentales,frec_simples_zona1,frec_simples_zona2,frec_dobles_recombinantes)
      )
      resultados <- list(
        Resultados_estimacion = Data.frame_resultados,
        Frecuencia_parental = frec_parentales,
        Frecuencia_simple_zona1 = frec_simples_zona1,
        Frecuencia_simple_zona2 = frec_simples_zona2,
        Frecuencia_dobles_Recombinantes = frec_dobles_recombinantes
        
      )
    }
  } else{ # Aqui vamos a poner lo que pasa si total = FALSE
    Data.frame_resultados <- data.frame(
      Tipo_cruza = c("Parental","Recombinante simple de zona 1", "Recombinante simple de zona 2",
                     "Recombinante doble"),
      Genotipo = c(c(paste(gen1_parental,gen2_parental,gen3_parental,"and",gen1_parental,"-",gen2_parental,"-",gen3_parental,"-")),
                   c(paste(gen1_parental,gen2_parental,"-",gen3_parental,"-","and",gen1_parental,"-",gen2_parental,gen3_parental)),
                   c(paste(gen1_parental,gen2_parental,gen3_parental,"-","and",gen1_parental,"-",gen2_parental,"-",gen3_parental)),
                   c(paste(gen1_parental,gen2_parental,"-",gen3_parental,"and",gen1_parental,"-",gen2_parental,gen3_parental,"-"))),
      Frecuencia = c(frec_parentales,frec_simples_zona1,frec_simples_zona2,frec_dobles_recombinantes)
    )
    resultados <- list(
      Resultados_estimacion = Data.frame_resultados,
      Frecuencia_parental = frec_parentales,
      Frecuencia_simple_zona1 = frec_simples_zona1,
      Frecuencia_simple_zona2 = frec_simples_zona2,
      Frecuencia_dobles_Recombinantes = frec_dobles_recombinantes
      
    )
  }
}

# practicando con la funcion inversa

eje2 <- prueba3puntos.inv(0.994035,7,5.03)
eje2

eje3 <- prueba3puntos.inv(0.994035,7,5.03, total = 4000)
eje3

eje <- prueba3puntos.inv(0.994035,7,5.03,gen_parental = c("ct","v","cv"),
                         total = 4000, inf.observados = TRUE)
eje

