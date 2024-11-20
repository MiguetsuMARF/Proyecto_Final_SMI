library("qtl")
library(ape)

prueba3puntos.no <- function(gen1,gen2,gen3, dominancia = FALSE){
  posibles_combinaciones <- list(
    c("s","s","s"),
    c("n","s","s"),
    c("n","s","n"),
    c("n","n","s"),
    c("n","n","n"),
    c("s","n","s"),
    c("s","n","n"),
    c("s","s","n")
  )
  a <- 0
  b <- 0
  c <- 0
  d <- 0
  e <- 0
  f <- 0
  if (dominancia == FALSE){
    while (a == 0){
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 1? s/n")) -> parental1_1
      if (parental1_1 == "s" | parental1_1 == "n"){
        a <- a + 1
      }else{
        print ("Por favor ingresa un caracter valido (s/n)")
      }
    }
    while (b == 0){
      readline(prompt = cat("Hay expresion fenotipica de", gen2,"en el progenitor 1? s/n")) -> parental1_2
      if (parental1_2 == "s" | parental1_2 == "n"){
        b <- b + 1
      }else{
        print ("Por favor ingresa un caracter valido (s/n)")
      }
    }
    while (c == 0){
      readline(prompt = cat("Hay expresion fenotipica de", gen3,"en el progenitor 1? s/n")) -> parental1_3
      if (parental1_3 == "s" | parental1_3 == "n"){
        c <- c + 1
      }else{
        print ("Por favor ingresa un caracter valido (s/n)")
      }
    }
    expresion_parental1 <- c(
      parental1_1,
      parental1_2,
      parental1_3)
    while (d == 0){
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 2? s/n")) -> parental2_1
      if (parental2_1 == "s" | parental2_1 == "n"){
        d <- d + 1
      }else{
        print ("Por favor ingresa un caracter valido (s/n)")
      }
    }
    while (e == 0){
      readline(prompt = cat("Hay expresion fenotipica de", gen2,"en el progenitor 2? s/n")) -> parental2_2
      if (parental2_2 == "s" | parental2_2 == "n"){
        e <- e + 1
      }else{
        print ("Por favor ingresa un caracter valido (s/n)")
      }
    }
    while (f == 0){
      readline(prompt = cat("Hay expresion fenotipica de", gen3,"en el progenitor 2? s/n")) -> parental2_3
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
    if (all((expresion_parental1 == expresion_parental2) == FALSE)){
      if (all (posibles_combinaciones[[1]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, gen3, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: "))))
        )
      }else if(all (posibles_combinaciones[[2]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen2,gen3, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: "))))
        )
      }else if(all (posibles_combinaciones[[3]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen2,gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: "))))
        )
      }else if(all (posibles_combinaciones[[4]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen2,gen3, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: "))))
        )
      }else if(all (posibles_combinaciones[[5]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen2,gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: "))))
        )
      }else if(all (posibles_combinaciones[[6]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, gen3, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: "))))
        )
      }else if(all (posibles_combinaciones[[7]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, gen3, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: "))))
        )
      }else if(all (posibles_combinaciones[[8]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")),
                                    readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")))),
          recombinantes_simples_1 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")))),
          recombinantes_simples_2 = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                                 readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")))),
          recombinantes_dobles = as.numeric(c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")),
                                              readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, gen3, "?: "))))
        )
      }
      dz1 <- ((sum(frecuencias_fenotipicas$recombinantes_simples_1)/sum(frecuencias_fenotipicas))+
                (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas)))*100
      dz2 <- ((sum(frecuencias_fenotipicas$recombinantes_simples_2)/sum(frecuencias_fenotipicas))+
                (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas)))*100
      cc <- (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas))/
        ((dz1/100)*(dz2/100))
      ci <- abs(1-cc)
      resultados <- list(
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

prueba3puntos <- function(data, frecuencias, wt = FALSE, fr.wt = FALSE){
  orden <- c ()
  vector <- data[[frecuencias]]
  orden <- sort(vector, decreasing = TRUE)
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

prueba3puntos.inv <- function(Coeficiente_consistencia, Distancia_zona1, Distancia_zona2, 
                              gen_parental = c("A","B","C"), total = FALSE, inf.observados = FALSE){
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
        Frecuencia_dobles_Recombinantes = frec_dobles_recombinantes,
        Distancias_Zona_1 = Distancia_zona1,
        Distancias_Zona_2 = Distancia_zona2
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
        Frecuencia_dobles_Recombinantes = frec_dobles_recombinantes,
        Distancias_Zona_1 = Distancia_zona1,
        Distancias_Zona_2 = Distancia_zona2
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
      Frecuencia_dobles_Recombinantes = frec_dobles_recombinantes,
      Distancias_Zona_1 = Distancia_zona1,
      Distancias_Zona_2 = Distancia_zona2
    )
  }
}

as.mapa.genetico.matrix <- function(x, etiquetas = FALSE) {
  if (ncol(x) != nrow(x)) {
    print("Por favor introduce una matriz cuadrada")
  } else
    if (all(etiquetas != FALSE) & nrow(x) == length(etiquetas)){
      rownames(x) <- etiquetas
      colnames(x) <- etiquetas
    } else {
      rownames(x) <- LETTERS[1:nrow(x)]
      colnames(x) <- LETTERS[1:ncol(x)]
    }
  
  clusters <- hclust(as.dist(x), method = "average")
  
  nuevo_arreglo <- clusters$labels[clusters$order]
  
  matriz_ordenada <- x[nuevo_arreglo, nuevo_arreglo]
  
  nueva_diagonal<-diag(matriz_ordenada[-1, -ncol(matriz_ordenada)])
  posiciones<-cumsum(c(0,nueva_diagonal))
  
  
  plot(
    posiciones, rep(0, length(posiciones)), 
    pch = 16, xlab = "Distancia (cM)", ylab = "",col = "darkblue",
    axes =FALSE, main = "Mapa genético"
  )
  text(posiciones, rep(0, length(posiciones)), labels = nuevo_arreglo, pos = 3)
  axis(1, at = posiciones, labels = posiciones)
  
}

as.mapa.genetico <- function(data, Genes = c("A","B","C")) {
  
  Nuestro_primer_mapa <-sim.map(len = rep(100,1), n.mar = c(3, 1), anchor.tel = FALSE, include.x = FALSE,
                                sex.sp =FALSE, eq.spacing = FALSE) #Objeto base
  
  names(Nuestro_primer_mapa) <- "Prueba de 3 puntos"
  
  # Modificar para que el usuario meta el nombre de los genes
  names(Nuestro_primer_mapa[["Prueba de 3 puntos"]]) <- Genes
  
  Nuestro_primer_mapa[["Prueba de 3 puntos"]][[Genes[2]]] <- data$Distancias_Zona_1
  Nuestro_primer_mapa[["Prueba de 3 puntos"]][[Genes[3]]] <- data$Distancias_Zona_1 +  data$Distancias_Zona_2
  
  linkmap(Nuestro_primer_mapa, chr = "Prueba de 3 puntos", m.cex = 0.8, interval = TRUE) -> Mapa_genetico
  
  print(Mapa_genetico)
}

linkmap <- function(object, chr, chr.space = 2, m.cex = 0.6, interval = FALSE, ruler = FALSE, ...){
  # VERSION: 1.1.0
  # object: a "cross" object from R/qtl, or a "map" class from the output of "pull.map" in R/qtl, or a data frame with marker column, chromosme column and position column named as "mar", "chr" and "pos", respectively
  # chr: a vector of chromosome names that need to be drawn.
  # chr.space: space between each chromosomes
  # m.cex: font size
  # interval: NULL/TRUE/FALSE: plot no distance/marker interval/absolute distance. Default is absolute distance.
  # ruler: whether to draw a common left ruler on the left
  # ...: other plot parameters
  if ("data.frame" %in% class(object)){ # transform to a list
    pos = object$pos
    names(pos) = object$mar
    map = split(pos,object$chr)
  } else if ("cross" %in% class(object)){
    map <- pull.map(object)
  } else map = object # a map object or list
  dots <- list(...) # extra parameters inputed by user
  old.xpd <- par("xpd")
  par(xpd = TRUE)
  on.exit(par(xpd = old.xpd))
  
  if(!missing(chr)) {
    if(any(is.na(pmatch(chr, names(map)))))
      stop("Some names of chromosome(s) subset do not match names of map.")
    map <- map[chr]
  }
  n.chr <- length(map)
  #mt <- list()
  
  maxlen <- max(unlist(lapply(map, max)))
  minlen <- min(unlist(lapply(map, min)))
  omap <- map # omap is original map
  if(!is.na(pmatch("cex", names(dots))))
    dots$cex <- NULL
  #    else cex <- par("cex")
  chrpos <- seq(1, n.chr * chr.space, by = chr.space)
  thelim <- range(chrpos) + c(-1.0, 1.0)
  
  # function to get actual plotting postion
  finalPos = function(pos, maxlen) {#pos is a vector of positions of A genetic map; maxlen is the length of chromosome segments
    posnew = pos
    if (length(posnew) > 1){
      conv <- par("pin")[2]/maxlen # pin is The current plot dimensions, (width, height), in inches.
      for(j in 1:(length(pos) - 1)){
        ch <- posnew[j + 1]*conv - (posnew[j]*conv + 10*par("csi")*m.cex/9) # csi is the default font height
        #cat(ch)
        if (ch < 0){
          temp <- posnew[j]*conv + 10*par("csi")*m.cex/9
          posnew[j + 1] <- temp/conv
        }
      }
    }
    return(posnew)
  }
  
  # function to get interval
  getInterval = function(pos){#pos is a vector of positions of A genetic map
    pos2 = unique(round(pos,1))
    ll = length(pos2)
    pos3 = (pos2[1:(ll-1)] + pos2[2:ll])/2 # map position
    pos4 = pos2[2:ll] - pos2[1:(ll-1)] # intervals
    return(list(pos3,pos4))
  }
  
  # plot left side
  total_len = maxlen - minlen # in case chromosome did not start from 0
  mt = lapply(map, finalPos, maxlen= total_len) # final label postion on the right
  if (!is.null(interval)) {
    if (interval) {
      map2 = lapply(map, function(x) getInterval(x)[[1]]) # chromsome position for the left
      map3 = lapply(map, function(x) getInterval(x)[[2]]) # intervals
      mt2 = lapply(map2, finalPos, maxlen=total_len) # distance plotting position on the left
    } else {
      map2 = map # left and right are the same
      map3 = map
      mt2 = mt # left and right are the same
    }
  }
  
  maxlen <- max(c(unlist(lapply(omap, max)),unlist(lapply(mt, max))))
  names(mt) <- names(map)
  
  par(mar=c(0.6 ,1.1 ,2.6 ,1.1))
  if (ruler) par(mar=c(0.6, 2.1, 2.6, 1.1))
  
  plot(0, 0, type = "n", ylim = c(maxlen, minlen), xlim = thelim,
       xaxs = "i", ylab = "", xlab = "", axes = FALSE, ...)
  if (ruler) axis(side = 2,  ylim = c(maxlen, minlen))
  else chrpos = chrpos - 0.3
  
  ## Draw chromosomes
  barwidth = 0.14
  seglen = 0.06
  
  for(i in 1:n.chr) {
    # for the right side plotting
    Rstart = chrpos[i] + barwidth # start point for legs on the RIGHT
    segments(Rstart, map[[i]], Rstart + seglen, map[[i]])
    segments(Rstart + seglen, map[[i]], Rstart + seglen*3, mt[[i]])
    segments(Rstart + seglen*3, mt[[i]], Rstart + seglen*4, mt[[i]])
    alis <- list(x = Rstart + seglen*4 + 0.05, y = mt[[i]], labels = names(map[[i]]), adj = c(0, 0.5), cex = m.cex)
    do.call("text", c(alis, dots)) # draw right labels
    # JZ: add distance on the left
    if (!is.null(interval)){
      Lstart = chrpos[i] - barwidth # start point for legs on the LEFT
      segments(Lstart, map2[[i]], Lstart - seglen, map2[[i]])
      segments(Lstart - seglen, map2[[i]], Lstart - seglen*3, mt2[[i]])
      segments(Lstart - seglen*3, mt2[[i]], Lstart - seglen*4, mt2[[i]])
      alisL <- list(x = Lstart - seglen*4 - 0.05, y =  mt2[[i]], labels = format(round(map3[[i]], 1),nsmall=1),adj = c(1, 0.5), cex = m.cex)
      do.call("text", c(alisL, dots)) # draw left labels
    }
    # draw chromosome bar
    map[[i]] <- omap[[i]]
    barl <- chrpos[i] - barwidth/2
    barr <- chrpos[i] + barwidth/2
    segments(barl, min(map[[i]]), barl, max(map[[i]]), lwd = 3)
    segments(barr, min(map[[i]]), barr, max(map[[i]]), lwd = 3)
    segments(barl - barwidth/2, map[[i]], barr + barwidth/2, map[[i]]) # bar ribs
    # attempt to put curves at ends of chromosomes
    rs <- seq(0,pi,len=100)
    r <- (barr - barl)/2 # radius
    xunit = par("pin")[1]/abs(par("xaxp")[2] - par("xaxp")[1])
    yunit = par("pin")[2]/abs(par("yaxp")[2] - par("yaxp")[1])
    xseq <- r*cos(rs) 
    yseq <- r*sin(rs)*(xunit/yunit)
    lines(xseq + chrpos[i], min(map[[i]]) - yseq, lwd=3)
    lines(xseq + chrpos[i], max(map[[i]]) + yseq, lwd=3)
  }
  axis(side = 3, at = chrpos, labels = names(map), tick = F, cex.axis=1.5) # side = 1 for bottom, side=3 for top
  #if(is.na(pmatch("main", names(dots))) & !as.logical(sys.parent()))
  #  title("Genetic Map")
  invisible(list(mt = mt, map = map, chrpos = chrpos))
}