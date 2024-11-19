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

prueba3puntos.inv <- function(Coeficiente_consistencia, Distancia_zona1, Distancia_zona2, 
                              gen1_parental = "A", gen2_parental = "B", gen3_parental = "C", 
                              total = FALSE, inf.observados = FALSE){
  frec_dobles_recombinantes <- ((Distancia_zona1/100)*(Distancia_zona2/100))*Coeficiente_consistencia
  frec_simples_zona1 <- ((Distancia_zona1/100)-frec_dobles_recombinantes)
  frec_simples_zona2 <- ((Distancia_zona2/100)-frec_dobles_recombinantes)
  frec_parentales <- 1 - (frec_simples_zona2+frec_simples_zona1)
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
        Observados_esperados = c(num_parentales/2,num_parentales/2,num_simples_zona1/2,num_simples_zona1/2,
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
        Observados_esperados = c(num_parentales,num_simples_zona1,num_simples_zona2,num_dobles_recombinantes),
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

Mapa_genetico<- function(x, etiquetas = FALSE) {
  if (ncol(x) != nrow(x)) {
    print("Por favor introduce una matriz cuadrada")
  } else
    if (etiqueta == FALSE){
      rownames(x) <- LETTERS[1:nrow(x)]
      colnames(x) <- LETTERS[1:ncol(x)]
    } else {
      
      rownames(x) <- etiqueta
      colnames(x) <- etiqueta
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