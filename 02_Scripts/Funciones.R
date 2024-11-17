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
  if (dominancia == FALSE){
    expresion_parental1 <- c(
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 1? s/n")),
      readline(prompt = cat("Hay expresion fenotipica de", gen2,"en el progenitor 1? s/n")),
      readline(prompt = cat("Hay expresion fenotipica de", gen3,"en el progenitor 1? s/n"))
    )
    expresion_parental2 <- c(
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 2? s/n")),
      readline(prompt = cat("Hay expresion fenotipica de", gen2,"en el progenitor 2? s/n")),
      readline(prompt = cat("Hay expresion fenotipica de", gen3,"en el progenitor 2? s/n"))
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
      print("ERROR \n
            Tu prueba de 3 puntos está mal planeada ya que los individuos parentales tienen genotipos similares")
    }
  }else{
    print("ERROR \n
            Tu prueba de 3 puntos está mal planeada ya que los genes a evaluar deben de tener expresion recesiva, no dominante")## Aqui va lo que pasa si el fenotipo no es recesivo
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