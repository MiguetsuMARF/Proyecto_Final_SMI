

# primer intento de funcion para prueba de 3 puntos
# Para empezar vamos a trabajar una funcion. Cosas que necesita la funcion
## Ingresar cuales son los alelos
## ingresar las frecuencias de cada genotipo de las pruebas de cruza

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
      print("ERROR \n
            Tu prueba de 3 puntos está mal planeada ya que los individuos parentales tienen genotipos similares")
    }
  }else{
    print("ERROR \n
            Tu prueba de 3 puntos está mal planeada ya que los genes a evaluar deben de tener expresion recesiva, no dominante")## Aqui va lo que pasa si el fenotipo no es recesivo
  }
}





#### Aqui adelante son pruebas aisladas para verificar que cosas del codigo funcionen de forma aislada


expresionparental1 <- c("s","n","s")
if (all(prueba2 == "n")){
  expresion_parental1 <- c(0,0,0)
}else if(any(prueba2 == "n")){
  expresion_parental1 <- c(0,0,0)
  expresion_parental1[which(prueba2 == "s")] <- 1
}else{
  expresion_parental1 <- c(1,1,1)
}
expresion_parental1

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

gen1 <- "sc"
gen2 <- "ec"
gen3 <- "vg"
expresion_parental1 <- c(
  readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 1? s/n")),
  readline(prompt = cat("Hay expresion fenotipica de", gen2,"en el progenitor 1? s/n")),
  readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 1? s/n"))
)
expresion_parental2 <- c(
  readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 2? s/n")),
  readline(prompt = cat("Hay expresion fenotipica de", gen2,"en el progenitor 2? s/n")),
  readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 2? s/n"))
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
}
frecuencias_fenotipicas # INPUT PARA HACER EL MAPA

dz1 <- ((sum(frecuencias_fenotipicas$recombinantes_simples_1)/sum(frecuencias_fenotipicas))+
  (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas)))*100
dz2 <- ((sum(frecuencias_fenotipicas$recombinantes_simples_2)/sum(frecuencias_fenotipicas))+
          (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas)))*100
cc <- (sum(frecuencias_fenotipicas$recombinantes_dobles)/sum(frecuencias_fenotipicas))/
  ((dz1/100)*(dz2/100))
ci <- abs(1-cc)
frecuencias_fenotipicas
dz1
dz2
cc
ci
print(cc)
print(format(cc, digits = 6))

options(digits=8)
(((frecuencias_fenotipicas$recombinantes_simples_1[1]+frecuencias_fenotipicas$recombinantes_simples_1[2])/sum(frecuencias_fenotipicas))+
    ((frecuencias_fenotipicas$recombinantes_dobles[1]+frecuencias_fenotipicas$recombinantes_dobles[2])/sum(frecuencias_fenotipicas)))*100
(((frecuencias_fenotipicas$recombinantes_simples_2[1]+frecuencias_fenotipicas$recombinantes_simples_2[2])/sum(frecuencias_fenotipicas))+
    ((frecuencias_fenotipicas$recombinantes_dobles[1]+frecuencias_fenotipicas$recombinantes_dobles[2])/sum(frecuencias_fenotipicas)))*100

options(digits=8)
((frecuencias_fenotipicas$recombinantes_dobles[1]+frecuencias_fenotipicas$recombinantes_dobles[2])/sum(frecuencias_fenotipicas))/
((((frecuencias_fenotipicas$recombinantes_simples_1[1]+frecuencias_fenotipicas$recombinantes_simples_1[2])/sum(frecuencias_fenotipicas))+
    ((frecuencias_fenotipicas$recombinantes_dobles[1]+frecuencias_fenotipicas$recombinantes_dobles[2])/sum(frecuencias_fenotipicas))))*
(((frecuencias_fenotipicas$recombinantes_simples_2[1]+frecuencias_fenotipicas$recombinantes_simples_2[2])/sum(frecuencias_fenotipicas))+
    ((frecuencias_fenotipicas$recombinantes_dobles[1]+frecuencias_fenotipicas$recombinantes_dobles[2])/sum(frecuencias_fenotipicas)))

((((frecuencias_fenotipicas$recombinantes_simples_1[1]+frecuencias_fenotipicas$recombinantes_simples_1[2])/sum(frecuencias_fenotipicas))+
    ((frecuencias_fenotipicas$recombinantes_dobles[1]+frecuencias_fenotipicas$recombinantes_dobles[2])/sum(frecuencias_fenotipicas))))*
  (((frecuencias_fenotipicas$recombinantes_simples_2[1]+frecuencias_fenotipicas$recombinantes_simples_2[2])/sum(frecuencias_fenotipicas))+
     ((frecuencias_fenotipicas$recombinantes_dobles[1]+frecuencias_fenotipicas$recombinantes_dobles[2])/sum(frecuencias_fenotipicas)))

# ej.1

prueba3puntos.no("EC","CV","CT") -> eccvct
eccvct
i <- 0
si <- c()
while (i == 0){
readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 1? s/n")) -> si
if (si == "s" | si == "n"){
  i <- i + 1
}else{
  print ("Por favor ingresa un caracter valido (s/n)")
}
}
print ("Por favor ingresa un caracter valido (s/n)")

print(paste("hola \n como estas?"))
cat("hola","\n", "como estas?")
