

# primer intento de funcion para prueba de 3 puntos
# Para empezar vamos a trabajar una funcion. Cosas que necesita la funcion
## Ingresar cuales son los alelos
## ingresar las frecuencias de cada genotipo de las pruebas de cruza

prueba3 <- funtion(gen1,gen2,gen3, dominancia = FALSE){
  posibles_combinaciones <- list(
    c(1,1,1),
    c(0,1,1),
    c(0,1,0),
    c(0,0,1),
    c(0,0,0),
    c(1,0,1),
    c(1,0,0),
    c(1,1,0)
  )
  if (dominancia == FALSE){
    parental1 <- c(
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 1? s/n")),
      readline(prompt = cat("Hay expresion fenotipica de", gen2,"en el progenitor 1? s/n")),
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 1? s/n"))
    )
    if (all(parental1 == "n")){
      expresion_parental1 <- c(0,0,0)
    }else if(any(parental1 == "n")){
      expresion_parental1 <- c(0,0,0)
      expresion_parental1[which(parental1 == "s")] <- 1
    }else{
      expresion_parental1 <- c(1,1,1)
    }
    parental2 <- c(
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 2? s/n")),
      readline(prompt = cat("Hay expresion fenotipica de", gen2,"en el progenitor 2? s/n")),
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 2? s/n"))
    )
    if (all(parental2 == "n")){
      expresion_parental2 <- c(0,0,0)
    }else if(any(parental2 == "n")){
      expresion_parental2 <- c(0,0,0)
      expresion_parental2[which(parental2 == "s")] <- 1
    }else{
      expresion_parental2 <- c(1,1,1)
    }
    if (all((expresion_parental1 == expresion_parental2) == FALSE)){
      if (all (posibles_combinaciones[[1]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                         readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: "))),
          recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, gen3, "?: "))),
          recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: "))),
          recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")),
                                   readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")))
        )
      }else if(all (posibles_combinaciones[[2]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen2,gen3, "?: ")),
                         readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: "))),
          recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: "))),
          recombinantes_simples_2 = c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: "))),
          recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")),
                                   readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")))
        )
      }else if(all (posibles_combinaciones[[3]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")),
                         readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: "))),
          recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: "))),
          recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen2,gen3, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: "))),
          recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                   readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")))
        )
      }else if(all (posibles_combinaciones[[4]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")),
                         readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: "))),
          recombinantes_simples_1 = c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: "))),
          recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: "))),
          recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen2,gen3, "?: ")),
                                   readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")))
        )
      }else if(all (posibles_combinaciones[[5]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                         readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: "))),
          recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen2,gen3, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: "))),
          recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: "))),
          recombinantes_dobles = c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")),
                                   readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")))
        )
      }else if(all (posibles_combinaciones[[6]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")),
                         readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: "))),
          recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: "))),
          recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, gen3, "?: "))),
          recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                   readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")))
        )
      }else if(all (posibles_combinaciones[[7]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")),
                         readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, gen3, "?: "))),
          recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: "))),
          recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: "))),
          recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")),
                                   readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")))
        )
      }else if(all (posibles_combinaciones[[8]] == expresion_parental1)){
        frecuencias_fenotipicas <- data.frame(
          parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")),
                         readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: "))),
          recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: "))),
          recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                      readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: "))),
          recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")),
                                   readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, gen3, "?: ")))
        )
      }
      
      
    }else{
      print("ERROR \n
            Tu prueba de 3 puntos está mal planeada ya que los individuos tienen genotipos similares")
    }
  }else{
    ## Aqui va lo que pasa si el fenotipo no es recesivo
  }
  # Aqui vamos a poner todo lo que queremos que de como resultado la funcion
  return(frecuencias_fenotipicas) # La base de datos que contiene ordenados los datos de frecuencia
}


#### Aqui adelante son pruebas aisladas para verificar que cosas del codigo funcionen de forma aislada


prueba2 <- c("s","n","s")
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
  c(1,1,1),
  c(0,1,1),
  c(0,1,0),
  c(0,0,1),
  c(0,0,0),
  c(1,0,1),
  c(1,0,0),
  c(1,1,0)
)

gen1 <- "K"
gen2 <- "e"
gen3 <- "cd"
  if (all (posibles_combinaciones[[1]] == expresion_parental1)){
    frecuencias_fenotipicas <- data.frame(
      parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                     readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: "))),
      recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, gen3, "?: "))),
      recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: "))),
      recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")),
                               readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")))
    )
  }else if(all (posibles_combinaciones[[2]] == expresion_parental1)){
    frecuencias_fenotipicas <- data.frame(
      parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen2,gen3, "?: ")),
                     readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: "))),
      recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: "))),
      recombinantes_simples_2 = c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: "))),
      recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")),
                               readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")))
    )
  }else if(all (posibles_combinaciones[[3]] == expresion_parental1)){
    frecuencias_fenotipicas <- data.frame(
      parentales = c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")),
                     readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: "))),
      recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: "))),
      recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen2,gen3, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: "))),
      recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                               readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")))
    )
  }else if(all (posibles_combinaciones[[4]] == expresion_parental1)){
    frecuencias_fenotipicas <- data.frame(
      parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")),
                     readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: "))),
      recombinantes_simples_1 = c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: "))),
      recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: "))),
      recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen2,gen3, "?: ")),
                               readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")))
    )
  }else if(all (posibles_combinaciones[[5]] == expresion_parental1)){
    frecuencias_fenotipicas <- data.frame(
      parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")),
                     readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: "))),
      recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen2,gen3, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: "))),
      recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: "))),
      recombinantes_dobles = c(readline (prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: ")),
                               readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")))
    )
  }else if(all (posibles_combinaciones[[6]] == expresion_parental1)){
    frecuencias_fenotipicas <- data.frame(
      parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")),
                     readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: "))),
      recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: "))),
      recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, gen3, "?: "))),
      recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                               readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: ")))
    )
  }else if(all (posibles_combinaciones[[7]] == expresion_parental1)){
    frecuencias_fenotipicas <- data.frame(
      parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")),
                     readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, gen3, "?: "))),
      recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: "))),
      recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: "))),
      recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")),
                               readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: ")))
    )
  }else if(all (posibles_combinaciones[[8]] == expresion_parental1)){
    frecuencias_fenotipicas <- data.frame(
      parentales = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen2, "?: ")),
                     readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen3, "?: "))),
      recombinantes_simples_1 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de",gen1,gen3, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, "?: "))),
      recombinantes_simples_2 = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, gen2,gen3, "?: ")),
                                  readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que no presentan expresion fenotipica de alguno de los genes (wt)?: "))),
      recombinantes_dobles = c(readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen1, "?: ")),
                               readline(prompt = cat("Cual es la frecuencia de individuos luego de cruza de prueba que presentan expresion fenotipica de", gen2, gen3, "?: ")))
    )
  }

frecuencias_fenotipicas

