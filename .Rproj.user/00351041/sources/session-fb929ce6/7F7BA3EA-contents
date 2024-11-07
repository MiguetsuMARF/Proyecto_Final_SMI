

# primer intento de funcion para prueba de 3 puntos
# Para empezar vamos a trabajar una funcion. Cosas que necesita la funcion
## Ingresar cuales son los alelos
## ingresar las frecuencias de cada genotipo de las pruebas de cruza

prueba3 <- funtion(gen1,gen2,gen3){
  rec <- readline(prompt = "La expresion fenotipica es recesiva? s/n") # Aqui es necesario que se cicle hasta que de una respuesta que sea s o n.
  if (rec == "s"){
    parental1 <- c(
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 1? s/n")),
      readline(prompt = cat("Hay expresion fenotipica de", gen2,"en el progenitor 1? s/n")),
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 1? s/n"))
    )
    if (any(parental1 == "n")){
      expresion_parental1 <- which(parental1 == "s")
    }else{
      expresion_parental1 <- c(1,2,3)
    }
    parental2 <- c(
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 2? s/n")),
      readline(prompt = cat("Hay expresion fenotipica de", gen2,"en el progenitor 2? s/n")),
      readline(prompt = cat("Hay expresion fenotipica de", gen1,"en el progenitor 2? s/n"))
    )
    if (any(parental2 == "n")){
      expresion_parental2 <- which(parental2 == "s")
    }else{
      expresion_parental2 <- c(1,2,3)
    }
    if (all((expresion_parental1 == expresion_parental2) == FALSE)){
    data.frame(
      
    )
    }
  }
}

prueba <- function(gen1){
  readline(prompt = cat("eres", gen1,"con respecto a tu padre?"))
}

prueba("igual")
prueba("A")
prueba2 <- c("s","s","n")
if (any(prueba2 == "n")){
  expresion_parental <- which(prueba2 == "s")
}else{
  expresion_parental <- c(1,2,3)
}
expresion_parental
prueba3 <- c("n","n","s")
if (any(prueba3 == "n")){
  expresion_parental2 <- which(prueba3 == "s")
}else{
  expresion_parental2 <- c(1,2,3)
}
expresion_parental == expresion_parental2
