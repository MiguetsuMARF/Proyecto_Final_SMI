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

# practicando con la funcion

eje2 <- prueba3puntos.inv(0.994035,7,5.03)
eje2

eje3 <- prueba3puntos.inv(0.994035,7,5.03, total = 4000)
eje3

eje <- prueba3puntos.inv(0.994035,7,5.03,gen_parental = c("ct","v","cv"),
                  total = 4000, inf.observados = TRUE)
eje



