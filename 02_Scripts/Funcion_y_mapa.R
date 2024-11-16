          # Genrando mapa a partir del resultado de la funcion #

frecuencias_fenotipicas # Resultado de la función
str(frecuencias_fenotipicas)

# Primero haré un codigo para obtener las distancias a partir de la info de las
# frecuencias fenotipicas.

frecuencias_fenotipicas$parentales <- as.numeric(frecuencias_fenotipicas$parentales)
frecuencias_fenotipicas$recombinantes_simples_1 <- as.numeric(frecuencias_fenotipicas$recombinantes_simples_1)
frecuencias_fenotipicas$recombinantes_simples_2 <- as.numeric(frecuencias_fenotipicas$recombinantes_simples_2)
frecuencias_fenotipicas$recombinantes_dobles <- as.numeric(frecuencias_fenotipicas$recombinantes_dobles)

sum(frecuencias_fenotipicas$recombinantes_simples_1,
    frecuencias_fenotipicas$recombinantes_dobles) -> numerador_1

sum(frecuencias_fenotipicas$recombinantes_simples_2,
    frecuencias_fenotipicas$recombinantes_dobles) -> numerador_2

sum(frecuencias_fenotipicas$parentales,
    frecuencias_fenotipicas$recombinantes_simples_1,
    frecuencias_fenotipicas$recombinantes_simples_2,
    frecuencias_fenotipicas$recombinantes_dobles) -> denominador



dist_gen1_gen2 <- (numerador_1/denominador)*100

B <- dist_gen1_gen2

dist_gen2_gen3 <- (numerador_2/denominador)*100

C <- dist_gen2_gen3

# Ahora usaré esta información para graficar el mapa genetico

Nuestro_primer_mapa <-sim.map(len = rep(100,1), n.mar = c(3, 1), anchor.tel = FALSE, include.x = FALSE,
                         sex.sp =FALSE, eq.spacing = FALSE) #Objeto base

Nuestro_primer_mapa[["1"]][["D1M2"]] <- (numerador_1/denominador)*100 # Distancia de A a B
Nuestro_primer_mapa[["1"]][["D1M3"]] <- (numerador_2/denominador)*100 # Distancia de A a C

Genes <- c("Gen 1","Gen 2","Gen 3")
names(Nuestro_primer_mapa[["1"]]) <- Genes

linkmap(Nuestro_primer_mapa, chr = "1") -> Mapa_genetico

png("Mapa_genetico.png", width = 400, height = 600)
linkmap(Nuestro_primer_mapa, chr = "Prueba de 3 puntos")-> Mapa_genetico
dev.off()

# futuras ediciones :
# Me gustaria enderezar la linea del gen 2. No se porque se estira hasta abajo.
# Cambiar el 1 de hasta arriba.
# Hacer algo para que se vea más detalle en la grafica o proporcionar más graficas.




# Cambio de titulo
names(Nuestro_primer_mapa) <- "Prueba de 3 puntos"
View(Nuestro_primer_mapa)
linkmap(Nuestro_primer_mapa, chr = "Prueba de 3 puntos",m.cex = 0.8) -> Mapa_genetico



# Ahora Añadire una linea entre los genes 1-2 y entre 2-3 
# que me indicará cual es la distancia entre ellos.


Genes <- c("Gen 1","Gen 2","Gen 3")
names(Nuestro_primer_mapa[["Prueba de 3 puntos"]]) <- Genes

Nuestro_primer_mapa[["Prueba de 3 puntos"]][["Gen 2"]] <- (numerador_1/denominador)*100 # Distancia de A a B
Nuestro_primer_mapa[["Prueba de 3 puntos"]][["Gen 3"]] <- ((((numerador_2/denominador)*100))+((numerador_1/denominador)*100))  # Distancia de A a C

# La primer linea nos da la distancia de A a B
# La segunda de A a C
# Esto es un problema porque 

linkmap(Nuestro_primer_mapa, chr = "Prueba de 3 puntos", m.cex = 0.8, interval = TRUE) -> Mapa_genetico

# 
png("Mapa_genetico_v2.png", width = 400, height = 600)
linkmap(Nuestro_primer_mapa, chr = "Prueba de 3 puntos")-> Mapa_genetico
dev.off()

