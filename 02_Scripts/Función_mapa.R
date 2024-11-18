library(ape)

prueba_distancias <- matrix(
  c(0, 10, 20, 30, 40, 50,
    10, 0, 15, 25, 35, 45,
    20, 15, 0, 10, 20, 30,
    30, 25, 10, 0, 15, 25,
    40, 35, 20, 15, 0, 10,
    50, 45, 30, 25, 10, 0),
  nrow = 6,
  ncol = 6,
  byrow = TRUE)
rownames(prueba_distancias)<-  c("A", "B", "C", "D", "E", "F")
colnames(prueba_distancias)<-  c("A", "B", "C", "D", "E", "F")

Mapa_genetico<- function(x) {
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
  text(posiciones, rep(0, length(genetic_positions)), labels = nuevo_arreglo, pos = 3)
  axis(1, at = posiciones, labels = posiciones)
  
}
Mapa_genetico(prueba_distancias)