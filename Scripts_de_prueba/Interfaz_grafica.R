          # Ultimo intento: tengo 20 min #
           # Tarde 12 min en resolverlo #

# Resumen: La funcion que hace los mapas es muy compleja de utilizar
# No por la funcion, sino por la estructura del objeto que sirve
# como input de la misma, en palabras de los autores "es aburrido y complicado"
# Por lo tanto opte por editar a nuestro gusto un objeto que hace un simulador
# Sale con x datos y yo lo edito para que imprimia un plot con los alelos
# y distancias de mi interes.


install.packages("qlt")
library("qtl")

                      # funcion que usaremos #
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

  
                      # Generando mapa #  

Mi_primer_mapa <-sim.map(len = rep(100,1), n.mar = c(3, 1), anchor.tel = FALSE, include.x = FALSE,
                         sex.sp =FALSE, eq.spacing = FALSE) # simulador

linkmap(Mi_primer_mapa, chr = c(1))

View(Mi_primer_mapa)

# Edición de las distancias entre marcadores
Mi_primer_mapa[["1"]][["D1M1"]]
Mi_primer_mapa[["1"]][["D1M2"]] <- 10 # Si jalo :p
Mi_primer_mapa[["1"]][["D1M3"]] <- 20 # Si jalo :p

linkmap(Mi_primer_mapa, chr = c(1))

# Ahora me gustaria cambiar el nombre de los marcadores
names(Mi_primer_mapa[["1"]]) <- Genes

Genes <- c("A","B","C")

Mi_primer_mapa[["1"]][["A"]]
Mi_primer_mapa[["1"]][["B"]]
Mi_primer_mapa[["1"]][["C"]]

# LA FUCKIN PRUEBA DE FUEGO !!!!

linkmap(Mi_primer_mapa, chr = c(1)) # Si funciono c:

# Ahora tratare de conectarlo con los resultados de la funcion de mike.

