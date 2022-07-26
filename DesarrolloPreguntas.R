install.packages("tidyverse")
library(tidyverse)

# Trabajo Uno
pathMiPc <- setwd("~/GitHub/Trabajo-Uno")


#################Ejercicio 1#############################

#Sistema De Prebiscito

set.seed(10)
sample(c("SI","NO"),10, replace = T)

# Funcion plebiscito, recibe los parametros entregados: total; total; votosSI; votosNO.
Plebiscito <- function(total,votosSI,votosNO){
  if (votosSI >= ((total*1/2)+1)) {
    print("mayoria de votos SI")
  } else{
    print("votosSI menor a 30% de votos")
    if (votosSI >= total*0.3) {
      print("votosSI mayor o igual a un 30% de votos")
      if (votosSI >= total*0.3 & votosNO < total*0.3) {
        print("mayoria de votos SI")
      } else{
        if (votosNO >= ((total*1/2)+1)) {
          print("mayoria de votos NO")
        } else{
          print("votosNO menor a 30% de votos")
          if (votosSI == votosNO) {
            print("misma cantidad de votos, ha ganado el SI debido al empate")
          } else{
            if (votosSI > votosNO) {
              print("mayoria de votos SI")
            } else{
              print("mayoria de votos NO")
            }
          }
        }
      }
    } else{
      print("mayoria de votos NO")
    }
  }
}

#parametros: total; votosSI; votosNO, para ejecutar la función#
Plebiscito_total(10,5,5)
Plebiscito_total <- function(total){
  set.seed(total)
  padron <- sample(c("SI","NO"),total,replace = TRUE)
  padron <- as.data.frame(padron)
  names(padron) <- c("votos")
  votosSI <- sum(with(padron,votos == "SI"))
  votosNO <- sum(with(padron,votos == "NO"))
  
  if (votosSI >= ((total*1/2)+1)) {
    print("Mayoría de votos SI")
  } else{
    print("votosSI menor a 30% de votos")
    if (votosSI >= total*0.3) {
      print("votosSI mayor o igual a un 30% de votos")
      if (votosSI >= total*0.3 & votosNO < total*0.3) {
        print("Ha ganado el SI")
      } else{
        if (votosNO >= ((total*1/2)+1)) {
          print("Ha ganado el NO")
        } else{
          print("votosNO mayor a un 30% de votos")
          if (votosSI == votosNO) {
            print("Misma cantidad de votos, ha ganado el SI debido al empate")
          } else{
            if (votosSI > votosNO) {
              print("mayoria de votos SI")
            } else{
              print("mayoria de votos NO")
            }
          }
        }
      }
    } else{
      print("mayoria de votos NO")
    }
  }
}
Plebiscito_total(10)
#En la funcion, se ingresa el total de 10 votos, según el padron electoral designada por el profesor.
#set.seed() esta función fija numeros aleatorios originados por samples() logrando reproducir la misma secuencia al ser ejecutadas juntas.
#samples() es una función que reproduce datos aleatorios con base en una muestra dada.



###########Ejercicio 2#########


#A continuación se le entrega una lista con medidas de protección (mp) y oficios (of):
 
 listaDocumentos <- list(c("mp","Juan","Christofer"),c("of","av01","ampr"),c("of","av01","ante"),
                          c("of","av08","arme"),c("of","av02","ante"),c("of","av07","ampr"),
                          c("of","av03","dape"),c("of","av01","meca"),c("of","av02","dape"),
                          c("mp","Antonia"),c("mp","Christian","Mario"),
                          c("mp","Jose","Pedro","Antonela"),c("of","av05","meca"),
                          c("of","av04","dape"),c("of","av02","arme"))

  escrutinio <- list()
  for(documento in listaDocumentos){
    if(documento[1] == "mp")
      encontro <- F
      if(length(escrutinio) != 0){
        for (posicionescrutinio in 1:length(escrutinio)) {
          escrutiniounlist <- unlist(escrutinio[posicionescrutinio])
          if(escrutiniounlist[1] == (length(documento)-1)){
            escrutiniounlist[2] <- escrutiniounlist[2]+1
            escrutinio[posicionescrutinio] <- list(c(escrutiniounlist))
            encontro <- T
          }
        }
      }
      #Es generada un nuevo registro del escrutinio 
      if(!encontro){
        nuevaescrutinio <- c()
        nuevaescrutinio[1] <- (length(documento)-1)
        nuevaescrutinio[2] <- 1
        escrutinio <- c(escrutinio, list(c(nuevaescrutinio)))
      }
    }
  #Resultado final de escrutinio
  for (escrutinio in escrutinio) {
    print(paste("Se cuentan con", escrutinio[2],"mp de", escrutinio[1],"niños"))
  }