# Trabajo Uno
pathMiPc <- "~/GitHub/Trabajo-Uno" 

#################Ejercicio 1#############################

#Sistema De Prebiscito
set.seed(10)
sample(c("SI","NO"), 10,replace = T)
var_list<- list("SI","SI","NO","SI","NO","NO","NO","NO","SI","SI")
var_list<-10
if(var_list=="si"){
  print("votosSI")
} else if(var_list%%3=="NO"){
 print("votosNO") 
}else{
  print("Votos")
}
