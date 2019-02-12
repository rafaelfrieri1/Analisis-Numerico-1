# Punto 2

alg<-function(n){
  cont=0
  while(n>0){
    d=n%%2
    n=floor(n/2)
    print(d)
    cont=cont+1
  }
  cat("Cantidad de iteraciones:",cont,"\n")
}

# Parte a

alg(73)

# Parte b

# Se declara la función T, número de divisiones para un n
T<-function(n){
  if(n>0){
    return(2*floor(log2(n)+1))
  }else{
    return(0)
  }
}

# Se evalua en el valor de prueba(73) para comprobar que funciona
cat("Cantidad de divisiones:",T(73),"\n")

