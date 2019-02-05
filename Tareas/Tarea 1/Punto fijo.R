#Remueve los objetos del entorno
remove(list=ls())

puntoFijo<-function(x0){ #Se declara la función de punto fijo con un x0 inicial arbitrario
  #Se inicializan variables, error en 100 para iniciar las iteraciones, valor arbitrario
  ant=x0
  error=100
  cont=0
  while(error>0.00000001){ #Se terminará el proceso cuando el error sea menor a 10^-8, se realiza método de punto fijo, se calcula error y cuentan iteraciones
    x0=f(x0)
    error=abs(x0-ant)/abs(x0)
    ant=x0
    cont=cont+1
    cat("Raiz=",x0,"    Error=",error,"\n") #Se muestra por pantalla resultados de la raiz y el error de cada una de las iteraciones
  }
  cat("La cantidad de iteraciones fue:",cont,"\n") #Se muestra la cantidad de iteraciones por pantalla
}
#Se usa la opción de dígitos para poner la cantidad de puntos decimales necesaria, y se realizan ambos despejes
#de la función para hallar las dos raíces, el valor inicial fue escogido para que el método convergiera
options(digits=8)
f<-function(x){exp(x)/pi}
puntoFijo(1)
options(digits=9)
f<-function(x){log(pi*x)}
puntoFijo(10)