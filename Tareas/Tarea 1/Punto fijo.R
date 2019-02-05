#Remueve los objetos del entorno
remove(list=ls())

puntoFijo<-function(a,b,x0){ #Se declara la función de punto fijo con un x0 inicial arbitrario
  #Se inicializan variables, error en 100 para iniciar las iteraciones, valor arbitrario
  ant=x0
  error=100
  cont=0
  if((a>f(a) && b<f(b))||(a<f(a) && b>f(b))){
    while(error>0.00000001){ #Se terminará el proceso cuando el error sea menor a 10^-8, se realiza método de punto fijo, se calcula error y cuentan iteraciones
      x0=f(x0)
      error=abs(x0-ant)/abs(x0)
      ant=x0
      cont=cont+1
      cat("Raiz=",x0,"    Error=",error,"\n") #Se muestra por pantalla resultados de la raiz y el error de cada una de las iteraciones
    }
    cat("La cantidad de iteraciones fue:",cont,"\n") #Se muestra la cantidad de iteraciones por pantalla
  }else{
    print("El intervalo escogido no tiene una raiz para la ecuación presentada.")
  }
}
#Se usa la opción de dígitos para poner la cantidad de puntos decimales necesaria, y se realizan ambos despejes
#de la función para hallar las dos raíces, el valor inicial fue escogido para que el método convergiera
options(digits=8)
f<-function(x){exp(x)/pi}
puntoFijo(0,1.5,1) #Caso en el que funciona el intervalo
puntoFijo(-1,0,-0.5) #Caso en el que no funciona el intervalo
options(digits=9)
f<-function(x){log(pi*x)}
puntoFijo(1.5,2,1.8) #Segundo caso en el que funciona el intervalo
puntoFijo(2,10,5) #Segundo caso en el que no funciona el intervalo