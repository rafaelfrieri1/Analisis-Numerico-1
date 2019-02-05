#Remueve objetos del entorno y crea función a la que se le hallará las raices
remove(list=ls())
f<-function(x){exp(x)-pi*x}

#Función de bisección que recibe como parámetros los número del intervalo [a,b]
biseccion<-function(a,b){
  #Se grafica la función en el intervalo escogido de color rojo
  x<-seq(a,b,0.001)
  plot(x,f(x),type="l",col="red")
  abline(h=0,col="red")
  #Se halla primer valor del 0 posible y se calcula el error
  mid<-(a+b)/2
  error=abs(a+b)/2
  #Se inicializa el contador de iteraciones
  cont=0
  #Si se escoge un intervalor erroneo se le menciona al usuario, de otra forma se continua con el método
  if(f(a)*f(b)>0){
    print("Escoger otro intervalo, ambos números resultan en valores positivos.")
  }else{
    #Se realiza el método de bisección con conteo de iteraciones, reducción del intervalo y se grafican cada una de las posibles raices
    while(error>0.00000001){ #Se para el proceso cuando se tiene un error de 10^-8
      cont=cont+1
      if(f(mid)*f(a)<0){
        b=mid
      }else if(f(mid)*f(b)<0){
        a=mid
      }else{
        break
      }
      ant=mid
      mid=(a+b)/2
      points(rbind(c(mid,0)),pch=17,cex=0.7,col="black")
      text(mid,0,cont,cex=0.8,col="black")
      error=abs(mid-ant)/abs(mid)
      cat("raiz=",mid,"      error=",error,"\n") #Se muestra el valor de la raiz y el error para cada iteración
    }
    #Muestra por pantalla la cantidad de iteraciones
    cat("Número de iteraciones:",cont,"\n")
  }
}
#Se configuran cantidad de dígitos decimales y se hace bisección para las dos raices de la ecuación, los intervalos fueron escogidos para encontrar las 2 raices
options(digits=8)
biseccion(0,1)
options(digits=9)
biseccion(1,2)