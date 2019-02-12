# Se tienen dos ecuaciones, igualándolas y dejando los términos de un solo lado resulta la siguiente ecuación
f<-function(t){cos(3*t)+exp(t)}

# Para el método de Newton la derivada de f(t) es
dfdt<-function(t){exp(t)-3*sin(3*t)}

# Se declara función para cálculo de r luego de obtener t
r<-function(t){2-exp(t)}

# Se declara otra función para el cálculo de r para poder graficar soluciones
r1<-function(t){2+cos(3*t)}

#Se resolverá por el método de bisección y el método de Newton

biseccion<-function(a,b){
  if(f(a)*f(b)<0){
    c=(a+b)/2
    error=100
    ant=c
    while(error>0.00000001){
      if(f(c)==0){
        break
      }else{
        if(f(a)*f(c)<0){
          b=c
        }else{
          a=c
        }
      }
      c=(a+b)/2
      error=abs(c-ant)/abs(c)
      ant=c
    }
  }else{
    print("No hay una solución única en el intervalo escogido")
  }
  return(c)
}

Newton<-function(a,b,t0){
  if(f(a)*f(b)<0){
    error=100
    ant=t0
    while(error>0.00000001){
      if(dfdt(t0)!=0){
        t0=t0-f(t0)/dfdt(t0)
      }else{
        print("El método no converge para el t0 inicial")
      }
      error=abs(t0-ant)/abs(t0)
      ant=t0
    }
  }else{
    print("El intervalo escogido no tiene una raiz única.")
  }
  return(t0)
}

# Se usan ambos métodos para hallar una solución de la ecuación (Preferiblemente la misma, ya que tiene infinitas soluciones por las oscilaciones del coseno)
# Se calcula el punto de intersección con ambos métodos y se muestran por la consola

tRaizBi=biseccion(-1,-0.5)
rBi=r(tRaizBi)
cat("La solución dada por el método de bisección fue: t=",tRaizBi,", r= ",rBi,"\n")

tRaizNe=Newton(-1,-0.5,-0.6)
rNe=r(tRaizNe)
cat("La solución dada por el método de Newton fue: t=",tRaizNe,", r= ",rNe,"\n")

# Realización de la gráfica
plot(seq(-1,-0.5,0.000001),r(seq(-1,-0.5,0.000001)),type="l",col="blue")
abline(h=0,col="black")
lines(seq(-1,-0.5,0.000001),r1(seq(-1,-0.5,0.000001)),type="l",col="red")
points(rbind(c(tRaizBi,rBi)),pch=17,cex=1.5,col="red")
points(rbind(c(tRaizNe,rNe)),pch=17,cex=1.5,col="blue")

#Gráfica en coordenadas polares

#plot.new()
#polar <- function (theta, r, color=4){
#  y <- 0
#  x <- 0
#  ejex <- 1
#  for (i in 1:length(r)){
#    if(is.nan(r[i])== T){
#      r[i] <- 0
#    }
#  }

#  angulo <- seq(-max(theta),max(theta),by=theta[2]-theta[1])
#  y <- r*sin(theta)
#  x <- r*cos(theta)
#  plot.window(xlim = c(-max(r), max(r)), ylim = c(-max(r), max(r)), asp = 1)

#  aux <- max(r)
#  while (aux > 0){
#    fi <- aux*sin(angulo)
#    cir <- aux*cos(angulo)
#    points(cir,fi,pch="-",col="gray",cex=0.3)
#    text(ejex+0.2,-0.2,ejex,col="gray")
#    ejex <- ejex + 1
#    aux <- aux - 1
#  }
  
#  abline(v=((max(cir)+min(cir))/2),col="gray")
#  abline(h=((max(cir)+min(cir))/2),col="gray")
#  segments(-max(r)+0.5,-max(r)+0.5,max(r)-0.5,max(r)-0.5,col="gray")
#  segments(-max(r)+0.5,max(r)-0.5,max(r)-0.5,-max(r)+0.5,col="gray")
  
#  points(x,y,pch=20,col=color,cex=1)
#}

#dim <- seq(-1,1,by=0.01)
#r2=r(dim)
#polar(dim,r2,"blue")
#r3=r1(dim)
#polar(dim,r3,"red")

#Si se desea observar las soluciones en el sistema polar y no en el cartesiano es necesario remover los comentarios de la linea 79 a la 117, si se vuelven a poner se tendrá la solución graficada en el sistema cartesiano
