# Se tienen dos ecuaciones, igualándolas y dejando los términos de un solo lado resulta la siguiente ecuación
f<-function(t){cos(3*t)+exp(t)}

# Para el método de Newton la derivada de f(t) es
dfdt<-function(t){exp(t)-3*sin(3*t)}

# Se declara función para cálculo de r luego de obtener t
r<-function(t){2-exp(t)}

# Se declara otra función para el cálculo de r para poder graficar soluciones
r1<-function(t){2+cos(3*t)}

#Se resolverá por el método de la secante, método de posición falsa y método de Newton con aceleración de delta de Aitken

secante<-function(a,b,x0){
  if(f(a)*f(b)<0){
    error=100
    ant=x0+1
    cont=0
    while(error>0.00000001){
      if(x0!=ant){
        x1=x0-((x0-ant)/(f(x0)-f(ant)))*f(x0)
      }else{
        print("Se ha llegado a una indeterminación en el proceso, no hay solución disponible")
        break
      }
      ant=x0
      x0=x1
      error=abs(x0-ant)/abs(x0)
      cont=cont+1
    }
    cat("Iteraciones método secante:",cont,".\n")
    return(x0)
  }else{
    print("No existe una solucion en el intervalo [a,b]")
  }
}

posicionFalsa<-function(a,b){
  if(f(a)*f(b)<0){
    c=(f(b)*a-f(a)*b)/(f(b)-f(a))
    error=100
    ant=c
    cont=0
    while(error>0.00000001){
      if(f(a)*f(c)<0){
        b=c
      }else{
        a=c
      }
      if(a==b){
        break
      }
      c=(f(b)*a-f(a)*b)/(f(b)-f(a))
      error=abs(c-ant)/abs(c)
      ant=c
      cont=cont+1
    }
    cat("Iteraciones método posición falsa:",cont,".\n")
    return(c)
  }else{
    print("No se puede encontrar una raiz a partir de dicho intervalo.")
  }
}

deltaAitkenNewton<-function(a,b,x0){
  if(f(a)*f(b)<0){
    error=100
    cont=0
    while(error>0.00000001){
      if(dfdt(x0)!=0){
        Xn1=x0-f(x0)/dfdt(x0)
      }else{
        print("El método no converge correctamente")
        break
      }
      if(dfdt(Xn1)!=0){
        Xn2=Xn1-f(Xn1)/dfdt(Xn1)
      }else{
        print("El método no converge correctamente")
        break
      }
      if(Xn2-2*Xn1+x0!=0){
        xAt=Xn2-(Xn2-Xn1)^2/(Xn2-2*Xn1+x0)
      }else{
        print("El método no converge correctamente")
        break
      }
      error=abs(xAt-x0)/abs(xAt)
      x0=xAt
      cont=cont+1
    }
    cat("Iteraciones método Newton con delta de Aitken:",cont,".\n")
    return(xAt)
  }else{
    print("No existe una solución única en el intervalo dado, intentar uno diferente.")
  }
}
# Se usan ambos métodos para hallar una solución de la ecuación (Preferiblemente la misma, ya que tiene infinitas soluciones por las oscilaciones del coseno)
# Se calcula el punto de intersección con ambos métodos y se muestran por la consola

tRaizSe=secante(-1,-0.5,-0.6)
rSe=r(tRaizSe)
cat("La solución dada por el método de la secante fue: t=",tRaizSe,", r= ",rSe,"\n")

tRaizPF=posicionFalsa(-1,-0.5)
rPF=r(tRaizPF)
cat("La solución dada por el método de posición falsa fue: t=",tRaizPF,", r= ",rPF,"\n")

tRaizNAt=deltaAitkenNewton(-1,-0.5,-0.6)
rNAt=r(tRaizNAt)
cat("La solución dada por el método de Newton con delta de Aitken fue: t=",tRaizNAt,", r= ",rNAt,"\n")

# Realización de la gráfica
plot(seq(-1,-0.5,0.000001),r(seq(-1,-0.5,0.000001)),type="l",col="blue")
abline(h=0,col="black")
lines(seq(-1,-0.5,0.000001),r1(seq(-1,-0.5,0.000001)),type="l",col="red")
points(rbind(c(tRaizSe,rSe)),pch=17,cex=1.5,col="red")
points(rbind(c(tRaizPF,rPF)),pch=17,cex=1.5,col="blue")

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
