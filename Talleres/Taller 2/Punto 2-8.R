library("BB")

# Parte A

# Se declaran las funciones no lineales de las que se encontrará la solución
inter=function(x){
  n=length(x)
  f=rep(NA,n)
  f[1]=+x[1]^2+x[2]^2-1
  t=2:(n-1)
  f[2]=x[1]-x[2]
  f
}
# Ecuaciones a graficar
f1=function(x){
  sqrt(1-x^2)
}

f2=function(x){
  -sqrt(1-x^2)
}

f3=function(x){
  x
}
# Se encuentra la solución del sistema de ecuaciones no lineales
sol=BBsolve(par=c(1,1),fn=inter)
cat("Solución (inicial de (1,1)):\n",sol[[1]],"\n")
sol2=BBsolve(par=c(-1,1),fn=inter)
cat("Solución (inicial de (-1,1)):\n",sol2[[1]],"\n")

# Se grafican ecuaciones y puntos de intersección
x=seq(-1,1,0.0001)
plot(x,f3(x),type="l",col="red")
lines(x,f1(x),type="l",col="black")
lines(x,f2(x),type="l",col="black")
abline(h=0,col="blue")
points(rbind(c(sqrt(2)/2,sqrt(2)/2)),pch=17,cex=1.5,col="red")
points(rbind(c(-sqrt(2)/2,-sqrt(2)/2)),pch=17,cex=1.5,col="red")

# Parte B

trigexp = function(x) {
  n = length(x)
  F = rep(NA, n)
  F[1] = 3*x[1]^2 + 2*x[2] - 5 + sin(x[1] - x[2]) * sin(x[1] + x[2])
  tn1 = 2:(n-1)
  F[tn1] = -x[tn1-1] * exp(x[tn1-1] - x[tn1]) + x[tn1] *
    ( 4 + 3*x[tn1]^2) + 2 * x[tn1 + 1] + sin(x[tn1] -
                                               x[tn1 + 1]) * sin(x[tn1] + x[tn1 + 1]) - 8
  F[n] = -x[n-1] * exp(x[n-1] - x[n]) + 4*x[n] - 3
  F
}
n = 10000
p0 = runif(n) # n initial random starting guesses
sol = BBsolve(par=p0, fn=trigexp)
sol$par

"Primero se declara la función que contiene el sistema de ecuaciones del cual
se encontrarán las soluciones, dicho sistema de ecuaciones puede ser de tamaño n
arbitrario, la función 1 está determinada como 3x^2+2y-5+sin(x-y)*sin(x+y) luego 
el siguiente set de ecuaciones (2 a n-1) tienen la forma -x[tn-1]*e^(x[tn-1]-x[tn])
+x[tn]*(4+3*x[tn]^2)+2*x[tn+1]+sin(x[tn]-x[tn+1])*sin(x[tn]+x[tn+1]) y la última
está descrita como -z*e^(z-w)+4z-3, luego se escoje la cantidad de ecuaciones que habrá
se escoge un punto inicial al azar y por último se soluciona el sistema y se muestra la 
solución. Para este caso habrá 10000 ecuaciones para demostrar la efectividad del método
BBsolve para solucionar sistemas de ecuaciones no lineales."