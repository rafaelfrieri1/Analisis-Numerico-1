# Punto 1

# Se declara la función para resolver ecuaciones por método de Horner
Horner<-function(p,n,x0){
  y=p[1]
  sumas=0
  multis=0
  for(i in c(2:n)){
    y=x0*y+p[i]
    sumas=sumas+1
    multis=multis+1
  }
  tot=c(y,sumas,multis)
  return(tot)
}

# Se declara la función para resolver ecuaciones de forma convencional
Evaluation<-function(p,n,x0){
  s=p[n]
  sumas=0
  multis=0
  for(i in c(1:(n-1))){
    s=s+p[i]*x0^(n-i)
    sumas=sumas+1
    multis=multis+n-i
  }
  tot=c(s,sumas,multis)
  return(tot)
}
# Se evaluan los polinomios y se muestra su resultado, número de operaciones óptimas y número de operaciones convencionales
Px=c(2,0,-3,3,-4)
n=length(Px)
x0=-2
res1=Horner(Px,n,x0)
res2=Evaluation(Px,n,x0)

cat("El resultado del polinomio 2x^4-3x^2+3x-4 evaluado en x0=-2 es:",res1[1],"\nEl número de operaciones realizadas evaluando convencinalmente fue de:",res2[2]+res2[3],", con ",res2[2]," sumas y ",res2[3]," multiplicaciones.\nEl número de operaciones realizadas con el método de Horner fue de:",res1[2]+res1[3],", que es el número de operaciones mínimo para evaluar el polinomio, con ",res1[2]," sumas y ",res1[3]," multiplicaciones.\n\n")

Px=c(7,6,-6,0,3,-4)
n=length(Px)
x0=3
res1=Horner(Px,n,x0)
res2=Evaluation(Px,n,x0)

cat("El resultado del polinomio 7x^5+6x^4-6x^3+3x-4 evaluado en x0=3 es:",res1[1],"\nEl número de operaciones realizadas evaluando convencinalmente fue de:",res2[2]+res2[3],", con ",res2[2]," sumas y ",res2[3]," multiplicaciones.\nEl número de operaciones realizadas con el método de Horner fue de:",res1[2]+res1[3],", que es el número de operaciones mínimo para evaluar el polinomio, con ",res1[2]," sumas y ",res1[3]," multiplicaciones.\n\n")

Px=c(-5,0,3,0,2,-4,0)
n=length(Px)
x0=-1
res1=Horner(Px,n,x0)
res2=Evaluation(Px,n,x0)

cat("El resultado del polinomio -5x^6+3x^4+2x^2-4x evaluado en x0=-1 es:",res1[1],"\nEl número de operaciones realizadas evaluando convencinalmente fue de:",res2[2]+res2[3],", con ",res2[2]," sumas y ",res2[3]," multiplicaciones.\nEl número de operaciones realizadas con el método de Horner fue de:",res1[2]+res1[3],", que es el número de operaciones mínimo para evaluar el polinomio, con ",res1[2]," sumas y ",res1[3]," multiplicaciones.\n\n")
