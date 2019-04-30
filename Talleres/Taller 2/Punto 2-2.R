# Punto 2

# Parte A

A=matrix(c(-8.1,-7,6.123,-2,
             -1,4,-3,-1,
             0,-1,-5,0.6,
             -1,0.33,6,0.5),nrow=4,byrow=4)
U=triu(A,k=1)
L=tril(A,k=-1)
D=diag(diag(A))

cat("L:\n")
write.table(L,sep=" ",row.names=FALSE,col.names=FALSE)
cat("D:\n")
write.table(D,sep=" ",row.names=FALSE,col.names=FALSE)
cat("U:\n")
write.table(U,sep=" ",row.names=FALSE,col.names=FALSE)

# Parte B

b=c(1.45,3,5.12,-4)
b=t(t(b))
x=itersolve(A,b,tol=1e-9,method="Gauss-Seidel")
print(x)

"Se observará que el método no converge, ya que para que Gauss-Seidel
tenga convergencia segura la matriz debe ser diagonal dominante of simétrica
definida positivamente, y no es ninguna de las dos anteriores, por lo
cual es posible que tenga convergencia o no (lo más probable es que no),
luego este es el resultado esperado."

# Parte C

Jacobi=function(A,b,x0){
  error=100
  ant=x0
  U=triu(A,k=1)
  L=tril(A,k=-1)
  D=diag(diag(A))
  cont=0
  while(cont<5){
    x0=inv(D)%*%(b-(L+U)%*%x0)
    error=abs(Norm(x0)-Norm(ant))/Norm(x0)
    ant=x0
    cont=cont+1
    cat("El error relativo en la iteración ",cont," es:",error,"\nX en la iteración ",cont," es:",x0,"\n")
  }
  cat("El valor de la solución X es:",x0,"\n")
}

Jacobi(A,b,t(t(c(1,1,1,1))))

"Se puede ver que el error oscila alrededor de 0,8 y 1,5 en cada iteración
pero no disminuye en cada iteración, por lo cual se sabe que el método no converge
para la matriz dada, esto debido a que no se cumple el criterio de convergencia
que es que rho(D^-1(L+U))<1, donde la función rho es la magnitud del valor propio
más grande de dicha matriz resultante, que en este caso es mayor a 1. Otra condición
es que la matriz A se estrictamente o irreduciblemente dominante diagonal, lo cual
ya se explicó en el punto anterior que no sucede, luego el método de Jacobi es posible
que tenga convergencia o no (lo más probable es que no), y en este caso no convergerá
como se muestra en los errores y en el resultado."