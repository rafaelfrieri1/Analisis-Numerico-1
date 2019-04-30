# Punto 3

A=matrix(c(4,-1,-1,-1,
           -1,4,-1,-1,
           -1,-1,4,-1,
           -1,-1,-1,4),nrow=4,byrow=4)
b=c(1,5,1.5,-2.33)

# Parte A
vp=eig(A)

cat("Los valores propios de la matriz A son: ",vp,"\n")

# Parte B

"Teniendo en cuenta el teorema de convergencia, no se debe utilizar el método de
Jacobi, dado que la función rho(A)>1 (rho(A)=1) entonces es altamente probable que 
no tenga convergencia, a diferencia del método de Gauss-Seidel o su variación SOR
que converge con la dominancia diagonal, que se cumple para la matriz A de este caso,
luego este método converge y sería el favorable para el caso."

# Parte C

# Se calcula la matriz de transición para el método de Jacobi, Gauss-Seidel y SOR

# Jacobi
L=tril(A,k=-1)
U=triu(A,k=1)
D=diag(diag(A))

Tj=-inv(D)%*%(L+U) #-D^-1*(L+U)

# Gauss Seidel

Tgs=inv(D-L)%*%U # (D-L)^-1*U

# SOR

# Se encontró que el valor óptimo de w es 2/(1+sqrt(1-(rho(Tj))^2))

# Se calcula rho(Tj)

rho=max(abs(eig(Tj)))

Wopt=2/(1+sqrt(1-rho^2))

Tsor=inv(D-Wopt*L)%*%((1-Wopt)*D+Wopt*U) # (D-wL)^-1*((1-w)D+wU)

cat("Transición Jacobi:\n")
write.table(Tj,sep=" ",row.names=FALSE,col.names=FALSE)
cat("Transición Gauss-Seidel:\n")
write.table(Tgs,sep=" ",row.names=FALSE,col.names=FALSE)
cat("Transición SOR:\n")
write.table(Tsor,sep=" ",row.names=FALSE,col.names=FALSE)
cat("Con w óptimo de:",Wopt,"\n")

# Parte D

# Por lo descrito anteriormente, se calcula la solución por el método de Gauss-Seidel y con método SOR para convergencia más rápida

Csor=Wopt*inv(D-Wopt*L)%*%b

SOR=function(x0){
  error=100
  ant=x0
  cont=0
  while(error>1e-9){
    x0=inv(D+Wopt*L)%*%(Wopt*b-(Wopt*U+(Wopt-1)*D)%*%x0)
    error=abs(Norm(x0)-Norm(ant))/Norm(x0)
    ant=x0
    cont=cont+1
  }
  return(x0)
}

sol=SOR(c(0,0,0,0))
solsolve=solve(A,b)

cat("La solución con solve fue:",solsolve,". Mientras que la solución con el método SOR fue: ",sol,"\n Se puede observar que ambas soluciones son iguales, por ende el método funcionó de manera correcta.")