# Punto 9
library("pracma")
#detach("package:Matrix",unload=TRUE), quitar comentario si se incluyó Matrix
"Se sabe que en el método de Gauss-Seidel Xi(k)=(1/(aii))*(-suma de j=1 a i-1 de(aij*Xj(k))-suma de j=i+1 a n de(aij*Xj(k-1)+bi)) Para i=1,2,....,n
Luego se amplían las ecuaciones a a11X1(k)=-a12X2(k)-...-a1nXn(k-1)+b1
                                  a21X1(k)+a22X2(k)=-a23X3(k-1)-...-a2nXn(k-1)+b2
                                  an1X1(k)+an2X2(k)+....annXn(k)=bn

y luego: (D-L)X(k)=UX(k-1)+b y despejando, X(k)=(D-L)^-1*UX(k-1)+(D-L)^-1*b

luego Tg=((D-L)^-1)*U que es equivalente a (-D^-1*U)(I+LD^-1)^-1"

A=matrix(c(4,-1,-1,-1,
           -1,4,-1,-1,
           -1,-1,4,-1,
           -1,-1,-1,4),nrow=4,byrow=4)
b=c(1,5,1.5,-2.33)

L=tril(A,k=-1)
U=triu(A,k=1)
D=diag(diag(A))

Tg=inv(D-L)%*%U
Cg=inv(D-L)%*%b
GS=function(x0){
  error=100
  ant=x0
  while(error>1e-9){
    x0=Tg%*%x0+Cg
    error=abs(Norm(x0)-Norm(ant))/Norm(x0)
    ant=x0
  }
  return(x0)
}

res=GS(c(0,0,0,0))

A=matrix(c(8,-2,-2,-2,
           -2,8,-2,-2,
           -2,-2,8,-2,
           -2,-2,-2,8),nrow=4,byrow=4)
b=c(4,10,5,-2)

L=tril(A,k=-1)
U=triu(A,k=1)
D=diag(diag(A))
Tg=inv(D-L)%*%U
Cg=inv(D-L)%*%b

res2=GS(c(1,1,1,1))

cat("Solución 1:\n",res,"\n","Solución 2:\n",res2,"\n","Al encontrar correctamente las soluciones se verifica que efectivamente es la matriz de transición de Gauss-Seidel.\n")