# Punto 1

# Parte A

library("pracma")

# Tomando m y n como 4
n=4
m=4
D1<-eye(n,m=n)
D2<-ones(n,m=n)
D3<-zeros(n,m=n)
cat("D1:\n")
write.table(D1,sep=" ",row.names=FALSE,col.names=FALSE)
cat("D2:\n")
write.table(D2,sep=" ",row.names=FALSE,col.names=FALSE)
cat("D3:\n")
write.table(D3,sep=" ",row.names=FALSE,col.names=FALSE)

# Parte B

#Como dicen las instrucciones se utiliza la matriz del punto 2

mat=matrix(c(-8.1,-7,6.123,-2,
             -1,4,-3,-1,
             0,-1,-5,0.6,
             -1,0.33,6,0.5),nrow=4,byrow=4)
# Para evaluar la matriz de transición se descompone la matriz en L,D,U

Lmat=matrix(c(0,0,0,0,
              -1,0,0,0,
              0,-1,0,0,
              -1,0.33,6,0),nrow=4,byrow=4)

Umat=matrix(c(0,-7,6.123,-2,
             0,0,-3,-1,
             0,0,0,0.6,
             0,0,0,0),nrow=4,byrow=4)

Dmat=matrix(c(-8.1,0,0,0,
             0,4,0,0,
             0,0,-5,0,
             0,0,0,0.5),nrow=4,byrow=4)

# Se calcula la matriz de transición T=(D-wL)^(-1)*((1-w)D+wU), tomando w=1, para el método SOR

w=1

transicion=inv(Dmat-w*Lmat)%*%((1-w)*Dmat+w*Umat)

cat("Matriz de transición:\n")
write.table(transicion,sep=" ",row.names=FALSE,col.names=FALSE)


