# Punto 7 (6)

library("Matrix")

A=matrix(c(-8.1,-7,6.123,-2,
           -1,4,-3,-1,
           0,-1,-5,0.6,
           -1,0.33,6,0.5),nrow=4,byrow=4)
# Factorización LU
LUdec=expand(lu(A))
L=matrix(nrow=size(A)[1],ncol=size(A)[2])
U=matrix(nrow=size(A)[1],ncol=size(A)[2])

for(i in c(1:size(A)[1])){
  for(j in c(1:size(A)[2])){
    if(is.na(LUdec[[1]][i,j])){
      L[i,j]=0
    }else{
      L[i,j]=LUdec[[1]][i,j]
    }
    if(is.na(LUdec[[2]][i,j])){
      U[i,j]=0
    }else{
      U[i,j]=LUdec[[2]][i,j]
    }
  }
}

cat("L:\n")
write.table(L,sep=" ",row.names=FALSE,col.names=FALSE)
cat("U:\n")
write.table(U,sep=" ",row.names=FALSE,col.names=FALSE)

# Factorización QR

QRdec=qr(A)[[1]]
R=matrix(nrow=4,ncol=4)
for(i in c(1:size(QRdec)[1])){
  for(j in c(1:size(QRdec)[2])){
    if(j>=i){
      R[i,j]=QRdec[i,j]
    }else{
      R[i,j]=0
    }
  }
}

Q=A%*%inv(R)

cat("Q:\n")
write.table(Q,sep=" ",row.names=FALSE,col.names=FALSE)
cat("R:\n")
write.table(R,sep=" ",row.names=FALSE,col.names=FALSE)

