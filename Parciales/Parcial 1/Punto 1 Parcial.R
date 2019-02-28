# Punto 1

# Parte A

# Se encontró durante el trabajo externo que el número de multiplicaciones en el método de gauss y gauss-jordan son respectivamente

Wg=function(n){
  (n^3)/3+n^2-n/3
}

Wgj=function(n){
  (n^3)/2+(n^2)/2
}

# Las gráficas y tablas se realizaron en MATLAB, se adjuntan a un archivo word en el parcial

# Parte B
# Se declaran la matriz original y la matriz modificada
Ao=matrix(c(2.6,0.3,2.4,6.2,
           7.7,0.4,4.7,1.4,
           5.1,9.9,9.5,1.5,
           6.0,7.0,8.5,4.8),nrow=4,byrow=4)

Am=matrix(c(2.6,0.3,2.4,6.2,
            7.7,0.4,4.7,1.4,
            5.1,9.9,9.5,1.5,
            6.1,7.0,8.5,4.8),nrow=4,byrow=4)

b=c(4,5,6,11)

# Se calculan ambas soluciones utilizando eliminación gaussiana

gauss = function(A, b){ # Se supone det(A) != 0
  n = nrow(A) # = ncol(A) para que sea cuadrada
  # matriz ampliada
  Ab = cbind(A,b)
  # Eliminación
  for (k in 1:(n-1)){ # desde columna k=1 hasta k=n-1
    if(Ab[k,k]==0){ # intercambio de fila
      fila = which(Ab[k, ]!=0)[1]
      Ab[c(k, fila), ] = Ab[c(fila, k), ]
    }
    # Eliminación columna k
    for (i in (k+1):n){# debajo de la diagonal
      # Fi = Fi - a_ik/a_kk * Fk, i=k+1,...,n
      Ab[i, ] = Ab[i, ] - Ab[i, k]/Ab[k,k]*Ab[k, ]
    }
  }
  # Sustitución hacia atrás-------------------------
  # b(i) = A[i, n+1]
  x = rep(NA, times=n)
  x[n] = Ab[n, n+1]/Ab[n,n] # xn = bn/a_nn
  for(i in (n-1):1 ){
    x[i]= (Ab[i, n+1] -sum(Ab[i, (i+1):n]*x[(i+1):n]) ) /Ab[i,i]
  }
  return(x)
}

# Se calculan las soluciones con ambas matrices
Xo=gauss(Ao,b)
Xm=gauss(Am,b)

# Para calcular la variación se calcula el error entre la norma de ambas

error=((Norm(Xo)-Norm(Xm))/Norm(Xo))*100
cat("La variación de la solución de la matriz modificada respecto a la solución de la matriz original fue de un: ",error,"%\n")

# Luego para calcular la cota del error se utiliza la fórmula que incluye al número de condición y el error entre las magnitudes de la matriz A original y A modificada que se muestra en el libro

cond=norm(Ao,type="i")*norm(inv(Ao),type="i") # Cálculo del número de condición
exmax=(cond*norm(Ao-Am,type="i")/norm(Ao,type="i"))*100 # Cálculo de la cota del error cond(Ao)*mag(Am-Ao)/mag(Am), utilizada en el libro

cat("La cota del error es de: ",exmax,"%\n") # Se muestra cota de error dada por la ecuación sacada del libro