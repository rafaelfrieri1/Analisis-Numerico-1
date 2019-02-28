# Punto 2

# Parte a se desea hallar la intersección entre ambas funciones
cat("Método Parte A\n\n\n")
f=function(x){
  tan(pi*x)-cos(pi*x)
}

parteA=function(x0){
  error=100;
  ant=x0;
  ant2=0;
  cont=0;
  cat("----------------------------------------------------------\n")
  cat(formatC( c("Xn","Xn-1","Xn-2","Error est."), width = -15, format = "f", flag = " "), "\n")
  cat("----------------------------------------------------------\n")
  cat(formatC( c(x0,ant,ant2,error), digits=7, width = -15, format = "f", flag = " "), "\n")
  while(error>10e-4){
    x0=ant-(f(ant)*(ant-ant2)/(f(ant)-f(ant2)));
    error=abs(x0-ant)/abs(x0);
    ant2=ant;
    ant=x0;
    cat(formatC( c(x0,ant,ant2,error), digits=7, width = -15, format = "f", flag = " "), "\n")
    cont=cont+1;
  }
  cat("----------------------------------------------------------\n\n")
  cat("Cero de f es approx: ", x0, "con error <=", error,", valor de f(Xn) = ",f(x0)," y número de iteraciones:",cont,"\n")
}
parteA(0.4)
parteA(-1.2)

cat("\n\n\nNewton\n\n\n")
#Parte b, se utilizará el método de Newton para comparar
#Se escribe la función de la derivada para el método de Newton
dfdx=function(x){
  pi*(1/cos(pi*x))^2+pi*sin(pi*x)
}

g<-function(x){x-f(x)/dfdx(x)} # Se declara fórmula iterativa de Newton

newton<-function(a,b,x0){
  cont=0
  if(f(a)*f(b)<0){
    error=100;
    ant=x0;
    cat("----------------------------------------------------------\n")
    cat(formatC( c("Xn","Xn-1","f(Xn)","Error est."), width = -15, format = "f", flag = " "), "\n")
    cat("----------------------------------------------------------\n")
    cat(formatC( c(x0,ant,f(x0),error), digits=7, width = -15, format = "f", flag = " "), "\n")
    while(error>10e-4){
      ant=x0;
      if(dfdx(x0)>=0.00000000000000000000000000000000000000000000000001){
        x0=g(x0);
      }else{
        print("Error por singularidad en el sistema")
        break
      }
      error=abs(x0-ant)/abs(x0);
      cont=cont+1
      cat(formatC( c(x0,ant,f(x0),error), digits=7, width = -15, format = "f", flag = " "), "\n")
    }
  }else{
    print("El intervalo escogido no contiene una raiz de la ecuación.")
  }
  cat("----------------------------------------------------------\n\n")
  cat("Cero de f es approx: ", x0, "con error <=", error,", valor de f(Xn) = ",f(x0)," y número de iteraciones:",cont,"\n")
}
newton(0,0.5,0.4)
newton(-1.5,-1,-1.2)

"Se puede observar en la cantidad de iteraciones que el método de Newton converge más rápido
que el método usado en la parte A, esto posiblemente porque el método de Newton tiene convergencia
cuadrada. Además se puede observar que el método de Newton converge a un valor más exacto de la
solución debido a que la función evaluada en la raiz encontrada en el método de Newton se acerca
más a 0 que con el método de la parte A. Por lo tanto el método de Newton es más eficiente incluyendo
que gracias a que se tiene la derivada calculada, utiliza menos operaciones que el método de la parte
A. (En la tabla se mostrarán n+1 filas ya que la primera es el estado inicial.)"