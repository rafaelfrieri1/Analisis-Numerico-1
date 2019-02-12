# Punto 15

# Parte a

# Resolviendo y despejando la ecuación se obtiene la siguiente función

f<-function(x){
  5*x-exp(x)-1
}

# Parte b

# Se grafica la función
inter=seq(0,3,0.0001)
plot(inter,f(inter),type="l",col="blue")
abline(h=0,col="black")
# Mediante el gráfico aproximado mostrado, se puede observar que las raices reales se encuentran aproximadamente en un poco más de 0,5 y un poco menos de 2,5.

# Parte c

# Se propone utilizar la siguiente ecuación despejada g(x)=x

g<-function(x){
  (1+exp(x))/5
}

# Para que la serie converja la magnitud de la derivada de g debe ser menor o igual a 1 en cada iteración.
# Si se deriva g(x), se obtiene g'(x)=(e^x)/5, e igualando a 1, x<=ln(5) para que el método converja con dicho despeje

# Parte d

# Se declara la derivada de g, que se usará para verificar que se cumple la condición de convergencia en cada iteración
dgdx<-function(x){
  exp(x)/5
}
# Se declara función del método de punto fijo solicitado
puntoFijo<-function(a,b,x0){
  if((g(a)>a && g(b)<b)||(g(a)<a && g(b)>b)){
    ant=x0
    for(i in c(1:5)){
      if(abs(dgdx(x0))>1){ # Verificacion de criterio de convergencia del punto fijo
        print("Se ha dejado de cumplir la condición de convergencia.")
        break
      }
      cat("|g'(x0)|=",abs(dgdx(x0)),"\n") # Se muestra magnitud de derivada para verificar criterio
      x0=g(x0)
      error=abs(x0-ant)/abs(x0)
      ERROR=abs(x0-ant)
      ant=x0
    }
    return(c(x0,error,ERROR))
  }else{
    print("Dicho intervalo no es apto para el método de punto fijo, utilizar otro.")
  }
}
# Se llama la función con un intervalo apropiado, y un valor inicial que cumple con los criterios de convergencia, luego se muestran resultados finales
res=puntoFijo(0,1,0.5)
cat("Resultado:",res[1],"\nError relativo final:",res[2],"\nError absoluto final:",res[3])

"A partir de los cálculos de error se puede estimar que el error relativo estuvo en al rededor de 0.000739 con respecto al último resultado
y 0.000402 de error absoluto con respecto al último y penúltimo resultado. Además luego de comparar el resultado de la última iteración con
un resultado más exacto dado por una calculadora de alto desempeño se encontró que el error fue aún menor, siendo de al rededor de 0.0002."