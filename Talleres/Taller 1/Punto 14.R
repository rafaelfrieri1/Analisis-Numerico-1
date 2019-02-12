# Punto 14

# Parte a
"Para asegurar la existencia de una raiz en el intervalo real positivo [a,b], se
debe cumplir que f(a)*f(b)<0 y que f sea continua en dicho intervalo, 
ya que esto denota al menos un cambio de signo de f en el intervarlo. 
Para que dicha solución sea única, la cantidad de cambios de signo
en el intervalo [a,b] de la función f debe ser exclusivamente 1, o
en otras palabras que la secuencia de Sturm en dicho intervalor tenga
solo un cambio de signo, lo que significa que hay un único c, tal que
f(c)=0. Si la función cumple con las condiciones anteriores, su raiz 
siempre podrá ser calculada, sin embargo su valor exacto solo se podrá
hallar si se trata de un número racional, porque de lo contrario el 
resultado será una aproximación de la solución."

# Parte b

"El orden de convergencia del método es lineal, y el factor de convergencia (pendiente de la recta de caida del error)
debe ser de 1/10, ya que este es el factor mediante el cual x se va acercando
o alejando de la raiz de la ecuación."

# Parte c
# Se declara una función para calcular su raiz (Puede ser cualquiera)
f<-function(x){exp(x)-pi*x}

# Código del proceso descrito
calcRaiz<-function(a,b,E){
  x=a
  if(a>=0 && b>0 && f(a)*f(b)<0){
    ant=f(x)
    d=(b-a)/10
    while(d>E){
      x=x+d
      nuevo=f(x)
      if(ant*nuevo<0){
        x=x-d
        d=d/10
      }else{
        ant=nuevo
      }
    }
    print(x)
  }else{
    print("Ingrese otro intervalo, ya que este no aplica para el método numérico.")
  }
}

# Se calculan ambas raices de la ecuación con precision de 10^-8
calcRaiz(0,1,0.00000001)
calcRaiz(1,2,0.00000001)