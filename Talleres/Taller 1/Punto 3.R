# Punto 3

# Se utiliza distancia euclidiana para saber cuando la párticula estará más cerca del punto

f<-function(t){sqrt((2*cos(t)-2)^2+(sin(t)-1)^2)}

# Luego se calcula su derivada para poder calcular sus raices que serán los puntos de máxima y mínima distancia

dfdt<-function(t){(4*sin(t)-(3*sin(t)+1)*cos(t))/(sqrt((sin(t))^2-2*sin(t)+4*(cos(t))^2-8*cos(t)+5))}

# Se realiza método de Newton para llegar al resultado 

Newton<-function(a,b,t0){
  if(dfdt(a)*dfdt(b)<0){
    error=100
    ant=t0
    cont=0
    d2fdt2=deriv(~(4*sin(t)-(3*sin(t)+1)*cos(t))/(sqrt((sin(t))^2-2*sin(t)+4*(cos(t))^2-8*cos(t)+5)),"t",TRUE)
    if(t0==0.5){
      plot(seq(0.58,0.61,0.0001),dfdt(seq(0.58,0.61,0.0001)),type="l",col="blue") #Intervalo escogido para mejor visualización de la convergencia
    }else if(t0==30){
      plot(seq(23,70,0.0001),dfdt(seq(23,70,0.0001)),type="l",col="blue") #Intervalo escogido para mejor visualización de la convergencia
    }else{
      plot(seq(a,b,0.0001),dfdt(seq(a,b,0.0001)),type="l",col="blue") #Caso general
    }
    abline(h=0,col="black")
    while(error>0.0001){
      if(attr(d2fdt2(t0),"gradient")[1]!=0){
        t0=t0-dfdt(t0)/(attr(d2fdt2(t0),"gradient")[1])
      }else{
        print("Método no converge con dicho valor inicial")
        break
      }
      error=abs(t0-ant)/abs(t0)
      ant=t0
      text(t0,0,cont,cex=1.5,col="red")
      cont=cont+1
    }
  }else{
    print("Ingrese otro intervalo ya que el ingresado no tiene raiz única para ser calculada.")
  }
  return(t0)
}

# Debido a que la ecuación de la posición de la partícula es periódica y tiene infinitas soluciones para cuando está más cerca de la partícula se procedió a calcular y graficar el comportamiento de convergencia del primer tiempo positivo en el que sucede
options(digits=4)

min=Newton(0,1,0.5) #Se escogió el x0 para que convergiera en el primer tiempo de distancia mínima

cat("El primer tiempo (positivo) en el cual la distancia es mínima es de:",min,".\nEn este tiempo la distancia de la párticula al punto es de:",f(min),".\n")

#IMPORTANTE
#Si se desea ver con mayor claridad el comportamiento gráfico de la convergencia deben comentarse las 3 líneas anteriores y descomentarse las 3 de abajo, de esta forma se encuentra otro tiempo en el cual la distancia es mínima y hay mayor detalle en la gráfica de convergencia

#options(digits=6)

#min=Newton(60,70,30) #Se escogió x0 para que tuviera suficientes iteraciones de forma que se pudiera observar el comportamiento de la convergencia

#cat("Otro tiempo en el cual la distancia es mínima es de (como en el primer caso):",min,".\nEn este tiempo la distancia de la párticula al punto es de:",f(min),".\n")
