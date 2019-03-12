require(PolynomF)
# Parte A (gráficas y resultados estimados) y B (polinomios ajustados) (más los otros requisitos pedidos en clase) del ejercicio están organizados en el código
x = c(6,8,10,12,14,16,18,20)
y = c(8.5,9.2,12.7,18.4,21.6,17.9,11,9)
datx3 = c(x[1],x[3],x[5],x[7]); daty3 = c(y[1],y[3],y[5],y[7]) # Datos tomados para el ajuste de 3er orden
datx5 = c(x[1],x[2],x[3],x[6],x[7],x[8]); daty5 = c(y[1],y[2],y[3],y[6],y[7],y[8])  # Datos tomados para el ajuste de 5to orden
datx7 = x; daty7 = y # Datos tomados para el ajuste de 7mo orden
# Se calculan los 3 ajustes polinomiales
polyAjuste3 = poly.calc(datx3,daty3)
polyAjuste5 = poly.calc(datx5,daty5)
polyAjuste7 = poly.calc(datx7,daty7)
# Se realiza la gráfica deseada con los puntos medidos, los puntos de los ajustes, las curvas de los ajustes, la etiqueta de cada una con distintos colores y los títulos tanto de los ejes como el gráfico.
plot(x,y, pch=19, cex=1, col = "red", asp=1,xlab="t (hr)",ylab="T (ºC)",main="Temperatura a diferentes horas, con 3 ajustes polinomiales distintos")
points(datx3,daty3,pch=17,cex=1,col="green")
points(datx5,daty5,pch=11,cex=1,col="orange")
curve(polyAjuste3,add=T,col="red")
#polyAjuste5=polyAjuste5+1
curve(polyAjuste5,add=T,col="blue")
curve(polyAjuste7,add=T,col="black")
text(2,18,labels="Poly3",col="red")
text(22.8,18,labels="Poly5",col="blue")
text(6,18,labels="Poly7",col="black")
# Se muestran los polinomios de ajuste
cat("El polinomio de ajuste de grado 3 es:")
print(polyAjuste3)
cat("\nEl polinomio de ajuste de grado 5 es:")
print(polyAjuste5)
cat("\nEl polinomio de ajuste de grado 7 es:")
print(polyAjuste7)
# Se calculan las temperaturas estimadas a las horas deseadas y se muestran en una matriz o tabla
hrs=seq(7,19,2)

est3=polyAjuste3(hrs)
est5=polyAjuste5(hrs)
est7=polyAjuste7(hrs)

esttot=matrix(c(t(t(hrs)),t(t(est3)),t(t(est5)),t(t(est7))),nrow=7,ncol=4)
colnames(esttot)=c("x","Poly3","Poly5","Poly7")
print("Las estimaciones de temperatura en grados celsius en las horas impares con cada polinomio son:")
print(esttot)

# Se calculan resultados con valores conocidos y error para mostrarlos en una matriz o tabla y llegar a la conclusión de cual es el polinomio de mayor confiabilidad
val3=polyAjuste3(x)
val5=polyAjuste5(x)
val7=polyAjuste7(x)

err3=(abs(y-val3)/abs(y))*100
err5=(abs(y-val5)/abs(y))*100
err7=(abs(y-val7)/abs(y))*100

errtot=matrix(c(t(t(x)),t(t(y)),t(t(val3)),t(t(val5)),t(t(val7)),t(t(err3)),t(t(err5)),t(t(err7))),nrow=8,ncol=8);
colnames(errtot)=c("x","y","PolyVal3","PolyVal5","PolyVal7","%Error3","%Error5","%Error7")
print("Los errores con las horas conocidas son:")
print(errtot)

"Se llegó a la conclusión de que el polinomio de mayor confiabilidad por su menor error
en cada caso es el de grado 7, sin embargo el de grado 5 también tiene una buena confiabilidad
y al ser de menor orden gasta menor recurso computacional. Por tanto dando a este un pequeño 
ajuste de nivel se podría considerar el más eficiente en cuanto a resultados de la misma calidad
que el polinomio de grado 7 y gasto de menor recurso computacional. Sin embargo se realizó 
ajuste y se mostró el resultado gráfico y teórico luego de realizar dicho ajuste, gráficamente
se ve que fue correcto, sin embargo al observar los errores, se vuelve una ecuación de mucha menor
confiabilidad, por lo cual se llega a que la más eficiente es la de grado 7. Si se quiere observar
el efecto de el ajuste del polinomio de grado 5 descomentar línea 16."