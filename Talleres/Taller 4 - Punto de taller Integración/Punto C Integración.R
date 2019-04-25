# Punto C integración

library("PolynomF")
# Se guardan los puntos por los que pasa la gráfica
x=c(0.1,0.2,0.3,0.4,0.5)
y=c(1.8,2.6,3,2.8,1.9)
# Para tener una función sobre la cual utilizar la reglas de simpson se utiliza un ajuste polinomial
polyAjuste=poly.calc(x,y)
cat("La función interpolada a integrar será: ")
print(polyAjuste)
# Se grafican para mostrar que se obtiene un comportamiento correcto
plot(x,y)
lines(polyAjuste,col="blue")

# Se utiliza fórmula de Simpson para hallar área bajo la curva, límites 0.1 a 0.5

a=0.1
b=0.5

integr=((b-a)/6)*(polyAjuste(a)+4*polyAjuste((a+b)/2)+polyAjuste(b))

# Se halla el error máximo
h=(b-a)/2

error=-((h^5)/90)*(125/3*24)

cat("\nEl valor de la integral en el intervalo entre 0.1 y 0.5 calculado con la fórmula de simpson fue de:",integr,"\nCon un error de:",error," es decir el valor real de la integral es de: ",integr+error,"\n")

# Se obtiene luego la misma integral con la regla del trapecio

integrtrap=(b-a)*(polyAjuste(a)+polyAjuste(b))/2

# Se calcula el error para la regla del trapecio

errortrap=-(1/12)*(175/6-75*6*0.2754+4*125*0.2754^2)*(b-a)^3

cat("\nEl valor de la integral en el intervalo entre 0.1 y 0.5 calculado con la fórmula de simpson fue de:",integrtrap,"\nCon un error de:",errortrap," es decir el valor real de la integral es de: ",integrtrap+errortrap,"\n")

"Se observa que el error es mayor el error utilizando el método del trapecio que el error dado por el método
de simpson, y esto se puede observar en las fórmulas de error, por lo cual el método de simpson arrojó un valor
más cercano al valor de la integral que el método del trapecio como se observó en los resultados. Esto
lógicamente depende de la cantidad de trapecios utilizados en el método del trapecio, ya que a mayor cantidad
el error puede disminuir igual que en el método de simpson, que si se usa una variación más compleja el error
debe disminuir, sin embargo en el caso básico que es el mostrado el resultado es descrito como se dijo. Cuando
se suman los errores a los resultados arrojados por cada uno de los métodos se obtiene el valor real e la
integral, verificado por una calculadora online, integrando la función interpolada de 0.1 a 0.5, se asume que
la función interpolada no incluye error, solo se usa como instrumento para los métodos, es decir esta es correcta (Se desea analizar error de métodos de integración).
Luego una forma de utilizar poco recurso computacional y obtener el resultado deseado es sumando el error al final
del cálculo. En conclusión al problema principal del ejercicio, el método de Simpson es más efectivo que el método
del trapecio, ambos en su expresión más simple, por lo cual si se debe utilizar alguno es preferible este."