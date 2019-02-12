raizN<-function(a,n,x0){ #Donde a es el número del que se sacará raiz, n el orden de la raiz y x0 el punto donde comienzan las iteraciones
  cont=0
  ant=x0
  while(cont<100){
    x0=(1/n)*((n-1)*x0+a/(x0)^(n-1))
    ant=x0
    cont=cont+1
  }
  print(x0)
}

#Cálculos de algunas raices de distinto orden y distinto x0
raizN(4,2,0.4)
raizN(-27,3,4)
raizN(3,5,-3)