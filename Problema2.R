#Codigo para problema 2
iris
mis_dades <- iris
y = mis_dades$Sepal.Length
y

x = mis_dades$Petal.Length
x

plot(x,y)

xbar = mean(x)
xbar
ybar = mean (y)
ybar

m = sum((x-xbar)*(y-ybar))/sum((x-xbar)^2)
m

b = ybar-m*xbar
b

m*1.5+b

mod = lm(y~x)
lm(y~x)
data.frame(x=x)
recta = predict (mod, data.frame(x=x)) # Predice los valores que tomaran cada y para formar la recta de regresión es decir por minimos cuadrados
recta

plot(x,y, pch = 16, col="red")
lines(x,recta) #Superpone al gráfico anterior para ver el gráfico con la recta de regresión

#Cual es el coeficiente de determinación? la dispersión respecto la recta de reresión, 1 es No hay dispersión, 0 hay mucha dispersión

Rsq = sum((recta-ybar)^2)/sum((y-ybar)^2)
Rsq # resultado 0,76, es decir hay poca dispersión

summary (mod)
#Multiple R-squared --> El que hay que mirar

#coeficiente de correlacion?
sqrt(Rsq)

cor.test(x,y)



