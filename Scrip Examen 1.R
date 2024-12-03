#Scrip Examen 1



# 1. Abrir un documento web
# 2. Abrir documentos de un lugar seguro
# 3. Abrir archivo en excel
# 4. Ayuda de una funcion
# 5. Ver datos
# 6. Sacar media 
# 7. Varianza
# 8. Sacar desviación estandar


# 9. Restricciones
# 10. Estadistica descriptiva (Tapply, Summary)
# 11. Determinar observaciones menores o iguales a 16.9 cm de Diamtero
# 12. Llamar a la variable como factor
# 13. Hacer un Boxplot
# 14. Realizar un histograma

# 15. Estadistica parametrica
# 15. Establecer hipotesis
# 16. Valor de confiabilidad
# 17. Probabilidad- P-VALUE
# 18. Tipos de distribuciones
# 17. Prueba de normalidad Shapiro
# 18. Prueba de homogenidad de varianzas
# 19. Transformar datos
# 20. Realizar analisis de variansas (ANOVA) B4
# 21. Pruba de t independientes
# 22. Prueba t dependientes
# 23. Prueba de Tukey
# 24. Realizar plot, linea de tendencia central y formula
# 25. Correlación
# 26. Regresión lineal
# 27. Ajustar tabla


# 1. Abrir un documento web
prof_url <- "http://www.profepa.gob.mx/innovaportal/file/7635/1/accionesInspeccionfoanp.csv"
profepa <- read.csv(prof_url, encoding = "latin1")
View(profepa)

#2. Abrir documentos de un lugar seguro

library(repmis)
conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")
View(conjunto)


# 3. Abrir archivo en excel, seleccionar archivo en files
prod <- read.csv("produccion.CSV", header = T)
View(prod)

# Columna -> Variable
# Fila -> Datos de colecta

# 4. Ayuda de una funcion
?tapply

# 5.Ver datos
summary(profepa)


# 6. Sacar media
mean(profepa$Inspección)

# 7. Varianza, cuanto varian los datos alrrededor de la media
var(conjunto$Altura)

# 8. Sacar desviación estandar, calcula cuanto se alejan los datos de la media
sd(conjunto$Altura)


# 9.Restricciones
#igual: ==
# diferente: !=
# mayor igual: >=
# menor igual: <=

ins <- subset(profepa, profepa$Inspección >= mean(profepa$Inspección))
View(ins)

bajo <- subset(profepa, profepa$Inspección <= mean(profepa$Inspección))
View(bajo)

cero <- subset(profepa, profepa$Inspección == 0)
View(cero)

todos <- subset(profepa, profepa$Inspección != mean(profepa$Inspección))
View(todos)

spFH <- subset(conjunto, conjunto$Especie != "C")
View(spFH)

# 10. Estadistica descriptiva (Tapply, Summary)
# La funcion tapply sirve para ver en este caso el promedio de una variable en especifico
# La funcion tapply sirve tambien para ver el número de factores

tapply(spFH$Diametro, spFH$Especie,mean)

tapply(madera$Peso_g,madera$Especie, mean)
tapply(madera$Peso_g,madera$Especie, var)

#Summary te muestra los datos generales
summary(madera)


# 11. Determinar observaciones menores o iguales a 16.9 cm de Diamtero

ObsDi <- subset(conjunto, conjunto$Diametro <= 16.9)
View(ObsDi)
# .......31 observaciones

# 12.Llamar a la variable como factor

conjunto$Especie <- as.factor(conjunto$Especie)
conjunto$Clase <- as.factor(conjunto$Clase)


# 13. Hacer un Boxplot, se puede hacer de una o varias variables

boxplot(spFH$Diametro ~ spFH$Especie,
        xlab = "Especies",
        ylab = "Diametro",
        col= "green")
# Los boxtplot se componen de datos atipicos, max, cuartil 3, cuartil dos (mediana), cuartil 1, minimo

# 14. Realizar un histograma

hist(conjunto$Altura, xlab = "altura", ylab = "frecuencia", main = "Histograma de Altura",
     xlim = c(8,24), ylim = c(0, 14), col = "darkseagreen1")

hist(conjunto$Diametro, xlab = "diametro", ylab = "frecuencia", main = "Histograma de Diametro",
     , col = "cadetblue1")


# 15. Estadistica parametrica
# Metodo que se aplica para una distribución normal, si no se consigue se puede transformar, con log, squrt
# Tambien se pueden tomar mas datos 


# 15. Establecer hipotesis
#Ho:Posee diferencias del peso del la madera entre Barreta y Gavia
#H1:No posee diferencias del peso del la madera entre Barreta y Gavia


# 16. Valor de confiabilidad
# alfa= 0.05


# 17. Probabilidad- P-VALUE
#Sirve para saber si se acepta o se niega una hipotesis

# 18. Tipos de distribuciones
#Normal, campana de Gauss (n)
#Binomiales (m)

# 16. Prueba de normalidad Shapiro, se acepta por que el valor de P es mayor 0.05 

shapiro.test(spFH$Diametro)

# 17. Prueba de homogenidad de varianzas, se acepta por que el valor de P es mayor 0.05

bartlett.test(spFH$Diametro, spFH$Especie)


# 18. Transformar datos, sirve para corregir las varianzas
madera$Peso_t <- log10(madera$Peso_g + 0.5)
madera$Peso_t <- log(madera$Peso_g + 0.5)
sitios$dapt <- log(sitios$DAP+1)
sitios$dapt <- sqrt(sitios$DAP)


# 19. Realizar analisis de variansas (ANOVA) B4
#se acepta si el valo de P es mayor a 0.05
# Se utiliza para 3 repeticiones o mas

sit.aov <- aov(sitios$dapt ~ sitios$Paraje)
summary(sit.aov)

#PRUEBA DE T


# 19. Pruba de t independientes, antes de hacer una prueba de T se tiene que hacer una prueba de normalidad y homogenidad de las varianzas

#DATOS QUE ARROJA:
# (Pruba de t) df son grados de libertad n-2, sirve para corroborar
# (Pruba de t) p se acepta la hipotesis nula, de qie los valores son semejantes
# (Prueba de t) de una muestra: mu- media teorica
# Se acepta la hipotesis nula por que p value es mayor 0.05

t.test(conjunto$Diametro, mu = 22)


# 20. prueba t dependientes
# La diferencia no es significativa
t.test(prod$Kgsem ~ prod$Tiempo, paired = T)


# 21. Prueba de T, con corrector de varianzas

t.test(madera$Peso_g ~ madera$Especie, var.equal = F)

# 22. Prueba de Tukey, se puede analizar con el valor de p si es mayor a 0.05, o si el lwr y el upr no hay diferencias y viceversa 
TukeyHSD(sit.aov)
plot(TukeyHSD(sit.aov))

# 23. Realizar plot
#Dependiente: Edad

plot(edad$DAP, edad$EDAD, pch= 19, col= "darkgreen",
     xlab = "Diametro (cm)",
     ylab = "Edad",
     ylim = c(20,140),
     xlim = c(10,50))

#Linea de tendencia central

abline(edad.lm)

#Formula

text(20, 120, "y = -8.4 * 2.4 (x)")


# 24. Correlación

#En una correlación hay variables dependientes e independientes

#alfa=0.05
#Ho= sI HAY CORRELACIÓN
#H1= No hay correlación
#Correlación 0.79
#Ver P para saber si es significativo, si es menor 0.001
cor.test(edad$DAP, edad$EDAD)


# 25. Regresión lineal
# y=alfa+ beta* X
# alfa  (INTERCEPTO DE X ES IGUAL A 0)
# beta (Incremento que se genera entre X y Y)
edad.lm <- lm(edad$EDAD ~ edad$DAP)
edad.lm
#alfa, intercepto -8.4 beta, 2.4 aumenta

#Para obtener la significancia aplico summary
summary(edad.lm)
#Residuales:diferencia entre el valor observado y el valor predicho, debe ser cercano a 0
#coeficiente: evalua la significancia de beta y alfa, si tiene * para que sea significativo
#R-squared:Predice el valor de la variable dependiente, en este caso la edad
#Ocupa tener mas datos para que alfa sea significativo


# 26. Ajustar tabla

erupciones$res <- erupciones.lm$residuals
erupciones$edprim <- erupciones.lm$fitted.values
erupciones$com.res <- erupciones$eruptions - erupciones$edprim

# 27. Suma de residuales
sum(erupciones$res)

#Varianza 5270 df- "gl"
sum(erupciones$res^2)
sum(erupciones$res^2)/270

# 28.Estimar la edad (prima) para los valores de DAP: 15, 30, 45, 47
# prima <- Intercep + beta*(valores)
valores <- c(15,30,45,47)
prima <- -1.87 + 0.75*(valores)
prima
