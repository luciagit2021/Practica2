
#' "Heart Attack Analysis & Prediction dataset"
#' Autor: Lucía Fernández González
#' output: csv
#' ---

rm(list = ls())
getwd()
pacman::p_load(glue, openxlsx, funModeling, reshape2, arules, Hmisc, tidyverse, tidyselect)

######## 2.	INTEGRACIÓN Y SELECCIÓN DE LOS DATOS

datos= read.csv2("heart.csv", sep = ",", header = TRUE )
names(datos)[c(1:14)]<- c("edad", "sexo", "tipo_dolor", "presion", "colest", "azucar", "result_electro",
                          "frec_card", "angina_ejer", "pico_previo", "pendiente_ST", "num_vasos", 
                          "talasemia", "output")  ###cambio el nombre de las variables para interpretarlas mejor

clase = c() ### compruebo la clase de variable que es cada uno de los campos
for (i in 1:14) {clase[i] <- class(datos[1:302,i])
cat(names(datos)[c(i)],"-", clase[i], " ") }

datos <- datos %>%
  mutate(across(
    .cols = c(sexo, tipo_dolor, azucar, result_electro, angina_ejer, pendiente_ST, talasemia, output),
    ~ as.factor(.x))) %>%
  mutate(pico_previo = as.numeric (pico_previo))

datos = unique(datos)  ###elimino registros duplicados
datos = datos[,-c(12)] ### prescindo del campo nº de vasos principales

v = discretize(datos$edad, breaks = 4, onlycuts = TRUE) ## intervalos con frecuencias iguales
datos2 <- datos %>% mutate (tramos_edad = cut (edad, breaks = c (0,44,54,64,80),
                                              labels = c("<=44", "45-54", "55-64", "de 65 o más")))

######## 3.	LIMPIEZA DE LOS DATOS
####### 3.1 CEROS O ELEMENTOS VACÍOS

status(datos2) ## comprobamos los tipos de datos, si hay valores perdidos, o ceros en las variables enteras.

numer = c()   ###### Existencia de valores atípicos en las variables numéricas
factor = c()
for (i in 1:13) {if (class(datos[1:302,i])!= "factor") numer[i]= i else factor[i] = i}
numer = na.omit(numer)
factor = na.omit (factor)
datos_numer <-  datos %>% select(all_of(numer))

long <- melt(datos_numer)
plot( value ~ variable , data=long)
atipicos = lapply(numer,function(i) boxplot.stats(datos[,i])$out)
atipicos
#### tipifico las variables para mejorar la comparabilidad
datos_numer1 <- as.data.frame(scale(datos_numer, center = TRUE, scale = TRUE))
long <- melt(datos_numer1)
plot( value ~ variable , data=long)

write.csv2(datos, "resultados.csv", row.names =FALSE)
############# 4 ANÁLISIS DE LOS DATOS

describe(datos) ## una breve descripción de cada variable

#n: cantidad de filas que no son NA. En este caso, ninguna
#missing: cantidad de valores faltantes. 
#unique: cantidad de valores únicos (o distintos).
#Info: un estimador de la cantidad de información presente en la variable y que no es importante en este punto.
#Mean: promedio o media.
#Números: .05, .10, .25, .50, .75, .90 y .95 son percentiles. Estos valores son muy útiles ya que nos ayudan a describir la distribución. 
#lowest y highest: los cinco valores mínimos/máximos. Aquí podemos detectar valores atípicos y errores de datos. Por ejemplo, si la variable representa un porcentaje, entonces no puede contener valores negativos.

profiling_num(datos)
pico_previo_min_max = data.frame(pp_norm=datos$pico_previo/max(datos$pico_previo)+1)
profiling_num(pico_previo_min_max)
# coeficiente de var: desv típica/media. Mide la dispersión de los datos.
#kurtosis: describe las colas de la distribución; dicho en términos simples, un número alto puede indicar la presencia de valores atípicos (tal como veremos más adelante para la variable SI.POV.URGP que tiene un valor atípico cerca de 50). Para leer un repaso completo de asimetría y curtosis, diríjanse a las Referencias (McNeese 2016) y (Handbook 2013).
#iqr: el rango intercuartil es el resultado de observar los percentiles 0.25 y 0.75, e indica, en la misma unidad de la variable, el largo de dispersión del 50% de los valores. Cuanto mayor sea el valor, más dispersa es la variable.
#range_98 y range_80: indican el rango en el que el se encuentra el 98% de los valores. Quita el 1% inferior y superior (ergo, el número 98%). Es bueno saber cuál es el rango de la variable sin valores atípicos potenciales. Por ejemplo, pop_living_slums va de 0 a 76.15. Es más robusto que comparar los valores mínimos y máximos. range_80 es igual que range_98 pero sin el 10% inferior y superior.

plot_num(datos, bins = 20)
freq(datos)

#### comparación entre grupos
cross_plot(datos, input="sexo", target="output")
cross_plot(datos2, input="tramos_edad", target="output", path_out="my_plots")
vars_to_analyze=c("pico_previo", "frec_card")
plotar(data=datos, input=vars_to_analyze, target="output", plot_type = "histdens")


#### 4.2 COMPROBACIÓN DE LA NORMALIDAD Y HOMOGENEIDAD DE LA VARIANZA
## normalidad shapiro wilks
SW= lapply (datos_numer, shapiro.test)
pv=data.frame()
for (i in 1:5){pv[i,1:2] = data.frame(variable = names(datos_numer)[c(i)],p.value= SW[[i]][2])}
pv<- pv %>% mutate(rechazo = ifelse(p.value < 0.05, 1, 0))


### homogeneidad de varianzas Fligner-Killeen
datos_test = cbind(datos_numer, output = datos$output)
fk = data.frame()
for (i in 1:5){fk[i,1:2] = data.frame(variable = names(datos_test)[c(i)],p.value=fligner.test(datos_test[,i] ~ output, data = datos_test)$p.value)}
fk<- fk %>% mutate(rechazo = ifelse(p.value < 0.05, 1, 0))

######## 4.3 PRUEBAS ESTADÍSTICAS 
### CORRELACIÓN DE LAS VARIABLES
##Vamos a ver cómo está correlacionadas entre sí las variables numéricas 
sp1 = matrix(ncol=5,nrow=5,dimnames = list(names(datos_numer),names(datos_numer)))
sp_pv= matrix(ncol=5,nrow=5,dimnames = list(names(datos_numer),names(datos_numer)))                                           
for (i in 1:5){for (j in 1:5) { sp1[i,j] = cor.test(datos_numer[,i], datos_numer[,j],method = "spearman")$estimate}}
for (i in 1:5){for (j in 1:5) { sp_pv[i,j] = cor.test(datos_numer[,i], datos_numer[,j],method = "spearman")$p.value}}

##### CHI-CUADRADO PARA COMPARAR FACTORES
datos_fact <-  datos %>% select(all_of(factor))
chi2 = data.frame()
for (i in 1:7){chi2[i,1:2] = data.frame(variable = names(datos_fact)[c(i)],p.value=chisq.test(datos_fact[,i], datos_fact$output)$p.value)}
chi2<- chi2 %>% mutate(rechazo = ifelse(p.value < 0.05, 1, 0))

##### WILCOXON Y MANN-WHITNEY PARA COMPARAR UN FACTOR Y UNA VARIABLE NUMÉRICA
W = data.frame()
for (i in 1:5){W[i,1:2] = data.frame(variable = names(datos_test)[c(i)],p.value=wilcox.test(datos_test[,i] ~ output, data = datos_test)$p.value)}
W<- W %>% mutate(rechazo = ifelse(p.value < 0.05, 1, 0))

####REGRESIÓN LOGISTICA
model1 = glm(output ~ presion , family = "binomial",data = datos_test)
model2 = glm(output ~   frec_card +  presion   , family = "binomial",
             data = datos)
predict(model2, data.frame( frec_card = 150, presion = 140))
    
