# Ejercicios -----------

library(tidyverse)

options(scipen = 999) # Elimina la notación científica de los gráficos

# Cargar encuesta de sueldos de RH
encuesta_rh <- read_delim("https://raw.githubusercontent.com/r4hr/r4hr_introduccion_dplyr/main/Datos/encuesta_sueldos.csv", delim = ",")

# Creo un dataframe para calcular los sueldos por puesto y su desvío estándar.
sueldos_puesto <- encuesta_rh %>% 
  select(puesto, sueldo_bruto) %>% 
  group_by(puesto) %>% 
  summarise(salarios = list(mean_se(sueldo_bruto))) %>% # Crea una lista con el sueldo promedio y los valores máximos y mínimos del desvío estándar
  unnest(salarios) # Deshace la lista para mostrar los resultados como tabla.


sueldos_puesto

# Gráfico del resultado
sueldos_puesto %>% 
  ggplot(aes(x=puesto, y=y)) +
  geom_col()+
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = "dodge")+
  coord_flip() # Rota el gráfico para facilitar la lectura de las etiquetas


# Incorporemos el análisis de género
# Creo un dataframe para calcular los sueldos por puesto y su desvío estándar.
sueldos_puesto2 <- encuesta_rh %>% 
  select(puesto, genero, sueldo_bruto) %>% 
  group_by(puesto, genero) %>% 
  summarise(salarios = list(mean_se(sueldo_bruto))) %>% # Crea una lista con el sueldo promedio y los valores máximos y mínimos del desvío estándar
  unnest(salarios) # Deshace la lista para mostrar los resultados como tabla.


sueldos_puesto2

# Gráfico del resultado
sueldos_puesto2 %>% 
  ggplot(aes(x=puesto, y=y, fill = genero)) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = "dodge")+
  coord_flip() # Rota el gráfico para facilitar la lectura de las etiquetas



# Ejercicio 2
library(ggplot2)
library(tidyverse)

hr_data <- read_delim("HRDataset_v13.csv", delim = ";")

# Calculo el desempeño promedio, agrupado por fuente de reclutamiento
perf_by_source <- hr_data %>% 
  select(RecruitmentSource, PerfScoreID) %>% # Selecciona las columnas
  group_by(RecruitmentSource) %>% 
  summarise(performance_promedio = mean(PerfScoreID)) %>% 
  arrange(-performance_promedio)

perf_by_source

# Hago el gráfico
ggplot(perf_by_source, aes(x=performance_promedio, 
                           y = reorder(RecruitmentSource, performance_promedio))) +
  geom_point() 

# Otra versión del gráfico

#install.packages("hrbrthemes")
library(hrbrthemes) # Agrega nuevos estilos a los gráficos

ggplot(perf_by_source, aes(x=performance_promedio, 
                           y = reorder(RecruitmentSource, performance_promedio))) +
  geom_point(color = ft_cols$yellow, size = 2) +
  labs(title="Desempeño promedio por \n fuente de reclutamiento", # Divide el titulo en dos renglones
       y="",
       x="Desempeño Promedio")+
  theme_ft_rc()+
  theme(plot.title = element_text(hjust = 2)) # Desplaza el título horizontalmente
# Barra invertida (\) Alt + 92 



# Ejercicio 3

ej3 <- hr_data %>% 
  filter(!is.na(EmpSatisfaction)) %>% 
  select(Department, EmpSatisfaction) %>% 
  mutate(EmpSatisfaction = factor(EmpSatisfaction, # Para considerarlos como valores nominales
                                  levels = c(1,2,3,4,5))) 

# Gráfico original
ggplot(ej3,aes(x = Department, fill = EmpSatisfaction))+
  geom_bar(position = "fill") +
  coord_flip() +
  labs(title = "Nivel de Satisfacción por Departamento",
       x="", y="")


# Cambiando la paleta de colores
ggplot(ej3,aes(x = Department, fill = EmpSatisfaction))+
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_brewer() + # Cambia la paleta de Colores
  labs(title = "Nivel de Satisfacción por Departamento",
       x="", y="")



# Ejercicio 4


# Los colores de Data 4HR
data4hr_col <- c("#04B1B5", "#323131")


library(lubridate)

# Creo un dataframe para las altas, y otro para las bajas
altas <- hr_data %>% 
  select(DateofHire) %>% 
  filter(DateofHire != "") %>% 
  mutate(DateofHire = mdy(DateofHire),
         Year = year(DateofHire),
         Count = 1) %>% 
  group_by(Year) %>% 
  summarise(Hires = sum(Count)) %>% 
  filter(between(Year, 2010, 2016)) # Me quedo sólo con los datos entre 2010 y 2016

bajas <- hr_data %>% 
  select(DateofTermination) %>% 
  filter(DateofTermination != "") %>% 
  mutate(DateofTermination = mdy(DateofTermination),
         Year = year(DateofTermination),
         Count = 1) %>% 
  group_by(Year) %>% 
  summarise(Terminations = sum(Count))

altas
bajas

# Uno ambos dataframes
rotacion <- altas %>% 
  left_join(bajas, by = "Year")

rotacion


# Creo un dataset "largo"
rotacion <- rotacion %>% 
  pivot_longer(cols= c("Hires", "Terminations"), # Variables a combinar 
               names_to = "Movimientos",  # Nombre de la nueva variable combinada
               values_to = "Cantidades") # Nombre de la variable con los valores

head(rotacion,8) # Ver las primeras 8 filas

write_delim(x = rotacion, file = "rotacion.csv", delim = ";")

# Gráfico
ggplot(rotacion, aes(x = Year, y = Cantidades, color = Movimientos)) +
  geom_line(size = 1)+
  scale_color_manual(values = data4hr_col) # Uso los colores definidos en el vector de colores


#Ejercicio 5

#Filtro datos espurios de la encuesta.
encuesta_rh <- encuesta_rh %>% 
  filter(between(sueldo_bruto, 20000, 500000))


# Creo un objeto para ahorrar código
grafico <- ggplot(encuesta_rh, aes(x=sueldo_bruto))

# Histograma
grafico + geom_histogram()
# Densidad
grafico + geom_density()
# Boxplot
grafico + geom_boxplot()



#### Análisis Predictivo ####

library(readr) # Cargar archivos csv
library(tidyverse) # Limpiar y manipular datos
library(funModeling)

# Cargo los datos desde una página web
datos_rh <- read_csv("https://raw.githubusercontent.com/mlambolla/Analytics_HR_Attrition/master/HR_comma_sep.csv")

# Exploro el dataset
glimpse(datos_rh)

status(datos_rh) # Para verificar si hay algún dato faltante.

# Elminamos la variable 'sales' y cambiemos los valores de 'salary' a numéricos.
datos_rh <- datos_rh %>% 
  select(-sales) %>%
  mutate(salary = as.numeric(case_when(
    salary == 'low' ~ 0,
    salary == 'medium' ~ 1,
    salary == 'high' ~ 2
  )))


# Cuento cuántos empleados se fueron 
datos_rh %>% 
  count(left)


library(caret) # Paquete para hacer análisis predictivos

# Defino una semilla para poder replicar los resultados
set.seed(234)

# Parto el índice para dividir el dataset en training y test
modelo_hr <- createDataPartition(y = datos_rh$left, p = 0.7,
                                    list = FALSE)

head(modelo_hr,25)

#Armo el dataframe de training [fila, columna]
modelo_hr_train <- datos_rh[modelo_hr,]

# Con el signo - (menos), creamos el dataset de testing, con todas las filas 'que no estén en modelo_hr'
modelo_hr_test <- datos_rh[-modelo_hr,]

# Controlo que las proporciones de bajas sean similares en training y test
modelo_hr_train %>%
  summarise(turnover = mean(left))

modelo_hr_test %>%
  summarise(turnover = mean(left))


# Calculamos un modelo de entrenamiento
modelo_glm2 <- glm(left ~. , family = "binomial",
                   data = modelo_hr_train)

summary(modelo_glm2)


# Chequeo multicolinealidad
library(car)

vif(modelo_glm2)

# VIF cercano a 1 - No hay multicolinealidad
# VIF entre 1 a 5 - Multicolinealidad moderada
# VIF Mayor a 5 - Multicolinealidad alta


# Entreno el modelo - Calculo las probabilidades en los datos de entrenamiento
pred_train <- predict(modelo_glm2, newdata = modelo_hr_train, type = "response")


# Luego aplica esos cálculos en el dataset de test
pred_test <- predict(modelo_glm2, newdata = modelo_hr_test, type = "response")

# Veo los primeros 20 resultados
pred_test[1:20]


# Analizo la distribución de las predicciones
hist(pred_test)


# Asigna las probabilidades a una variable nueva llamada "score".
modelo_hr_test$score <- pred_test

glimpse(modelo_hr_test)

# Luego en base al score, asigno una clase predicha en función a si la probabilidad es mayor a 0.5
modelo_hr_test <- modelo_hr_test %>% 
  mutate(prediccion = ifelse(score > 0.5, 1, 0))

View(modelo_hr_test)


#### Analizando la calidad del modelo ####

# Creo la matriz de confusión
conf_matrix <- table(modelo_hr_test$prediccion, modelo_hr_test$left)
conf_matrix

confusionMatrix(conf_matrix)

# Curva ROC y AUC
library(pROC)

pROC_obj <- roc(modelo_hr_test$left, modelo_hr_test$score,
                smoothed = FALSE,
                # argumentos del intervalo de confianza
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # argumentos del gráfico
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)


# Gain y lift
library(funModeling)

gain_lift(data = modelo_hr_test, score = "score", target = "left")


#### Análisis adicionales ####
ggplot(modelo_hr_test, aes(x = last_evaluation, y = satisfaction_level, color = factor(left)))+
  geom_point(alpha = 0.8)+
  scale_color_manual(values = c("#BFC9CA","#2874A6"))


# Análisis de clusters

# Seleccionamos las variables para elegir los clusters
variables_cluster <- modelo_hr_test %>%
  select(last_evaluation, satisfaction_level)

# Preparo los datos para hacer el cálculo
vc <- scale(variables_cluster)

# Corro el algoritmo de clustering
fit_vc <- kmeans(vc, 3)

# Agrego los clusters ajustados (calculados) al dataset
modelo_hr_test$cluster <- fit_vc$cluster

library(ggthemes) # Para modificar el estilo del gráfico


ggplot(modelo_hr_test, aes(x = last_evaluation, y = satisfaction_level, color = factor(cluster)))+
  geom_point(alpha = 0.8)+
  scale_color_colorblind()


# Filtramos los datos del cluster 2 (Alto desempeño)
modelo_hr_c1 <- modelo_hr_test %>% 
  filter(cluster == 2)

conf_matrix_c1 <- table(modelo_hr_c1$prediccion, modelo_hr_c1$left)

# Veamos todas las métricas de la matriz con esta función del paquete caret
confusionMatrix(conf_matrix_c1)
  
