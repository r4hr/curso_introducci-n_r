
#### Objetos y vectores ####

# Esto es un comentario


objeto_1 <- 1979

objeto_2 = 2021 - 1979

objeto_2


(objeto_3 = objeto_1 + objeto_2)

# Vector: conjunto de valores
objeto_4 <- c(1,8,6,9,7)

# Para ver el resultado del objeto uso las teclas Ctrl + Enter
# También puedo usar el ícono  "Run"
objeto_1
objeto_2



# Vector de Texto
nombres <- c("John", "Mick", "Ringo", "George") 

nombres

# Las variables de texto van siempre con comillas.

# Cambiar un elemento del vector 
nombres[2] <- "Paul"
nombres

# Vector numérico
anio_nacimiento <- c(1940, 1942, 1940, 1943)
viven <- c(0,1,1,0)


#### Dataframes ####

instrumentos <- c("guitar", "bass", "drums", "piano")

# Crear un dataframe con los vectores
beatles <- data.frame(nombres, instrumentos, anio_nacimiento, viven)

beatles


# Seleccionando elementos de un dataframe nombre_dataframe[fila, columna]

beatles[1,3]

# Seleccionando una fila entera
beatles[3,]


# Seleccionando una columna entera
beatles[,3]
beatles[,c("nombres", "anio_nacimiento")]


#Modificar un elemento puntual de un dataframe
beatles[4,2] <- "guitar"
beatles


# Seleccionar columnas contiguas
beatles[,2:4]


# Seleccionar columnas no contiguas
beatles[, c(2, 4)]


# También se pueden seleccionar por nombre
beatles[,c("nombres", "viven")]


# Ejemplo en vivo
comision1 <- c('Sergio',"Charo", "Jelvis", "Luis", "Maxi")

comision2 <- c("Fabián","Mariana", "Fernando", "Tatiana", "Tomás")


curso <- data.frame(comision1, comision2)

curso

# Otra forma

curso2 <- cbind(comision1, comision2)

curso2

#### Cargar un archivo ####

install.packages("openxlsx") 

library(openxlsx)


# Cargar un archivo de excel

nomina <- read.xlsx("Nomina.xlsx")



#### Funciones exploratorias ####

# Ver los primeros 6 registros
head(nomina)

# Ver los primeros 10 registros
head(nomina, 10)

# Ver la estructura del dataset
str(nomina)

# Ver los nombres de las variables
names(nomina)

# Función para explorar información estadística del dataset
summary(nomina)


# Con las variables tipo chr no obtuvimos ninguna información salvo la cantidad de filas
# Cambiamos el tipo de variable de 'character' a 'factor'
nomina$ESTADO_CIVIL <- as.factor(nomina$ESTADO_CIVIL)

# Ahora al menos podemos ver la cantidad de personas casadas, en concubinato y solteras.
summary(nomina)

# Una gran librería para explorar datasets
# install.packages("funModeling")
library(funModeling)

status(nomina)


#### Limpiar datos con Tidyverse ####
## install.packages("tidyverse")

library(tidyverse)



# Los verbos de dplyr y el pipe
nomina %>%
  select(AREA, EDAD) %>%          # Selecciona las columnas
  group_by(AREA) %>%              # Agrupa por la variable AREA
  summarise(edad_promedio = mean(EDAD)) %>%   # Crea una variable con la edad promedio
  arrange(-edad_promedio)     # Ordena descendentemente los resultados por la variable que pasemos.

# Problema: listar los empleados con hijos, ordenados por área.
# Tarea: seleccionar las columnas, Area, ID de Empleado, e Hijos, filtrar por los que tienen hijos,
# y ordenarlos por área.

# Tarea ejecutada sin el pipe
arrange(filter(select(nomina, AREA,ID, HIJOS), HIJOS > 0), AREA)

# Misma tarea encadenando las funciones con el pipe
nomina %>%
  select(AREA, ID, HIJOS) %>%
  filter(HIJOS>0) %>%
  arrange(AREA) 

regalos <- nomina %>%
  select(AREA, ID, HIJOS) %>%
  filter(HIJOS>0) %>%
  arrange(AREA) 

write.xlsx(x = regalos, file = "regalos.xlsx")


# La función select
nomina %>% 
  select(AREA, ID, HIJOS) 


nomina %>% 
  select(-HIJOS)


# Variante para seleccionar sólo los datos numéricos.
nomina %>%
  select_if(is.numeric) 

# La función filter
empleados_con_hijos <- nomina %>%
  select(ID, HIJOS) %>% 
  filter(HIJOS > 0) 

empleados_con_hijos



# Filtrar todos los empleados de RRHH
nomina %>% 
  filter(AREA == "RRHH") # Atención al doble signo igual ==

# Filtrar todos los empleados MENOS los de RRHH
nomina %>% 
  filter(AREA != "RRHH") # Este es el operador lógico NOT EQUAL


# Filtrar por dos condiciones
nomina %>%
  filter(ANTIGUEDAD < 1 & AREA == "INSERTOS")

nomina %>%
  filter(ANTIGUEDAD < 1, AREA == "INSERTOS")


#### Prestar atención a las condiciones lógicas

# Tarea: Seleccionar empleados de las áreas de Finanzas y de RRHH
# Esto no funciona
nomina %>%
  filter(AREA == "FINANZAS" &  AREA == "RRHH") # Ninguna persona pertenece a Finanzas y a RRHH 

# Esto sí funciona
nomina %>%
  filter(AREA == "FINANZAS" | AREA == "RRHH")

nomina %>% 
  filter(AREA %in% c("FINANZAS", "RRHH"))


# La función arrange
nomina %>%
  select(ID, EDAD, AREA) %>%
  arrange(EDAD) 

# También se puede ordenar por variables de texto
nomina %>%
  select(ID, AREA, N_CATEG) %>%
  arrange(AREA)

# Ordenar descentemente
# Alternativa 1: la función desc
nomina %>%
  select(ID, EDAD) %>%
  arrange(desc(EDAD))

# Alternativa 2: Usando el símbolo -
nomina %>%
  select(ID, ANTIGUEDAD) %>%
  arrange(-ANTIGUEDAD)

# Función group by
nomina %>%
  select(AREA, ANTIGUEDAD) %>%
  group_by(AREA) %>% 
  tally() # Contar por grupo

# La función summarise
nomina %>%
  select(AREA, ANTIGUEDAD) %>%
  group_by(AREA) %>%
  summarise(Ant_Promedio = mean(ANTIGUEDAD)) # Crea una columna llamada Ant_Promedio 
                                            # y calcula el promedio de antigüedad por área

nomina %>%
  select(AREA, ANTIGUEDAD) %>%
  group_by(AREA) %>%
  summarise(Ant_Promedio = mean(ANTIGUEDAD),
            desvio = sd(ANTIGUEDAD))




# Crea columnas nuevas. También se usa para modificar las columnas.
nomina %>%
  select(ID, EDAD) %>%
  mutate(Diferencia_Edad = EDAD - mean(EDAD),
         conteo = 1) 

# Función rename
nomina <- nomina %>%
  rename(CATEGORIA = N_CATEG) # Primero va el nombre nuevo, y luego el nombre original de la variable

names(nomina)


#### Unir archivos ####

# Traemos una tabla nueva
puestos <- read.xlsx("Nomina.xlsx", sheet = "Puestos")

# Usemos una función de tidyverse para explorar el dataframe
glimpse(puestos)

# La función left_join
nomina_full <- nomina %>%       # Creamos un nuevo objeto con ambas tablas
  left_join(puestos, by = "ID") # Primero el nombre del dataframe, luego la variable de unión.

glimpse(nomina_full)

# Creo una subtabla nueva filtrando los datos no nulos de la variable PUESTO
mensuales <- nomina_full %>% 
  filter(!is.na(PUESTO))


glimpse(mensuales)

# Creo un objeto con los sueldos promedios por puesto
sueldos_promedios <- mensuales %>% 
  select(PUESTO, SUELDO) %>% 
  group_by(PUESTO) %>% 
  summarise(sueldo_promedio = mean(SUELDO))

sueldos_promedios

# Guardar el resultado en un archivo de Excel
write.xlsx(x = sueldos_promedios, file = "Sueldos promedios.xlsx") 

# Nuestro primer gráfico
ggplot(sueldos_promedios, aes(x = PUESTO, y = sueldo_promedio)) +
  geom_col()


# Mejoremos la calidad del gráfico
ggplot(sueldos_promedios, 
       aes(x = reorder(PUESTO, sueldo_promedio), # Ordena el gráfico por sueldos_promedios 
           y = sueldo_promedio)) + 
  geom_col() +
  coord_flip() + # Rota  el gráfico
  labs(title = "Sueldos Promedios por Puesto", # labs nos permite controlar el texto del gráfico
       subtitle = "Año 2016",
       x = "Puesto",
       y = "Sueldo Promedio",
       caption = "Datos: Curso Introducción a R para RRHH") +
  theme_minimal() # Cambia el estilo del gráfico
  

status(nomina_full)

mensuales %>% 
  select(SUELDO) %>% 
   mutate(SUELDO_ANUAL = SUELDO * 13)

#### Ejercicios ####

library(readr)
hr_data <- read_delim("Datasets/HRDataset_v13.csv", delim = ";")

perf_by_source <- hr_data %>% 
  select(RecruitmentSource, PerfScoreID) %>% 
  group_by(RecruitmentSource) %>% 
  summarise(performance_promedio = mean(PerfScoreID)) %>% 
  arrange(-performance_promedio)

write.xlsx(x = perf_by_source, file = "desempeño_por_fuente.xlsx")

hr_data %>% 
  select(RecruitmentSource, PerfScoreID) %>% 
  group_by(RecruitmentSource) %>% 
  summarise(performance_promedio = mean(PerfScoreID)) %>%
  arrange(-performance_promedio) %>% 
  top_n(5)

library(lubridate)

hr_terminate <- hr_data %>%
  select(Sex, DateofTermination) %>%
  filter(DateofTermination != "") %>%                # Filtramos las celdas vacías
  mutate(DateofTermination = mdy(DateofTermination), # El campo tiene un formato mes, día, año
         Year_Termination = year(DateofTermination), # Extraemos el año de la variable DateofTermination
         Sex = factor(Sex, levels = c("F", "M"), labels = c("Female", "Male")),
         Count = 1) %>%         # Agregamos una columna con un valor = 1 para poder sumar los casos
  group_by(Year_Termination, Sex) %>%
  summarise(Terminations = sum(Count))

ggplot(hr_terminate, aes(x = Year_Termination, y = Terminations, color = Sex))+
  geom_line(size = 1)

library(funModeling)

status(hr_data)

mens<- mensuales %>% 
  select(PUESTO, SUELDO) %>% 
  group_by(PUESTO) %>% 
  summarise(sueldo_promedio = list(mean_se(SUELDO))) %>% 
  unnest(sueldo_promedio)

mens

mens %>% 
  ggplot(aes(x=PUESTO, y=y)) +
  geom_col()+
  geom_errorbar(aes(ymin = ymin, ymax = ymax), position = "dodge")+
  coord_flip()

