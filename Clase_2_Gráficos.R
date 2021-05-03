# Paquetes ---------------------------------------------

# Instalar paquetes
install.packages("tidyverse")
install.packages("openxlsx")
install.packages("readr")
install.packages("ggplot2")

# Cargar paquetes
library(tidyverse)
library(openxlsx)
library(readr)
library(ggplot2)


getwd() # Cuál es el directorio de trabajo


# Carga de datos ----------------------------------------
# Cargo los dataframes de Nómina


hr_data2 <- read_delim("Datasets/HRDataset_v13.csv", delim = ";")

nomina <- read.xlsx("Nomina.xlsx")
puestos <- read.xlsx("Nomina.xlsx", sheet = "Puestos")


# Uno las tablas y creo el dataframe de los mensuales
mensuales <- nomina %>% 
  left_join(puestos, by = "ID") %>% 
  filter(!is.na(PUESTO))

#### Ejercicios Clase 1 ####

# Ejercicio 1
mensuales %>% 
  select(AREA, SUELDO) %>% 
  group_by(AREA) %>% 
  summarise(Dispersion_Salarial = sd(SUELDO)) %>% 
  arrange(Dispersion_Salarial)


# Ejercicio 2
nomina %>% 
  filter(HIJOS == 0) %>% 
  tally() # Es una función para contar resultados

# Ejercicio 2 - Variante
nomina %>% 
  count(HIJOS==0)

# Ejercicio 2 - Variante
library(funModeling)

status(nomina)


# Cargo el archivo HRDataset_v13
hr_data <- read_delim("Datasets/HRDataset_v13.csv", delim = ";")

dim(hr_data)

# Ejercicio 3
perf_by_source <- hr_data %>% 
  select(RecruitmentSource, PerfScoreID) %>% 
  group_by(RecruitmentSource) %>% 
  summarise(performance_promedio = mean(PerfScoreID)) %>% 
  arrange(-performance_promedio)

perf_by_source

library(hrbrthemes)

ggplot(perf_by_source, aes(x=performance_promedio, 
                           y = reorder(RecruitmentSource, performance_promedio))) +
  geom_point(color = ft_cols$yellow, size = 3) +
  labs(title="Desempeño promedio por \n fuente de reclutamiento",
       y="Fuente de Reclutamiento",
       x="Desempeño Promedio", 
       caption = "Datos ficticios - Curso Introducción a R para RRHH")+
  theme_ft_rc()+
  theme(plot.title = element_text(hjust = 2),
        axis.title.y = element_text(size=8))

ggsave(filename = "perf-by-source.png", dpi = 1200, plot = last_plot(), height = c(cm= 6))

grafico_prueba <- ggplot(perf_by_source, aes(x=performance_promedio, 
                                             y = reorder(RecruitmentSource, performance_promedio)))

grafico_prueba +
  geom_col()


hr_data %>% 
  select(RecruitmentSource, PerfScoreID) %>% 
  group_by(RecruitmentSource) %>% 
  summarise(performance_promedio = mean(PerfScoreID)) %>% 
  arrange(-performance_promedio) %>% 
  ggplot(aes(x=performance_promedio, 
             y = reorder(RecruitmentSource, performance_promedio))) +
  geom_point() 

mensuales


# Grabar el resultado en un archivo
write.xlsx(x = perf_by_source, file = "desempeño_por_fuente.xlsx")


#### ggplot2 ####

write.xlsx(mensuales, file = "mensuales.xlsx")


## install.packages("ggplot2")
library(ggplot2)

# Creo rangos de edad, combinando las funciones mutate y case_when
mensuales <- mensuales %>% 
  mutate(Rangos_Edad = case_when(
    EDAD %in% 18:30 ~ "Hasta 30",
    EDAD %in% 31:40 ~ "Entre 31 y 40",
    EDAD %in% 41:50 ~ "Entre 41 y 50",
    EDAD %in% 51:70 ~ "Más de 50"),
    Rangos_Edad = factor(Rangos_Edad, levels = c("Hasta 30", "Entre 31 y 40",
                                                 "Entre 41 y 50", "Más de 50")))


# Histogramas
ggplot(mensuales, aes(SUELDO)) +
  geom_histogram() 

# Modificar la cantidad de bins
ggplot(mensuales, aes(SUELDO))+
  geom_histogram(bins = 15)+
  ggtitle("bins = 15") # Agrega el título al gráfico

# Modificar el rango de los bins
ggplot(mensuales, aes(SUELDO))+
  geom_histogram(binwidth = 5000)+
  ggtitle("bindwidth = 5000") # Agrega el título al gráfico


# Gráfico de densidad
ggplot(mensuales, aes(SUELDO)) +
  geom_density(color="blue", fill="blue", alpha=0.3)


summary(mensuales$SUELDO)
summary(mensuales)


# Boxplot
ggplot(mensuales, aes(x = PUESTO, y = SUELDO)) +
  geom_boxplot()


summary(mensuales$SUELDO)

# Barras con geom_col
mensuales %>%
  group_by(PUESTO) %>%
  summarise(Sueldo_Promedio = mean(SUELDO)) %>%
  ggplot(aes(x = PUESTO, y = Sueldo_Promedio))+
  geom_col()+ ggtitle("geom_col") +
  theme(axis.text.x = element_text(angle = 90)) # Rota el texto del eje x en 90°


  
# Barras con geom_bar
ggplot(mensuales, aes(x = PUESTO))+
  geom_bar()+
  ggtitle("geom_bar")+
  theme(axis.text.x = element_text(angle = 90)) # Rota el texto del eje x en 90°


ggplot(mensuales, aes(x = AREA, fill  factor(EDAD)))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Empleados por rango de edad por área")



ggplot(mensuales, aes(x = AREA, fill = Rangos_Edad))+
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Empleados por rango de edad por área")


# Gráfico apilado al 100%
ggplot(mensuales, aes(x = AREA, fill = Rangos_Edad))+
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Proporción de rangos de edad por área")


# Gráfico con barras una al lado de la otra.
mensuales %>%
  filter(AREA %in% c("LAMINADO", "TEMPLADO")) %>%
  ggplot(aes(x = AREA, fill = Rangos_Edad))+
  geom_bar(position = "dodge")+
  ggtitle("Rangos de edad en Laminado y Templado")

ggplot(mensuales, aes(x = AREA))+
  geom_bar(position = "dodge")+
  ggtitle("Rangos de edad")

# Gráfico de barras con geom_col
mensuales %>%
  group_by(PUESTO) %>%
  summarise(Sueldo_Promedio = mean(SUELDO)) %>%
  ggplot(aes(x = PUESTO, y = Sueldo_Promedio)) +
  geom_col() +
  ggtitle("Sueldos promedios por puestos") 

mens <- mensuales %>%
  group_by(PUESTO) %>%
  summarise(Sueldo_Promedio = mean(SUELDO))


ggplot(mens, aes(y = PUESTO, x = Sueldo_Promedio)) +
  geom_col() +
  ggtitle("Sueldos promedios por puestos") 

# Gráfico ordenado
mensuales %>%
  group_by(PUESTO) %>%
  summarise(Sueldo_Promedio = mean(SUELDO)) %>%
  ggplot(aes(x = reorder(PUESTO, -Sueldo_Promedio), 
             y = Sueldo_Promedio))+
  geom_col()+
  ggtitle("Sueldos promedios por puesto")+
  theme(axis.text.x = element_text(angle = 90))

library(lubridate) # Paquete para trabajar con fechas
library(stringr)
library(readr)

# Cargo un nuevo dataframe
hr_data <- read_delim("rotacion.csv", delim = ";")

glimpse(hr_data)

hr_data


# Gráfico de líneas
ggplot(hr_data, aes(x = Year, y = Cantidades))+
    geom_line()

# Gráfico de líneas configurado
ggplot(hr_data, aes(x = Year, y = Cantidades, color = Movimientos))+
  geom_line(size = 1)

# Gráfico de líneas configurado
ggplot(hr_data, aes(x = Year, y = Cantidades, color = Movimientos))+
  geom_line(size = 1) +
  geom_point(size = 2)+
  theme_minimal() +
  labs(title = "Ingresos y Egresos por Año",
       x = "", y = "") # Elimina las etiquetas de los ejes

install.packages("gargle")
install.packages("googlesheets4")

library(gargle) # Mejora la conversión de archivos de Google al español
library(googlesheets4) # Carga planillas de cálculo de Google

# Cargamos los datos de la encuesta de Sysarmy
gs4_deauth() # No hace falta registrarse en google para acceder al archivo
options(scipen = 999) # Cambia la notación científica de los gráficos

encuesta <- sheets_read("1f3MCA81NnjI48C03GxxGu69i2hKZLrpxf4tHzKkXL_k")

glimpse(encuesta_sysarmy)


# Filtramos algunas variables para trabajar
seleccion <- c("Años de experiencia",
               "Trabajo de",
               "Salario mensual BRUTO (en tu moneda local)", 
               "Me identifico" )

# Creo un nuevo dataframe sólo con las columnas seleccionadas
sysarmy <- encuesta_sysarmy %>%
  select(seleccion)

# Filtro algunos datos para que tengan más sentido
sysarmy <- sysarmy %>%
  rename(Sueldo = "Salario mensual BRUTO (en tu moneda local)", # Cambio nombres de variables
        Puesto = "Trabajo de",
        Experiencia = "Años de experiencia",
        Sexo = "Me identifico") %>%
  filter(between(Sueldo,20000,1000000)) %>% # Filtro los sueldos entre AR$ 20.000 y 1.000.000
  mutate(Experiencia = unlist(Experiencia), # Elimino el formato lista de la variable
         Experiencia = as.numeric(Experiencia), # Le pido a R que trate a esta variable como numérica
         Puesto = factor(Puesto),
         Sexo = factor(Sexo))
           
summary(sysarmy$Sueldo)


ggplot(sysarmy, aes(x=Sueldo))+
  geom_histogram()

ggplot(sysarmy, aes(x=Sexo))+
  geom_bar()



# Filtramos algunos puestos de interés
analisis_puestos <- sysarmy %>%
  filter(Puesto %in% c("Developer", "SysAdmin / DevOps / SRE", "Technical Leader"))

# Un scatterplot básico por variables numéricas
ggplot(analisis_puestos, aes(x = Experiencia, y = Sueldo, color=Puesto))+
  geom_point()+
  facet_wrap(~Puesto)

# Un scatterplot básico con una variable numérica y otra nominal
ggplot(analisis_puestos, aes(x = Puesto, y = Sueldo)) +
  geom_point(position="jitter", alpha = 0.2)

ggplot(analisis_puestos, aes(x = Puesto, y = Sueldo, shape = Sexo)) +
  geom_point(alpha = 0.4, size=3, position = "jitter")


library(ggthemes) # Cambia el estilo de los gráficos ampliando los temas
library(scales) # Controla la forma de mostrar decimales

ggplot(analisis_puestos, aes(x = Puesto, y = Sueldo, color = Sexo)) +
  geom_point(position = "jitter", alpha = 0.5, size = 2) +
  scale_color_colorblind() + # Del paquete ggthemes
  labs(title = "Sueldos brutos por puesto",
       subtitle = "Argentina 2020",
       caption = "Fuente: Encuesta de Sueldos Sysarmy 2020.1",
       x = "", y = "")+
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))

# Encuesta de sueldos --------------------------------


options(scipen = 999) # Elimina la notación científica de los gráficos

# Cargar encuesta de sueldos de RH
encuesta_rh <- read_delim("https://raw.githubusercontent.com/r4hr/r4hr_introduccion_dplyr/main/Datos/encuesta_sueldos.csv", delim = ",")

# Exploremos el dataset
glimpse(encuesta_rh)

summary(encuesta_rh)

ggplot(encuesta_rh, aes(x = anios_rh)) +
  geom_histogram()

summary(encuesta_rh$sueldo_bruto)




# Como tenemos una persona que cargó que tiene 121 años de experiencia,
# vamos a filtrar la cantidad de años a menos de 30 años.

encuesta_rh <- encuesta_rh %>% 
  filter(anios_rh <= 30)

# Exploremos la variable sueldo_bruto

ggplot(encuesta_rh, aes(x = sueldo_bruto)) +
  geom_histogram()

ggplot(encuesta_rh, aes(x = sueldo_bruto)) +
  geom_boxplot()

# Dado que hay valores extremos (outliers) que ensucian el análisis. 
# También podemos ver que que hay respuestas de sueldos de AR$ 2 lo cual es absurdo.
# Filtraremos los sueldos que estén entre AR$ 20.000 y a AR$ 500.000

encuesta_rh <- encuesta_rh %>% 
  filter(between(sueldo_bruto, 20000, 500000))

ggplot(encuesta_rh, aes(x = sueldo_bruto)) +
  geom_histogram()

ggplot(encuesta_rh, aes(x = sueldo_bruto)) +
  geom_boxplot()


# Gráfico de dispersión
options(scipen = 999) # Elimina la notación científica de los gráficos

ggplot(encuesta_rh, aes(x = anios_rh, y = sueldo_bruto)) +
  geom_point(shape = 10)

  
dispersion_2 <- encuesta_rh %>% 
  select(sueldo_bruto, universidad, genero) 


ggplot(dispersion_2, aes(x = universidad, y = sueldo_bruto)) + 
  geom_point() 

ggplot(dispersion_2, aes(x = universidad, y = sueldo_bruto)) +
  geom_point(position = "jitter", alpha = 0.3, size = 2) 

ggplot(encuesta_rh, aes(x = anios_rh, y = sueldo_bruto)) +
  geom_point(position = "jitter", alpha = 0.3, size = 2) 


# Gráfico completo

install.packages("ggthemes")
install.packages("scales")

library(ggthemes)

ggplot(encuesta_rh, aes(x = universidad, y = sueldo_bruto, color = genero)) +
  geom_point(position = "jitter", alpha = 0.3, size = 2) +
  scale_color_colorblind() + # Del paquete ggthemes
  labs(title = "Sueldos brutos por tipo de universidad",
       subtitle = "Argentina 2020",
       caption = "Fuente: Encuesta KIWI de Sueldos de RH",
       x = "", y = "",
       color = "Género")+
  scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ","))
