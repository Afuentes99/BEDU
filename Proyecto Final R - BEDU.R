#Importar, explorar y limpiar los datos
# Instalación y carga de paquetes necesarios
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("corrplot")

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(corrplot)

# Importar los datos
data <-read_csv("/Users/alejandrofuentes/Downloads/fast_food.csv")

# Exploración de datos
str(data)          # Estructura de los datos
head(data)         # Primeras filas del dataset
summary(data)      # Resumen estadístico
tail(data)         # Últimas filas del dataset
colnames(data)     # Nombres de las columnas
dim(data)          # Dimensiones del dataset
View(data)         # Vista de los datos en formato de tabla

# Seleccionar todas las columnas excepto la primera y la de ensaladas
#Eliminar columnas que no me sirven
data <- data %>% select(-1, -salad)

# Reemplazar valores NA con 0
data[is.na(data)] <- 0

# Renombrar columnas para mayor claridad
data <- data %>% 
  rename(
    Restaurante = restaurant,
    Articulo = item,
    Calorías = calories,
    Grasa_Calorica = cal_fat,
    Grasa_total = total_fat,
    Grasa_sat = sat_fat,
    Grasa_trans = trans_fat,
    Colesterol = cholesterol,
    Sal = sodium,
    Carbohidratos = total_carb,
    Fibra = fiber,
    Azúcar = sugar,
    Proteína = protein,
    Calcio = calcium
  )

#Agregar columnas
data <- data %>% mutate(Alto_en_Calorías = ifelse(Calorías > 500, "Sí", "No"))

head(data)


# Análisis descriptivo

# Análisis descriptivo de variables clave - resumen estasdistico de columnas clave
summary(data$Calorías)
summary(data$Grasa_total)
summary(data$Proteína)

# Histograma de la distribución de calorías
ggplot(data, aes(x = Calorías)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Calorías de Todos los Productos",
       x = "Calorías",
       y = "Frecuencia") +
  theme_minimal()

# Histograma de la distribución de grasa total
ggplot(data, aes(x = Grasa_total)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Grasa Total de Todos los Productos",
       x = "Grasa Total",
       y = "Frecuencia") +
  theme_minimal()

# Histograma de la distribución de proteínas
ggplot(data, aes(x = Proteína)) +
  geom_histogram(binwidth = 5, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Proteína de Todos los Productos",
       x = "Proteína",
       y = "Frecuencia") +
  theme_minimal()

#Conteo de productos por restaurante

# Calcular el conteo de productos por restaurante
conteo <- data %>%
  group_by(Restaurante) %>%
  summarise(count_items = n())

# Imprimir el conteo de productos
print(conteo)

# Gráfica de barras del conteo de productos por restaurante
ggplot(conteo, aes(x = reorder(Restaurante, -count_items), y = count_items)) +
  geom_bar(stat = "identity") +
  labs(title = "Número de Articulos por Restaurante",
       x = "Restaurante",
       y = "Número de Articulos") +
  theme_minimal()



#Dispersión para ver si hay variables asociadas mutuamente, aumentan o disminuyen juntas, ver relaciones
# Crear un gráfico de dispersión
#correlación, aumenta calorias con proteina, estan altamente relacionadas, se ve una linea recta
ggplot(data, aes(x = Calorías, y = Proteína)) +
  geom_point() +
  labs(title = "Relación entre Calorías y Proteína",
       x = "Calorías",
       y = "Proteína") +
  theme_minimal()

# Regresión lineal y su gráfica

# Regresión lineal entre calorías y proteínas
model <- lm(Proteína ~ Calorías, data = data)
summary(model)

# Gráfica de la regresión lineal
ggplot(data, aes(x = Calorías, y = Proteína)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Regresión Lineal: Proteína vs Calorías",
       x = "Calorías",
       y = "Proteína") +
  theme_minimal()

#Serie de tiempo y su gráfica

# Crear una columna de fecha ficticia si no existe
set.seed(123)
data$date <- seq(from = as.Date("2020-01-01"), by = "month", length.out = nrow(data))

# Calcular la media de calorías por fecha
data_ts <- data %>%
  group_by(date) %>%
  summarise(mean_calories = mean(Calorías))

# Gráfica de la serie de tiempo
ggplot(data_ts, aes(x = date, y = mean_calories)) +
  geom_line() +
  labs(title = "Serie de Tiempo: Media de Calorías por Fecha",
       x = "Fecha",
       y = "Media de Calorías") +
  theme_minimal()

#Análisis de correlación y su gráfica

# Calcular la matriz de correlación entre variables seleccionadas
cor_matrix <- cor(data %>% select(Calorías, Grasa_total, Proteína, Sal))

# Visualizar la matriz de correlación con un heatmap
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", tl.srt = 45)

#Prueba de hipótesis

# Comparar calorías entre dos restaurantes (ajustar nombres de restaurantes según dataset)
restaurant_1 <- data %>% filter(Restaurante == "Restaurant1") %>% select(Calorías)
restaurant_2 <- data %>% filter(Restaurante == "Restaurant2") %>% select(Calorías)

# Realizar t-test para comparar calorías
t.test(restaurant_1$Calorías, restaurant_2$Calorías)

# Gráfica de comparación de calorías entre restaurantes
ggplot(data, aes(x = Restaurante, y = Calorías, fill = Restaurante)) +
  geom_boxplot() +
  labs(title = "Comparación de Calorías entre Restaurantes",
       x = "Restaurante",
       y = "Calorías") +
  theme_minimal()

#Estimar probabilidades

# Estimar la probabilidad de que un producto tenga más de 500 calorías
prob_alto_en_calorias <- mean(data$Calorías > 500)
prob_alto_en_calorias

# Tabla de cocientes (frecuencia relativa de productos altos en calorías por restaurante)
tabla_cocientes <- data %>%
  group_by(Restaurante) %>%
  summarise(prob_alto_cal = mean(Calorías > 500))

# Imprimir la tabla de cocientes
print(tabla_cocientes)

# Gráfica de barras de probabilidades marginales estimadas
ggplot(tabla_cocientes, aes(x = reorder(Restaurante, -prob_alto_cal), y = prob_alto_cal)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Probabilidad de Productos con Más de 500 Calorías por Restaurante",
       x = "Restaurante",
       y = "Probabilidad") +
  theme_minimal()




