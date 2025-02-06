## Analisis de Datos de un Supermercado con R

## 1-Obtencion de Datos

# Crear un dataset ficticio de ventas
set.seed(123)
ventas <- data.frame(
  Fecha = seq.Date(from = as.Date("2024-01-01"), by = "day", length.out = 30),
  Producto = sample(c("Manzana", "Leche", "Pan", "Arroz", "Carne"), 30, replace = TRUE),
  Cantidad_Vendida = sample(1:20, 30, replace = TRUE),
  Precio_Unitario = sample(c(10, 20, 30, 40, 50), 30, replace = TRUE),
  Categoría = sample(c("Frutas", "Lácteos", "Panadería", "Granos", "Carnes"), 30, replace = TRUE)
)

# Calcular la columna de Total Venta
ventas$Total_Venta <- ventas$Cantidad_Vendida * ventas$Precio_Unitario

# Guardar en un archivo CSV
write.csv(ventas, "ventas_supermercado.csv", row.names = FALSE)

# Ver los primeros registros
print(head(ventas))

## 2-Carga y Exploracion de Datos en R

#Leer el archivo CSV
ventas_df <- read.csv("ventas_supermercado.csv")

#Mostrar los primeros registros
head(ventas_df)

#Ver la estructura del dataset
str(ventas_df)

#Estadisticas generales
summary(ventas_df)

## 3-Transformaciones y Analisis Estadistico

# Agrupar por categoría y sumar las ventas totales
library(dplyr)
ventas_por_categoria <- ventas_df %>%
  group_by(Categoría) %>%
  summarise(Venta_Total = sum(Total_Venta))

# Mostrar el resultado
print(ventas_por_categoria)

# Calcular promedio de ventas diarias
promedio_ventas <- ventas_df %>%
  group_by(Fecha) %>%
  summarise(Promedio_Venta = mean(Total_Venta))

# Mostrar el resultado
print(promedio_ventas)

# Identificar el producto con más cantidad vendida
producto_top <- ventas_df %>%
  group_by(Producto) %>%
  summarise(Total_Vendido = sum(Cantidad_Vendida)) %>%
  arrange(desc(Total_Vendido))

# Mostrar el producto más vendido
print(producto_top[1, ])






## 4-Visualizacion de Datos


# Instalar y cargar ggplot2
install.packages("ggplot2")
library(ggplot2)

# Gráfico de barras de ventas por categoría
ggplot(data = ventas_por_categoria, aes(x = Categoría, y = Venta_Total, fill = Categoría)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Ventas Totales por Categoría", x = "Categoría", y = "Venta Total")

# Gráfico de líneas de ventas diarias
ggplot(data = promedio_ventas, aes(x = Fecha, y = Promedio_Venta)) +
  geom_line(color = "blue", size = 1) +
  theme_minimal() +
  labs(title = "Promedio de Ventas Diarias", x = "Fecha", y = "Venta Promedio")



## 5-Exportacion de Resultados

# Guardar los resultados de análisis en un CSV
write.csv(ventas_por_categoria, "ventas_por_categoria.csv", row.names = FALSE)
write.csv(promedio_ventas, "promedio_ventas_diarias.csv", row.names = FALSE)
write.csv(producto_top, "producto_mas_vendido.csv", row.names = FALSE)
