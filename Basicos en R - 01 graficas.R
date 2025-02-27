## Raúl Octavio Martínez Rincón

### Figuras basicas parte 1 ###
# 1. Temas ggplot2
# 2. Dispersión
# 3. Barras
# 4. Pastel
# 5. Líneas
# 6. Histograma y densidad
# 7. Exportar figuras en diferentes tipos de archivo

#### Librerías (paquetes) ####
library(tidyverse) # Colección de librerias

#### Directorio de trabajo ####
# Es importante definir para importar/exportar datos/figuras
setwd("C:/Raul/Tutoriales en R/Basicos en R")

#### temas ggplot2 ####
?iris

ggplot(iris, aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point()

ggplot(iris, aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point() +
  theme_bw()

ggplot(iris, aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point() +
  theme_dark()

ggplot(iris, aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point() +
  theme_classic()

ggplot(iris, aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point() +
  theme_minimal()

ggplot(iris, aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point() +
  theme_void()

## Definir tema para toda la sesion
theme_set(theme_bw())

#### Figuras de dispersión ####
?cars

ggplot(cars, aes(speed, dist))

ggplot(cars, aes(speed, dist)) + 
  geom_point()

ggplot(cars, aes(speed, dist)) + 
  geom_point() +
  labs(x = "Velocidad", y = "Distancia", title = "Datos de 1920")

ggplot(cars, aes(speed, dist, col = dist, size = dist)) + 
  geom_point() +
  labs(x = "Velocidad", y = "Distancia", title = "Datos de 1920")

ggplot(cars, aes(speed, dist, col = dist, size = dist)) + 
  geom_point() +
  labs(x = "Velocidad", y = "Distancia", title = "Datos de 1920", col = "Distancia", size = "Distancia")

ggplot(cars, aes(speed, dist)) + 
  geom_point(aes(col = dist, size = dist)) +
  labs(x = "Velocidad", y = "Distancia", title = "Datos de 1920", col = "", size = "")

# bottom, left, top, right, none
ggplot(cars, aes(speed, dist, col = dist, size = dist)) + 
  geom_point() +
  labs(x = "Velocidad", y = "Distancia", title = "Datos de 1920") +
  theme(legend.position = "bottom")

ggplot(cars, aes(speed, dist, col = dist, size = dist)) + 
  geom_point() +
  labs(x = "Velocidad", y = "Distancia", title = "Datos de 1920") +
  theme(legend.position = "none")

## Paleta de colores personalizada
miPal <- colorRampPalette(c("blue", "cyan", "yellow", "red"))

ggplot(cars, aes(speed, dist, col = dist, size = dist)) + 
  geom_point() +
  labs(x = "Velocidad", y = "Distancia", title = "Datos de 1920") +
  scale_color_gradientn(colours = miPal(10)) +
  theme(legend.position = "none")
ggsave("Figura de dispersión.png", width = 7, height = 5)

#### Figuras de barras ####
?mtcars

mtcars %>% 
  group_by(Cilindros = cyl) %>% 
  summarise(Cuenta = n())

Tabla1 <- mtcars %>% 
  group_by(Cilindros = factor(cyl)) %>% 
  summarise(Cuenta = n())

ggplot(Tabla1, aes(Cilindros, Cuenta)) +
  geom_col()

ggplot(Tabla1, aes(Cilindros, Cuenta, fill = Cilindros)) +
  geom_col()

ggplot(Tabla1, aes(Cilindros, Cuenta, fill = Cilindros)) +
  geom_col() +
  geom_text(aes(label = Cuenta))

ggplot(Tabla1, aes(Cilindros, Cuenta, fill = Cilindros)) +
  geom_col() +
  geom_text(aes(label = Cuenta), size = 4, vjust = -1)

ggplot(Tabla1, aes(Cilindros, Cuenta, fill = Cilindros)) +
  geom_col() +
  geom_text(aes(label = Cuenta), size = 4, vjust = -1) +
  labs(x = "Número de cilíndros", y = "Número de vehículos") +
  theme(legend.position = "none")
ggsave("Figura de barras con texto.png", width = 7, height = 5)

#### figura de pastel ####
ggplot(Tabla1, aes(Cilindros, Cuenta, fill = Cilindros)) +
  geom_col() +
  coord_polar()

ggplot(Tabla1, aes(Cilindros, Cuenta, fill = Cilindros)) +
  geom_col() +
  coord_polar(theta = "y")

ggplot(Tabla1, aes(Cilindros, Cuenta, fill = Cilindros)) +
  geom_col() +
  geom_text(aes(label = Cuenta)) +
  coord_polar(theta = "y")

ggplot(Tabla1, aes(x = "", Cuenta, fill = Cilindros)) +
  geom_col(color = "black") +
  geom_text(aes(label = Cuenta), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(x = "", y = "", fill = "Número de cilindros") +
  theme_void() +
  theme(legend.position = "top")
ggsave("Figura de pastel.png", width = 7, height = 7)

#### figura de lineas ####
?Loblolly

ggplot(Loblolly, aes(age, height)) + 
  geom_line()

ggplot(Loblolly, aes(age, height, col = Seed)) + 
  geom_line()

ggplot(Loblolly, aes(age, height, col = Seed)) + 
  geom_line(linewidth = 1) +
  theme(legend.position = "top")

ggplot(Loblolly, aes(age, height, col = Seed)) + 
  geom_line(linewidth = 1) +
  geom_point() +
  theme(legend.position = "top")

ggplot(Loblolly, aes(age, height, col = Seed)) + 
  geom_line(linewidth = 1) +
  geom_point() +
  theme(legend.position = "top") +
  labs(x = "Edad", y = "Altura", col = "")
ggsave("Figura de lineas.png", width = 7, height = 7)

#### figura de histogramas y densidades ####
?iris

ggplot(iris, aes(Petal.Length)) +
  geom_histogram()

ggplot(iris, aes(Petal.Length)) +
  geom_histogram(col = "black")

ggplot(iris, aes(Petal.Length)) +
  geom_histogram(col = "black", bins = 10)

ggplot(iris, aes(Petal.Length)) +
  geom_histogram(col = "black", bins = 50)

ggplot(iris, aes(Petal.Length)) +
  geom_histogram(col = "black", binwidth = .5)

ggplot(iris, aes(Petal.Length, fill = Species)) +
  geom_histogram(col = "black", binwidth = .5)

ggplot(iris, aes(Petal.Length, fill = Species)) +
  geom_histogram(col = "black", binwidth = .5) + 
  theme(legend.position = "bottom")

ggplot(iris, aes(Petal.Length, fill = Species)) +
  geom_histogram(col = "black", binwidth = .5) + 
  theme(legend.position = "bottom") +
  labs(x = "Longitud del pétalo", y = "Frecuencia", fill = "Especie")
ggsave("Histograma de frecuencias.png", width = 7, height = 5)

ggplot(iris, aes(Petal.Length, fill = Species)) +
  geom_histogram(col = "black", binwidth = .5) + 
  facet_wrap(~Species, ncol = 1) +
  theme(legend.position = "none") +
  labs(x = "Longitud del pétalo", y = "Frecuencia", fill = "Especie")

ggplot(iris, aes(Petal.Length, fill = Species)) +
  stat_density() + 
  theme(legend.position = "bottom")

ggplot(iris, aes(Petal.Length, fill = Species)) +
  stat_density(col = "black", alpha = .5) + 
  theme(legend.position = "bottom") +
  labs(x = "Longitud del pétalo", y = "Densidad", fill = "Especie")
ggsave("Figura de densidades.png", width = 7, height = 5)

ggplot(iris, aes(Petal.Length, fill = Species)) +
  stat_density(col = "black", alpha = .5) + 
  theme(legend.position = "none") + 
  facet_wrap(~Species, ncol = 1) +
  labs(x = "Longitud del pétalo", y = "Frecuencia", fill = "Especie")

#### Exportar figuras en diferentes tipos de archivo ####
?quakes

ggplot(quakes, aes(long, lat, col = mag, size = mag)) +
  geom_point(alpha = .3) +
  scale_color_gradientn(colours = miPal(10)) +
  labs(title = "Terremotos en Fiji (1964)", x = "Longitud", y = "Latitud", col = "Magnitud", size = "") +
  coord_fixed() 
ggsave("Figura terremotos.png", width = 7, height = 7)
ggsave("Figura terremotos.pdf", width = 7, height = 7)
ggsave("Figura terremotos.tiff", width = 7, height = 7)
ggsave("Figura terremotos.jpeg", width = 7, height = 7)
