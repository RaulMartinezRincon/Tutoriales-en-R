## Raúl Octavio Martínez Rincón

### Mapas en R - terra parte 1 ###
# 1. Descarga de datos vectoriales
# 2. Descarga de datos raster (elevacion - mexico)
# 3. Datos vectoriales - México y Sonora
# 4. Datos ráster - Recorte usando un polígono

#### Librerías (paquetes) ####
library(terra) # Librería para manejo de datos vectoriales y ráster
library(geodata) # Librería para descarga de datos espaciales

help(package = "geodata")

#### Directorio de trabajo ####
# Es importante definir para importar/exportar datos/figuras
setwd("C:/Raul/Tutoriales en R/Mapas en R/")

#### 1. Descarga de datos vectoriales ####
?gadm
## Divisiones politicas - México
Mex0 <- gadm(country="MEX", level=0, path = "gadm") # País
Mex1 <- gadm(country="MEX", level=1, path = "gadm") # Estados

Mex0
Mex1

plot(Mex0) # México sin divisiones políticas
plot(Mex1) # Todos los estados

Mex1$NAME_1 # Muestra los 32 nombres de estado

#### 2. Descarga de datos raster (elevacion) ####
elevMex <- elevation_30s(country ="MEX", path = "elev")
elevMex # 30 segundos ~= 1 km

plot(elevMex)

miPal <- colorRampPalette(c("darkgreen", "green", "yellow", "orange", "red", "darkred", "white"))

plot(elevMex, col = miPal(150))

#### 3. Datos vectoriales - México y Sonora ####
# Extraer el polígono de Sonora
Sonora <- Mex1[Mex1$NAME_1 == "Sonora", ]

plot(Sonora)

Capital.SON <- vect(data.frame(x = -110.97732, 
                               y = 29.1026, 
                               Ciudad = "Hermosillo"),
                    geom = c("x", "y"),
                    crs = "+proj=lonlat +datum=WGS84")

plot(Sonora)
plot(Capital.SON, pch = 17, add = TRUE, cex = 1.5)
text(Capital.SON, "Ciudad", pos = 4)

# Agregar Sonora con un color diferente
plot(Mex0)
plot(Sonora, col = "gray90", add = TRUE)
plot(Capital.SON, pch = 17, add = T, cex = 1)
text(Capital.SON, "Ciudad", pos = 4, cex = .75)
north() # Agrega el norte
sbar() # Agrega una escala

# Exportar figura con estilo personalizado
png("Figura 1. Mapa de México resaltando Sonora.png", width = 960, height = 650, pointsize = 24)
plot(Mex0, mar = c(2, 2, 1, 1))
add_grid() # Agregue una cuadrícula de referencia
plot(Sonora, col = "gray90", add = TRUE)
plot(Capital.SON, pch = 17, add = T, cex = 1)
text(Capital.SON, "Ciudad", pos = 4, cex = .75)
north(type = 2) # Agrega el norte. Pruebe números entre 1 y 12
sbar(d = 1000, type = "bar", divs = 4, below = "Km") # Agrega una escala. Distancia = 1000 km
dev.off()

#### 4. Datos ráster - Recorte usando un polígono ####
elevSonora <- crop(elevMex, Sonora) # Recorte usando las extensiones de su poligono
plot(elevSonora)

elevSonora <- mask(elevSonora, Sonora) # Remueva valores fuera del poligono
plot(elevSonora)

png("Figura 2. Elevación de Sonora.png", width = 960, height = 960, pointsize = 24)
plot(elevSonora, col = miPal(150))
grid()
plot(Capital.SON, pch = 17, add = T, cex = 1.5)
text(Capital.SON, "Ciudad", pos = 4, cex = 1)
plot(Mex0, add = T)
north(type = 2) # Agrega el norte. Pruebe números entre 1 y 12
sbar(d = 200, xy = "top", type = "bar", divs = 4, below = "Km") # Agrega una escala. Distancia = 1000 km
dev.off()