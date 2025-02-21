## Raúl Octavio Martínez Rincón

### Correlaciones ###
# 1. Datos ejemplo
# 2. Edición de datos
# 3. Figuras exploratorias 
# 4. Métodos
# 5. Correlaciones 2 variables
# 6. Correlaciones más de 2 variables
# 7. Figura para correlacion entre dos variables
# 8. Figura para matriz de correlaciones

#### Librerías (paquetes) ####
library(rstatix) # Pruebas estadistícas básicas coherente con tidyverse
library(ggpubr) # Funciones para figuras "listas para publicación"
library(GGally) # Extensión de ggplot2

theme_set(theme_bw()) # thema blanco y negro

#### Directorio de trabajo ####
# Es importante definir para importar/exportar datos/figuras
setwd("C:/Raul/Tutoriales en R/Correlaciones/")

#### datos ####
?iris

# Seleccionamos una especie
virginica <- iris %>% 
  filter(Species == "virginica")

# Quitamos la columna species
virginica <- tibble(virginica[, -5])

# Renombramos
names(virginica) <- c("LS", "AS", "LP", "AP")
virginica

#### Figuras exploratorias ####
## Una variable
ggplot(virginica, aes(LP, LS)) + 
  geom_point()

## Todas las combinaciones posibles
g1 <- ggplot(virginica, aes(LS, AS)) + 
  geom_point()
g2 <- ggplot(virginica, aes(LS, LP)) + 
  geom_point()
g3 <- ggplot(virginica, aes(LS, AP)) + 
  geom_point()
g4 <- ggplot(virginica, aes(AS, LP)) + 
  geom_point()
g5 <- ggplot(virginica, aes(AS, AP)) + 
  geom_point()
g6 <- ggplot(virginica, aes(LP, AP)) + 
  geom_point()

ggarrange(g1, g2, g3, g4, g5, g6, labels = "AUTO")
ggsave("Fig 1. Figura de dispersión todas las variables.png", width = 9, height = 7)

# Combinaciones pareadas más prueba de correlación de Pearson
ggpairs(virginica)

#### Métodos ####
# 1. Pearson - paramétrica
# 2. Spearman - no paramétrica

## Normalidad
virginica %>% 
  shapiro_test(LS, AS, LP, AP)

#### Correlacion de pearson ####
virginica[, 1:2] %>% # Las primeras dos columnas
  cor_test(method = "pearson")

# Spearman
virginica[, 1:2] %>% # Las primeras dos columnas
  cor_test(method = "spearman")

virginica %>% 
  cor_test(method = "pearson")

virginica %>% 
  cor_test(method = "pearson") %>% 
  write.csv("Tabla de correlaciones (virginica).csv")

#### Figuras finales ####
ggplot(virginica, aes(LP, LS)) + 
  geom_point() + 
  stat_cor(p.accuracy = 0.05)

gCor1 <- ggplot(virginica, aes(LS, AS)) + 
  geom_point() + 
  stat_cor(p.accuracy = 0.05)
gCor2 <- ggplot(virginica, aes(LS, LP)) + 
  geom_point() + 
  stat_cor(p.accuracy = 0.05)
gCor3 <- ggplot(virginica, aes(LS, AP)) + 
  geom_point() + 
  stat_cor(p.accuracy = 0.05)
gCor4 <- ggplot(virginica, aes(AS, LP)) + 
  geom_point() + 
  stat_cor(p.accuracy = 0.05)
gCor5 <- ggplot(virginica, aes(AS, AP)) + 
  geom_point() + 
  stat_cor(p.accuracy = 0.05)
gCor6 <- ggplot(virginica, aes(LP, AP)) + 
  geom_point() + 
  stat_cor(p.accuracy = 0.05)

ggarrange(gCor1, gCor2, gCor3, gCor4, gCor5, gCor6, labels = "AUTO")
ggsave("Fig 2. Figura correlaciones todas las variables.png", width = 9, height = 7)

ggpairs(virginica)
ggsave("Fig 3. Variables pareadas con correlació Pearson.png", width = 9, height = 7)
