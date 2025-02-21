## Raúl Octavio Martínez Rincón

### Análisis de componentes principales ###
# 1. Librerías
# 2. Datos ejemplo
# 3. Edición de datos
# 4. Figuras exploratorias 
# 5. Análisis de componentes principales (PCA)
# 6. Figuras PCA

#### Librerías (paquetes) ####
library(tibble) # Permite una mejor visualización de tablas de datos
library(FactoMineR) # Análisis explotatorio para datos multivariados y minería de datos
library(factoextra) # Visualización de análisis multivariados
library(GGally) # Complemento de ggplot2

?PCA
help("PCA")

## Definir directorio de trabajo
setwd("C:/Raul/Tutoriales en R/PCA")

#### Datos ejemplo ####
?iris
iris <- tibble(iris)
iris

#### Edición de datos ####
names(iris) <- c("LS", "AS", "LP", "AP", "Especies")
iris

#### Análisis exploratorios ####
## Matriz de correlaciones
cor(iris)
cor(iris[, -5]) # remover la variable categórica

## Figuras de variables pareadas
ggpairs(iris[, -5])

#### PCA ####
pcaIris <- PCA(iris)
pcaIris <- PCA(iris[, -5]) # remover la variable categórica

## Contenido del objeto PCA
print(pcaIris)

## Varianza explicada por componente
head(pcaIris$eig)

## Varianza explicada por componente (gráfica)
fviz_screeplot(pcaIris)

fviz_screeplot(pcaIris) +
  labs(x = "Componentes", y = "Porcentaje de varianza explicada", title = "")
ggsave("Fig 1. Varianza explicada por componente.png", width = 9, height = 7)

## Gráfica de las variables (Componentes 1 y 2)
fviz_pca_var(pcaIris)

fviz_pca_var(pcaIris, col.var = "contrib")

fviz_pca_var(pcaIris, col.var = "contrib") +
  labs(x = "Componente principal 1 (73%)", y = "Componente principal 2 (22.9%)", title = "", colour = "Contribución")
ggsave("Fig 2. Contribución de las variables (Componentes 1 y 2).png", width = 9, height = 7)

## contribución de las variables (dimensión 1)
pcaIris$var$contrib

fviz_contrib(pcaIris, choice = "var", axes = 1) +
  labs(y = "Contribución  CP1 (%)", title = "Variables")
ggsave("Fig 3. Contribución de las variables (Componente 1).png", width = 9, height = 7)

## contribución de las variables (dimensión 2)
fviz_contrib(pcaIris, choice = "var", axes = 2) +
  labs(y = "Contribución  CP2 (%)", title = "Variables")
ggsave("Fig 4. Contribución de las variables (Componente 2).png", width = 9, height = 7)

## Contribución de los individuos (obsevaciones)
head(pcaIris$ind$coord)

fviz_pca_ind(pcaIris, col.ind = "contrib") +
  labs(x = "Componente principal 1 (73%)", y = "Componente principal 2 (22.9%)", title = "", colour = "Contribución")
ggsave("Fig 5. Contribución de los individuos (Componentes 1 y 2).png", width = 9, height = 7)

## Contribución de los individuos (obsevaciones) por grupos (especie)
fviz_pca_ind(pcaIris, habillage = iris$Especies)

fviz_pca_ind(pcaIris, habillage = iris$Especies, geom = "point") +
  labs(x = "Componente principal 1 (73%)", y = "Componente principal 2 (22.9%)", title = "", colour = "Especies", shape = "Especies", fill = "Especies")
ggsave("Fig 6. Contribución de los individuos por especie (Componentes 1 y 2).png", width = 9, height = 7)

## Elipses al 95%
fviz_pca_ind(pcaIris, habillage = iris$Especies, addEllipses = TRUE, geom = "point") +
  labs(x = "Componente principal 1 (73%)", y = "Componente principal 2 (22.9%)", title = "", colour = "Especies", shape = "Especies", fill = "Especies")
ggsave("Fig 7. Contribución de los individuos por especie (Componentes 1 y 2, elipses).png", width = 9, height = 7)

## contribución de las individuos (dimensión 1)
fviz_contrib(pcaIris, choice = "ind", axes = 1)
fviz_contrib(pcaIris, choice = "ind", axes = 1, top = 20) +
  labs(y = "Contribución  CP1 (%)", title = "Individuos")

## contribución de las individuos (dimensión 2)
fviz_contrib(pcaIris, choice = "ind", axes = 2, top = 20) +
  labs(y = "Contribución  CP2 (%)", title = "Individuos")

## Biplot
fviz_pca_biplot(pcaIris, habillage = iris$Especies, addEllipses = TRUE, geom = "point") +
  labs(x = "Componente principal 1 (73%)", y = "Componente principal 2 (22.9%)", title = "", colour = "Especies", shape = "Especies", fill = "Especies")
ggsave("Fig 8. Contribución de las variables e individuos por especie (Componentes 1 y 2, elipses).png", width = 9, height = 7)
