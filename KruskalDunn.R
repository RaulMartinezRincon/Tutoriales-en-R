## Raúl Octavio Martínez Rincón

### Kruskal-Wallis ###
# 1. Datos ejemplo
# 2. Edición de datos
# 3. Prueba de normalidad
# 4. Prueba de homogeneidad de varianzas
# 5. Prueba Kruskal-Wallis
# 6. Prueba de Dunn
# 7. Figura para Kruskal-Wallis y Dunn  

#### Librerías (paquetes) ####
library(tidyverse) # Colección de librerias
theme_set(theme_bw()) # Tema blanco y negro
library(rstatix) # Pruebas estadistícas básicas coherente con tidyverse
library(ggpubr) # Funciones para figuras "listas para publicación"

#### Directorio de trabajo ####
# Es importante definir para importar/exportar datos/figuras
setwd("C:/Raul/Tutoriales en R/Kruskal y Dunn")

#### datos ejemplo ####
?iris

datos <- tibble(iris) # Convertir a tibble
datos

#### Edición de datos ####
# Renombrar las columnas ("variables)
names(datos) <- c("LS", "AS", "LP", "AP", "Especie")

datos

#### Prueba de normalidad ####
# Prueba de normalidad de Shapiro-Wilk
datos %>% 
  shapiro_test(LS) # p<0.05 = No normales

datos %>% 
  shapiro_test(LS, AS, LP, AP)

# Nueva columna para definir el nivel de significancia
datos %>% 
  shapiro_test(LS, AS, LP, AP) %>% 
  mutate(pSig = ifelse(p < 0.05, "p<0.05*", "p>0.05"))

#### Prueba de homogeneidad de varianzas ####
# Prueba de homogeneidad de varianzas de Levene
datos %>% 
  levene_test(LS ~ Especie) # p<0.05 = No homogeneos

# Evaluar homocedásticidad de todas las variables
bind_rows(datos %>% levene_test(LS ~ Especie), 
          datos %>% levene_test(AS ~ Especie),
          datos %>% levene_test(LP ~ Especie),
          datos %>% levene_test(AP ~ Especie)) %>% 
  mutate(pSig = ifelse(p < 0.05, "p<0.05*", "p>0.05")) %>% 
  select(df1, df2, statistic, p, pSig)

#### Prueba de Kruskal-Wallis ####
datos %>% 
  kruskal_test(LS ~ Especie) # p<0.05 = Diferencias significativas

# Crear objeto para figuras
kwLS <- datos %>% 
  kruskal_test(LS ~ Especie)
kwLS

#### Prueba de Dunn ####
datos %>% 
  dunn_test(LS ~ Especie)

# Exportar tabla
datos %>% 
  dunn_test(LS ~ Especie) %>% 
  write_csv("Tabla 1. Prueba de Dunn.csv")

# Crear objeto para figuras
dunnLS <- datos %>% 
  dunn_test(LS ~ Especie) %>% 
  add_xy_position(x = "Especie")

dunnLS

#### Figura para Kruskal-Wallis y Dunn ####
ggplot(datos, aes(Especie, LS, col = Especie)) +
  geom_boxplot() +
  labs(y = "Longitud del sépalo", x = "") +
  theme(legend.position = "none")
# puntos = valores extremos, lineas verticales = minimo y maximo o Q1 - 1.5*IQR y Q3 + 1.5*IQR, cajas = Q1 y Q3, lineas horizontales

ggLS <- ggplot(datos, aes(Especie, LS, col = Especie)) +
  geom_boxplot() +
  geom_jitter() +
  labs(y = "Longitud del sépalo", x = "") +
  theme(legend.position = "none")

ggLS

# Figura con resultados de pruebas estadísticas
ggLS +
  stat_pvalue_manual(dunnLS, hide.ns = TRUE) +
  labs(title = get_test_label(kwLS, detailed = TRUE),
       caption = get_pwc_label(dunnLS))
ggsave("Fig 1. Kruskal-Wallis y Dunn.png", width = 9, height = 7)

#### preguntas ####
ggplot(datos, aes(Especie, LS)) + 
  stat_summary(fun = "median", geom = "bar", aes(fill = Especie)) + 
  stat_summary(fun.data = "median_hilow") +
  stat_pvalue_manual(dunnLS, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(kwLS, detailed = TRUE),
    caption = get_pwc_label(dunnLS)
  ) +
  theme(legend.position = "none")
