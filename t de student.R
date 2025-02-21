## Raúl Octavio Martínez Rincón

### t de student ###
# 1. Datos ejemplo
# 2. Edición de datos
# 3. Prueba de normalidad
# 4. Prueba de homogeneidad de varianzas
# 5. Prueba t de student
# 6. Figura para t de student  
# 7. Distribucion t

#### Librerías (paquetes) ####
library(tidyverse) # Colección de librerias
theme_set(theme_bw()) # Tema blanco y negro
library(rstatix) # Pruebas estadistícas básicas coherente con tidyverse
library(ggpubr) # Funciones para figuras "listas para publicación"

#### Directorio de trabajo ####
# Es importante definir para importar/exportar datos/figuras
setwd("C:/Raul/Tutoriales en R/t de student/")

#### datos ejemplo ####
?sleep

datos <- tibble(sleep) # Convertir a tibble
datos

#### Edición de datos ####
# Renombrar las columnas ("variables")
names(datos) <- c("Horas.extra", "Grupo", "ID")

datos <- datos %>% 
  mutate(Grupo = factor(Grupo, levels = c(1, 2), labels = c("Control", "Tratamiento")))

datos

#### Prueba de normalidad ####
# Prueba de normalidad de Shapiro-Wilk
datos %>% 
  shapiro_test(Horas.extra) # p>0.05 = Distr. normal

#### Prueba de homogeneidad de varianzas ####
# Prueba de homogeneidad de varianzas de Levene
datos %>% 
  levene_test(Horas.extra ~ Grupo) # p>0.05 = Homogeneos

#### Prueba t de student ####
datos %>% 
  t_test(Horas.extra ~ Grupo) # p>0.05 = No hay diferencias significativas

# Crear objeto para figuras
tsHE <- datos %>% 
  t_test(Horas.extra ~ Grupo)
tsHE

#### Figura para t de student ####
ggplot(datos, aes(Grupo, Horas.extra, col = Grupo)) +
  geom_boxplot() +
  geom_jitter() +
  labs(y = "Horas extra", x = "") +
  theme(legend.position = "none")
# lineas verticales = minimo y maximo, cajas = Q1 y Q3, lineas horizontales = mediana

ggHE <- ggplot(datos, aes(Grupo, Horas.extra, col = Grupo)) +
  geom_boxplot() +
  geom_jitter() +
  labs(y = "Horas extra", x = "") +
  theme(legend.position = "none")

ggHE

# Figura con resultados de pruebas estadísticas
ggHE +
  labs(title = get_test_label(tsHE, detailed = TRUE))
ggsave("Fig 1. t de student.png", width = 9, height = 7)

#### Distribucion t de student ####
n <- 18

curve(dt(x, df = n), xlim=c(-3.5, 3.5), main= 'Distribución t student\n n = 18', ylab="Densidad", xlab = "")
abline(h = 0, lty = 2, col = "gray")
polygon(c(qt(0.025, df = n), seq(qt(0.025, df = n), qt(0.975, df = n), length=100), qt(0.975, df = n)), c(0, dt(seq(qt(0.025, df = n), qt(0.975, df = n), length=100), df = n), 0), col = rgb(0, 0, 1, 0.3))
c(qt(0.025, df = n), qt(0.975, df = n))

abline(v = tsHE$statistic, col = 2, lty = 2)

tsHE
