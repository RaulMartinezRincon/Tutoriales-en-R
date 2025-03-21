## Raúl Octavio Martínez Rincón

### Figuras prueba de Tukey y Dunn ###
# 1. Anova y Tukey
# 2. Kruskal-Wallis y Dunn

#### Librerías (paquetes) ####
library(tidyverse)
library(rstatix)
library(ggpubr)
library(multcompView)
theme_set(theme_bw())

## Definir directorio de trabajo
setwd("C:/Raul/Tutoriales en R/Basicos en R")

#### 1. Anova y Tukey ####
datosPeso <- tibble(chickwts)
datosPeso
names(datosPeso) <- c("Peso", "Grupo")

Grupos <- unique(datosPeso$Grupo)
Grupos

datosPeso <- datosPeso %>% mutate(Grupo = factor(Grupo, levels = Grupos, labels = LETTERS[1:length(Grupos)]))

datosPeso

# Crear objeto con el resultado del ANOVA
aovPeso <- datosPeso %>% 
  anova_test(Peso ~ Grupo)
aovPeso

# Crear objeto con el resultado de la prueba de Tukey
TukPeso <- datosPeso %>% 
  tukey_hsd(Peso ~ Grupo) %>% 
  add_xy_position(x = "Grupo")
TukPeso

TukPeso %>% 
  select(group1, group2, estimate, p.adj) %>% 
  mutate(Sig = ifelse(p.adj < 0.05, "Si", "No"))

# vector con letras para identificar grupos diferentes
vectPeso <- as.vector((TukPeso %>% select(p.adj)) < 0.05)
vectPeso
names(vectPeso) <- paste(TukPeso$group1, TukPeso$group2, sep = "-")
vectPeso
vectPeso <- multcompLetters(vectPeso)
vectPeso
vectPeso <- vectPeso$Letters
vectPeso

# Tabla con nombres de grupo y letras
tLetrasPeso <- tibble(Grupo = names(vectPeso), Letras = vectPeso)
tLetrasPeso

# Agregar posiciones (maximo * 1.1) de letras
tLetrasPesoPos <- datosPeso %>% 
  group_by(Grupo) %>% 
  summarise(y = max(Peso, na.rm = T) * 1.1) %>% 
  left_join(tLetrasPeso)
tLetrasPesoPos

ggplot(datosPeso, aes(Grupo, Peso)) + 
  geom_boxplot(aes(fill = Grupo), show.legend = F) +
  geom_text(data = tLetrasPesoPos, 
            aes(Grupo, y, label = Letras)) +
  labs(subtitle = get_test_label(aovPeso, detailed = TRUE),
       caption = get_pwc_label(TukPeso)) +
  theme(legend.position = "none")
ggsave("Figura Anova y Tukey.png", width = 9, height = 7)

TukPeso %>% 
  select(group1, group2, estimate, p.adj) %>% 
  mutate(Sig = ifelse(p.adj < 0.05, "Si", "No"))

#### 2. Kruskal-Wallis y Dunn ####
?iris

datosIris <- tibble(iris) # Convertir a tibble
# Renombrar las columnas ("variables)
names(datosIris) <- c("LS", "AS", "LP", "AP", "Especie")

datosIris

# Crear objeto con el resultado de Kruskal-Wallis
kwLS <- datosIris %>% 
  kruskal_test(LS ~ Especie)
kwLS

# Crear objeto con el resultado de la prueba de Dunn
dunnLS <- datosIris %>% 
  dunn_test(LS ~ Especie) %>% 
  add_xy_position(x = "Especie")

dunnLS

# vector con letras para identificar grupos diferentes
vectIris <- as.vector((dunnLS %>% select(p.adj)) < 0.05)
vectIris
names(vectIris) <- paste(dunnLS$group1, dunnLS$group2, sep = "-")
vectIris
vectIris <- multcompLetters(vectIris)
vectIris
vectIris <- vectIris$Letters
vectIris

# Tabla con nombres de grupo y letras
tLetrasIris <- tibble(Especie = names(vectIris), Letras = vectIris)

# Agregar posiciones (maximo * 1.1) de letras
tLetrasIrisPos <- datosIris %>% 
  group_by(Especie) %>% 
  summarise(y = max(LS, na.rm = T) * 1.1) %>% 
  left_join(tLetrasIris)
tLetrasIrisPos

ggplot(datosIris, aes(Especie, LS)) + 
  geom_boxplot(aes(fill = Especie), show.legend = F) +
  geom_text(data = tLetrasIrisPos, 
            aes(Especie, y, label = Letras)) +
  labs(subtitle = get_test_label(kwLS, detailed = TRUE),
       caption = get_pwc_label(dunnLS)) +
  theme(legend.position = "none")
ggsave("Figura Kruskal y Dunn.png", width = 9, height = 7)
