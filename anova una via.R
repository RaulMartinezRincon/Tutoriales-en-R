## Raúl Octavio Martínez Rincón

### ANOVA una vía y la prueba de Tukey ###
# 1. Datos ejemplo
# 2. Edición de datos
# 3. Figuras exploratorias 
# 4. Supuestos estadísticos
# 5. Análisis de varianza para un factor (ANOVA una vía)
# 6. Prueba de Tukey (comparaciones pareadas)
# 7. Figura para ANOVA de una vía
# 8. Figura para ANOVA de una vía y prueba de Tukey

#### Librerías (paquetes) ####
library(tidyverse)
library(rstatix) # Pruebas estadistícas básicas coherente con tidyverse
library(ggpubr) # Funciones para figuras "listas para publicación"

help(package = "rstatix")
help(package = "ggpubr")

theme_set(theme_bw()) # Tema blanco y negro para figuras

#### Directorio de trabajo ####
# Es importante definir para importar/exportar datos/figuras
setwd("C:/Raul/Tutoriales en R/Anova una via")

#### datos ####
?chickwts

# Formato tibble
MisDatos <- tibble(chickwts)
MisDatos %>% 
  print(n = 5)

levels(MisDatos$feed)

# Cambio de nombres de variables
names(MisDatos) <- c("Peso", "Suplemento")

MisDatos %>% 
  print(n = 5)

# Cambio de niveles de la variable suplemento
originales <- as.character(unique(MisDatos$Suplemento)) # vector con los nombres originales
originales
editadas <- LETTERS[1:length(originales)] # vector con las claves a usar
editadas

tibble(originales, editadas)

MisDatos <- MisDatos %>% 
  mutate(Suplemento = factor(Suplemento, levels = originales, labels = editadas))

MisDatos %>% 
  print(n = 5)

#### Figuras exploratorias ####
ggplot(MisDatos, aes(Suplemento, Peso, fill = Suplemento)) + 
  geom_boxplot(show.legend = F) + 
  labs(y = "Peso (g)")
ggsave("Fig 1. Figura de caja y bigotes.png", width = 9, height = 7)

ggplot(MisDatos, aes(Suplemento, Peso, fill = Suplemento)) + 
  stat_summary(fun = "mean", geom = "bar") + 
  stat_summary(fun.data = "mean_se") + 
  labs(y = "Peso (g)") + 
  theme(legend.position = "none")
ggsave("Fig 2. Promedios y error estándar.png", width = 9, height = 7)

#### supuestos estadísticos ####
## Normalidad
# H0 = los datos no provienen de una distribución normal
shapiro.test(MisDatos$Peso)

MisDatos %>% 
  shapiro_test(Peso)

# p>0.05, Concluimos que no podemos rechazar la hipótesis nula.

## Homogeneidad de varianzas
# H0 = los datos no tienen varianzas homogéneas
bartlett.test(Peso ~ Suplemento, data = MisDatos)

MisDatos %>% 
  levene_test(Peso ~ Suplemento)

# p>0.05, Concluimos que no podemos rechazar la hipótesis nula.

#### ANOVA una vía ####
# H0 = No hay diferencias significativas en ninguno de los grupos
summary(aov(Peso ~ Suplemento, data = MisDatos))

MisDatos %>% 
  anova_test(Peso ~ Suplemento)

# p<0.05, Concluimos que al menos uno de los grupos es diferente.

#### Tukey HSD ####
TukeyHSD(aov(Peso ~ Suplemento, data = MisDatos))

MisDatos %>% 
  tukey_hsd(Peso ~ Suplemento) %>% 
  select(group1, group2, estimate, p.adj, p.adj.signif) %>%
  print(n = Inf, width = Inf)

# Exportar tabla
MisDatos %>% 
  tukey_hsd(Peso ~ Suplemento) %>% 
  write.csv(file = "Tabla Tukey ANOVA una vía.csv", row.names = F)

#### Figura ANOVA y Tukey ####
# ANOVA una vía
ggboxplot(MisDatos, x = "Suplemento", y = "Peso") +
  stat_anova_test(label = "F({DFn}, {DFd}) = {F}, p = {p.format}, n = {n}", label.x.npc = "left")
ggsave("Fig 3. Caja y bigotes (ANOVA).png", width = 9, height = 7)

# ANOVA y Tukey
resAnova <- MisDatos %>% 
  anova_test(Peso ~ Suplemento)
resAnova

# comparaciones pareadas
comPar <- MisDatos %>% 
  tukey_hsd(Peso ~ Suplemento)
comPar <- comPar %>% add_xy_position(x = "Suplemento")
comPar

ggboxplot(MisDatos, x = "Suplemento", y = "Peso", fill = "Suplemento") +
  stat_pvalue_manual(comPar, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(resAnova, detailed = TRUE),
    caption = get_pwc_label(comPar)
  ) +
  theme(legend.position = "none")
ggsave("Fig 4. Caja y bigotes (ANOVA y Tukey).png", width = 9, height = 7)

#### preguntas ####
ggplot(MisDatos, aes(Suplemento, Peso)) + 
  stat_summary(fun = "mean", geom = "bar", aes(fill = Suplemento)) + 
  stat_summary(fun.data = "mean_se") +
  stat_pvalue_manual(comPar, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(resAnova, detailed = TRUE),
    caption = get_pwc_label(comPar)
  ) +
  theme(legend.position = "none")
