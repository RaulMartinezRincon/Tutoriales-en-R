## Regresión lineal simple
## Raúl Octavio Martínez Rincón (rrincon@cibnor.mx)

library(tidyverse) # Colección de librerías
library(ggpubr) # Figuras con resultados estadísticos
library(RColorBrewer) # Paletas de colores

theme_set(theme_bw()) # Define el tema de las figuras en blanco y negro

## Definir directorio de trabajo
setwd("C:/Raul/Tutoriales en R/Regresion lineal simple/")

#### Datos de ejemplo ####
?faithful
class(faithful)
head(faithful)

plot(faithful$eruptions, faithful$waiting)

plot(faithful)
#### Modelo lineal en R base ####
?lm

## objeto del modelo
lm.faithful <- lm(waiting ~ eruptions, data = faithful)

lm.faithful # Muestra la fórmula y los coeficientes

class(lm.faithful)

names(lm.faithful)

lm.faithful$coefficients

a <- lm.faithful$coefficients[1]
b <- lm.faithful$coefficients[2]

## supuestos estadísticos
plot(lm.faithful, which = 2) # Normalidad de residuales
plot(lm.faithful, which = 1) # Homocedásticidad de residuales

## resumen estadístico
sum.lm.faithful <- summary(lm.faithful)
sum.lm.faithful

## Estadísticos de los coeficientes del modelo
sum.lm.faithful$coefficients

## R cuadrado
sum.lm.faithful$r.squared

## tabla de análisis de varianza
anova(lm.faithful)

## Valores esperados e intervalo de confianza
pred.lm.faithful <- data.frame(eruptions = seq(1.5, 5.5, length = 100))
pred.lm.faithful <- cbind(pred.lm.faithful, as.data.frame(predict(lm.faithful, pred.lm.faithful, interval = "confidence")))

head(pred.lm.faithful)

## Figura con ecuación y R cuadrado
plot(faithful$eruptions, faithful$waiting, xlab = "Tiempo de erupción (min)", ylab = "Tiempo de espera (min)")
abline(lm.faithful)
lines(pred.lm.faithful$eruptions, pred.lm.faithful$lwr, col = 2, lty = 2)
lines(pred.lm.faithful$eruptions, pred.lm.faithful$upr, col = 2, lty = 2)
legend("topleft", c(paste0("y = ", round(a, 1), " + ", round(b, 1), "*X"), expression(R^2==0.81)), bty = "n")
legend("bottomright", c("Valor ajustado", "Int. de confianza"), col = c(1, 2), lty = c(1, 2))

#### gráficos en tidyverse ####
col.pal <- rev(brewer.pal(n = 9, name = "Spectral"))

ggplot(faithful, aes(eruptions, waiting)) +
  geom_point(aes(col = waiting), show.legend = F) +
  geom_smooth(method = "lm") +
  labs(x = "Tiempo de erupción (min)", y = "Tiempo de espera (min)") +
  stat_regline_equation(aes(label = paste(eq.label, ..rr.label.., sep = "~~~~"))) + 
  scale_color_gradientn(colours = col.pal)
ggsave("Fig. Reg. lin. faithful.png", width = 9, height = 7)
