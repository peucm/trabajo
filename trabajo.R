library(moments)

carburantes_mensual <- read.csv(file = './precio_carburantes_mensual.csv')
carburantes_trimestral <- read.csv(file = './precio_carburantes_trimestral.csv')
pib_trimestral <- read.csv(file = './pib_trimestral.csv')
petroleo_mensual <- read.csv(file = './precio_petroleo.csv')
ipc_mensual <- read.csv(file = './ipc_mensual.csv')

# Funciones
crear_scatterplot <- function(x, y, x_names, xlab, ylab, main) {
  plot(1:length(x),
       y, 
       type = "b", 
       pch = 18,
       col = 4,
       xaxt = "n",
       xlab = xlab, 
       ylab = ylab,
       main = main)
  
  axis(1, at = 1:length(x), labels = x_names)
  text(1:length(x), y, y, font = 2, cex = 0.5, pos = 3)
}

moda <- function(x) {
  return(as.numeric(names(which.max(table(x)))))
}

crear_scatterplot_lm <- function(x, y, lm, xlab, ylab, main) {
  coeff = coefficients(lm)
  eq = paste0("Recta de regresión: y = ", round(coeff[2], 3), "x + ", round(coeff[1], 3))
  
  plot(x,
       y,
       pch = 18,
       col = 4,
       xlab = xlab, 
       ylab = ylab,
       main = main,
       sub = eq,
       col.sub = "#242323")
  abline(lm, col = 'red') 
}

# Scatterplots de carburantes
meses = paste(carburantes_mensual$ano, carburantes_mensual$mes, sep = "-")
crear_scatterplot(carburantes_mensual$mes, carburantes_mensual$gasolina, meses, "Mes", "Precio (€/l)", "Precio medio de la gasolina")
crear_scatterplot(carburantes_mensual$mes, carburantes_mensual$gasoil, meses, "Mes", "Precio (€/l)", "Precio medio del diésel")

# Scatterplots PIB e IPC
trimestres = paste(pib_trimestral$ano, pib_trimestral$trimestre, sep = "-")
crear_scatterplot(pib_trimestral$trimestre, pib_trimestral$var_trim_pib, trimestres, "Trimestre", "Variación (%)", "Variación trimestral del PIB")
crear_scatterplot(ipc_mensual$mes, ipc_mensual$ipc, meses, "Mes", "Variación (%)", "Variación mensual del IPC")

# Scatterplot petróleo
crear_scatterplot(petroleo_mensual$mes, petroleo_mensual$petroleo, meses, "Mes", "Precio (€/Barril)", "Precio medio del barril de petróleo")

# Medidas de distribución de carburantes
media_gasolina = mean(carburantes_mensual$gasolina)
mediana_gasolina = median(carburantes_mensual$gasolina)
moda_gasolina = moda(carburantes_mensual$gasolina)
desv_tipica_gasolina = sd(carburantes_mensual$gasolina)
max_gasolina = max(carburantes_mensual$gasolina)
min_gasolina = min(carburantes_mensual$gasolina)
curtosis_gasolina = kurtosis(carburantes_mensual$gasolina)
quintil_gasolina = quantile(carburantes_mensual$gasolina, prob = seq(0, 1, 1 / 5))
percentil_gasolina = quantile(carburantes_mensual$gasolina, prob = seq(0, 1, length = 101))

media_gasoil = mean(carburantes_mensual$gasoil)
mediana_gasoil = median(carburantes_mensual$gasoil)
moda_gasoil = moda(carburantes_mensual$gasoil)
desv_tipica_gasoil = sd(carburantes_mensual$gasoil)
max_gasoil = max(carburantes_mensual$gasoil)
min_gasoil = min(carburantes_mensual$gasoil)
curtosis_gasoil = kurtosis(carburantes_mensual$gasoil)
quintil_gasoil = quantile(carburantes_mensual$gasoil, prob = seq(0, 1, 1 / 5))
percentil_gasoil = quantile(carburantes_mensual$gasoil, prob = seq(0, 1, length = 101))

# Medidas de distribución del PIB e IPC
media_pib = mean(pib_trimestral$var_trim_pib)
mediana_pib = median(pib_trimestral$var_trim_pib)
moda_pib = moda(pib_trimestral$var_trim_pib)
desv_tipica_pib = sd(pib_trimestral$var_trim_pib)
max_pib = max(pib_trimestral$var_trim_pib)
min_pib = min(pib_trimestral$var_trim_pib)
curtosis_pib = kurtosis(pib_trimestral$var_trim_pib)
quintil_pib = quantile(pib_trimestral$var_trim_pib, prob = seq(0, 1, 1 / 5))
percentil_pib = quantile(pib_trimestral$var_trim_pib, prob = seq(0, 1, length = 101))

media_ipc = mean(ipc_mensual$ipc)
mediana_ipc = median(ipc_mensual$ipc)
moda_ipc = moda(ipc_mensual$ipc)
desv_tipica_ipc = sd(ipc_mensual$ipc)
max_ipc = max(ipc_mensual$ipc)
min_ipc = min(ipc_mensual$ipc)
curtosis_ipc = kurtosis(ipc_mensual$ipc)
quintil_ipc = quantile(ipc_mensual$ipc, prob = seq(0, 1, 1 / 5))
percentil_ipc = quantile(ipc_mensual$ipc, prob = seq(0, 1, length = 101))

# Medidas de distribución del barril de petróleo
media_petroleo = mean(petroleo_mensual$petroleo)
mediana_petroleo = median(petroleo_mensual$petroleo)
moda_petroleo = moda(petroleo_mensual$petroleo)
desv_tipica_petroleo = sd(petroleo_mensual$petroleo)
max_petroleo = max(petroleo_mensual$petroleo)
min_petroleo = min(petroleo_mensual$petroleo)
curtosis_petroleo = kurtosis(petroleo_mensual$petroleo)
quintil_petroleo = quantile(petroleo_mensual$petroleo, prob = seq(0, 1, 1 / 5))
percentil_petroleo = quantile(petroleo_mensual$petroleo, prob = seq(0, 1, length = 101))

# Barplot de cuantiles de carburantes
barplot(quintil_gasolina, main = "Quintiles del precio medio de la gasolina")
barplot(percentil_gasolina, main = "Percentiles del precio medio del gasolina")
barplot(quintil_gasoil, main = "Quintiles del precio medio del diésel")
barplot(percentil_gasoil, main = "Percentiles del precio medio del diésel")

# Barplot de cuantiles de IPC y PIB
barplot(quintil_pib, main = "Quintiles de la variación trimestral del PIB")
barplot(percentil_pib, main = "Percentiles de la variación trimestral del PIB")
barplot(quintil_ipc, main = "Quintiles de la variación mensual del IPC")
barplot(percentil_ipc, main = "Percentiles de la variación mensual del IPC")

# Barplot de cuantiles de petróleo
barplot(quintil_petroleo, main = "Quintiles del precio del barril de petróleo")
barplot(percentil_petroleo, main = "Percentiles del precio del barril de petróleo")

# Datasets conjuntos
carburantes_petroleo = merge(carburantes_mensual, petroleo_mensual, by = c("ano", "mes"))
carburantes_pib = merge(carburantes_trimestral, pib_trimestral, by = c("ano", "trimestre"))
carburantes_ipc = merge(carburantes_mensual, ipc_mensual, by = c("ano", "mes"))

# Coeficiente de correlación de Pearson para los diferentes factores
cor_gasolina_petroleo = cor(carburantes_mensual$gasolina, petroleo_mensual$petroleo)
cor_gasoil_petroleo = cor(carburantes_mensual$gasoil, petroleo_mensual$petroleo)
cor_gasolina_pib = cor(carburantes_trimestral$gasolina, pib_trimestral$var_trim_pib)
cor_gasoil_pib = cor(carburantes_trimestral$gasoil, pib_trimestral$var_trim_pib)
cor_gasolina_ipc = cor(carburantes_mensual$gasolina, ipc_mensual$ipc)
cor_gasoil_ipc = cor(carburantes_mensual$gasoil, ipc_mensual$ipc)

# Modelos de regresión lineal
modelo_gasolina_gasoil = lm(gasolina ~ gasoil, carburantes_mensual)
modelo_gasolina_petroleo = lm(gasolina ~ petroleo, carburantes_petroleo)
modelo_gasoil_petroleo = lm(gasoil ~ petroleo, carburantes_petroleo)
modelo_gasolina_pib = lm(gasolina ~ var_trim_pib, carburantes_pib)
modelo_gasoil_pib = lm(gasoil ~ var_trim_pib, carburantes_pib)
modelo_gasolina_ipc = lm(gasolina ~ ipc, carburantes_ipc)
modelo_gasoil_ipc = lm(gasoil ~ ipc, carburantes_ipc)

crear_scatterplot_lm(carburantes_mensual$gasoil, carburantes_mensual$gasolina, modelo_gasolina_gasoil, "Precio medio del diésel (€/l)", "Precio medio de la gasolina (€/l)", "Precio de la gasolina y precio del diésel")
crear_scatterplot_lm(carburantes_petroleo$petroleo, carburantes_petroleo$gasolina, modelo_gasolina_petroleo, "Precio del barril de petróleo (€/barril)", "Precio medio de la gasolina (€/l)", "Precio de la gasolina y precio del barril de petróleo")
crear_scatterplot_lm(carburantes_petroleo$petroleo, carburantes_petroleo$gasoil, modelo_gasoil_petroleo, "Precio del barril de petróleo (€/barril)", "Precio medio del diésel (€/l)", "Precio del diésel y precio del barril de petróleo")
crear_scatterplot_lm(carburantes_pib$var_trim_pib, carburantes_pib$gasolina, modelo_gasolina_pib, "Variación (%)", "Precio medio de la gasolina (€/l)", "Precio de la gasolina y variación trimestral del PIB")
crear_scatterplot_lm(carburantes_pib$var_trim_pib, carburantes_pib$gasoil, modelo_gasoil_pib, "Variación (%)", "Precio medio del diésel (€/l)", "Precio del diésel y variación trimestral del PIB")
crear_scatterplot_lm(carburantes_ipc$ipc, carburantes_ipc$gasolina, modelo_gasolina_ipc, "Variación (%)", "Precio medio de la gasolina (€/l)", "Precio de la gasolina y variación mensual del IPC")
crear_scatterplot_lm(carburantes_ipc$ipc, carburantes_ipc$gasoil, modelo_gasoil_ipc, "Variación (%)", "Precio medio del diésel (€/l)", "Precio del diésel y variación mensual del IPC")