carburantes_mensual <- read.csv(file = './precio_carburantes.csv')
pib_trimestral <- read.csv(file = './pib.csv')

crear_scatterplot <- function(x, y, xlab, label) {
  plot(1:length(x),
       y, 
       type = "b", 
       pch = 18,
       col = 4,
       xaxt = "n",
       xlab = "Mes", 
       ylab = "Precio (€/l)",
       main = label)
  
  axis(1, at = 1:length(x), labels = xlab)
  text(1:length(x), y, y, font = 2, cex = 0.5, pos = 3)
}

# Scatterplots carburantes
xlab = paste(carburantes_mensual$ano, carburantes_mensual$mes, sep = "-")
crear_scatterplot(carburantes_mensual$mes, carburantes_mensual$gasolina, xlab, "Precio medio de la gasolina")
crear_scatterplot(carburantes_mensual$mes, carburantes_mensual$gasoil, xlab, "Precio medio del diésel")

xlab = paste(pib_trimestral$ano, pib_trimestral$trimestre, sep = "-")
crear_scatterplot(pib_trimestral$trimestre, pib_trimestral$var_trim_pib, xlab, "Variación trimestral del PIB")