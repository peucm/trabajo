library("rjson")
library("ggplot2")

regions = c("País Vasco", "Castilla la Mancha", "Comunidad Valenciana", "Andalucia", "Castilla y León", "Extremadura", "Baleares", "Cataluña", "Galicia", "Aragón", "Rioja (La)", "Madrid", "Murcia", "Navarra", "Asturias", "Canarias", "Cantabria", "Ceuta", "Melilla")

load_data <- function(year) {
  result <- fromJSON(file = sprintf("../avg_price_monthly_report_%d.json", year))
}

extract_values <- function(data, region, type) {
  result <- vector()
  
  for (month in data) {
    result <- c(result, round(month[[region]][[type]], 3))
  }
  
  result
}

create_plot <- function(year, chart_data, region, type, typeDisplayName, filename) {
  values <- extract_values(chart_data, region, type)
  df <- data.frame(mes = names(chart_data), precio = values)
  df$mes <- factor(df$mes, levels = df$mes)
  
  p <- ggplot(data = df, aes(x = mes, y = precio)) +
    geom_bar(stat="identity", , fill = "steelblue") +
    geom_text(aes(label = precio), vjust = -0.3, size = 3.5) +
    ylim(0, 1.5) +
    labs(title = sprintf("Año %d - %s - %s", year, region, typeDisplayName), x = "Mes", y = "Precio (€/l)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(filename)
  p
}

year <- 2011
type <- "Precio Gasolina 95 E5"
displayName <- "Gasolina 95"
fileName <- "Gasolina95"

while (year <= 2022) {
  print(sprintf("Año %d", year))
  result <- load_data(year)
  
  for (region in regions) {
    create_plot(year, result, region, type, displayName, sprintf("plots/%d_%s_%s.jpg", year, fileName, region))
  }
  
  year <- year + 1
}
