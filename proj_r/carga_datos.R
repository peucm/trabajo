library("rjson")
library("ggplot2")

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

create_plot <- function(year, chart_data, region, type, typeDisplayName) {
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
  p
}

year = 2012
region = "Madrid"
type = "Precio Gasoleo A"
displayName = "Gasóleo A"

result = load_data(year)
create_plot(year, result, type, displayName)
