# install.packages("rstudioapi")
# install.packages("plotly")
library(plotly)
library(dplyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

data_df <- read.table(
  file = "./vaccination-data.csv", 
  sep=",", 
  quote="\"",
  header = T, stringsAsFactors = F
)

#### Seleção de dados ####

# Cria um novo dataframe contendo as 4 colunas especificadas
filtered_data <- data_df[
  (!is.na(data_df$FIRST_VACCINE_DATE) & data_df$WHO_REGION != ""), 
  c("COUNTRY", "ISO3", "FIRST_VACCINE_DATE", "WHO_REGION"
)]

# Transforma a coluna em questão para ser do tipo data e ordena a tabela com base nesta coluna
class(filtered_data$FIRST_VACCINE_DATE)
filtered_data$FIRST_VACCINE_DATE <- as.Date(filtered_data$FIRST_VACCINE_DATE)

# Menor e maior valor dentre a coluna $FIRST_VACCINE_DATE removendo os NA
first_vaccine_day <- min(filtered_data$FIRST_VACCINE_DATE, na.rm=TRUE)
last_vaccine_day <- max(filtered_data$FIRST_VACCINE_DATE, na.rm = TRUE)

# Adiciona a coluna $DELAY_DAYS sendo a diferença entre a $FIRST_VACCINE_DATE e first_vaccine_day
filtered_data$DELAY_DAYS <- as.numeric(
  filtered_data$FIRST_VACCINE_DATE - first_vaccine_day
)


#### Configuração da visualização ####

# Cria um array contendo as datas espaçadas de 30 em 30 dias entre first_vaccine_day
# e o maior valor presente no filtered_data
periods = seq(
  as.Date(first_vaccine_day),
  as.Date(last_vaccine_day) + 30,
  by = "1 month"
)
print(periods)

# Cria uma nova coluna $PERIOD informando em qual intervalo de periods aquela linha está contida
filtered_data$PERIOD <- cut(
  filtered_data$FIRST_VACCINE_DATE, 
  periods, 
  include.lowest = TRUE,
  right = FALSE
)

# Transformar o período em um formato legível
# utiliza o levels para garantir que serão pegos periods com representatividade
period_labels <- levels(filtered_data$PERIOD)
print(period_labels)

levels(filtered_data$PERIOD) <- format(
  as.Date(sub("\\[(.+),.*", "\\1", period_labels)), "%Y-%m"
)

print(filtered_data$PERIOD)

# Criar uma tabela com contagem por WHO_REGION e período
period_counts <- table(filtered_data$WHO_REGION, filtered_data$PERIOD)
print(period_counts)

# Converter para um data frame para plotagem
period_df <- as.data.frame(period_counts)
print(period_df)
colnames(period_df) <- c("WHO_REGION", "PERIOD", "COUNT")

# Criar uma paleta de cores básica
palette <- colors()
selected_colors <- palette[seq(1, length(palette), length.out = length(unique(period_df$WHO_REGION)))]
print(selected_colors)
region_colors <- setNames(selected_colors, unique(period_df$WHO_REGION))
print(region_colors)

# Criar o gráfico de barras agrupadas
plot <- plot_ly()

# Adicionar barras para cada WHO_REGION
unique_regions <- unique(period_df$WHO_REGION)
print(unique_regions)

for (region in unique_regions) {
  region_data <- subset(period_df, WHO_REGION == region)
  plot <- add_trace(
    plot,
    x = region_data$PERIOD,  # Use diretamente a coluna PERIOD
    y = region_data$COUNT,    # Use diretamente a coluna COUNT
    type = 'bar',
    name = region,
    marker = list(color = region_colors[region])
  )
}

# Configurar layout do gráfico
plot <- layout(
  plot,
  yaxis = list(title = "Count"),
  xaxis = list(title = "Period (monthly)"),
  barmode = "group",
  title = "Occurrences of FIRST_VACCINE_DATE by WHO Region"
)

# Mostrar o gráfico
plot
