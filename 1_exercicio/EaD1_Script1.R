# Navegar automaticamente para o diretório onde o script está executando
#install.packages("rstudioapi")
# install.packages("plotly")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#' Este script acompanha o "Exercício 1", carregando no RStudio os dados 
#' indicados na tarefa EaD1.

#### DATA ####
data_df <- read.table(file = "./vaccination-data.csv", sep=",", quote="\"",
                      header = T, stringsAsFactors = F)
# colnames(data_df)
# # [1] "COUNTRY"                              "ISO3"                                
# # [3] "WHO_REGION"                           "DATA_SOURCE"                         
# # [5] "DATE_UPDATED"                         "TOTAL_VACCINATIONS"                  
# # [7] "PERSONS_VACCINATED_1PLUS_DOSE"        "TOTAL_VACCINATIONS_PER100"           
# # [9] "PERSONS_VACCINATED_1PLUS_DOSE_PER100" "PERSONS_FULLY_VACCINATED"            
# # [11] "PERSONS_FULLY_VACCINATED_PER100"      "VACCINES_USED"                       
# # [13] "FIRST_VACCINE_DATE"                   "NUMBER_VACCINES_TYPES_USED" 
# 
# class(data_df)
# "data.frame"
# 
# head(data_df)[1:3,1:6]
# # COUNTRY ISO3 WHO_REGION DATA_SOURCE DATE_UPDATED TOTAL_VACCINATIONS
# # 1 Afghanistan  AFG       EMRO   REPORTING   2022-03-06            5597130
# # 2     Albania  ALB       EURO   REPORTING   2022-02-20            2707658
# # 3     Algeria  DZA       AFRO   REPORTING   2022-03-09           13704895
# 
# class(data_df$WHO_REGION)

#### EXAMPLES ####
# Confere classes com laço 'for'
# for(i in 1:ncol(data_df)){
#   tp <- class(data_df[,i])
#   print(tp)
# }
# # Confere classes com 'lapply'
# classes_df <- lapply(data_df, class)
# classes_df <- unlist(classes_df)
# # Sumário de variáveis numéricas
# data_df_numeric <- data_df[,classes_df=="numeric"]
# summary(data_df_numeric)
# # Boxplot de variáveis numéricas
# boxplot(data_df_numeric)
# boxplot(data_df_numeric[,-1])
# # Ajusta escala de variáveis numéricas para visualização
# data_df_numeric[,1] <- data_df_numeric[,1]/50000
# boxplot(data_df_numeric, outline = F)
# 
# 
# rownames(data_df) <- data_df$COUNTRY
# rownames(data_df)
#### Continuação ####

# Cria um novo dataframe contendo as 4 colunas especificadas
filtered_data <- data_df[
  (!is.na(data_df$FIRST_VACCINE_DATE) & data_df$WHO_REGION != ""), 
  c("COUNTRY", "ISO3", "FIRST_VACCINE_DATE", "WHO_REGION"
)]

# Transforma a coluna em questão para ser do tipo data e ordena a tabela com base nesta coluna
class(filtered_data$FIRST_VACCINE_DATE)
filtered_data$FIRST_VACCINE_DATE <- as.Date(filtered_data$FIRST_VACCINE_DATE)

# Cria uma nova coluna DELAY_DAYS que calcula a diferença entre o valor da $FIRST_VACCINE_DATE
# entre o menor valor existente na tabela e o valor da linha em questão
filtered_data$DELAY_DAYS <- as.numeric(
  filtered_data$FIRST_VACCINE_DATE - min(
    filtered_data$FIRST_VACCINE_DATE,
    na.rm = TRUE
  )
)

first_vaccine_day <- min(filtered_data$FIRST_VACCINE_DATE, na.rm=TRUE)

#### Seleciona os 5 maiores delays ####
#slow_countries <- filtered_data[order(-filtered_data$DELAY_DAYS), ][1:5, ]
# Calcula a moda da coluna de delays
#mode_delay <- as.numeric(names(sort(table(filtered_data$DELAY_DAYS), decreasing = TRUE)[1]))
# Seleciona 5 países mais próximos da moda
#mode_countries <- filtered_data[abs(filtered_data$DELAY_DAYS - mode_delay) <= 1, ][1:5, ]
# Seleciona os 5 países mais rápidos
#fast_countries <- filtered_data[order(filtered_data$DELAY_DAYS), ][1:5, ]
# Novo dataframe com uma amostra dos países e seus delays
#selected_countries <- rbind(fast_countries, mode_countries, slow_countries)
#selected_countries <- selected_countries[order(selected_countries$DELAY_DAYS), ]
#colors <- rainbow(length(selected_countries))
#par(mar = c(5, 5, 5, 5)) # Ajustar margens para acomodar os nomes dos países
#barplot(
#   selected_countries$DELAY_DAYS,
#   names.arg = selected_countries$COUNTRY,
#   las = 2,
#   col = colors,
#   main = sprintf('Delay em dias entre %s e a primeira vacina', as.character(first_vaccine_day)),
#   xlab = "",
#   ylab = "Dias desde o primeiro acesso"
# )
# plot(
#   filtered_data$FIRST_VACCINE_DATE,    # Datas no eixo X
#   filtered_data$DELAY_DAYS,            # Dias transcorridos no eixo Y
#   xlab = "Datas (FIRST_VACCINE_DATE)", # Rótulo do eixo X
#   ylab = "Dias Transcorridos (DELAY_DAYS)", # Rótulo do eixo Y
#   main = "Delay de Vacinação por País",    # Título do gráfico
#   pch = 19,      # Estilo do ponto (círculo sólido)
#   col = "blue"   # Cor dos pontos
# )
# 
# # Adicionar rótulos aos pontos
# text(
#   filtered_data$FIRST_VACCINE_DATE,    # Posições no eixo X
#   filtered_data$DELAY_DAYS,            # Posições no eixo Y
#   labels = filtered_data$COUNTRY,      # Países como rótulos
#   pos = 4,                         # Posição do texto (4 = à direita)
#   cex = 0.8,                       # Tamanho do texto
#   col = "darkred"                  # Cor dos rótulos
# )
#### using plotly ####
library(plotly)
library(dplyr)
# Instalar e carregar a biblioteca necessária
if (!requireNamespace("plotly", quietly = TRUE)) {
  install.packages("plotly")
}
library(plotly)

# Supondo que 'filtered_data' já foi processado conforme o script fornecido
# E que 'first_vaccine_day' foi definido anteriormente como o menor valor de FIRST_VACCINE_DATE

# Criar períodos de mês em mês a partir de 'first_vaccine_day'
filtered_data$PERIOD <- cut(
  filtered_data$FIRST_VACCINE_DATE,
  breaks = seq(
    as.Date(first_vaccine_day),
    max(filtered_data$FIRST_VACCINE_DATE, na.rm = TRUE) + 30,
    by = "1 month"
  ),
  include.lowest = TRUE,
  right = FALSE  # Inclui o limite inferior e exclui o superior
)

# Transformar o período em um formato legível
period_labels <- levels(filtered_data$PERIOD)
levels(filtered_data$PERIOD) <- format(
  as.Date(sub("\\[(.+),.*", "\\1", period_labels)), "%Y-%m"
)

# Criar uma tabela com contagem por WHO_REGION e período
period_counts <- table(filtered_data$WHO_REGION, filtered_data$PERIOD)

# Converter para um data frame para plotagem
period_df <- as.data.frame(period_counts)
colnames(period_df) <- c("WHO_REGION", "PERIOD", "COUNT")

# Criar uma paleta de cores básica
palette <- colors()  # Gera uma lista de cores padrão
selected_colors <- palette[seq(1, length(palette), length.out = length(unique(period_df$WHO_REGION)))]
region_colors <- setNames(selected_colors, unique(period_df$WHO_REGION))

# Criar o gráfico de barras agrupadas
plot <- plot_ly()

# Adicionar barras para cada WHO_REGION
unique_regions <- unique(period_df$WHO_REGION)

for (region in unique_regions) {
  region_data <- subset(period_df, WHO_REGION == region)
  plot <- add_trace(
    plot,
    x = ~region_data$PERIOD,
    y = ~region_data$COUNT,
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
