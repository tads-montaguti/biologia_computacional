if (!require(BiocManager)) install.packages("BiocManager")
if (!require(igraph)) install.packages("igraph")
if (!require(rstudioapi)) install.packages("rstudioapi")

BiocManager::install("RedeR")

library("RedeR")
library(igraph)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Carrega os dados
load("./merge_df12.RData")

startRedeR()

# Seleciona as colunas de interesse
data_clean <- merge_df12[
  !is.na(merge_df12$FIRST_VACCINE_DATE) &
  !is.na(merge_df12$COUNTRY) &
  !is.na(merge_df12$WHO_REGION), 
]

# Converte a coluna para Date
data_clean$FIRST_VACCINE_DATE <- as.Date(data_clean$FIRST_VACCINE_DATE)

# Define a WHO_REGION sobre a qual queremos visualizar os dados
selected_region <- "AMRO"

# Filtra os dados da região especificada
selected_countries <- data_clean[data_clean$WHO_REGION == selected_region, ]

# Somente os 15 primeiros (para melhor visualização)
# selected_countries <- head(region_countries, 50)

# Data da primeira vacina entre os países filtrados
first_vac_date <- min(selected_countries$FIRST_VACCINE_DATE, na.rm = TRUE)

# Cria a coluna de DELYA_DAYS para cada país
selected_countries$DELAY_DAYS <- as.numeric(selected_countries$FIRST_VACCINE_DATE - first_vac_date, na.rm=TRUE)

# Threshold para definir se a relação entre os países (menor que 10 então conecta-se)
threshold <- 15

# Cria um dataframe vazio para armazenar as arestas
edges <- data.frame(from = character(), to = character(), weight = numeric(), stringsAsFactors = FALSE)

for (i in 1:(nrow(selected_countries) - 1)) {
  for (j in (i + 1):nrow(selected_countries)) {
    country1 <- selected_countries$COUNTRY[i]
    country2 <- selected_countries$COUNTRY[j]
    delay_diff <- abs(selected_countries$DELAY_DAYS[i] - selected_countries$DELAY_DAYS[j])
    
    # Adicionar aresta somente se a diferença for menor que o threshold
    if (!is.na(delay_diff) && delay_diff <= threshold) {
      edges <- rbind(edges, data.frame(from = country1, to = country2, weight = delay_diff))
    }
  }
}

g <- graph_from_data_frame(edges, directed = FALSE)

E(g)$weight <- E(g)$weight + 1

resetRedeR()
addGraphToRedeR(g)

# plot(
#   g,
#   vertex.label = V(g)$name,
#   vertex.size = 10,
#   edge.width = 1 + E(g)$weight / max(E(g)$weight),  # Ajusta largura com base nos pesos normalizados
#   vertex.color = "lightblue",
#   edge.color = "gray",
#   layout = layout_with_fr,
#   main = paste("Grafo de Proximidade Temporal (Threshold:", threshold, "dias)")
# )

# 
# data_clean <- merge_df12[
#   !is.na(merge_df12$Cases_cumulative_total) &
#   !is.na(merge_df12$Deaths_cumulative_total) &
#   !is.na(merge_df12$FIRST_VACCINE_DATE), ]
# 
# # Convertendo o tipo da coluina FIRST_VACCINE_DATE para Date
# class(data_clean$FIRST_VACCINE_DATE)
# data_clean$FIRST_VACCINE_DATE <- as.Date(data_clean$FIRST_VACCINE_DATE)
# class(data_clean$FIRST_VACCINE_DATE)
# 
# # Média de Cases_cumulative_total
# mean_cases <- as.integer(mean(data_clean$Cases_cumulative_total))
# # Média de Deaths_cumulative_total
# mean_deaths <- as.integer(mean(data_clean$Deaths_cumulative_total))
# 
# first_vac_date = min(data_clean$FIRST_VACCINE_DATE, na.rm = TRUE)
# 
# # Data da primeira vacina
# data_clean$DELAY_DAYS <- as.numeric(data_clean$FIRST_VACCINE_DATE - first_vac_date)
# 
# data_clean <- data_clean[!is.na(data_clean$DELAY_DAYS), ]
# 
# top_deaths <- data_clean[order(data_clean$Deaths_cumulative_total, decreasing = TRUE), ]
# top_deaths <- top_deaths[1:10, ]
# 
# edges <- data.frame(from = character(), to = character(), weight = numeric(), stringsAsFactors = FALSE)
# 
# threshold <- 30
# 
# for (i in 1:(nrow(top_deaths) - 1)) {
#   for (j in (i + 1):nrow(top_deaths)) {
#     country1 <- top_deaths$COUNTRY[i]
#     country2 <- top_deaths$COUNTRY[j]
#     delay_diff <- abs(top_deaths$DELAY_DAYS[i] - top_deaths$DELAY_DAYS[j])
#     
#     # Adicionar aresta somente se a diferença for menor que o threshold
#     if (delay_diff < threshold) {
#       weight <- abs(top_deaths$Deaths_cumulative_total[i] - top_deaths$Deaths_cumulative_total[j])
#       edges <- rbind(edges, data.frame(from = country1, to = country2, weight = weight))
#     }
#   }
# }
# 
# g <- graph_from_data_frame(edges, directed = FALSE)
# 
# plot(
#   g,
#   vertex.label = V(g)$name,
#   vertex.size = 10,
#   edge.width = E(g)$weight / max(E(g)$weight) * 2,  # Ajusta largura com base nos pesos normalizados
#   vertex.color = "lightblue",
#   edge.color = "gray",
#   layout = layout_with_fr,
#   main = "Grafo de Países com Maior Mortalidade"
# )
# # edges <- list() #Lista de arestas vazia
# # 
# # # Pegando as colunas COUNTRY e WHO_REGION para fazer uma associação entre países de uma região
# # data_selected <- merge_df12[, c("COUNTRY", "WHO_REGION")]
# # 
# # # Iterando para cada $WHO_REGION diferente
# # unique_regions <- unique(data_selected$WHO_REGION)
# # unique_regions <- unique_regions[1]
# # for (region in unique_regions) {
# #   # Selecionar os países na região
# #   countries <- data_selected$COUNTRY[data_selected$WHO_REGION == region]
# #   
# #   # Populando os vértices
# #   if (length(countries) > 1) {
# #     for (i in 1:(length(countries) - 1)) {
# #       for (j in (i + 1):length(countries)) {
# #         edges <- append(edges, list(c(countries[i], countries[j])))
# #       }
# #     }
# #   }
# # }
# # 
