if (!require(plotly)) install.packages("plotly")
if (!require(tidyr)) install.packages("tidyr")

library(plotly)
library(tidyr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#### DATA #####
df1 <- read.table(file = "../1_exercicio/vaccination-data.csv", sep=",", quote="\"", header = T, stringsAsFactors = F)
df2 <- read.table(file = "../2_exercicio/global-table-data.csv", sep=",", quote="\"", header = T, stringsAsFactors = F)

sum(df1$COUNTRY %in% df2$Name, na.rm = TRUE)

any(duplicated(df1$COUNTRY))
any(duplicated(df2$Name))

rownames(df1) <- df1$COUNTRY
rownames(df2) <- df2$Name

country_names <- intersect(rownames(df1), rownames(df2))
df1 <- df1[country_names,]
df2 <- df2[country_names,]
all(rownames(df1)==rownames(df2))
sum(colnames(df1) %in% colnames(df2))

merge_df12 <- cbind(df1, df2)
colnames(merge_df12)

#### FILTERING DATA ####

# Selecionando as linhas de uma região específica
selected_region <- "AMRO"  
filtered_data <- subset(merge_df12, WHO_REGION == selected_region)

filtered_data <- subset(
  filtered_data,
  !is.na(Deaths_newly_reported_in_last_7_days) & 
    !is.na(Cases_newly_reported_in_last_7_days) & 
    Cases_newly_reported_in_last_7_days > 0
)

# Nova coluna de classificação da análise
# ALTA = taxa de morte / taxa de novos casos > 10%
# MOREDADO = taxa de morte / taxa de novos casos < 10% & > 3%
# BAIXA = taxa de morte / taxa de novos casos < 3% 
filtered_data$Category <- with(
  filtered_data, 
  ifelse(
    Deaths_newly_reported_in_last_7_days / Cases_newly_reported_in_last_7_days >= 0.1, "alta",
    ifelse(
      Deaths_newly_reported_in_last_7_days / Cases_newly_reported_in_last_7_days >= 0.03, "moderada",
      "baixa"
    )
  )
)


#### PLOT ####
#### Cria o gráfico especificando o marcador em cada ponto ao passar o mouse em cima
plot <- plot_ly(
  data = filtered_data,
  x = ~Cases_newly_reported_in_last_7_days,
  y = ~Deaths_newly_reported_in_last_7_days,
  type = 'scatter',
  mode = 'markers',
  color = ~Category,
  text = ~paste("Country: ", COUNTRY, 
                "<br>Cases (last 7 days): ", Cases_newly_reported_in_last_7_days, 
                "<br>Deaths (last 7 days): ", Deaths_newly_reported_in_last_7_days,
                "<br>Mortalidade: ", round((Deaths_newly_reported_in_last_7_days / Cases_newly_reported_in_last_7_days) * 100, 1), "%"
                ),
  marker = list(size = 10),
  hoverinfo = "text"
)

plot <- layout(
  plot,
  title = paste("COVID-19 Severity by Country -", selected_region),
  xaxis = list(title = "Cases (last 7 days)", zeroline = FALSE),
  yaxis = list(title = "Deaths (last 7 days)", zeroline = FALSE),
  legend = list(title = list(text = "Severity Category"), orientation = "h", x = 0, y = -0.2)
)

plot