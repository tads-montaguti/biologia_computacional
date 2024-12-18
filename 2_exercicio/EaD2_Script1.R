

################################################################################
### Combina os datasets
################################################################################

#--- Verifica compatibilidade entre possiveis "chaves" para alinhamento
sum(data1_df$COUNTRY %in% data2_df$Name, na.rm = TRUE)
# [1] 225
sum(data1_df$WHO_REGION%in%data2_df$WHO_Region)
# [1] 0

#-------------------------------------------------------------------------------
#--- Executa alinhamento com a melhor 'chave' disponível

#--- Verifica se há nomes duplicados
any(duplicated(data1_df$COUNTRY))
# [1] FALSE
any(duplicated(data2_df$Name))
# [1] FALSE

#--- Nomei linhas com as chaves de identificação de cada dataset
rownames(data1_df) <- data1_df$COUNTRY
rownames(data2_df) <- data2_df$Name

#--- Gere um vetor de nomes comuns entre os dois datasets
country_names <- intersect(rownames(data1_df), rownames(data2_df))

#--- Alinha os dois datasets usando o vetor "country_names"
data1_df <- data1_df[country_names,]
data2_df <- data2_df[country_names,]
all(rownames(data1_df)==rownames(data2_df))
# [1] TRUE

#--- Verifica se há alguma duplicidade entre nomes de colunas
sum(colnames(data1_df) %in% colnames(data2_df))
# [1] 0

#-------------------------------------------------------------------------------
#--- Combina datasets alinhados, e salva no formato '.RData' 
#--- para análises subsequentes
merge_data12_df <- cbind(data1_df, data2_df)
save(merge_data12_df, file = "merge_data12_df.RData")
