setwd(dirname(rstudioapi::getSourceEditorContext()$path))

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
save(merge_df12, file = "merge_df12.RData")