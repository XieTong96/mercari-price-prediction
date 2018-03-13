#read in the data
install.packages("data.table")
library("data.table")
data <- as.data.frame(fread("train.tsv"))

boxplot(data$price)