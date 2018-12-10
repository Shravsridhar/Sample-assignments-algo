library(arules)
cosmetics.df<-read.csv("Cosmetics.csv")

# removing first column and converting to matrix 
cos_data = as.matrix(cosmetics.df[, -1])
head(cos_data)

# Converting binary incidence matrix into a transactions database 
cos_trans <- as(cos_data, "transactions")
inspect(cos_trans)

# Getting rules
rules <- apriori(cos_trans, parameter = list(supp = 0.2,conf = 0.6, target= "rules"))
inspect(rules)

# Inspecting first 5 rules sorting by their lift ratio
inspect(head(sort(rules, by = "lift"), n = 5))

# Finding Redundant Rules
redundant <- which (colSums (is.subset (rules, rules)) > 1)

inspect(rules[2:3])
