library('FactoMineR')
library(Factoshiny)

data=read.csv("train_cleandata.csv")

numeric_vars <- data[,c("targeted_productivity","smv","wip",
                        "over_time","incentive","idle_time",
                        "idle_men","no_of_style_change",
                        "no_of_workers")]
for(i in 1:ncol(numeric_vars)){
  print(shapiro.test(rnorm(1000)))
}
productivity_state=as.factor(as.numeric(data$actual_productivity>=data$targeted_productivity))
categorical_vars <- data[, c("date","quarter", "day","team","department")]  # Replace with actual categorical columns
categorical_vars=cbind(categorical_vars, productivity_state)
categorical_vars <- data.frame(lapply(categorical_vars, as.factor))
comb_data=cbind(numeric_vars, categorical_vars)
head(comb_data)

res <- Factoshiny(processed_data)


###### sig check
library(psych)
# Test all numerical pairs (example for a subset)
pairs_to_test <- list(
  c("targeted_productivity", "incentive"),
  c("idle_time", "over_time"),
  c("smv", "wip")
)


results_num_num <- list()
for (pair in pairs_to_test) {
  test_result <- cor.test(comb_data[[pair[1]][1]], comb_data[[pair[2]]], method = "spearman")
  results_num_num[[paste(pair[1], "vs", pair[2])]] <- test_result
}

# Print results
print(results_num_num)

table(comb_data$productivity_state[comb_data$department=="finishing"])
table(comb_data$productivity_state[comb_data$department=="sewing"])

#######
cor.test(data$targeted_productivity, data$incentive, method = "kendall") # ok

cor.test(data$targeted_productivity, data$wip, method = "kendall") # no

cor.test(data$targeted_productivity, data$smv, method = "kendall") # no...a little

cor.test(data$targeted_productivity, data$no_of_workers, method = "kendall") # no

cor.test(data$targeted_productivity, data$over_time, method = "kendall") # no

cor.test(data$targeted_productivity, data$no_of_style_change, method = "kendall") # ok_neg

cor.test(data$targeted_productivity, data$idle_men, method = "kendall") # no

cor.test(data$targeted_productivity, data$idle_time, method = "kendall") # no

######## one group
cor.test(data$idle_time, data$idle_men, method = "kendall") # yes

cor.test(data$no_of_style_change, data$idle_men, method = "kendall") # yes

cor.test(data$no_of_style_change, data$idle_time, method = "kendall") # yes

###### one group
cor.test(data$no_of_workers, data$smv, method = "kendall") # yes
cor.test(data$no_of_workers, data$over_time, method = "kendall") # yes
cor.test(data$over_time, data$smv, method = "kendall") # yes

########
cor.test(data$targeted_productivity, data$no_of_style_change, method = "kendall") # ok_neg
cor.test(data$targeted_productivity, data$incentive, method = "kendall") # ok
cor.test(data$no_of_style_change, data$incentive, method = "kendall") # no

####
chisq.test(data$date,data$quarter)
fisher.test(comb_data$date, comb_data$quarter, simulate.p.value=TRUE)
chisq.test(data$date,data$day)
fisher.test(comb_data$date, comb_data$day, simulate.p.value=TRUE)

####
# https://medium.com/@jumbongjunior1999/mixed-data-correlation-analysis-using-factorial-analysis-in-r-famd-a1c440a91a6e
# https://youtu.be/KtRLF6rAkyo?si=DKTRDiXod_jfXMQV <-cluster analysis video
# https://www.ibm.com/docs/en/cognos-analytics/11.1.0?topic=terms-cramrs-v
# https://youtu.be/4XrgWmN9erg?si=PzI1iWad3Tfa9cyQ <-factominer video 
# https://cran.r-project.org/web/packages/FactoMineR/FactoMineR.pdf
# packages:-
## FactoMineR
## Factoshiny
## cluster :- silhouthe test
## ggplot

