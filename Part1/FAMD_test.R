library('FactoMineR')
## Graphical interface
library(Factoshiny)

data <- read.csv("train_cleandata.csv")
productivity_state=as.factor(as.numeric(data$actual_productivity>=data$targeted_productivity))
data=cbind(data, "productivity_state"=productivity_state)

categorical_vars <- data[, c("date","quarter", "day","team","department","productivity_state")]
head(categorical_vars)
categorical_vars <- data.frame(lapply(categorical_vars, as.factor))
numeric_vars <- data[,c("targeted_productivity","smv","wip",
                        "over_time","incentive","idle_time",
                        "idle_men","no_of_style_change",
                        "no_of_workers")]

processed_data <- cbind(categorical_vars, numeric_vars)
head(processed_data)
ncol(processed_data)
res <- FAMD(processed_data)
summary(res)
plot(res, habillage=2, label = FALSE)

plot(res, habillage = 2, choix = "ind") + theme_void()  # Removes all text

res <- Factoshiny(processed_data)

# https://medium.com/@jumbongjunior1999/mixed-data-correlation-analysis-using-factorial-analysis-in-r-famd-a1c440a91a6e


