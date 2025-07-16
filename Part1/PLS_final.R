library(mdatools)

data=read.csv("train_cleandata.csv")

data=data[,c("targeted_productivity","smv","wip",
             "over_time","incentive","idle_time",
             "idle_men","no_of_style_change",
             "no_of_workers","actual_productivity")]

Xc=data[,-10]
yc=data[,10]
#Fitting PLR model
Model1<-pls(Xc, yc, scale = TRUE, center=TRUE, cv=1)
#Model summary
summary(Model1)

x_var_exp <- round(Model1$calres$xdecomp$expvar,3)
y_var_exp <-round( Model1$calres$ydecomp$expvar,3)

plot(Model1)

plotXScores(Model1,show.labels = FALSE)
plotXYLoadings(Model1,show.labels =TRUE)
