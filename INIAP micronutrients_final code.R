library(dplyr)
library(tidyr)
library(glmnet)
library(ggplot2)

#### LOAD IN DATA ####
getwd()
setwd("/Users/TheDankness/Dropbox")
INIAP.data.prelim <- read.csv("Wade Margenot/INIAP, Ecuador/Micronutrients MS/Submission/INIAP micronutrients data_final.csv")
# note: variables labeled "S.x" are soil variables of nutrient X and variables labeled "L.x" are leaf variables of nutrient x.

## Add in derived variables and wrangle the data ##
INIAP.data.prelim$S.Inorg.N <- INIAP.data.prelim$S.NO3.N + INIAP.data.prelim$S.NH4.N
INIAP.data.prelim$S.CaMg <- INIAP.data.prelim$S.Ca/INIAP.data.prelim$S.Mg
INIAP.data.prelim$L.MgCa <- INIAP.data.prelim$L.Mg/INIAP.data.prelim$L.Ca
INIAP.data.prelim$Input_trt <- as.factor(INIAP.data.prelim$Input_trt)
INIAP.data.prelim$Agroforestry_trt <- as.factor(INIAP.data.prelim$Agroforestry_trt)
str(INIAP.data.prelim)

## Rename levels to English naming system ##
levels(INIAP.data.prelim$Input_trt)[levels(INIAP.data.prelim$Input_trt)=="AC"] <- "HC"
levels(INIAP.data.prelim$Input_trt)[levels(INIAP.data.prelim$Input_trt)=="BO"] <- "LO"
levels(INIAP.data.prelim$Input_trt)[levels(INIAP.data.prelim$Input_trt)=="MC"] <- "LC"
levels(INIAP.data.prelim$Input_trt)[levels(INIAP.data.prelim$Input_trt)=="OI"] <- "HO"

levels(INIAP.data.prelim$Agroforestry_trt)[levels(INIAP.data.prelim$Agroforestry_trt)=="Forestal"] <- "Myroxylon"
levels(INIAP.data.prelim$Agroforestry_trt)[levels(INIAP.data.prelim$Agroforestry_trt)=="Frutal"] <- "Inga"
levels(INIAP.data.prelim$Agroforestry_trt)[levels(INIAP.data.prelim$Agroforestry_trt)=="Pleno sol"] <- "Full sun (control)"
levels(INIAP.data.prelim$Agroforestry_trt)[levels(INIAP.data.prelim$Agroforestry_trt)=="Servicio"] <- "Erythrina"

# order levels #
INIAP.data.prelim$Input_trt <- factor(INIAP.data.prelim$Input_trt, levels = c("LC", "HC", "LO", "HO"))

## Average subsamples by plot ##
INIAP.data <- as.data.frame(INIAP.data.prelim %>% dplyr::group_by(Input_trt, Agroforestry_trt, Block) %>% summarize_at(vars(CEC:L.MgCa), .funs=mean))


#### DATA ANALYSES ####
### What combination(s) of things predict yield (2019)? ###
lasso.data <- INIAP.data[,c(5:33)]

X.lasso <- as.matrix(lasso.data[,c(1:28)])
Y.lasso <- as.matrix(lasso.data[,29])
Y.lasso2 <- as.data.frame(INIAP.data[,c(1,2,33)])

set.seed(1234)
lasso.model <- glmnet(X.lasso, Y.lasso, alpha=1, family="gaussian")
cv.lasso.model<-cv.glmnet(X.lasso, Y.lasso, nfolds=10, type.measure='mse', alpha=1, family="gaussian", nlambda=1000)
summary(cv.lasso.model$glmnet.fit)
cv.lasso.model$lambda.min
plot(cv.lasso.model)

coeff.pred <- predict(cv.lasso.model, s="lambda.min", type="coefficient")
round(coeff.pred, digits=2)

Y.pred <- predict(cv.lasso.model, newx=X.lasso, s="lambda.min", type="response")
round(sqrt(mean((Y.pred - Y.lasso)^2)),2) # calculate RMSE
round(1-((sum((Y.pred - Y.lasso) ^ 2))/(sum((Y.lasso - mean(Y.lasso)) ^ 2))),2) # R-squared

# Model fits excluding "high" yields (>1.0) #
Y.pred.low <- as.data.frame(cbind(Y.pred, Y.lasso))
Y.pred.low <- subset(Y.pred.low, V2 < 1.0)
round(sqrt(mean((Y.pred.low$`1` - Y.pred.low$V2)^2)),2) # calculate RMSE


### Figure 2: Predicted vs. actual plot ###
model.data <- as.data.frame(cbind(Y.lasso2, Y.pred))
colnames(model.data)[which(names(model.data)=="Input_trt")] <- "Input"
colnames(model.data)[which(names(model.data)=="Agroforestry_trt")] <- "Shade"
colnames(model.data)[3]<-"Y.actual"
colnames(model.data)[4]<- "Y.pred"
levels(model.data$Shade)[levels(model.data$Shade)=="Monocrop"] <- "full sun (control)"
levels(model.data$Shade)[levels(model.data$Shade)=="Timber"] <- "Myroxylon"
levels(model.data$Shade)[levels(model.data$Shade)=="Fruit"] <- "Inga"
levels(model.data$Shade)[levels(model.data$Shade)=="N-fixing"] <- "Erythrina"

RMSE.txt <- expression("RMSE = 0.23 kg"^{-1}~"tree")
R2.txt <- expression("R^2 == 0.74")
fit.plot <- ggplot(model.data, aes(x=Y.actual, y = Y.pred, color=Agroforestry_trt)) + geom_point(aes(shape=Input, color=Shade), size=3) + geom_abline(intercept = 0, slope = 1, linetype=2, size=1) + scale_color_manual(values=c("#E69F00", "#56B4E9", "#D55E00",  "#009E73")) + scale_shape_manual(values=c(15,17:19)) + theme_bw() + labs(x=expression(Actual~Yield~" (kg"^{-1}~"tree)"), y=expression(Predicted~Yield~" (kg"^{-1}~"tree)"), shape="Input", color="Shade") + annotate("text", x=1.50, y=0.6, label=RMSE.txt, parse=TRUE) + annotate("text", x=1.50, y=0.5, label=R2.txt, parse=TRUE)
fit.plot
ggsave("Lasso.fit.pdf", width=6, height=4)

### Random in-text statements aka "data not shown" in order of appearance ###
summary(INIAP.data$L.Mg)
summary(INIAP.data$L.Ca)
summary(INIAP.data$S.Ca)
summary(INIAP.data$S.Mg) 
cor.test(INIAP.data$S.CaMg, INIAP.data$Y.2019)

## Is soil Ca lower than background? ##
wilcox.test(INIAP.data$S.Ca, alternative = "less", mu=1851, exact=TRUE) # Note: weighted average of M3-Ca in surface = 1851 mg/kg

#### CHECK WHICH PACKAGES ARE NECESSARY ####
library(NCmisc)
NCmisc::list.functions.in.file("Wade Margenot/INIAP, Ecuador/Micronutrients MS/Submission/INIAP micronutrients_final code.R")
