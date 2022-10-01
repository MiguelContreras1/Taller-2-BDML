rm(list = ls()) # limpiar
install.packages("pacman") # instalar pacman
install.packages("xtable")
library(pacman) # llamo 
p_load(tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr,caret,fastDummies,glmnet,MLmetrics,modelsummary,gamlr,class,AER,tidymodels,themis, stargazer, xtable) # cargo
library(tidyverse)
library(xtable)
library(knitr)
library(dplyr)
library(kableExtra)
library(here)
library(jtools)
library(ggplot2)
library(ggstance)
library(broom)
library(broom.mixed)
library(skimr)
library(lattice)
library(caret)
library(fastDummies)
library(glmnet)
library(MLmetrics)
library(modelsummary)
library(gamlr)
library(class)
library(AER)
library(tidymodels)
library(themis)
library(stargazer)

# cargar bases 
test_hogares <-readRDS(here("C:/Users/MIGUEL  CONTRERAS/Desktop/Taller-2-BDML/data/test_hogares.Rds"))
test_personas <-readRDS(here("C:/Users/MIGUEL  CONTRERAS/Desktop/Taller-2-BDML/data/test_personas.Rds"))
train_hogares <-readRDS(here("C:/Users/MIGUEL  CONTRERAS/Desktop/Taller-2-BDML/data/train_hogares.Rds"))
train_personas <-readRDS(here("C:/Users/MIGUEL  CONTRERAS/Desktop/Taller-2-BDML/data/train_personas.Rds"))

# ver lo que se cargó
View(test_hogares)
View(test_personas)
View(train_hogares)
View(train_personas)

# reviso las bases
head(test_hogares)
head(test_personas)
head(train_hogares)
head(train_personas)
colnames(test_hogares)
colnames(test_personas)
colnames(train_hogares)
colnames(train_personas)
skim(test_hogares)
skim(test_personas)
skim(train_hogares)
skim(train_personas)

# Para unir: crear una variable que sea la suma de los ingresos de los individuos en el hogar a partir de la base de personas.

require("here")
require("tidyverse")

# Supongamos que quiero crear una variable que sea la suma de los ingresos de los individuos en el hogar a partir de la base de personas. Entonces:
sum_ingresos<-train_personas %>% group_by(id) %>% summarize(Ingtot_hogar=sum(Ingtot,na.rm = TRUE)) 
summary(sum_ingresos)
# tengo entonces una base con id y la variable que acabo de crear Ingtot_hogar.

# Unir bases
# Puedo entonces unirla a la base de hogares. Para ello voy a usar la función left_join() de dplyr.
train_hogares<-left_join(train_hogares,sum_ingresos)
colnames(train_hogares)

# Tengo ahora una columna extra que es Ingtot_hogar
head(train_hogares[c("id","Ingtotug","Ingtot_hogar")])

# Cálculo de Pobreza
# Según la base del DANE un hogar es clasificado pobre si el “Ingreso percápita de la unidad de gasto con imputación de arriendo a propietarios y usufructuarios” es menor a la Linea de pobreza que le corresponde al hogar.
table(train_hogares$Pobre) 
# no pobre 131936 y pobre 33024 

# Para testear si esto es cierto comparemos la variable Pobre incluida en la base con una creada por nosotros siguiendo el enfoque del DANE.
train_hogares<- train_hogares %>% mutate(Pobre_hand=ifelse(Ingpcug<Lp,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand)

# PREPARACIÓN DE LAS BASES DE DATOS

library(haven)

# variables que se van a utilizar:

# Pobre
# Clase
# P5010 - cantidad de cuartos de dormir (sirve para sacar cuantos duermen por cuarto)
# P5090 – vivienda propia
# Nper = número de personas
# Npersug = número de personas por unidad de gasto
# Intotug (hay que hacer un logintotug para segundo punto)
# Ingtotugarr
# Ingpcug
# Lp
# P6020 – sexo 
# P6040 – edad
# P6050 - jefe hogar
# P6100 - regimen salud
# P6210 – nivel educativo
# P6430 - posición en trabajo
# P6920 - cotiza pensión
# Oc – ocupado 

# dejar en las bases solamente las variables que voy a usar

test_hogares <- select(test_hogares, id, Clase, P5010, P5090, Nper, Npersug, Lp)
test_personas <- select(test_personas, id, Clase, P6020, P6040, P6050, P6100, P6210, P6430, P6920, Oc)
train_hogares <- select(train_hogares, id, Clase, P5010, P5090, Nper, Npersug, Ingtotug, Ingtotugarr, Ingpcug, Lp, Pobre, Ingtot_hogar)
train_personas <- select(train_personas, id, Clase, P6020, P6040, P6050, P6100, P6210, P6430, P6920, Oc, Ingtot)

# se crean las variables 

personas_h = train_hogares$Nper/train_hogares$P5010 # personas por habitación
train_hogares <- cbind(train_hogares, personas_h)

mujer_jh <- as.data.frame(ifelse((train_personas$P6020==1 & train_personas$P6050==1),1,0))
train_personas <- cbind(train_personas, mujer_jh)
mujer_jh <- train_personas %>% group_by(id) %>% summarize(mujer_jh=sum(mujer_jh,na.rm = TRUE)) 

edad_jh <- (ifelse((train_personas$P6050==1),train_personas$P6040,0))
train_personas<- cbind(train_personas, edad_jh)
edad_jh <-train_personas %>% group_by(id) %>% summarize(edad_jh=sum(edad_jh,na.rm = TRUE)) 

edu_jh <- (ifelse((train_personas$P6050==1),train_personas$P6210,0))
train_personas<- cbind(train_personas, edu_jh )
edu_jh  <-train_personas %>% group_by(id) %>% summarize(edu_jh = sum(edu_jh ,na.rm = TRUE)) 

salud_jh <- (ifelse((train_personas$P6050==1),train_personas$P6100,0))
train_personas <- cbind(train_personas, salud_jh)
salud_jh <- train_personas %>% group_by(id) %>% summarize(salud_jh=sum(salud_jh,na.rm = TRUE)) 

trabajo_ocu_jh <- (ifelse((train_personas$P6050==1),train_personas$P6430,0))
train_personas<- cbind(train_personas, trabajo_ocu_jh)
trabajo_ocu_jh <-train_personas %>% group_by(id) %>% summarize(trabajo_ocu_jh=sum(trabajo_ocu_jh,na.rm = TRUE)) 

pension_jh <- (ifelse((train_personas$P6050==1),train_personas$P6920,0))
train_personas<- cbind(train_personas, pension_jh)
pension_jh <-train_personas %>% group_by(id) %>% summarize(pension_jh =sum(pension_jh ,na.rm = TRUE)) 

ocu_jh <- (ifelse((train_personas$P6050==1),train_personas$Oc,0))
train_personas<- cbind(train_personas, ocu_jh)
ocu_jh <-train_personas %>% group_by(id) %>% summarize(ocu_jh =sum(ocu_jh ,na.rm = TRUE)) 

# ahora se pegan las variables creadas a la base train_hogares
library(dplyr)

train_hogares<-left_join(train_hogares, mujer_jh)
train_hogares<-left_join(train_hogares, edad_jh)
train_hogares<-left_join(train_hogares, edu_jh)
train_hogares<-left_join(train_hogares, salud_jh)
train_hogares<-left_join(train_hogares, trabajo_ocu_jh)
train_hogares<-left_join(train_hogares, pension_jh)
train_hogares<-left_join(train_hogares, ocu_jh)

# ya me quedaron todas las variables en train hogares
# ahora creo base train_hogares2 (se trabajará sobre esta)

train_hogares2 <- train_hogares

# para las regresiones voy as trabajar con las variables: Pobre ~ Clase + personas_h + P5090 + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + trabajo_ocu_jh + pension_jh + ocu_jh
# se mira cuáles variables son factor y se convierten

View(train_hogares2)

# en base train_hogares2 las variables factor son: Clase, P5090, Pobre, mujer_jh, edu_jh, salud_jh, trabajo_ocu_jh, pension_jh, ocu_jh
# de ahí en adelante se trabaja con la base train hogares 2

train_hogares2 <- train_hogares2 %>%
  mutate_at(.vars = c("Clase", "P5090", "Pobre", "mujer_jh", "edu_jh", "trabajo_ocu_jh", "pension_jh", "ocu_jh", "salud_jh"),.funs = factor)

train_hogares2 <- train_hogares2 %>%
  mutate_at(.vars = c("salud_jh"),.funs = factor)

###############

# se siembra semilla y se parte la base train hogares 2 con nrow, en train y test

set.seed(1010) 
train_hogares2 <- train_hogares2 %>%
  mutate(holdout= as.logical(1:nrow(train_hogares2) %in%
                               sample(nrow(train_hogares2), nrow(train_hogares2)*.2))
  )
test<-train_hogares2[train_hogares2$holdout==T,] 
train<-train_hogares2[train_hogares2$holdout==F,] 

summary(test)
summary(train)

prop.table(table(train$Pobre))

# ver si hay missing values en las variables que voy a usar para los modelos:

sum(is.na(train$Pobre)) # 0 missing 
sum(is.na(train$Clase)) # 0 missing 
sum(is.na(train$personas_h)) # 0 missing 
sum(is.na(train$P5090)) # 0 missing 
sum(is.na(train$Npersug)) # 0 missing 
sum(is.na(train$Nper)) # 0 missing 
sum(is.na(train$mujer_jh)) # 0 missing 
sum(is.na(train$edad_jh)) # 0 missing 
sum(is.na(train$salud_jh)) # 0 missing 
sum(is.na(train$edu_jh)) # 0 missing 
sum(is.na(train$trabajo_ocu_jh)) # 0 missing 
sum(is.na(train$pension_jh)) # 0 missing 
sum(is.na(train$ocu_jh)) # 0 missing 

# Tablas de algunas de las variables que elegí

install.packages("gtsummary")
require ("gtsummary") 
require("haven")
train <- zap_labels(train)

train %>%
  select(Npersug, Pobre) %>%
  tbl_summary(by=Pobre) %>%
  add_overall() %>%
  add_n()

train %>%
  select(P5090, Pobre) %>%
  tbl_summary(by=Pobre) %>%
  add_overall() %>%
  add_n()

train %>%
  select(Clase, Pobre) %>%
  tbl_summary(by=Pobre) %>%
  add_overall() %>%
  add_n()

## armar graficas de estadisticas en base train hogares

box_plot <- ggplot(data=train , mapping = aes(as.factor(Pobre) , Ingtotug)) + 
  geom_boxplot()

box_plot <- box_plot +
  scale_fill_grey() + theme_classic()+
  labs(x= "Pobres", y ="ingreso total unidad de gasto") 

train_hogares2$log_ingh <- log(train_hogares2$Ingtotug+1)

box_plot2 <- ggplot(data=train , mapping = aes(as.factor(Pobre) , log_ingh)) + 
  geom_boxplot()

box_plot2 <- box_plot2 +
  scale_fill_grey() + theme_classic()+
  labs(x= "Pobres", y =" log ingreso total unidad de gasto")

pobre_bar <- ggplot(data = train, aes(x = Pobre, fill=Pobre)) +
  geom_bar() +
  labs (subtitle="base train",
        x= "Pobre = 1", y = "número de pobres")

# ver gráficas estadisticas descriptivas
box_plot
box_plot2
pobre_bar


###############

# MODELOS DE CLASIFICACIÓN

library(tidyverse)

## modelos
modelo1 <- as.formula("Pobre ~ personas_h + P5090 + mujer_jh + edad_jh + salud_jh + edu_jh + trabajo_ocu_jh + pension_jh + ocu_jh")
modelo2 <- as.formula("Pobre ~ Clase + personas_h + P5090 + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + pension_jh + ocu_jh")
modelo3 <- as.formula("Pobre ~ Clase + personas_h + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh")
modelo4 <- as.formula("Pobre ~ Clase + personas_h + P5090 + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh")


library(stargazer)

# LOGIT

## estimación logit
set.seed(1010) # sembrar semilla
logit1 <- glm(modelo1 , family=binomial(link="logit") , data=train)
logit1
summary(logit1)
stargazer(logit1, type="text")
tidy(logit1)

logit2 <- glm(modelo2 , family=binomial(link="logit") , data=train)
logit2
summary(logit2)
stargazer(logit2, type="text")
tidy(logit2)

logit3 <- glm(modelo3 , family=binomial(link="logit") , data=train)
logit3
summary(logit3)
stargazer(logit3, type="text")
tidy(logit3)

logit4 <- glm(modelo4 , family=binomial(link="logit") , data=train)
logit4
summary(logit4)
stargazer(logit4, type="text")
tidy(logit4)

# PROBIT

# estimación probit

probit1 <- glm(modelo3 , family=binomial(link="probit") , data=train)
probit1
summary(probit1)
stargazer(probit1, type="text")
tidy(probit1)

probit2 <- glm(modelo4 , family=binomial(link="probit") , data=train)
probit2
summary(probit2)
stargazer(probit2, type="text")
tidy(probit2)

stargazer(logit1, logit2, logit3, logit4, probit1, probit2, type = "text")

## ratio de los coeficientes de los 2 mejores modelos
logit4$coefficients / probit2$coefficients

prop.table(table(train$Pobre))

# predicction 

library(dplyr)
library("gamlr")

test$pred_log1 <- predict(logit1, newdata=test, type="response")
test$pred_log2 <- predict(logit2, newdata=test, type="response")
test$pred_log3 <- predict(logit3, newdata=test, type="response")
test$pred_log4 <- predict(logit4, newdata=test, type="response")
test$pred_pro1 <- predict(probit1, newdata=test, type="response")
test$pred_pro2 <- predict(probit2, newdata=test, type="response")

head(test)

ggplot(data=test , mapping = aes(Pobre, pred_log1)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=test , mapping = aes(Pobre, pred_log2)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=test , mapping = aes(Pobre, pred_log3)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=test , mapping = aes(Pobre, pred_log4)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=test , mapping = aes(Pobre, pred_pro1)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()

ggplot(data=test , mapping = aes(Pobre, pred_pro2)) + 
  geom_boxplot(aes(fill=Pobre)) + theme_test()


test <- test %>% 
  mutate(p_logit_1 = ifelse(pred_log1 < 0.23,0,1) %>% 
           factor(.,levels=c(0,1),labels=c("No_pobre","Pobre")))

###

## definir la regla (0.7)

rule=0.5
test$phat1 = ifelse(test$pred_log1>rule,1,0)
test$phat2 = ifelse(test$pred_log2>rule,1,0)
test$phat3 = ifelse(test$pred_log3>rule,1,0)
test$phat4 = ifelse(test$pred_log4>rule,1,0)
test$phat5 = ifelse(test$pred_pro1>rule,1,0)
test$phat6 = ifelse(test$pred_pro2>rule,1,0)

head(test)
View(test)

## Clasificación (sale matriz de confusión)

## logit
cm_log1 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$phat1) , mode="sens_spec" , positive="1")
cm_log1

cm_log2 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$phat2) , mode="sens_spec" , positive="1")
cm_log2

cm_log3 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$phat3) , mode="sens_spec" , positive="1")
cm_log3

cm_log4 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$phat4) , mode="sens_spec" , positive="1")
cm_log4

## probit
cm_pro1 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$phat5) , mode="sens_spec" , positive="1")
cm_pro1

cm_pro2 = confusionMatrix(data=factor(test$Pobre) , reference=factor(test$phat6) , mode="sens_spec" , positive="1")
cm_pro2

cm1 <- cm_log1$table # matriz de confusión del modelo logit 1
cm2 <- cm_log2$table # matriz de confusión del modelo logit 2
cm3 <- cm_log3$table # matriz de confusión del modelo logit 3
cm4 <- cm_log4$table # matriz de confusión del modelo logit 4
cm5 <- cm_pro1$table # matriz de confusión del modelo probit 1
cm6 <- cm_pro2$table # matriz de confusión del modelo probit 2

# métricas por modelo
cm1_metri <- cm_log1$byClass
cm2_metri <- cm_log2$byClass
cm3_metri <- cm_log3$byClass
cm4_metri <- cm_log4$byClass
cm5_metri <- cm_pro1$byClass
cm6_metri <- cm_pro1$byClass

# métricas agruipadas
gru_metri <-  rbind(cm1_metri, cm2_metri, cm3_metri, cm4_metri, cm5_metri, cm6_metri)

require(xtable)
xtable(gru_metri)

####
################
######################

require(pacman)
library(rio)
library(modelsummary)
library(gamlr)
library(class)
library(ggplot2)
library(pROC)

install.packages("plotROC")
library(plotROC)

## Misclassification Rates
## Curva de ROC

library("ROCR")
pred1 <- prediction(test$phat1, test$Pobre) ## paste predictions
roc1 <- performance(pred1,"tpr","fpr")  ## get ROC object 
plot(roc1, main = "ROC curve 1", col="blue") ## plot object
abline(a = 0, b = 1)
auc1 <- performance(pred1, measure = "auc")
auc1@y.values[[1]]


pred2 <- prediction(test$phat2, test$Pobre) ## paste predictions
roc2 <- performance(pred2,"tpr","fpr")  ## get ROC object 
plot(roc2, main = "ROC curve 2", col="red") ## plot object
abline(a = 0, b = 1)
auc2 <- performance(pred2, measure = "auc")
auc2@y.values[[1]]

pred3 <- prediction(test$phat3, test$Pobre) ## paste predictions
roc3 <- performance(pred3,"tpr","fpr")  ## get ROC object 
plot(roc3, main = "ROC curve 3", col="green") ## plot object
abline(a = 0, b = 1)
auc3 <- performance(pred3, measure = "auc")
auc3@y.values[[1]]

pred4 <- prediction(test$phat4, test$Pobre) ## paste predictions
roc4 <- performance(pred4,"tpr","fpr")  ## get ROC object 
plot(roc4, main = "ROC curve 4", col="purple") ## plot object
abline(a = 0, b = 1)
auc4 <- performance(pred4, measure = "auc")
auc4@y.values[[1]]

pred5 <- prediction(test$phat5, test$Pobre) ## paste predictions
roc5 <- performance(pred5,"tpr","fpr")  ## get ROC object 
plot(roc5, main = "ROC curve 5", col="brown") ## plot object
abline(a = 0, b = 1)
auc5 <- performance(pred5, measure = "auc")
auc5@y.values[[1]]

pred6 <- prediction(test$phat6, test$Pobre) ## paste predictions
roc6 <- performance(pred6,"tpr","fpr")  ## get ROC object 
plot(roc6, main = "ROC curve 6", col="black") ## plot object
abline(a = 0, b = 1)
auc6 <- performance(pred6, measure = "auc")
auc6@y.values[[1]]

# se seleccionó el modelo logit3

require(caret)

# Para desbalance de clases - evitar overfitting

require("tidyverse")
require("here")

# Partir base de datos train_hogares2

set.seed(1010)
# training
split1 <- createDataPartition(train_hogares2$Pobre, p = .7)[[1]]
length(split1)
other <- train_hogares2[-split1,]
training <- train_hogares2[ split1,]
#  ahora se crea evaluation y testing
set.seed(1010)
split2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]
evaluation <- other[ split2,]
testing <- other[-split2,]

dim(training)
dim(evaluation)
dim(testing)

summary(training$Pobre)

# se seleccionó el modelo logit 1

########################
############################
################################


################################
##########################
# MODELOS DE REGRESIÓN

library("dplyr") #for data wrangling
library("caret") #ML
set.seed(1010) #set the seed for replication purposes
str(train_hogares2) #conmpact display

ols <- train(Ingtotug ~ Clase + personas_h + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh, # model to fit
             data = train_hogares2,
             trControl = trainControl(method = "cv", number = 10),
             # Method: crossvalidation, 10 folds
             method = "lm")
# specifying regression model

ols

lambda <- 10^seq(-2, 3, length = 100)
lasso <- train(
  Ingtotug ~ Clase + personas_h + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh, data = train_hogares2, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda=lambda), preProcess = c("center", "scale")
)

lasso

ridge <- train(
  Ingtotug ~ Clase + personas_h + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh, data = train_hogares2, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0,lambda=lambda), preProcess = c("center", "scale")
)
ridge

install.packages("leaps")
require("leaps")
best<-regsubsets(Ingtotug ~ Clase + personas_h + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh, method="exhaustive",data = train_hogares2)
summary(best)


forward <- train(Ingtotug ~ Clase + personas_h + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh, data = train_hogares2,
                 method = "leapForward",
                 trControl = trainControl(method = "cv", number = 10))
forward

summary(forward$finalModel)

backwards <- train(Ingtotug ~ Clase + personas_h + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh, data = train_hogares2,
                   method = "leapBackward",
                   trControl = trainControl(method = "cv", number = 10))
backwards


summary(backwards$finalModel)


# Ingtotug ~ personas_h + P5090 + mujer_jh + edad_jh + salud_jh + edu_jh + trabajo_ocu_jh + pension_jh + ocu_jh


library("dplyr") #for data wrangling
library("caret") #ML
set.seed(1010) #set the seed for replication purposes
str(train_hogares2) #conmpact display

ols1 <- train(Ingtotug ~ personas_h + P5090 + mujer_jh + edad_jh + salud_jh + edu_jh + trabajo_ocu_jh + pension_jh + ocu_jh, # model to fit
             data = train_hogares,
             trControl = trainControl(method = "cv", number = 10),
             # Method: crossvalidation, 10 folds
             method = "lm")

ols1


# lasso1
lambda <- 10^seq(-2, 3, length = 100)
lasso1 <- train(
  Ingtotug ~ personas_h + P5090 + mujer_jh + edad_jh + salud_jh + edu_jh + trabajo_ocu_jh + pension_jh + ocu_jh, data = train_hogares, method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda=lambda), preProcess = c("center", "scale")
)

lasso1

# ridge1

ridge1 <- train(
  Ingtotug ~ personas_h + P5090 + mujer_jh + edad_jh + salud_jh + edu_jh + trabajo_ocu_jh + pension_jh + ocu_jh, data = train_hogares ,method = "glmnet",
  trControl = trainControl("cv", number = 10),
  tuneGrid = expand.grid(alpha = 0,lambda=lambda), preProcess = c("center", "scale")
)
ridge1

install.packages("leaps")
require("leaps")
best<-regsubsets(Ingtotug ~ Clase + personas_h + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh, method="exhaustive",data = train_hogares2)
summary(best)


forward <- train(Ingtotug ~ Clase + personas_h + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh, data = train_hogares2,
                 method = "leapForward",
                 trControl = trainControl(method = "cv", number = 10))
forward

summary(forward$finalModel)

backwards <- train(Ingtotug ~ Clase + personas_h + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh, data = train_hogares2,
                   method = "leapBackward",
                   trControl = trainControl(method = "cv", number = 10))
backwards


summary(backwards$finalModel)




# summary de modelos
summary(ols)
summary(ols1)
summary(lasso)
summary(lasso1)
summary(ridge)
summary(ridge1)





####### aquí termina


## otras corridas


# se realizan diferentes boxplot de las variables aparentemente mas releventes 

boxplot(train_hogares2$Ingtotug,main = "BOXPLOT - Ingreso Total", xlab = "Ingreso total", col = "110")
boxplot(train_hogares2$Ingtotug ~ train_hogares2$mujer_jh, main ="jefe hogar mujer", 
        xlab = "Sexo jefe del hogar", ylab = "Ingreso total", col= "110")
boxplot(train_hogares2$Ingtotug ~ train_hogares2$Clase, main ="Clase", 
        xlab = "Clase", ylab = "Ingreso total", col= "110")
boxplot(train_hogares2$Ingtotug ~ train_hogares2$edu_jh, main ="jefe hogar educación", 
        xlab = "educacion", ylab = "Ingreso total", col= "110")

#se realiza la partición de la base 

set.seed(1010)
split1 <- createDataPartition(train_hogares2$Pobre, p = .7)[[1]]
length(split1) 
train_hogares2 <- na.omit(train_hogares2)
                          
other <- train_hogares2[-split1,]
trtrain <- train_hogares2[split1,]

split2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]

treval <- other[ split2,]
trtest <- other[-split2,]

###########

y_train <- log(trtrain[,"Ingtotug"])
X_train <- select(trtrain, -Ingtotug)
y_test <- log(trtest[,"Ingtotug"])
X_test <- select(trtest, -Ingtotug)

train2 <- cbind(y_train, X_train)
modelo_reg <- lm("Ingtot_hogar ~ Clase + personas_h + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh", data = train2)
summary(modelo_reg)

df_coeficientes_reg <- modelo_reg$coefficients %>% enframe(name = "predictor", value = "coeficiente")

df_coeficientes_reg %>% filter(predictor != "`(Intercept)`") %>%
  ggplot(aes(x = reorder(predictor, abs(coeficiente)), y = coeficiente)) + 
  geom_col(fill = "darkblue") + coord_flip() + theme_test() +
  labs(title = "Coeficientes del modelo de regresion", x = "Variables", y = "Coeficientes")

y_hat_in1 <- predict(modelo_reg, newdata = X_train)
y_hat_out1 <- predict(modelo_reg, newdata = X_test)


r2_in1 <- R2_Score(y_pred = exp(y_hat_in1), y_true = exp(y_train))
rmse_in1 <- RMSE(y_pred = exp(y_hat_in1), y_true = exp(y_train))

r2_out1 <- R2_Score(y_pred = exp(y_hat_out1), y_true = exp(y_test))
rmse_out1 <- RMSE(y_pred = exp(y_hat_out1), y_true = exp(y_test))

resultados <- data.frame(Modelo = "Regresion lineal", 
                         Muestra = "Dentro",
                         R2_Score = r2_in1, RMSE = rmse_in1) %>%
  rbind(data.frame(Modelo = "Regresion lineal", 
                   Muestra = "Fuera",
                   R2_Score = r2_out1, RMSE = rmse_out1))

y=X_train$Ingtot_hogar

x=model.matrix(Ingtot_hogar~.,X_train)
x=as.matrix(Ingtot_hogar~.,X_train)

X_train = select(X_train, c("Ingtot_hogar", "Clase", "personas_h", "Npersug",  "mujer_jh", "edad_jh", "salud_jh",  "edu_jh",  "ocu_jh")) 
X_train <- na.omit(X_train)

## Lasso
# Para obtener un ajuste con regularizaci?n Lasso se indica argumento alpha = 1.
# Si no se especifica valor de lambda, se selecciona un rango autom?tico.
modelo_lasso <- glmnet(
  x = X_train,
  y = y_train,
  alpha = 1,
  nlambda = 300,
  standardize = F, family = gaussian()
)




############

library(pacman)
p_load(tidyverse, rio , skimr , fastDummies, caret, glmnet, MLmetrics , janitor)

### estimación
model_lm1 <- lm(Ingtotug ~ Clase + personas_h + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh, data = trtrain)
model_lm2 <- lm(Ingtotug ~ Clase + personas_h + P5090 + Npersug + mujer_jh + edad_jh + salud_jh + edu_jh + ocu_jh, data = trtrain)

model_lm1
model_lm2

stargazer(model_lm1,type="text")
stargazer(model_lm2,type="text")

summary(model_lm1)
summary(model_lm2)


df_coeficientes_reg <- model_lm1$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientes_reg %>%
  filter(predictor != "`(Intercept)`") %>%
  ggplot(aes(x = reorder(predictor, abs(coeficiente)), 
             y = coeficiente)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Coeficientes del modelo lm1", 
       x = "Variables",
       y = "Coeficientes") +
  theme_bw()


df_coeficientes_reg <- model_lm2$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

df_coeficientes_reg %>%
  filter(predictor != "`(Intercept)`") %>%
  ggplot(aes(x = reorder(predictor, abs(coeficiente)), 
             y = coeficiente)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Coeficientes del modelo lm2", 
       x = "Variables",
       y = "Coeficientes") +
  theme_bw()


##
train_hogares2 <- train_hogares
train_hogares2 <- select(train_hogares2, "Ingtot_hogar", "Clase", "personas_h", "Npersug",  "mujer_jh", "edad_jh", "salud_jh",  "edu_jh",  "ocu_jh")
train_hogares2 <- na.omit(train_hogares2)



