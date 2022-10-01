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

# dejar solamente las variables que voy a usar
test_hogares <- select(test_hogares, id, Clase, P5090, Npersug, Lp)
test_personas <- select(test_personas, id, Clase, P6020, P6040, P6050, P6210, P6240)
train_hogares <- select(train_hogares, id, Clase, P5090, Npersug, Ingtotug, Ingtotugarr, Ingpcug, Lp, Pobre)
train_personas <- select(train_personas, id, Clase, P6020, P6040, P6050, P6210, P6240, Ingtot)

# reviso cómo quedaron las bases
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
# scrip de Ignacio 

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

# otra forma de hacerlo 
train_hogares<- train_hogares %>% mutate(Pobre_hand_2=ifelse(Ingtotugarr<Lp*Npersug,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand_2)

# variables que se van a utilizar:

# Pobre
# Intotug (es igual a Intot_hogar cuando se hace la mescla) # Pobre_hand
# Clase
# P5090 – vivienda propia
# Npersug
# P6020 – sexo 
# P6040 – edad
# P6050 - jefe hogar
# P6210 – nivel educativo
# P6240 – actividad que se ocupó (cambiar por oc)

# Tratamiento de las variables categóricas
# HOGARES
# Pobre dummy (0 no pobre, 1 pobre)
# Clase dummy (1 cabecera, 2 resto)
# P5090 discreta
# PERSONAS
# Clase dummy (1 cabecera, 2 resto)
# P6020 dummy (1 hombre, 2 mujer)
# P6050 discreta # 
# P6210 discreta
# P6240 discreta

# PREPARACIÓN DE LAS BASES DE DATOS

library(haven)

# convertir en factor las variables que lo requieren EN LAS 4 BASES

train_hogares$Pobre <- factor(train_hogares$Pobre, labels = c("No Pobre", "Pobre"))
train_hogares$Pobre_hand <- factor(train_hogares$Pobre_hand, labels = c("No Pobre", "Pobre"))
train_hogares$Pobre_hand_2 <- factor(train_hogares$Pobre_hand_2, labels = c("No Pobre", "Pobre"))
train_hogares$Clase <- factor(train_hogares$Clase, labels = c("Cabecera", "Resto"))
train_hogares$P5090 <- factor(train_hogares$P5090)

test_hogares$Clase <- factor(test_hogares$Clase, labels = c("Cabecera", "Resto"))
test_hogares$P5090 <- factor(test_hogares$P5090)

train_personas$Clase <- factor(train_personas$Clase, labels = c("Cabecera", "Resto"))
train_personas$P6020 <- factor(train_personas$P6020, labels = c("Hombre", "Mujer"))
train_personas$P6050 <- factor(train_personas$P6050)
train_personas$P6210 <- factor(train_personas$P6210)
train_personas$P6240 <- factor(train_personas$P6240)

test_personas$Clase <- factor(test_personas$Clase, labels = c("Cabecera", "Resto"))
test_personas$P6020 <- factor(test_personas$P6020, labels = c("Hombre", "Mujer"))
test_personas$P6050 <- factor(test_personas$P6050)
test_personas$P6210 <- factor(test_personas$P6210)
test_personas$P6240 <- factor(test_personas$P6240)

# tablas de algunas de las variables que elegí

#Tabla de Npersug vs Pobre, y Pobre vs P5090

install.packages("gtsummary")
require ("gtsummary") 
require("haven")
train_hogares <- zap_labels(train_hogares)

train_hogares %>%
  select(Npersug, Pobre) %>%
  tbl_summary(by=Pobre) %>%
  add_overall() %>%
  add_n()

train_hogares %>%
  select(P5090, Pobre) %>%
  tbl_summary(by=Pobre) %>%
  add_overall() %>%
  add_n()

train_hogares %>%
  select(Clase, Pobre) %>%
  tbl_summary(by=Pobre) %>%
  add_overall() %>%
  add_n()

# ver si hay missing values en las siguientes variables:
# Pobre
# Intotug (es igual a Intot_hogar cuando se hace la mescla)
# Clase
# P5090 – vivienda propia
# Npersug
# P6020 – sexo 
# P6040 – edad
# P6050 - jefe hogar
# P6210 – nivel educativo
# P6240 – actividad que se ocupó . buscar variable oc (train personas)

sum(is.na(train_hogares$Pobre)) # 0 missing 
sum(is.na(train_hogares$Clase)) # 0 missing 
sum(is.na(train_hogares$P5090)) # 0 missing 
sum(is.na(train_hogares$Npersug)) # 0 missing 

sum(is.na(test_hogares$Clase)) # 0 missing 
sum(is.na(test_hogares$P5090)) # 0 missing 
sum(is.na(test_hogares$Npersug)) # 0 missing 

sum(is.na(train_personas$Clase)) # 0 missing 
sum(is.na(train_personas$P6020)) # 0 missing 
sum(is.na(train_personas$P6040)) # 0 missing
sum(is.na(train_personas$P6050)) # 0 missing
sum(is.na(train_personas$P6210)) # 22689 missing 
sum(is.na(train_personas$Oc)) # 

sum(is.na(test_personas$Clase)) # 0 missing 
sum(is.na(test_personas$P6020)) # 0 missing 
sum(is.na(test_personas$P6040)) # 0 missing 
sum(is.na(test_personas$P6050)) # 0 missing 
sum(is.na(test_personas$P6210)) # 9238 missing 
sum(is.na(test_personas$Oc)) # 

summary(train_hogares)
stargazer(train_hogares)

# quitar missing values ** (PREGUNTAR A LUCAS) **:
# de la clase 6 - imputar con el valor más frecuente: df$estrato[is.na(df$estrato)] <- 1 
# como la mayoría son niños, el nivel educativo sería 1 y la actividad en que se ocupó sería 6

train_personas$P6210
train_personas$P6240

test_personas$P6210
test_personas$P6240

# creo que sería: 
train_personas$P6210[is.na(train_personas$P6210)] <- 1
train_personas$P6240[is.na(train_personas$P6240)] <- 6
test_personas$P6210[is.na(test_personas$P6210)] <- 1
test_personas$P6240[is.na(test_personas$P6240)] <- 6

# crear las variables que necesito ** (PREGUNTAR A LUCAS) **: 
# jh_edu (nivel educativo jefe de hogar - sale de variables P6050 (jefe de hogar convertir en 0 todos los que no sean 1), y P6020 (sexo que debía poner primero 1,0? no sé)
# jh_mujer (jefe de hogar mujer - sale de P6240 (actividad en que se ocupó, convertir en 0 todos los que no sean 1), y P6020 (sexo que debía poner primero 1,0? no sé)
# jh_ocu (jefe de hogar ocupado - sale de P6240 (actividad en que se ocupó, convertir en 0 todos los que no sean 1), y P6050 (jefe de hogar convertir en 0 todos los que no sean 1)
# ocu_ug (ocupados por unidad de gasto - no sé si se puede crear esa variable)

# creo que sería:
jh <- ifelse(train_personas$P6050==1,1,0)
ocu <- ifelse(train_personas$P6240==1,1,0) # no usar esta sino la oc de la base train personas

jh_edu <- "jh x P6050" (jh:P6050) # 
jh_mujer <- "jh x P6020" (jh:P6020) # mujer ya lo tengo como factor Mujer
jh_ocu <- "jh x ocu" (jh:ocu)
jh_edad <- "jh x P6040" (jh:P6042)

# hacer la vaina de boosting para ver cuáles variables meter, con eso se ve la importancia (tratar con 2, 4 6, 10)
es

#####

# #Tabla estadisticas descriptivas - Variables Numericas

library(stargazer)

stargazer(train_hogares[c("P5090", "Npersug", "Ingtotug")], type = "text") 
stargazer(train_hogares[c("P5090", "Npersug", "Ingtotug")], type = "latex") 

install.packages("gtsummary")
library(gtsummary)

## armar graficas de estadisticas 

box_plot <- ggplot(data=train_hogares , mapping = aes(as.factor(Pobre) , Ingtotug)) + 
  geom_boxplot()

box_plot <- box_plot +
  scale_fill_grey() + theme_classic()+
  labs(x= "Pobres", y ="ingresos totales por unidad de gasto") 

train_hogares$log_ingh <- log(train_hogares$Ingtotug+1)

box_plot2 <- ggplot(data=train_hogares , mapping = aes(as.factor(Pobre) , log_ingh)) + 
  geom_boxplot()

box_plot2 <- box_plot2 +
  scale_fill_grey() + theme_classic()+
  labs(x= "Pobres", y =" log ingresos totales por unidad de gasto")

pobre_bar <- ggplot(data = train_hogares, aes(x = Pobre, fill=Pobre)) +
  geom_bar() +
  labs (subtitle="base train_hogares",
        x= "Pobre = 1", y = "número de pobres")

# ver gráficas estadisticas descriptivas
box_plot
box_plot2
pobre_bar

#########################################################

# crear bases train_h y test_h (estas agrupan las bases por id) esto toca después de haber creado las variables jh_edu y el resto

# agrupar train
require(dplyr)
train_h <- data.frame()
train_h <- inner_join(train_hogares, train_personas,
                        by = c("id"))
summary(train_h)

# agrupar test
test_h <- data.frame()
test_h <- inner_join(test_hogares, test_personas,
                       by = c("id"))
summary(test_h)

###############

# MODELOS DE CLASIFICACIÓN

tidy(logit1)

library(tidyverse)

# LOGIT

## modelos
modelo1 <- as.formula("Pobre ~ P5090 + Npersug + jh_mujer + jh_edu + jh_ocu + jh_edad")
modelo2 <- as.formula("Pobre ~ Clase + P5090 + Npersug + jh_mujer + jh_edu + jh_ocu + jh_edad")
modelo3 <- as.formula("Pobre ~ P5090 + Npersug + jh_edu + jh_ocu")
modelo4 <- as.formula("Pobre ~ P5090 + Npersug + jh_edu + jh_ocu + ocu_id")
modelo5 <- as.formula("Pobre ~ P5090 + Npersug + jh_mujer + jh_edu + ocu_id")

library(stargazer)

# LOGIT

## estimación logit
logit1 <- glm(modelo1 , family=binomial(link="logit") , data=train_h)
logit1
summary(logit1)
stargazer(logit1, type="text")
tidy(logit1)

logit2 <- glm(modelo2 , family=binomial(link="logit") , data=train_h)
logit2
summary(logit2)
stargazer(logit2, type="text")
tidy(logit2)

logit3 <- glm(modelo3 , family=binomial(link="logit") , data=train_h)
logit3
summary(logit3)
stargazer(logit3, type="text")
tidy(logit3)

logit4 <- glm(modelo4 , family=binomial(link="logit") , data=train_h)
logit4
summary(logit4)
stargazer(logit4, type="text")
tidy(logit4)

logit5 <- glm(modelo5 , family=binomial(link="logit") , data=train_h)
logit5
summary(logit5)
stargazer(logit5, type="text")
tidy(logit5)

# PROBIT

# estimación probit
probit1 <- glm(modelo5 , family=binomial(link="probit") , data=train_h)
probit1
summary(probit1)
stargazer(probit1, type="text")
tidy(probit1)

stargazer(logit1, logit2, logit3, logit4, logit5, probit1, type = "text")

## ratio de los coeficientes
logit5$coefficients / probit1$coefficients

prop.table(table(train_hogares$Pobre))

# predicction (correr los modelos en train_h) # NO SÉ SI ESTÁ BIEN ** PREGUNTAR A LUCAS **

train_h$Pobre_pred_log1 <- predict(logit1, newdata=train_h, type="response")
train_h$Pobre_pred_log2 <- predict(logit2, newdata=train_h, type="response")
train_h$Pobre_pred_log3 <- predict(logit3, newdata=train_h, type="response")
train_h$Pobre_pred_log4 <- predict(logit4, newdata=train_h, type="response")
train_h$Pobre_pred_log5 <- predict(logit5, newdata=train_h, type="response")
train_h$Pobre_pred_pro1 <- predict(probit1, newdata=train_h, type="response")

head(test_h)

## plot predictions (con base train_h)

ggplot(data=train_h , mapping=aes(Pobre,Pobre_pred_log1)) +   geom_boxplot(aes(fill=as.factor(Pobre))) + theme_test()

## definir la regla (con la base train_h)

rule=0.7
train_h$log1_07 = ifelse(train_t$Pobre_pred_log1>rule,1,0)
train_h$log2_07 = ifelse(train_t$Pobre_pred_log2>rule,1,0)
train_h$log3_07 = ifelse(train_t$Pobre_pred_log3>rule,1,0)
train_h$log4_07 = ifelse(train_t$Pobre_pred_log4>rule,1,0)
train_h$log5_07 = ifelse(train_t$Pobre_pred_log5>rule,1,0)
train_h$pro1_07 = ifelse(train_t$Pobre_pred_pro1>rule,1,0)
head(test_t)

## plot data (con base train_h)
 
train_h %>% group_by(sex,Pobre) %>% mutate(count=1) %>% summarize(n=sum(count)) %>% ggplot(., aes(fill=as.factor(Pobre), y=n, x=as.factor(sex))) + geom_bar(position="fill", stat="identity")
train_h %>% group_by(sex,log1_07) %>% mutate(count=1) %>% summarize(n=sum(count)) %>% ggplot(., aes(fill=as.factor(log1_07), y=n, x=as.factor(sex))) + geom_bar(position="fill", stat="identity")
train_h %>% group_by(sex,log2_07) %>% mutate(count=1) %>% summarize(n=sum(count)) %>% ggplot(., aes(fill=as.factor(log2_07), y=n, x=as.factor(sex))) + geom_bar(position="fill", stat="identity")
train_h %>% group_by(sex,log3_07) %>% mutate(count=1) %>% summarize(n=sum(count)) %>% ggplot(., aes(fill=as.factor(log3_07), y=n, x=as.factor(sex))) + geom_bar(position="fill", stat="identity")
train_h %>% group_by(sex,log4_07) %>% mutate(count=1) %>% summarize(n=sum(count)) %>% ggplot(., aes(fill=as.factor(log4_07), y=n, x=as.factor(sex))) + geom_bar(position="fill", stat="identity")
train_h %>% group_by(sex,log5_07) %>% mutate(count=1) %>% summarize(n=sum(count)) %>% ggplot(., aes(fill=as.factor(log5_07), y=n, x=as.factor(sex))) + geom_bar(position="fill", stat="identity")
train_h %>% group_by(sex,pro1_07) %>% mutate(count=1) %>% summarize(n=sum(count)) %>% ggplot(., aes(fill=as.factor(pro1_07), y=n, x=as.factor(sex))) + geom_bar(position="fill", stat="identity")

## Clasificación (sale matriz de confusión)

## logit
cm_log1 = confusionMatrix(data=factor(train_h$Pobre) , reference=factor(train_h$log1_07) , mode="sens_spec" , positive="1")
cm_log1

cm_log2 = confusionMatrix(data=factor(train_h$Pobre) , reference=factor(train_h$log2_07) , mode="sens_spec" , positive="1")
cm_log2

cm_log3 = confusionMatrix(data=factor(train_h$Pobre) , reference=factor(train_h$log3_07) , mode="sens_spec" , positive="1")
cm_log3

cm_log4 = confusionMatrix(data=factor(train_h$Pobre) , reference=factor(train_h$log4_07) , mode="sens_spec" , positive="1")
cm_log4

cm_log5 = confusionMatrix(data=factor(train_h$Pobre) , reference=factor(train_h$log5_07) , mode="sens_spec" , positive="1")
cm_log5

## probit
cm_pro1 = confusionMatrix(data=factor(train_h$Pobre) , reference=factor(train_h$pro1_07) , mode="sens_spec" , positive="1")
cm_pro1

cm1 <- cm_log1$table # matriz de confusión del modelo logit 1
cm2 <- cm_log2$table # matriz de confusión del modelo logit 2
cm3 <- cm_log3$table # matriz de confusión del modelo logit 3
cm4 <- cm_log4$table # matriz de confusión del modelo logit 4
cm5 <- cm_log5$table # matriz de confusión del modelo logit 5
cm6 <- cm_pro1$table # matriz de confusión del modelo probit 1

# métricas por modelo
cm1_metri <- cm_log1$byClass
cm2_metri <- cm_log2$byClass
cm3_metri <- cm_log3$byClass
cm4_metri <- cm_log4$byClass
cm5_metri <- cm_log5$byClass
cm6_metri <- cm_pro1$byClass

# métricas agruipadas
gru_metri <-  rbind(cm1_metri, cm2_metri, cm3_metri, cm4_metri, cm5_metri, cm6_metri)

require(xtable)
xtable(gru_metri)

## Misclassification Rates
## Curva de ROC

predic_log1_07 <- prediction(train_h$log1_07, train_h$Pobre) ## paste predictions
ROC_log1_07 <- performance(predic_log1_07,"tpr","fpr")  ## get ROC object 
plot(ROC_log1_07, main = "ROC curve", col="red") ## plot object
abline(a = 0, b = 1)

predic_log2_07 <- prediction(train_h$log2_07, train_h$Pobre) ## paste predictions
ROC_log2_07 <- performance(predic_log2_07,"tpr","fpr")  ## get ROC object 
plot(ROC_log2_07, main = "ROC curve", col="yellow") ## plot object
abline(a = 0, b = 1)

predic_log3_07 <- prediction(train_h$log3_07, train_h$Pobre) ## paste predictions
ROC_log3_07 <- performance(predic_log3_07,"tpr","fpr")  ## get ROC object 
plot(ROC_log3_07, main = "ROC curve", col="green") ## plot object
abline(a = 0, b = 1)

predic_log4_07 <- prediction(train_h$log4_07, train_h$Pobre) ## paste predictions
ROC_log4_07 <- performance(predic_log4_07,"tpr","fpr")  ## get ROC object 
plot(ROC_log4_07, main = "ROC curve", col="blue") ## plot object
abline(a = 0, b = 1)

predic_log4_07 <- prediction(train_h$log4_07, train_h$Pobre) ## paste predictions
ROC_log4_07 <- performance(predic_log4_07,"tpr","fpr")  ## get ROC object 
plot(ROC_log4_07, main = "ROC curve", col="black") ## plot object
abline(a = 0, b = 1)

predic_log5_07 <- prediction(train_h$log5_07, train_h$Pobre) ## paste predictions
ROC_log5_07 <- performance(predic_log5_07,"tpr","fpr")  ## get ROC object 
plot(ROC_log5_07, main = "ROC curve", col="orange") ## plot object
abline(a = 0, b = 1)

predic_pro1_07 <- prediction(train_h$pro1_07, train_h$Pobre) ## paste predictions
ROC_pro1_07 <- performance(predic_pro1_07,"tpr","fpr")  ## get ROC object 
plot(ROC_pro1_07, main = "ROC curve", col="orange") ## plot object
abline(a = 0, b = 1)

# AUC

ACU_ROC_log1_07 <- performance(predic_log1_07, measure = "auc") 
ACU_ROC_log2_07 <- performance(predic_log2_07, measure = "auc") 
ACU_ROC_log3_07 <- performance(predic_log3_07, measure = "auc") 
ACU_ROC_log4_07 <- performance(predic_log4_07, measure = "auc") 
ACU_ROC_log5_07 <- performance(predic_log5_07, measure = "auc") 
ACU_ROC_pro1_07 <- performance(predic_pro1_07, measure = "auc") 

require(caret)

train_h$Pobre<- factor((train_h$Pobre), levels = c(0, 1), labels = c("No", "Si"))

# Para desbalance de clases - evitar overfitting

require("tidyverse")
require("here")

# Partir base de datos train_h

set.seed(1010)
# training
split1 <- createDataPartition(train_h$Pobre, p = .7)[[1]]
length(split1)
other <- train_h[-split1,]
training <- train_h[ split1,]
#  ahora se crea evaluation y testing
set.seed(1010)
split2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]
evaluation <- other[ split2,]
testing <- other[-split2,]

dim(training)
dim(evaluation)
dim(testing)

summary(training$Pobre)

# se seleccionó el modelo logit 4 ## acá no sé qué pasa con el códico (creo que debe ir split1 por traincontrol)

# logit CV
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))

ctrl_pobre <- trainControl(method = "cv",
                           number = 5,
                           summaryFunction = fiveStats,
                           classProbs = TRUE,
                           verbose=FALSE,
                           savePredictions = T)
# logit caret
set.seed(1010)
logit_caret_pob <- train(logit4,
                         data = training,
                         method = "glm",
                         trControl = ctrl_pobre,
                         family = "binomial",
                         preProcess = c("center", "scale")
)

logit_caret_pob

# logit lasso acc
lambda_grid <- 10^seq(-4, 0.01, length = 200)
lambda_grid

set.seed(1010)
logit_lasso_acc <- train(logit4,
                    data = training,
                    method = "glmnet",
                    trControl = ctrl_pobre,
                    family = "binomial",
                    metric = "Accuracy",
                    tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                    preProcess = c("center", "scale")
)

logit_lasso_acc

logit_lasso_acc[["bestTune"]]

# logit lasso ROC
set.seed(1010)
logit_lasso_ROC<- train(logit4,
                        data = training,
                        method = "glmnet",
                        trControl = ctrl_pobre,
                        family = "binomial",
                        metric = "ROC",
                        tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                        preProcess = c("center", "scale")
)

logit_lasso_ROC

logit_lasso_ROC[["bestTune"]]

# logit caret sens
set.seed(1010)
logit_lasso_sens<- train(logit4,
                         data = training,
                         method = "glmnet",
                         trControl = ctrl_pobre,
                         family = "binomial",
                         metric = "Sens",
                         tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                         preProcess = c("center", "scale")
)

logit_lasso_sens

logit_lasso_sens[["bestTune"]]

# remuestreo upsampling downsampling 

# Upsampling

set.seed(1010)
upSampledTrain <- upSample(x = training,
                           y = training$Pobre,
                           yname = "Pobre")

dim(training)
dim(upSampledTrain)
table(upSampledTrain$Pobre)

set.seed(1010)
logit_lasso_upsample <- train(logit4,
                              data = upSampledTrain,
                              method = "glmnet",
                              trControl = ctrl_pobre,
                              family = "binomial",
                              metric = "ROC",
                              tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                              preProcess = c("center", "scale")
)

logit_lasso_upsample
logit_lasso_upsample[["bestTune"]]

# Downsampling
set.seed(1010)
downSampledTrain <- downSample(x = training,
                               y = training$Pobre,
                               yname = "Pobre")
dim(training)
dim(downSampledTrain)
table(downSampledTrain$Pobre)

set.seed(1010)
logit_lasso_downsample <- train(logit4,
                                data = downSampledTrain,
                                method = "glmnet",
                                trControl = ctrl_pobre,
                                family = "binomial",
                                metric = "ROC",
                                tuneGrid = expand.grid(alpha = 0,lambda=lambda_grid),
                                preProcess = c("center", "scale")
)

logit_lasso_downsample
logit_lasso_downsample[["bestTune"]]

# comparaciones

testResults$logit_caret <- predict(logit_caret_pob, newdata = testing, type = "prob")[,1]
testResults$lasso_acc <- predict(logit_lasso_acc, newdata = testing, type = "prob")[,1]
testResults$lasso_ROC <- predict(logit_lasso_ROC, newdata = testing, type = "prob")[,1]
testResults$lasso_sens <- predict(logit_lasso_sens, newdata = testing, type = "prob")[,1]
testResults$lasso_upsample <- predict(logit_lasso_upsample, newdata = testing, type = "prob")[,1]
testResults$lasso_downsample <- predict(logit_lasso_downsample, newdata = testing, type = "prob")[,1]

testResults <- testResults %>% mutate(logit_caret=ifelse(logit_caret>0.5,"Si","No"), 
                                    lasso_acc=ifelse(lasso_acc>0.5,"Si","No"), 
                                    lasso_ROC=ifelse(lasso_ROC>0.5,"Si","No"), 
                                    lasso_sens=ifelse(lasso_sen>0.5,"Si","No"),
                                    lasso_upsample=ifelse(lasso_upsample>0.5,"Si","No"),
                                    lasso_downsample=ifelse(lasso_downsample>0.5,"Si","No")
)

# TN TP FN FP
with(testResults,table(Pobre,logit_caret))
with(testResults,table(Pobre,lasso_acc))
with(testResults,table(Pobre,lasso_ROC))
with(testResults,table(Pobre,lasso_sens))
with(testResults,table(Pobre,lasso_upsample))
with(testResults,table(Pobre,lasso_downsample))

test_h$Pobre_predicho_final<-predict(logit_lasso_downsample, newdata=test_h)

summary(test_h$Pobre_predicho_final)


#############
# MODELOS DE REGRESIÓN PARA PREDICCIÓN DE INGRESOS

# siembra semilla y dividir la base en train y test
set.seed(777)
split1 <- createDataPartition(train_h$Pobre, p = .7)[[1]]
length(split1) 

other <- train_h[-split1,]
tr_train <- train_h[split1,] # base de entrenamiento de train

split2 <- createDataPartition(other$Pobre, p = 1/3)[[1]]

tr_eval <- other[ split2,] # base de evaluación de train
tr_test <- other[-split2,] # base de prueba de train

# modelos
modelo_reg1 <- as.formula("Intotpug ~ Clase + P5090 + Npersug + jh_mujer + jh_edu + jh_ocu")
modelo_reg2 <- as.formula("Intotpug ~ Clase + P5090 + Npersug + jh_edu + jh_ocu + ocu_id")
modelo_reg3 <- as.formula("Intotpug ~ Clase + P5090 + Npersug + jh_mujer + jh_edu + ocu_id - jh_edad")

# LINEAL

lineal1 <- lm(modelo_reg1 , data = tr_train)
lineal2 <- lm(modelo_reg2 , data = tr_train)
lineal3 <- lm(modelo_reg3 , data = tr_train)

summary(lineal1)
summary(lineal2)
summary(lineal3)

stargazer(lineal1,type="text")
stargazer(lineal2,type="text")
stargazer(lineal3,type="text")


lineal1_est <- train(lineal1,
                         data = tr_train,
                         trControl=trainControl(method="cv",number=10),
                         method="lm")

lineal2_est <- train(lineal2,
                         data = tr_train,
                         trControl=trainControl(method="cv",number=10),
                         method="lm")

lineal3_est <- train(lineal3,
                         data = tr_train,
                         trControl=trainControl(method="cv",number=10),
                         method="lm")

# predicción modelos lineales

lineal1_pre <- predict(lineal1_est,newdata = tr_test )
lineal2_pre <- predict(lineal2_est,newdata = tr_test )
lineal3_pre <- predict(lineal3_est,newdata = tr_test )

# cálculo del MSE 

MSE_lineal1 <- with (tr_test,mean((Ingtotug - lineal1_pre)^2))
MSE_lineal1

MSE_lineal2 <- with (tr_test,mean((Ingtotug - lineal2_pre)^2))
MSE_lineal2

MSE_lineal3 <- with (tr_test,mean((Ingtotug - lineal3_pre)^2))
MSE_lineal3

# guardar resultados
tr_test$lineal1 <- lineal1_pre
tr_test$lineal2 <- lineal2_pre
tr_test$lineal3 <- lineal3_pre


tr_test$clas_pobre_lineal1 <- factor(if_else( tr_test$lineal1 < tr_test$Lp, "Pobre", "No Pobre"))
tr_test$clas_pobre_lineal2 <- factor(if_else( tr_test$lineal2 < tr_test$Lp, "Pobre", "No Pobre"))
tr_test$clas_pobre_lineal3 <- factor(if_else( tr_test$lineal3 < tr_test$Lp, "Pobre", "No Pobre"))

summary(tr_test$clas_pobre_lineal1)
summary(tr_test$clas_pobre_lineal2)
summary(tr_test$clas_pobre_lineal3)

# matrices

cm_lineal1 <- confusionMatrix(data=tr_test$clas_pobre_lineal1, 
                              reference=tr_test$Pobre , 
                              mode="sens_spec" , positive="Pobre")

cm_lineal2 <- confusionMatrix(data=tr_test$clas_pobre_lineal2, 
                               reference=tr_test$Pobre , 
                               mode="sens_spec" , positive="Pobre")

cm_lineal3 <- confusionMatrix(data=tr_test$clas_pobre_lineal3, 
                              reference=tr_test$Pobre , 
                              mode="sens_spec" , positive="Pobre")

# LASSO

lambda <- 10^seq(-2, 3, length = 100)

lasso1 <- train(modelo_reg1,
                data = tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 1,lambda=lambda),
                preProcess = c("center", "scale"))

lasso2 <- train(modelo_reg2,
                data = tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 1,lambda=lambda),
                preProcess = c("center", "scale"))

lasso3 <- train(modelo_reg3,
                data = tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 1,lambda=lambda),
                preProcess = c("center", "scale"))

# predicción modelos lasso

lasso1_pre <- predict(lasso1,newdata = tr_test )
lasso2_pre <- predict(lasso2,newdata = tr_test )
lasso3_pre <- predict(lasso3,newdata = tr_test )

# guardar resultados

tr_test$lasso1 <- lasso1_pre
tr_test$lasso2 <- lasso2_pre
tr_test$lasso3 <- lasso3_pre


tr_test$clas_pobre_lasso1 <- factor(if_else( tr_test$lasso1 < tr_test$Lp, "Pobre", "No Pobre"))
tr_test$clas_pobre_lasso2 <- factor(if_else( tr_test$lasso2 < tr_test$Lp, "Pobre", "No Pobre"))
tr_test$clas_pobre_lasso3 <- factor(if_else( tr_test$lasso3 < tr_test$Lp, "Pobre", "No Pobre"))

summary(tr_test$clas_pobre_lasso1)
summary(tr_test$clas_pobre_lasso2)
summary(tr_test$clas_pobre_lasso3)

# matrices

cm_lasso1 <- confusionMatrix(data=tr_test$clas_pobre_lasso1, 
                                  reference=tr_test$Pobre , 
                                  mode="sens_spec" , positive="Pobre")

cm_lasso2 <- confusionMatrix(data=tr_test$clas_pobre_lasso2, 
                             reference=tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cm_lasso3 <- confusionMatrix(data=tr_test$clas_pobre_lasso3, 
                             reference=tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cm_lasso1
cm_lasso2
cm_lasso3

# RIDGE

ridge1 <- train(modelo_reg1,
                      data = tr_train,
                      method = "glmnet",
                      trControl = trainControl("cv", number = 10),
                      tuneGrid = expand.grid(alpha = 0,lambda=lambda),
                      preProcess = c("center", "scale"))

ridge2 <- train(modelo_reg2,
                data = tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 0,lambda=lambda),
                preProcess = c("center", "scale"))

ridge3 <- train(modelo_reg2,
                data = tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 0,lambda=lambda),
                preProcess = c("center", "scale"))


# predicción modelos ridge

ridge1_pre <- predict(ridge1,newdata = tr_test )
ridge2_pre <- predict(ridge2,newdata = tr_test )
ridge3_pre <- predict(ridge3,newdata = tr_test )

# guardar resultados

tr_test$ridge1 <- ridge1_pre
tr_test$ridge2 <- ridge2_pre
tr_test$ridge3 <- ridge3_pre

tr_test$clas_pobre_ridge1 <- factor(if_else( tr_test$ridge1 < tr_test$Lp, "Pobre", "No Pobre"))
tr_test$clas_pobre_ridge2 <- factor(if_else( tr_test$ridge2 < tr_test$Lp, "Pobre", "No Pobre"))
tr_test$clas_pobre_ridge3 <- factor(if_else( tr_test$ridge3 < tr_test$Lp, "Pobre", "No Pobre"))

summary(tr_test$clas_pobre_ridge1)
summary(tr_test$clas_pobre_ridge2)
summary(tr_test$clas_pobre_ridge3)

# matrices

cm_ridge1 <- confusionMatrix(data=tr_test$clas_pobre_ridge1, 
                             reference=tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cm_ridge2 <- confusionMatrix(data=tr_test$clas_pobre_ridge2, 
                             reference=tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cm_ridge3 <- confusionMatrix(data=tr_test$clas_pobre_ridge3, 
                             reference=tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cm_ridge1
cm_ridge2
cm_ridge3

# ELASTIC NET

elasticnet1 <- train(modelo_reg1,
                data = tr_train,
                method = "glmnet",
                trControl = trainControl("cv", number = 10),
                tuneGrid = expand.grid(alpha = 0.5,lambda=lambda),
                preProcess = c("center", "scale"))

elasticnet2 <- train(modelo_reg2,
                     data = tr_train,
                     method = "glmnet",
                     trControl = trainControl("cv", number = 10),
                     tuneGrid = expand.grid(alpha = 0.5,lambda=lambda),
                     preProcess = c("center", "scale"))

elasticnet3 <- train(modelo_reg3,
                     data = tr_train,
                     method = "glmnet",
                     trControl = trainControl("cv", number = 10),
                     tuneGrid = expand.grid(alpha = 0.5,lambda=lambda),
                     preProcess = c("center", "scale"))

# predicción modelos elastic net

elasticnet1_pre <- predict(elasticnet1,newdata = tr_test )
elasticnet2_pre <- predict(elasticnet2,newdata = tr_test )
elasticnet3_pre <- predict(elasticnet3,newdata = tr_test )

# guardar resultados

tr_test$elasticnet1 <- elasticnet1_pre
tr_test$elasticnet2 <- elasticnet2_pre
tr_test$elasticnet3 <- elasticnet3_pre

tr_test$clas_pobre_elasticnet1 <- factor(if_else( tr_test$elasticnet1 < tr_test$Lp, "Pobre", "No Pobre"))
tr_test$clas_pobre_elasticnet2 <- factor(if_else( tr_test$elasticnet2 < tr_test$Lp, "Pobre", "No Pobre"))
tr_test$clas_pobre_elasticnet3 <- factor(if_else( tr_test$elasticnet3 < tr_test$Lp, "Pobre", "No Pobre"))

summary(tr_test$clas_pobre_elasticnet1)
summary(tr_test$clas_pobre_elasticnet2)
summary(tr_test$clas_pobre_elasticnet3)

# matrices

cm_elasticnet1 <- confusionMatrix(data=tr_test$clas_pobre_elasticnet1, 
                             reference=tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cm_elasticnet2 <- confusionMatrix(data=tr_test$clas_pobre_elasticnet2, 
                             reference=tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cm_elasticnet3 <- confusionMatrix(data=tr_test$clas_pobre_elasticnet3, 
                             reference=tr_test$Pobre , 
                             mode="sens_spec" , positive="Pobre")

cm_elasticnet1
cm_elasticnet2
cm_elasticnet3


# comparación de modelos

modelos_predic_ingre <- list(lineal1,
                             lineal2,
                             lineal3,
                             lasso1,
                             lasso2,
                             lasso3,
                             ridge1, 
                             ridge2, 
                             ridge3, 
                             elasticnet1, 
                             elasticnet2, 
                             elasticnetnet3)

resamples(modelos_predic_ingre) %>% summary(metric = "RMSE")
modelos_predic_ingre



##

# hacer random forest, pero antes de es,hacer el vaino de las diapositivas de semana 8 para determinar cuales son las variables importantes / deben ser 4 m[as o menos










