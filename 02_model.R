rm(list = ls())

# Librerias ---------------------------------------------------------------

library(h2o)
library(scorecard)
library(InformationValue)
library(ggplot2)
library(plotly)
library(cowplot)
library(stats)
library(lubridate)
library(purrr)
library(tidyr)
library(matrixStats)
library(bit64)
library(stats)
library(data.table)
library(dplyr)
library(ROCit)
theme_set(theme_grey())

# -------------------------------------------------------------------------
# P4 ----------------------------------------------------------------------
# -------------------------------------------------------------------------

# Leer dataset  -----------------------------------------------------------

df_pred <- fread('~/Desktop/test/files/base_para_prediccion.csv') %>% data.table()

# Contruccion variables ---------------------------------------------------

df_pred[,c("anno","mes","semana","dia","dia_semana"):=list(lubridate::year(fecha),lubridate::month(fecha),
                                                                     lubridate::week(fecha),lubridate::day(fecha),
                                                                     base::weekdays(fecha %>% as.Date()))]
df_pred[,"weekend":=ifelse(dia_semana %in% c("Saturday","Sunday"),1,0)]        

# Creacion funcion grafica subestaciones  --------------------------------------------------------

plot_time_series_sub_estation <- function(dates,subestacion,variable,df=df_pred){
  plot_sub_estation <- function(date,subestacion,variable,df=df_pred){
    df <- df[fecha==as.Date(date) & nemotecnico_se==subestacion,] 
    df[,date_ymdh:=as.POSIXct(as.character(fecha),"UTC")+hours(hora)]
    ggplot(df , aes(x=date_ymdh, y=get(variable))) + geom_line() + ggtitle(paste0(" Serie de Tiempo subestacion: ",subestacion,' - Fecha:',date))
  }
  map(dates,~plot_sub_estation(.x,subestacion,variable))
}

#  Grafica curva de generación solar --------------------------------------

#plot_sub1 <- plot_time_series_sub_estation(c(as.Date('2019-01-10')+days(1:4)),'SE005T002','gen_solar_total_mwh')
#plot_sub2 <- plot_time_series_sub_estation(c(as.Date('2019-01-10')+days(1:4)),'SE127T005','gen_solar_total_mwh')

# Grafica curva de generación térmica, ------------------------------------

#plot_sub1 <- plot_time_series_sub_estation(c(as.Date('2019-05-14')+days(1:4)),'SE020G213','gen_termica_total_mwh')
#plot_sub2 <- plot_time_series_sub_estation(c(as.Date('2019-05-14')+days(1:4)),'SE106G216','gen_termica_total_mwh')

# -------------------------------------------------------------------------
# P5 ----------------------------------------------------------------------
# -------------------------------------------------------------------------

# Funciones ---------------------------------------------------------------

df_test_train <- function(df,Y_name,param){
  res <- NULL
  res$df <- df
  res$df[,target:=get(Y_name)]
  res$df <- res$df[original==1,]
  res$df[,c("date_x","date_y"):=NULL]
  # test 
  obs_test <- sample(seq_len(nrow(res$df)), size = floor(param$p_test*nrow(res$df)))
  res$df_test <- res$df[obs_test]
  res$df_test[,target:=NULL]
  # train
  res$df_train <- res$df[-1*obs_test]
  n1 <- sum(res$df_train$target);n0 <- ((n1*param$downsampling)/(1-param$downsampling)) %>% round()
  res$df_train <- res$df_train[c(which(res$df_train$target==0) %>% sample(n0,replace=TRUE),which(res$df_train$target==1))]
  res$df_train[,target:=NULL]
  res$df[,target:=NULL]
  res
}

perf_metrics <- function(Y,pred){
  res <- NULL
  res$M_conf <- ModelMetrics::confusionMatrix(Y,pred) 
  res$auc <- ModelMetrics::auc(Y,pred) 
  res$tpr <- ModelMetrics::tpr(Y,pred) 
  res$ks <- ks_stat(Y,pred) 
  res
}

# Crear flag --------------------------------------------------------------

df_pred[,flag:=ifelse(abs(cmg_desv_pct)>15,1,0)]
df_pred[,flag:=ifelse((is.na(cmg_real) | cmg_real==0 | is.na(cmg_prog) | cmg_prog==0),NA,flag)]

# Completar fechas faltantes ----------------------------------------------

df_pred[,c("date_ymdh","original"):=list(as.POSIXct(as.character(fecha),"UTC")+hours(hora),1)]
all_dates <- seq(from=as.POSIXct(as.character(min(df_pred$fecha)),"UTC"),to=as.POSIXct(as.character(max(df_pred$fecha)),"UTC"), by="hour")
dt <- tidyr::crossing(nemotecnico_se=unique(df_pred$nemotecnico_se),date_ymdh = all_dates) %>% data.table()
df_pred <- dt %>% left_join(df_pred,by=c('nemotecnico_se','date_ymdh')) %>% data.table()
rm(dt)

# Generar otras covariables -----------------------------------------------

# agregar variables cuadraticas y cubicas
vars_num <- df_pred %>% select(starts_with("gen"),starts_with("cmg"),demanda_mwh,cap_inst_mw) %>% names()
eleva <- function(x,n){x**n}
df_pred <- df_pred %>% mutate_at(vars(vars_num), .funs = list(cuad = ~eleva(.x,2))) 
df_pred <- df_pred %>% mutate_at(vars(vars_num), .funs = list(cub = ~eleva(.x,3))) 

# agregar estadisticos
vars_num <- c(vars_num,paste0(vars_num,"_cuad"),paste0(vars_num,"_cub"))
df_pred <- df_pred %>% group_by(date_ymdh) %>% mutate_at(vars(vars_num), .funs = list(mean = ~mean(.x,na.rm = TRUE))) 
df_pred <- df_pred %>% group_by(date_ymdh) %>% mutate_at(vars(vars_num), .funs = list(sd = ~sd(.x,na.rm = TRUE))) 
df_pred <- df_pred %>% group_by(date_ymdh) %>% mutate_at(vars(vars_num), .funs = list(median = ~median(.x,na.rm = TRUE))) 
df_pred <- df_pred %>% group_by(date_ymdh) %>% mutate_at(vars(vars_num), .funs = list(sum = ~sum(.x,na.rm = TRUE))) %>% data.table()
rm(vars_num)

# Generar la Y  -----------------------------------------------------------

df_pred <- df_pred[order(nemotecnico_se,-date_ymdh)]
df_pred[,c("hora","mes","dia","fecha","anno","semana"):=NULL]
df_pred <- df_pred %>% setnames(old='date_ymdh',new='date_x')

df_pred[,c("Y","Y2"):=list(lag(flag,1),lag(flag,12)),by="nemotecnico_se"] 
df_pred[,c("flag","weekend","date_y"):=list(as.character(flag),as.character(weekend),date_x+hours(1))]
df_pred[,camada:=as.Date(date_y) %>% format('%Y-%m-01') %>% as.Date()]

# sanity check
table(df_pred$Y);table(df_pred$Y2)
nrow(df_pred)

# Entrenar el modelo  -----------------------------------------------------

# Separar datas modelos 1 y 2
df_pred_M1 <- df_pred[!is.na(Y) & !is.na(cmg_prog) & !is.na(cmg_real) & cmg_prog!=0 & cmg_real!=0 & original==1,]   
df_pred_M2 <- df_pred[!is.na(Y2) & !is.na(cmg_prog) & !is.na(cmg_real) & cmg_prog!=0 & cmg_real!=0 & original==1,]

# Exportar datas ----------------------------------------------------------

write.csv(df_pred_M1,"~/Desktop/test/files/df_model1.csv")
write.csv(df_pred_M2,"~/Desktop/test/files/df_model2.csv")

# Sanity check
nrow(df_pred_M1);nrow(df_pred_M2)

# Rates
df_pred_M1[,.(p=mean(Y),n1=sum(Y),N=length(Y))]
df_pred_M2[,.(p=mean(Y2),n1=sum(Y2),N=length(Y2))]

#get var names 
var_names <- (df_pred %>% select(-c(Y,Y2,camada,nemotecnico_se,date_y,date_x))) %>% names()

# Def parametros modelamiento ---------------------------------------------

param <- NULL
param$p_test <- 0.2
param$downsampling <- 0.5  
param$importancia <- 0.95

# Downsampling ------------------------------------------------------------

df_modeling_Y1 <- df_test_train(df_pred_M1,"Y",param)

# Sanity Check ------------------------------------------------------------

df_modeling_Y1$df_train[,.(mean=mean(Y))]
df_modeling_Y1$df_test[,.(mean=mean(Y))]

# Parametros H2O ----------------------------------------------------------

h2o.init(min_mem_size = '1G', max_mem_size = '6G')
test_M1 <- as.h2o(df_modeling_Y1$df_test, destination_frame = 'test')
train_M1  <- as.h2o(df_modeling_Y1$df_train, destination_frame = 'train')

# Modelo regularizado -----------------------------------------------------

model_reg_M1 <- h2o.glm(y = 'Y', x = var_names, training_frame = train_M1, seed = 1234,family = 'binomial',
                     alpha = 1, lambda_search =  TRUE, nlambdas = 2000, max_active_predictors = 20 ,model_id = "model_reg_M1",
                     remove_collinear_columns = TRUE)

vars_M1 <- h2o::h2o.varimp(model_reg_M1) %>% mutate(perCumSum =cumsum(percentage)) %>% filter(perCumSum<param$importancia)
vars_M1 <- vars_M1$variable %>% as.character()
vars_M1

# Modelo real -------------------------------------------------------------

model_M1 <- h2o.glm(y = 'Y', x = vars_M1, training_frame = train_M1, validation_frame = test_M1, seed = 1234,family = 'binomial',
                 lambda=0,compute_p_values = T,model_id = "model_M1",remove_collinear_columns = TRUE)

# Performance modelo ------------------------------------------------------

h2o::h2o.auc(model_M1,train = T,valid = T)

## Test --------------------------------------------------------------------

pred_M1 <- (h2o.predict(object = model_M1, newdata = test_M1)$p1) %>% as.vector() %>% as.numeric()
Y_M1 <- df_modeling_Y1$df_test$Y
per_M1 <- perf_metrics(Y_M1,pred_M1)

per_M1$M_conf;per_M1$auc;per_M1$tpr;per_M1$ks

rocit(score=pred_M1,class=Y_M1) %>% plot()

# -------------------------------------------------------------------------
# P6 ----------------------------------------------------------------------
# -------------------------------------------------------------------------

# definir parametros 
param <- NULL
param$p_test <- 0.2
param$downsampling <- 0.5  
param$importancia <- 0.1

# REG LOGISTICA -----------------------------------------------------------

# Downsampling ------------------------------------------------------------

df_modeling_M2 <- df_test_train(df_pred_M2,"Y2",param)
df_modeling_M2$df_train <- df_modeling_M2$df_train[,Y2:=factor(Y2,levels=c("0","1"))]
df_modeling_M2$df_test <- df_modeling_M2$df_test[,Y2:=factor(Y2,levels=c("0","1"))]
df_modeling_M2$df_train[,fecha:=NULL]
df_modeling_M2$df_test[,fecha:=NULL]

# Sanity Check ------------------------------------------------------------

df_modeling_M2$df_train$Y2 %>% table()
df_modeling_M2$df_test$Y2 %>% table()

# Parametros H2O ----------------------------------------------------------

test_M2 <- as.h2o(df_modeling_M2$df_test, destination_frame = 'test')
train_M2  <- as.h2o(df_modeling_M2$df_train, destination_frame = 'train')

# Random forest -----------------------------------------------------------

model_M2 <- h2o.randomForest(y = 'Y2', x = c(var_names), training_frame = train_M2, validation_frame = test_M2, seed = 123,
                             stopping_metric ="mean_per_class_error",nfolds = 0)
vars_M2 <- h2o::h2o.varimp(model_M2) %>% as.data.frame() %>% mutate(perCumSum=cumsum(percentage %>% as.numeric())) %>% filter(perCumSum<param$importancia)
vars_M2 <- vars_M2$variable %>% as.character()
vars_M2

model_M2 <- h2o.randomForest(y = 'Y2', x = c(vars_M2), training_frame = train_M2, validation_frame = test_M2, seed = 123,
                             stopping_metric ="mean_per_class_error",nfolds = 0,model_id = "model_M2")

# Performance modelo ------------------------------------------------------

h2o::h2o.auc(model_M2,train = T,valid = T)

perf <- h2o::h2o.performance(model_M2,valid = T)
perf 

## Test --------------------------------------------------------------------

pred_M2 <- (h2o.predict(object = model_M2, newdata = test_M2)$p1) %>% as.vector() %>% as.numeric()
Y_M2 <- df_modeling_M2$df_test$Y2
per_M2 <- perf_metrics(Y_M2,pred_M2)

per_M2$M_conf;per_M2$auc;per_M2$tpr;per_M2$ks

rocit(score=pred_M2,class=Y_M2) %>% plot()

# -------------------------------------------------------------------------
# P7 ----------------------------------------------------------------------
# -------------------------------------------------------------------------

# Leer data ---------------------------------------------------------------

df_clima <- fread('~/Desktop/test/files/datos_clima.csv') %>% data.table()
df_clima[,fecha:= fecha %>% as.POSIXct("UTC") %>% floor_date("day")-days(1)]

# Sanity check ------------------------------------------------------------

df_clima$fecha %>% max();df_clima$fecha %>% min();nrow(df_clima)

# Añadir data clima -------------------------------------------------------

vars_clima <- df_clima %>% select(-c(fecha,subestacion)) %>% names()

df_pred_M2[,fecha:=date_x %>% as.POSIXct("UTC") %>% floor_date("day")]
df_pred_M3 <- df_pred_M2 %>% left_join(df_clima,by=c('nemotecnico_se'='subestacion','fecha')) %>% data.table()
df_pred_M3 <- df_pred_M3 %>% mutate_at(vars(vars_clima), .funs = list(cuad = ~eleva(.x,2))) 
df_pred_M3 <- df_pred_M3 %>% mutate_at(vars(vars_clima), .funs = list(cub = ~eleva(.x,3))) 

vars_clima <- c(vars_clima,paste0(vars_clima,'_cuad'),paste0(vars_clima,'_cub'))

df_pred_M3 <- df_pred_M3 %>% group_by(fecha) %>% mutate_at(vars(vars_clima), .funs = list(mean = ~mean(.x,na.rm = TRUE))) 
df_pred_M3 <- df_pred_M3 %>% group_by(fecha) %>% mutate_at(vars(vars_clima), .funs = list(sd = ~sd(.x,na.rm = TRUE))) 
df_pred_M3 <- df_pred_M3 %>% group_by(fecha) %>% mutate_at(vars(vars_clima), .funs = list(median = ~median(.x,na.rm = TRUE))) 
df_pred_M3 <- df_pred_M3 %>% group_by(fecha) %>% mutate_at(vars(vars_clima), .funs = list(sum = ~sum(.x,na.rm = TRUE))) %>% data.table()
df_pred_M3[,fecha:=NULL]

# Modelo ------------------------------------------------------------------

# definir parametros 
param <- NULL
param$p_test <- 0.2
param$downsampling <- 0.5 
param$importancia <- 0.15

# Donwsampling ------------------------------------------------------------

df_modeling_M3 <- df_test_train(df_pred_M3,"Y2",param)
df_modeling_M3$df_train <- df_modeling_M3$df_train[,Y2:=factor(Y2,levels=c("0","1"))]
df_modeling_M3$df_train <- df_modeling_M3$df_train[,Y2:=factor(Y2,levels=c("0","1"))]

# Sanity Check ------------------------------------------------------------

df_modeling_M3$df_train$Y2 %>% table()
df_modeling_M3$df_test$Y2 %>% table()

# Datas h2o ---------------------------------------------------------------

test_M3 <- as.h2o(df_modeling_M3$df_test, destination_frame = 'test')
train_M3  <- as.h2o(df_modeling_M3$df_train, destination_frame = 'train')

# Random forest -----------------------------------------------------------

model_M3 <- h2o.randomForest(y = 'Y2', x = c(var_names,vars_clima), training_frame = train_M3, validation_frame = test_M3, seed = 123,
                 stopping_metric ="mean_per_class_error",nfolds = 0)

vars_M3 <- h2o::h2o.varimp(model_M3) %>% as.data.frame() %>% mutate(perCumSum=cumsum(percentage %>% as.numeric())) %>% filter(perCumSum<param$importancia)
vars_M3 <- vars_M3$variable %>% as.character()
vars_M3

model_M3 <- h2o.randomForest(y = 'Y2', x = c(vars_M3), training_frame = train_M3, validation_frame = test_M3, seed = 123,
                             stopping_metric ="mean_per_class_error",nfolds = 0,model_id = "model_M3")

# Performance modelo ------------------------------------------------------

perf <- h2o::h2o.performance(model_M3,valid = T)
perf 

h2o::h2o.auc(model_M3,train = T,valid = T)

## Test --------------------------------------------------------------------

pred_M3 <- (h2o.predict(object = model_M3, newdata = test_M3)$p1) %>% as.vector() %>% as.numeric()
Y_M3 <- df_modeling_M3$df_test$Y2
per_M3 <- perf_metrics(Y_M3,pred_M3)

per_M3$M_conf;per_M3$auc;per_M3$tpr;per_M3$ks








