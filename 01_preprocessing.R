rm(list = ls())
source('~/Desktop/test/init.R')

# - -----------------------------------------------------------------------
# P1 - Costos Marginales --------------------------------------------------
# - -----------------------------------------------------------------------

# Leer dataset  -----------------------------------------------------------

cm_real <- fread('~/Desktop/test/files/costo_marginal_real.csv')
cm_prog <- fread('~/Desktop/test/files/costo_marginal_programado.csv')

# Eliminar datos con id duplicado -----------------------------------------

cm_real <- cm_real %>% group_by(barra_mnemotecnico,fecha,hora) %>% slice(1)
cm_prog <- cm_prog %>% group_by(mnemotecnico_barra,fecha,hora) %>% slice(1)

# Join --------------------------------------------------------------------

costo_marginal <- cm_real %>% left_join(cm_prog,by=c('barra_mnemotecnico'='mnemotecnico_barra','fecha','hora'))

# Sanity Check ------------------------------------------------------------

nrow(cm_real);nrow(cm_prog);nrow(costo_marginal)

# Analisis descriptivo ----------------------------------------------------

# histogramas 

hist <- function(df,X,title,xname){
 ggplot(df, aes(x=X)) +geom_histogram(color="black", fill="white")+
 ggtitle(paste0("Histograma ",title))+xlab(xname)
}

h1 <- hist(costo_marginal,costo_marginal$costo_en_pesos,"CM real en pesos","costo_en_pesos")
h2 <- hist(costo_marginal,costo_marginal$costo,"CM programado en pesos","costo")
plot_grid(h1,h2)

# summary 
costo_marginal %>% select(costo,costo_en_dolares,costo_en_pesos) %>% summary()

#En particular analiza las barras (barra_mnemotecnico)

dist_barra <- costo_marginal %>% group_by(barra_mnemotecnico) %>% summarise(n = n())

#sumary 
dist_barra$n %>% summary()

#¿Para cuántas barras se programa el costo?

n_costo_prog <- is.na(costo_marginal$costo) %>% sum()
n_costo_prog 

#¿Qué porcentaje es del total de barras que puedes observar en la base?

n_costo_prog/nrow(costo_marginal)

# - -----------------------------------------------------------------------
# P2 - Construccion de variables ------------------------------------------
# - -----------------------------------------------------------------------

# Construccion de variables  ----------------------------------------------

costo_marginal <- costo_marginal %>% mutate(desviacion=(costo_en_pesos-costo),
                                           desviacion_pct=(desviacion/costo_en_pesos),
                                           desviacion_cat=ifelse(abs(desviacion_pct)>0.15,1,0) %>% as.factor())

# Analisis descriptivo desviacion_cat -------------------------------------

# construccion variables de utilidad
costo_marginal <- costo_marginal %>% mutate(camada=as.Date(fecha,'%Y-%m-%d') %>% format('%Y-%m-01') %>% as.Date(), 
                                           date_ymdh=as.POSIXct(as.character(fecha),"UTC")+hours(hora))

#plot 1
df_plot <- costo_marginal %>% na.omit() %>% group_by(camada,desviacion_cat) %>% summarise(n=n()) %>% ungroup()
p1 <- ggplot(df_plot, aes(fill=desviacion_cat, y=n, x=camada)) + geom_bar(position="stack", stat="identity")+ggtitle("N desviacion cat por camada")
p1

#plot 2
df_plot <- costo_marginal %>% mutate(x=1,desviacion_cat=paste0("desv_",desviacion_cat)) 
df_plot <- dcast(df_plot, fecha ~ desviacion_cat, value.var = "x" ,fun.aggregate=sum,na.rm=TRUE)
df_plot <- df_plot %>% mutate(p=(desv_0/(desv_1+desv_0)))
p2 <- ggplot(df_plot , aes(x=fecha, y=p)) + geom_line() + ggtitle(" Serie de Tiempo prop var absoluta menor al 15%")
p2

# - -----------------------------------------------------------------------
# Visualiazacion ----------------------------------------------------------
# - -----------------------------------------------------------------------

time_plot_costo_barra <- function(codigo_barra,fecha_inicial,fecha_final,df=costo_marginal){
  df <- df %>% filter(barra_mnemotecnico==codigo_barra & date_ymdh>=as.Date(fecha_inicial) & date_ymdh<=as.Date(fecha_final)) 
  df <- df %>% select(date_ymdh,costo,costo_en_pesos) %>% gather("cm", "value", -date_ymdh) 
  ggplot(df, aes(x=date_ymdh, y=value, col=cm)) + geom_line() + ggtitle("Serie de tiempo costo marginal real y programado")
}

codigos <- c("BA01T002SE036T002","BA02T003SE004T003","BA83L131SE134L131","BA01G004SE008G004")
plots <- map(codigos,~time_plot_costo_barra(.x,"2019-01-24","2019-07-01"))

plot_grid(plots[[1]],plots[[2]],plots[[3]],plots[[4]])









