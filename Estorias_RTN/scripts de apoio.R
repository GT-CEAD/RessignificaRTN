####################################Início carga inicial

library(xlsx)
library(XLConnect)
library(tidyr)
library(RCurl)


URL_add<-"http://www.tesourotransparente.gov.br/ckan/dataset/ab56485b-9c40-4efb-8563-9ce3e1973c4b/resource/527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6/download/Anexos-RTN-Dez-2017.xlsx"

tmp = paste(getwd(),"temp.xlsx")


tmp = tempfile(fileext = ".xlsx")



download.file(URL_add,mode = "wb", destfile = tmp)
rtn_geral <- read.xlsx(tmp, sheetIndex=2, rowIndex = c(5:79))
rtn_receita<-read.xlsx(tmp, sheetIndex=4, rowIndex = c(5:53))
rtn_despesa<- read.xlsx(tmp, sheetIndex=8, rowIndex = c(5:91))
deflator_IPCA <- read.xlsx(tmp,sheetIndex=5, rowIndex = c(55:55),header=FALSE) #vai ser sempre a última linha de receita + 2
names(deflator_IPCA)<-names(rtn_receita)


URL_add<-"http://www.tesourotransparente.gov.br/ckan/dataset/ab56485b-9c40-4efb-8563-9ce3e1973c4b/resource/7a535375-4e15-4ebb-bb49-25daf05330bb/download/Metadados---NFGC.xlsx"

tmp = paste(getwd(),"temp.xlsx")


tmp = tempfile(fileext = ".xlsx")

download.file(URL_add,mode = "wb", destfile = tmp)


metadados_rec<- read.xlsx(tmp, sheetIndex=4, startRow = 5, colIndex = c(2,3))
metadados_desp<-read.xlsx(tmp, sheetIndex=8, startRow = 5, colIndex = c(1,2))
names(metadados_rec)<-c("Rubrica","Descricao")
names(metadados_desp) <- names(metadados_rec)
metadados_completo<- rbind(metadados_rec,metadados_desp)





names(rtn_geral)[1]<-"Rubrica"
series_temporais_analise<-gather(rtn_geral,Data, Valor,-Rubrica)
series_temporais_analise$Data<-gsub("X","",series_temporais_analise$Data)
series_temporais_analise$Data<-as.Date(as.numeric(series_temporais_analise$Data), origin="1899-12-30")
series_temporais_analise$Valor <-as.numeric(series_temporais_analise$Valor)
series_temporais_analise$Valor[is.na(series_temporais_analise$Valor)]<-0
rm(rtn_geral)


names(rtn_receita)[1]<-"Rubrica"
series_temporais_analise_rec<-gather(rtn_receita,Data, Valor,-Rubrica)
series_temporais_analise_rec$Data<-gsub("X","",series_temporais_analise_rec$Data)
series_temporais_analise_rec$Data<-as.Date(as.numeric(series_temporais_analise_rec$Data), origin="1899-12-30")
series_temporais_analise_rec$Valor <-as.numeric(series_temporais_analise_rec$Valor)
series_temporais_analise_rec$Valor[is.na(series_temporais_analise_rec$Valor)]<-0
rm(rtn_receita)

names(rtn_despesa)[1]<-"Rubrica"
series_temporais_analise_desp<-gather(rtn_despesa,Data, Valor,-Rubrica)
series_temporais_analise_desp$Data<-gsub("X","",series_temporais_analise_desp$Data)
series_temporais_analise_desp$Data<-as.Date(as.numeric(series_temporais_analise_desp$Data), origin="1899-12-30")
series_temporais_analise_desp$Valor <-as.numeric(series_temporais_analise_desp$Valor)
series_temporais_analise_desp$Valor[is.na(series_temporais_analise_desp$Valor)]<-0
rm(rtn_despesa)

names(deflator_IPCA)[1]<-"Rubrica"
series_temporais_analise_IPCA<-gather(deflator_IPCA,Data, Valor,-Rubrica)
series_temporais_analise_IPCA$Data<-gsub("X","",series_temporais_analise_IPCA$Data)
series_temporais_analise_IPCA$Data<-as.Date(as.numeric(series_temporais_analise_IPCA$Data), origin="1899-12-30")
series_temporais_analise_IPCA$Valor <-as.numeric(series_temporais_analise_IPCA$Valor)
rm(deflator_IPCA)





save(list = ls(),file = "todas_series.Rdata")
rm(series_temporais_analise)


################################ Fim carga inicial


################################ Início tratamento RP
load("todas_series.Rdata")
serie_trabalho <- series_temporais_analise[grep("VI. PRIM",series_temporais_analise$Rubrica),]
serie_trabalho <- rbind(serie_trabalho,series_temporais_analise[grep("III. RECEITA",series_temporais_analise$Rubrica),])
serie_trabalho <- rbind(serie_trabalho,series_temporais_analise[grep("IV. DESPESA TOTAL",series_temporais_analise$Rubrica),])
serie_trabalho <- rbind(serie_trabalho,series_temporais_analise[grep("V. FUNDO SOBERANO DO BRASIL",series_temporais_analise$Rubrica),])

rm(series_temporais_analise) 

ano_fim<-as.numeric(substr(serie_trabalho$Data[NROW(serie_trabalho)],1,4))
mes_fim<-as.numeric(substr(serie_trabalho$Data[NROW(serie_trabalho)],6,7))
ts_serie_trabalho_prim <- ts(serie_trabalho$Valor[grep("VI. PRIM",serie_trabalho$Rubrica)],start = c(1997,1),end = c(ano_fim,mes_fim),frequency = 12)
ts_serie_trabalho_rec  <- ts(serie_trabalho$Valor[grep("III. RECEITA",serie_trabalho$Rubrica)],start = c(1997,1),end = c(ano_fim,mes_fim),frequency = 12)
ts_serie_trabalho_desp  <- ts(serie_trabalho$Valor[grep("IV. DESPESA TOTAL",serie_trabalho$Rubrica)],start = c(1997,1),end = c(ano_fim,mes_fim),frequency = 12)
ts_serie_trabalho_fsb  <- ts(serie_trabalho$Valor[grep("V. FUNDO SOBERANO DO BRASIL",serie_trabalho$Rubrica)],start = c(1997,1),end = c(ano_fim,mes_fim),frequency = 12)
ts_serie_trabalho_fsb[ts_serie_trabalho_fsb==0]<-NA
save(list = ls(),file = "serie_trabalho_prim.Rdata")
y_min <- min(as.numeric(serie_trabalho$Valor))
y_max <- max(as.numeric(serie_trabalho$Valor))
rm(serie_trabalho)
series <-cbind(ts_serie_trabalho_prim,ts_serie_trabalho_rec,ts_serie_trabalho_desp,ts_serie_trabalho_fsb)

library(dygraphs) #Série temporal dinâmica


dygraph(series,main = "Resultado Primário - Governo central") %>%
  dyRangeSelector() %>%
  dyAxis(name= "y",valueRange = c(y_min,y_max)) %>%
  dySeries(label = "RP") %>%
  dySeries(label = "RL") %>%
  dySeries(label = "DT") %>%
  dySeries(label = "FSB") %>%
  dyLegend(show = "always", hideOnMouseOut = TRUE, width = 400)%>%
  dyOptions(connectSeparatedPoints = TRUE)%>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE)

load("serie_trabalho_prim.Rdata")
write.csv2(serie_trabalho, "./file.csv")

library(magrittr)
readLines("./file.csv") %>% 
  paste0(collapse="\n") %>% 
  openssl::base64_encode() -> encoded
rm(serie_trabalho)



################################# Início decomposição de série RP
ygraph(ts_serie_trabalho_prim,main = "Resultado Primário - Governo central") %>%
  dyRangeSelector() %>%
  dyAxis(name= "y",valueRange = c(min(as.numeric(ts_serie_trabalho_prim)),max(as.numeric(ts_serie_trabalho_prim))))

grafico<-ts_serie_trabalho_prim %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) 

par(bg="#b1e3ef")

plot(grafico)


################################Fim decomposição de série RP


################################ início visualização por seleção de séries RP por decomposição
GraficoSerieRTN <- function(rubrica, series_temporais_analise){
  
  library(dygraphs)
  
  
  mes_fim<-as.numeric(substr(series_temporais_analise$Data[nrow(series_temporais_analise)],6,7))
  ano_fim<-as.numeric(substr(series_temporais_analise$Data[nrow(series_temporais_analise)],1,4))
  ts_serie_trabalho<- ts(series_temporais_analise$Valor[grep(rubrica,series_temporais_analise$Rubrica)],start = c(1997,1),end = c(ano_fim,mes_fim),frequency = 12)
  
  
  
  dygraph(ts_serie_trabalho,main = rubrica) %>%
    dyRangeSelector() %>%
    dyAxis(name= "y",valueRange = c(min(as.numeric(ts_serie_trabalho)),max(as.numeric(ts_serie_trabalho))))
  
  
}

load("todas_series.Rdata")
varNames <- series_temporais_analise$Rubrica[which(!duplicated(series_temporais_analise$Rubrica))]


library(dygraphs)

dygraph(
  ts(series_temporais_analise$Valor[grep(input$rubrica,series_temporais_analise$Rubrica)],start = c(1997,1),end = c(ano_fim,mes_fim),frequency = 12),
        main = input$rubrica) %>%
  dyRangeSelector() %>%
  dyAxis(name= "y",valueRange = c(min(as.numeric(series_temporais_analise$Valor[grep(input$rubrica,series_temporais_analise$Rubrica)])),max(as.numeric(series_temporais_analise$Valor[grep(input$rubrica,series_temporais_analise$Rubrica)]))))


library(shiny)

graficoSTL<- function(rubrica, series_temporais_analise){
  library(tseries)
  
  mes_fim<-as.numeric(substr(series_temporais_analise$Data[nrow(series_temporais_analise)],6,7))
  ano_fim<-as.numeric(substr(series_temporais_analise$Data[nrow(series_temporais_analise)],1,4))
  
  
  ts_serie_stl <- ts(series_temporais_analise$Valor[grep(rubrica,series_temporais_analise$Rubrica)],start = c(1997,1),end = c(ano_fim,mes_fim),frequency = 12)
  
  #testa se a série é ou não estacionária
  teste_estacionario<-adf.test(ts_serie_stl, alternative = "stationary")
  if(teste_estacionario$p.value>=0.05){
    grafico<-ts_serie_stl %>%
      stl(t.window=13, s.window="periodic", robust=TRUE) 
  } else {
    grafico<-ts_serie_stl
    
    }
      
  par(bg="#b1e3ef")
  plot(grafico)
  
  
}

###############################Inicio visualização de séries RTN por seleçÕ
shinyApp(
  
  ui = fluidPage(
    selectInput("rubrica", "Rubricas",
                choices = c("Resultado Primário"= "VI. PRIM",
                            "Receita Líquida" = "III. RECEITA",
                            "Despesa Total" = "IV. DESPESA TOTAL",
                            "Fundo Soberando" = "V. FUNDO SOBERANO DO BRASIL"
                )),
    plotOutput("stlPlot")
  ),
  
  server = function(input, output) {
    output$stlPlot <- renderPlot({
      graficoSTL("VI. PRIM",series_temporais_analise)
    })
  },
  
  options = list(height = 500)
)

graficoSTL("V. FUNDO SOBERANO DO BRASIL",series_temporais_analise)


ts_serie_stl <- ts(series_temporais_analise$Valor[grep("IV. DESPESA TOTAL",series_temporais_analise$Rubrica)],start = c(1997,1),end = c(ano_fim,mes_fim),frequency = 12)

library(tseries)

teste_estacionario<-adf.test(ts_serie_stl, alternative = "stationary")
teste_estacionario$p.value

  
)

###############################Fim visualização de séries RTN por seleçÕ


################################## Início de funções diversas

FiltraSeries <- function(series_temporais_analise, rubricas, freq, avalia_parc=TRUE){
  library(lubridate)

  for (i in c(1:length(rubricas))) {

    if (avalia_parc) {
      serie_trabalho <- series_temporais_analise[grep(rubricas[i],series_temporais_analise$Rubrica),]
    }
    else {
      serie_trabalho <- series_temporais_analise[series_temporais_analise$Rubrica == rubricas[i],]
    }
    
    ano_ini <- as.numeric(substr(serie_trabalho$Data[1],1,4))
    mes_ini <- as.numeric(substr(serie_trabalho$Data[1],6,7))
    ano_fim<-as.numeric(substr(serie_trabalho$Data[NROW(serie_trabalho)],1,4))
    mes_fim<-as.numeric(substr(serie_trabalho$Data[NROW(serie_trabalho)],6,7))
    
    
    if (freq %in% c(12)){ #mensal
      per_ini <- mes_ini
      per_fim<-mes_fim
      
    }
    
    if (freq %in% c(1)){ #anual
      per_ini <- 1
      per_fim<-1
      
    }
    
    

    if (freq==4){#Trimestral
      per_ini<- as.numeric(substr(quarters(as.Date(serie_trabalho$Data[1])), 2, 2))
      per_fim<- as.numeric(substr(quarters(as.Date(serie_trabalho$Data[NROW(serie_trabalho)])), 2, 2))
    }
    
    if (freq==2){#Semestral
      per_ini<- semester(as.Date(serie_trabalho$Data[1]))
      per_fim<- semester(as.Date(serie_trabalho$Data[NROW(serie_trabalho)]))
    }
    
    
    
    if (i==1){
      series<- ts(serie_trabalho$Valor,start = c(ano_ini,per_ini),end = c(ano_fim,per_fim),frequency = freq)#,class="mts"
  
    } else {
      series<- cbind(series,ts(serie_trabalho$Valor,start = c(ano_ini,per_ini),end = c(ano_fim,per_fim),frequency = freq))}

  }
  if (length(rubricas)>1) {colnames(series)<-rubricas}
  series
}
  


RotulaSeries<- function(graph,rotulos){
  str<-""
  for (rotulo in rotulos) {
    str<-paste0(str,"%>% dySeries(label ='", rotulo,"')")
    
  }
  
  view_graph<-paste0(graph,str)
  view_graph
  
}


GraficoSeries <- function(series,rubricas,rotulos, nome_grafico){
  library(dygraphs) #Série temporal dinâmica
  
  y_min <- min(as.numeric(series))
  y_max <- max(as.numeric(series))
  
  series[series==0]<-NA
  
  
  # dygraph(series,main = nome_grafico) %>%  
  #       dyRangeSelector() %>%  
  #       dyAxis(name= 'y',valueRange = c(y_min,y_max)) %>%  
  #      dyLegend(show = 'follow', hideOnMouseOut = TRUE, width = 400)%>%  
  #      dyOptions(connectSeparatedPoints = TRUE)%>%  
  #       dyHighlight(highlightCircleSize = 5)
  # 
  
  graph<-paste0("dygraph(series,main = nome_grafico) %>%  dyRangeSelector() %>%  dyAxis(name= 'y',valueRange = c(",y_min,",",y_max,")) %>%  dyLegend(show = 'follow', hideOnMouseOut = TRUE, width = 400)%>%  dyOptions(connectSeparatedPoints = TRUE) %>% dyOptions( drawGrid = FALSE) %>% dyHighlight(highlightCircleSize = 5)")
  eval(parse(text=RotulaSeries(graph,rotulos)))
}


ExtraiRubricasRaiz <- function(tipo_fluxo=3){
  load("todas_series.RData")
  if (tipo_fluxo == 1)  {serie<-series_temporais_analise_rec}
  if (tipo_fluxo == 2)  {serie<-series_temporais_analise_desp}
  if (tipo_fluxo == 3)  {serie<-rbind(series_temporais_analise_rec,series_temporais_analise_desp)}
  
  rubricas<-c(as.character(unique(serie$Rubrica)))
  rubrica_raiz <- c(numeric())
  
  for (i in 1:(length(rubricas)-1)){
    codigo_corr<- strsplit(rubricas[i]," ")
    codigo_corr<- codigo_corr[[1]][1]
    ult_car<-substr(codigo_corr,nchar(codigo_corr),nchar(codigo_corr))
    if( ult_car!="."){
      codigo_prox<- strsplit(rubricas[i+1]," ") 
      codigo_prox<- codigo_prox[[1]][1]
      nivel_corr<-sum(unlist(strsplit(codigo_corr,""))==".")
      nivel_prox<-sum(unlist(strsplit(codigo_prox,""))==".")
      if (nivel_corr >= nivel_prox){
        rubrica_raiz <- append(rubrica_raiz,rubricas[i])
        
      }
      
    }
    
    
  }
  if (tipo_fluxo!=1){rubrica_raiz <- append(rubrica_raiz,rubricas[length(rubricas)])}
  rubrica_raiz
  
}

calcula_valor_indexado<- function(serie_nominal,serie_indexador){
  join<-merge(x=serie_nominal, y=serie_indexador,by.x = c("Data"), by.y = c("Data"))
  join$valor<- join$Valor.x*join$Valor.y
  join<-join[,c(2,1,6)]
  names(join)<-names(serie_nominal)
  
  join
  
}

Trata_Outlier <- function(series){
  library(strucchange)
  library(forecast)
  
  for (s in(1:NCOL(series))){
    serie_trabalho<- series[,s]
    #Verifica os pontos que são outliers
    
    #Verifica se os pontos que são outliers são também outliers em uma estrutura específica. Se fizerem serão descartados
    serie_cus <- efp(serie_trabalho ~ 1, type = "Rec-CUSUM")
    bound <- boundary(serie_cus, alpha = 0.01)
    
    true_outlier <- c(numeric())
    #Trata estruturas acima de uma linha de fronteira
    pontos<-which(serie_cus$process>=bound)
    if (length(pontos)>0){
      q<- quantile(serie_trabalho[pontos])
      vec_outlier<- pontos[which(serie_trabalho[pontos]>q[4]*1.5)]
      if (length(vec_outlier)>0) {true_outlier<-c(true_outlier,vec_outlier)}
      vec_outlier<- pontos[which(serie_trabalho[pontos]<q[2]*0.5)]
      if (length(vec_outlier)>0) {true_outlier<-c(true_outlier,vec_outlier)}
      
    }
    
    
    #Trata estruturas abaixo de uma linha de fronteira
    pontos<-which(serie_cus$process<=-bound)
    if (length(pontos)>0){
      q<- quantile(serie_trabalho[pontos])
      vec_outlier<- pontos[which(serie_trabalho[pontos]>q[4]*1.5)]
      if (length(vec_outlier)>0) {true_outlier<-c(true_outlier,vec_outlier)}
      vec_outlier<- pontos[which(serie_trabalho[pontos]<q[2]*0.5)]
      if (length(vec_outlier)>0) {true_outlier<-c(true_outlier,vec_outlier)}
      
    }
    
    #Trata estruturas entre duas linhas de fronteira
    pontos<-which(serie_cus$process>=-bound&serie_cus$process<=bound)
    if (length(pontos)>0){
      q<- quantile(serie_trabalho[pontos])
      vec_outlier<- pontos[which(serie_trabalho[pontos]>q[4]*1.5)]
      if (length(vec_outlier)>0) {true_outlier<-c(true_outlier,vec_outlier)}
      vec_outlier<- pontos[which(serie_trabalho[pontos]<q[2]*0.5)]
      if (length(vec_outlier)>0) {true_outlier<-c(true_outlier,vec_outlier)}
      
    }
    
    if (length(true_outlier)>0){
      true_outlier<-true_outlier [order(true_outlier)]
      
      
      for (i in (1:length(true_outlier))){
        if (length(serie_trabalho[1:true_outlier[i]-1])>0){
          fit<-auto.arima(serie_trabalho[1:true_outlier[i]-1])
          serie_trabalho[true_outlier[i]] <- forecast(fit,h=1)$mean
          
        }
        
      }
      
      
    }
    
    if (s==1) {
      series_ajustadas<-serie_trabalho}
    else {
      series_ajustadas<-cbind(series_ajustadas,serie_trabalho)}
    #print(s)
    
    
    
  }
  
  colnames(series_ajustadas)<-colnames(series)
  series_ajustadas
}


Identifica_series_estacionarias<- function(series){
  library(stats)
  
  nomes_series<-c(character())
  vazio<-TRUE
  for (i in (1:NCOL(series))){
    #Verifica se tem ou não raiz unitária pelo método Phillips-Perron para as séries que não estão completamente zeradas
    if (sum(series[,i])>0){
      res_test<-PP.test(series[,i]) 
      #Somente as séries que não têm raiz unitária serão incluídas na lista
      if (res_test$p.value <= 0.01){
        nomes_series<-c(nomes_series,colnames(series)[i])
        if (vazio){
          
          series_estacionarias<-series[,i]
          vazio<-FALSE
        } else{
          
          series_estacionarias<-cbind(series_estacionarias,series[,i])}
      }
    }
  }
  colnames(series_estacionarias)<-nomes_series
  series_estacionarias
}

acumula_12_meses <- function(series_temporais,rubricas,avalia_parc){
  library(zoo)
  
  serie_soma_12<- FiltraSeries(series_temporais,rubricas, 12,avalia_parc)
  serie_soma_12<-rollsum(serie_soma_12,12,align = "right")
  serie_soma_12
  
  
}



acumula_periodo<- function(series_temporais, periodo="a"){
  series_temporais$ano <- substr(series_temporais$Data,1,4)
  series_temporais$mes <- substr(series_temporais$Data,6,7)
  
  
  if (periodo =="a"){
    serie_acumulada<-series_temporais %>% 
      group_by(Rubrica, ano) %>%
      mutate(acum=cumsum(Valor))
    serie_acumulada$Valor<-serie_acumulada$acum
    
    
  }
  
  if (periodo=="s") {
    
    series_temporais$semestre<-NA
    series_temporais$semestre[which(series_temporais$mes %in% c("01","02","03","04","05","06"))]<-"1"
    series_temporais$semestre[which(series_temporais$mes %in% c("07","08","09","10","11","12"))]<-"2"
    
    serie_acumulada<-series_temporais %>% 
      group_by(Rubrica, ano, semestre) %>%
      mutate(acum=cumsum(Valor))
    serie_acumulada$Valor<-serie_acumulada$acum
    
  }
  
  
  if (periodo=="t") {
    
    series_temporais$trimestre<-NA
    series_temporais$trimestre[which(series_temporais$mes %in% c("01","02","03"))]<-"1"
    series_temporais$trimestre[which(series_temporais$mes %in% c("04","05","06"))]<-"2"
    series_temporais$trimestre[which(series_temporais$mes %in% c("07","08","09"))]<-"3"
    series_temporais$trimestre[which(series_temporais$mes %in% c("10","11","12"))]<-"3"
    
    serie_acumulada<-series_temporais %>% 
      group_by(Rubrica, ano, trimestre) %>%
      mutate(acum=cumsum(Valor))
    serie_acumulada$Valor<-serie_acumulada$acum
    
    
  }
  
  
  serie_acumulada<-serie_acumulada[,c(1:3)]
  
  serie_acumulada
}


Destaca_Periodo_Acumulado <- function(serie_trabalho,tipo_per){
  serie_acumulada<-acumula_periodo(serie_trabalho,tipo_per)
  
  if (tipo_per=="t"){
    serie_destacada<- serie_acumulada[which(substr(serie_acumulada$Data,6,7)%in%c("03","06","09","12")),]
    
  }
  
  if (tipo_per=="s"){
    serie_destacada<- serie_acumulada[which(substr(serie_acumulada$Data,6,7)%in%c("06","12")),]
    
  }
  
  if (tipo_per=="a"){
    serie_destacada<- serie_acumulada[which(substr(serie_acumulada$Data,6,7)%in%c("12")),]
    
  }
  serie_destacada

}



Grafico_selecao_usuario <- function(serie_trabalho,
                                    tipo_valor,
                                    tipo_per,
                                    rubricas, 
                                    rotulos, 
                                    nome_grafico,
                                    avalia_parc){
  freq<-12
  if (tipo_per=="t") {freq=4}
  if (tipo_per=="s") {freq=2}
  if (tipo_per=="a") {freq=1}
  
  if (length(rubricas)==0){
    rubricas<-serie_trabalho$Rubrica[1]
    rotulos<-rubricas
  }
  
  #Verifica se foi selecionada séries nominal ou indexada
  if (tipo_valor=="2"){
    load("todas_series.Rdata")
    serie_nominal<- serie_trabalho
    serie_indexador<-series_temporais_analise_IPCA
    serie_trabalho<- calcula_valor_indexado(serie_nominal,serie_indexador)
  }
  
  # Verifica se foi selecionado opção mensal, 12 meses, anual, trimestral ou semestral
  if (tipo_per %in% c("m","a","t","s")){
    if (tipo_per!="m") {serie_trabalho<-Destaca_Periodo_Acumulado(serie_trabalho, tipo_per)}
    series_grafico<- FiltraSeries(serie_trabalho,rubricas,freq,avalia_parc)
  }else 
    series_grafico <- acumula_12_meses(serie_trabalho,rubricas,avalia_parc)
  
  
  GraficoSeries(series_grafico,rubricas,rotulos, nome_grafico)
  
}


Busca_Metadados <- function(rubricas,tipo_fluxo){
  if (tipo_fluxo == 1) { #Receitas
    metadados<- metadados_rec
  }
  
  if (tipo_fluxo == 2) { #Despesa
    metadados<- metadados_desp
  }
  
  if (tipo_fluxo == 3) { #Despesa
    metadados<- metadados_completo
  }
  
  metadados_buscados <- metadados[metadados$Rubrica%in%rubricas,]
  metadados_buscados
}




Visualiza_Metadados <- function(rubricas, tipo_fluxo){
  library(DT)
  
  if (length(rubricas)==0){
    if (tipo_fluxo==1){
      rubricas<- metadados_rec$Rubrica[1]
    }
    if (tipo_fluxo==2){
      rubricas<- metadados_desp$Rubrica[1]
    }
    
    
  }
  
  dados_tabela<- Busca_Metadados(rubricas,tipo_fluxo)
  
  datatable(dados_tabela,rownames = FALSE)
  
  
}




################################### Início de tratamento de principais receitas
load("todas_series.Rdata")

rubricas <- c("I.1 -  Receita Administrada pela RFB",
              "I.2 -  Incentivos Fiscais",
              "I.3 -  Arrecada",
              "I.4 -  Receitas N",
              "II. TRANSF. POR REPARTI")
rotulos<-c("RARFB","IF","ALRFB","RNARFB","TRR")


freq<-12

nome_grafico<-"Principais Receitas"


series<-FiltraSeries(series_temporais_analise, rubricas, freq )  



GraficoSeries(series,rubricas,rotulos, nome_grafico)

################################### Fim de tratamento de principasi receitas


################################### Início de tratamento de mapa de calor
num_meses<-12
tipo_fluxo=1#1 para receita, 2 para despesa, 3 para ambos


avalia_parc<-FALSE




rubricas<-ExtraiRubricasRaiz(1)



series<- FiltraSeries(series_temporais_analise_rec,rubricas,12,avalia_parc=FALSE)

series<- Identifica_series_estacionarias(series)
series<- Trata_Outlier (series)



serie_totalizadora <-FiltraSeries(series_temporais_analise_rec,"I. RECEITA TOTAL",12,avalia_parc)


vazio<- TRUE
nomes<- c(character())
for (i in 1:NCOL(series)){
  
  stl<-  stl(series[,i],t.window=13, s.window="periodic", robust=TRUE)
  if (sum(stl$time.series[,2]) >0){
    nomes<- c(nomes,colnames(series)[i])

    if (vazio){
      vazio<-FALSE
      decomp<-stl$time.series[,2]
      peso <- (series[,i]/serie_totalizadora)
    } else {
      decomp<- cbind(decomp,stl$time.series[,2])
      peso  <- cbind(peso,(series[,i]/serie_totalizadora))}
      
    }

}
    

diferencial<-(diff(decomp)/decomp)*peso
colnames(diferencial)<- nomes


ult_valor <- data.frame(ordem_orig = c(1:NCOL(diferencial)), valor=as.numeric(diferencial[NROW(diferencial),]))
ordem<- sort(ult_valor$valor,decreasing = TRUE)
ult_valor <- ult_valor[order(ult_valor[2]),]

diferencial<-diferencial[, c(ult_valor$ordem_orig)]


library(reshape2)
library(ggplot2)





diferencial <- ts(diferencial[c((NROW(diferencial)-(num_meses-1)):NROW(diferencial)),],start = c(2017,1),end = c(2017,12),frequency = freq)

for (i in 1:NCOL(diferencial)){
  if (i==1){
    df_series_trabalho<-data.frame(Rubricas=colnames(diferencial)[i],Data=as.character(as.yearmon(time(diferencial[,i])),format="%Y/%m"), Variacao= as.numeric(diferencial[,i]*100))
    
  } else{
    df_series_trabalho<- rbind(df_series_trabalho,data.frame(Rubricas=colnames(diferencial)[i],Data=as.character(as.yearmon(time(diferencial[,i])),format="%Y/%m"), Variacao= as.numeric(diferencial[,i]*100)))

  }
}

graph<-ggplot(df_series_trabalho, aes(df_series_trabalho$Data,df_series_trabalho$Rubricas )) +
  geom_tile(aes(fill = Variacao), color = "white") +
  scale_fill_gradient2(low = "red", high = "steelblue") +
  ylab("Rubricas ") +
  xlab("Data") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(fill = "Variacao (%)")

library(plotly)

ggplotly(graph)


################################### Final de tratamento de mapa de calor




###############################Aplicativo shiny para visualização do Resultado primário
library(shiny)
load("todas_series.Rdata")
shinyApp(
  
  ui = fluidPage(
    sidebarPanel(
      selectInput("rubrica", "Rubricas",
                  choices = c("Resultado Primário"= "VI. PRIM",
                              "Receita Líquida" = "III. RECEITA",
                              "Despesa Total" = "IV. DESPESA TOTAL")
      ),
      selectInput("tipoValor", "Tipo Valor",
                  choices = c("Nominal"= "1",
                              "Indexado IPCA"="2")),
      selectInput("tipoPeriodo", "Mensal acumulado por",
                  choices = c("Mês"= "m",
                              "12 meses"="12",
                              "Trimestre"="t",
                              "Ano" ="a"))
      
    ),
    
    mainPanel(
      dygraphOutput("Grafico_selecao_usuario")
    )
    
  ),
  
  server = function(input, output) {
    output$Grafico_selecao_usuario <- renderDygraph({
      Grafico_selecao_usuario(series_temporais_analise,
                    input$tipoValor,
                    input$tipoPeriodo,
                    input$rubrica,
                    input$rubrica,
                    "Análise Resultado Primário",
                    TRUE)
    })
  },
  
  options = list(height = 500)
)


###############################Aplicativo shiny para visualização de Todas Receitas
library(shiny)
load("todas_series.Rdata")
shinyApp(
  
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("rubrica", "Rubricas",
                    choices = unique(series_temporais_analise_rec$Rubrica),
                    multiple = TRUE,            
                    selected = series_temporais_analise_rec$Rubrica[1]
        ),
        selectInput("tipoValor", "Tipo Valor",
                    choices = c("Nominal"= "1",
                                "Indexado IPCA"="2")),
        selectInput("tipoPeriodo", "Tipo Período",
                    choices = c("Mensal"= "m",
                                "Acumulado 12 meses"="12",
                                "Trimeste"="t",
                                "Ano" ="a"))
        
      ),
      
      mainPanel(
        dygraphOutput("Grafico_selecao_usuario"),
        dataTableOutput("Visualiza_Metadados")
      )
      
      
    )
    
  ),
  
  server = function(input, output) {
    output$Grafico_selecao_usuario <- renderDygraph({
      Grafico_selecao_usuario(series_temporais_analise_rec,
                              input$tipoValor,
                              input$tipoPeriodo,
                              input$rubrica,
                              input$rubrica,
                              "Análise Receitas",
                              FALSE)
      })
      output$Visualiza_Metadados <- renderDataTable({
        Visualiza_Metadados(input$rubrica,1)
      })
      
    
  },
  
  options = list(height = 500)
)


########################

###############################Aplicativo shiny para visualização de Todas Despesas
library(shiny)
load("todas_series.Rdata")
shinyApp(
  
  ui = fluidPage(
    sidebarLayout(    sidebarPanel(
      selectInput("rubrica", "Rubricas",
                  choices = unique(series_temporais_analise_desp$Rubrica),
                  multiple = TRUE,
                  selected = series_temporais_analise_desp$Rubrica[1]
      ),
      selectInput("tipoValor", "Tipo Valor",
                  choices = c("Nominal"= "1",
                              "Indexado IPCA"="2")),
      selectInput("tipoPeriodo", "Mensal acumulado por",
                  choices = c("Mês"= "m",
                              "12 meses"="12",
                              "Trimeste"="t",
                              "Ano" ="a"))
      
    ),
    
    mainPanel(
      dygraphOutput("Grafico_selecao_usuario"),
      dataTableOutput("Visualiza_Metadados")
      
    )
    
      
    )
    
  ),
  
  server = function(input, output) {
    output$Grafico_selecao_usuario <- renderDygraph({
      Grafico_selecao_usuario(series_temporais_analise_desp,
                              input$tipoValor,
                              input$tipoPeriodo,
                              input$rubrica,
                              input$rubrica,
                              "Análise Despesas",
                              FALSE)
    })
    output$Visualiza_Metadados <- renderDataTable({
      Visualiza_Metadados(input$rubrica,2)
    })
    
  },
  
  options = list(height = 500)
)


########################


library(shiny)

graficoSTL<- function(rubrica, series_temporais_analise){
  
  library(tseries)
  
  mes_fim<-as.numeric(substr(series_temporais_analise$Data[nrow(series_temporais_analise)],6,7))
  ano_fim<-as.numeric(substr(series_temporais_analise$Data[nrow(series_temporais_analise)],1,4))
  
  
  ts_serie_stl <- ts(series_temporais_analise$Valor[grep(rubrica,series_temporais_analise$Rubrica)],start = c(1997,1),end = c(ano_fim,mes_fim),frequency = 12)
  
  #testa se a série é ou não estacionária
  teste_estacionario<-adf.test(ts_serie_stl, alternative = "stationary")
  if(teste_estacionario$p.value>=0.05){
    grafico<-ts_serie_stl
    
  } else {
    grafico<-ts_serie_stl %>%
      stl(t.window=13, s.window="periodic", robust=TRUE) 
    
    
  }
  
  par(bg="#FFFFFF")
  plot(grafico)
  
  
}

shinyApp(
  
  ui = fluidPage(
    selectInput("rubrica", "Rubricas",
                choices = c("Resultado Primário"= "VI. PRIM",
                            "Receita Líquida" = "III. RECEITA",
                            "Despesa Total" = "IV. DESPESA TOTAL",
                            "Fundo Soberano" = "V. FUNDO SOBERANO DO BRASIL"
                )),
    plotOutput("stlPlot")
  ),
  
  server = function(input, output) {
    output$stlPlot <- renderPlot({
      graficoSTL(input$rubrica,series_temporais_analise)
    })
  },
  
  options = list(height = 500)
)


library(ckanr)

changes()
changes(as =
          '
        json
        '
)
fab<-changes(as =
          '
        table
        '
)


ckanr_setup(url = "http://demo.ckan.org/", key = getOption("ckan_demo_key"))
(resrcs <- resource_search(q ='name:data'))
resrcs$results
resrcs$results[[3]]

# create item class from only an item ID
as.ckan_resource(resrcs$results[[3]]$id)
# gives back itself
(x <- as.ckan_resource(resrcs$results[[3]]$id))
as.ckan_resource(x)


tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://www.tesourotransparente.gov.br/ckan")
fab$url
fab$last_modified



# eg. from the NHM CKAN store
resource_show(id = "05ff2255-c38a-40c9-b657-4ccb55ab2feb",
              url = "http://data.nhm.ac.uk")

ping("www.tesourotransparente.gov.br")


ckanr_setup(url = "https://www.tesourotransparente.gov.br")

resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6")



# Library
library(tidyverse)

# Create data
set.seed(1000)
data=data.frame(x=LETTERS[1:26], y=abs(rnorm(26)))

# Reorder the data
data=data %>%
  arrange(y) %>%
  mutate(x=factor(x,x))

rubricas<-ExtraiRubricasRaiz(1)
rubricas<-rubricas[-grep("II.",rubricas)]

rubrica_sel <- "I.1.6    PIS/PASEP"
data_sel <- "2016-10-01"

data<- series_temporais_analise_rec[series_temporais_analise_rec$Rubrica %in% rubricas & 
                                    series_temporais_analise_rec$Data== data_sel,]

serie_totalizadora <- series_temporais_analise_rec[series_temporais_analise_rec$Rubrica == "I. RECEITA TOTAL" & 
                                                     series_temporais_analise_rec$Data== data_sel,]

data$Valor <- (data$Valor/serie_totalizadora$Valor)*100

data=data %>%
  arrange(Valor) %>%
  mutate(Rubrica=factor(Rubrica,Rubrica))

# Plot
png("~/Dropbox/R_GG/R_GRAPH/#304_Lollipop_with_highlighted_group.png", height = 480, width=480)
p = ggplot(data, aes(x=data$Rubrica, y=data$Valor)) +
  geom_segment( aes(x=data$Rubrica, xend=data$Rubrica, y=0, yend=data$Valor ), color=ifelse(data$Rubrica %in% rubrica_sel, "orange", "grey"), size=ifelse(data$Rubrica %in% rubrica_sel, 1.3, 0.7) ) +
  geom_point( color=ifelse(data$Rubrica %in% rubrica_sel, "orange", "grey"), size=ifelse(data$Rubrica %in% rubrica_sel, 5, 2) ) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("% de Receita Total")
library(plotly)

ggplotly(p)

p
dev.off()

# You can even add a few annotation on the plot !
png("~/Dropbox/R_GG/R_GRAPH/#304_Lollipop_with_highlighted_group2.png", height = 480, width=480)
p + 
  annotate("text", x = grep("D", data$x), y = data$y[which(data$x=="D")]*1.2, label = "Group D is very impressive", color="orange", size=4 , angle=0, fontface="bold", hjust=0) + 
  annotate("text", x = grep("A", data$x), y = data$y[which(data$x=="A")]*1.2, label = paste("Group A is not too bad\n (val=",data$y[which(data$x=="A")] %>% round(2),")",sep="" ) , color="orange", size=4 , angle=0, fontface="bold", hjust=0) +
  ggtitle("How did groups A and D perform?")
