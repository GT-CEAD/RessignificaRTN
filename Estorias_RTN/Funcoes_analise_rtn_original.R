
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
  library(magrittr)
  str<-""
  for (rotulo in rotulos) {
    str<-paste0(str,"%>% dySeries(label ='", rotulo,"')")
    
  }
  
  view_graph<-paste0(graph,str)
  view_graph
  
}


GraficoSeries <- function(series,rubricas,rotulos, nome_grafico,width=NULL,height = NULL){
  library(magrittr)
  library(dygraphs) #Série temporal dinâmica
  library(lubridate)
  
  y_min <- min(as.numeric(series))
  y_max <- max(as.numeric(series))
  
  series[series==0]<-NA
  
  
  dygraph(series,main = nome_grafico, width="100%", height = height) %>%
    dyRangeSelector() %>%
    dyAxis(name= 'y',valueRange = c(y_min,y_max*1.05)) %>%
    dyLegend(show = 'follow', hideOnMouseOut = TRUE)%>%
    dyOptions(connectSeparatedPoints = TRUE)%>%
    dyOptions( drawGrid = FALSE) %>%
    dyHighlight(highlightCircleSize = 5)
  # 
  
  # graph<-paste0("dygraph(series,main = nome_grafico) %>%  dyRangeSelector() %>%  dyAxis(name= 'y',valueRange = c(",y_min,",",y_max,")) %>%  dyLegend(show = 'follow', hideOnMouseOut = TRUE, width = 400)%>%  dyOptions(connectSeparatedPoints = TRUE) %>% dyOptions( drawGrid = FALSE) %>% dyHighlight(highlightCircleSize = 5)")
  # eval(parse(text=RotulaSeries(graph,rotulos)))
}



ExtraiRubricasRaiz <- function(tipo_fluxo=3, rubrica_ref =NULL){
  load("todas_series.Rdata")
  if (tipo_fluxo == 1)  {serie<-series_temporais_analise_rec}
  if (tipo_fluxo == 2)  {serie<-series_temporais_analise_desp}
  if (tipo_fluxo == 3)  {serie<-rbind(series_temporais_analise_rec,series_temporais_analise_desp)}
  
  rubricas<-c(as.character(unique(serie$Rubrica)))
  
  if (!is.null(rubrica_ref)){
    codigo<-strsplit(rubrica_ref," ")[[1]][1]
    rubricas<-rubricas[grep(codigo,rubricas)]
  }
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
  if (tipo_fluxo!=1 || !is.null(rubrica_ref) ){rubrica_raiz <- append(rubrica_raiz,rubricas[length(rubricas)])}
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
  library(magrittr)
  library(dplyr)
  
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

Trata_Selecao_usuario<- function(serie_trabalho,
                                 tipo_valor,
                                 tipo_per,
                                 rubricas,
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
  
  series_grafico
  
}

Grafico_selecao_usuario <- function(serie_trabalho,
                                    tipo_valor,
                                    tipo_per,
                                    rubricas, 
                                    rotulos, 
                                    nome_grafico,
                                    avalia_parc,
                                    width = NULL,
                                    height = NULL){
  
  
  series_grafico<-Trata_Selecao_usuario(serie_trabalho,
                                        tipo_valor,
                                        tipo_per,
                                        rubricas, 
                                        avalia_parc)
  GraficoSeries(series_grafico,rubricas,rotulos, nome_grafico, width, height)
  
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


Busca_Metadados <- function(rubricas,tipo_fluxo){
  load("todas_series.Rdata")
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
  
  load("todas_series.Rdata")
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
  
  datatable(dados_tabela,rownames = FALSE,extensions = 'Responsive')
  
  
}


Monta_Arquivo_CSV <- function( serie_trabalho,
                               tipo_valor,
                               tipo_per,
                               rubricas, 
                               avalia_parc,
                               file){
  library(dplyr)
  
  
  series_grafico<-Trata_Selecao_usuario(serie_trabalho,
                                        tipo_valor,
                                        tipo_per,
                                        rubricas, 
                                        avalia_parc
  )
  
  
  if (length(rubricas)==1){
    series_arquivo<-data.frame(Data=as.Date(series_grafico),Rubrica= rubricas, valor=as.numeric(series_grafico))
    
  } else
  {
    for (i in 1:length(rubricas)){
      if (i==1){series_arquivo<-data.frame(Data=as.Date(series_grafico[,i]),Rubrica= rubricas[1], valor=series_grafico[,i])}
      else {
        novas_linhas <- data.frame(Data=as.Date(series_grafico[,i]),Rubrica= rubricas[i], valor=series_grafico[,i])
        #series_arquivo<-rbind(series_arquivo,novas_linhas)
        series_arquivo<-union_all(series_arquivo, novas_linhas) %>%
          arrange(Data)
        
        
      }
    }
  }  
  write.table(series_arquivo, file, sep = ";",row.names = FALSE,fileEncoding = "UTF-8",dec=",")
  
  
}


Visualizar_Grafico_Distribuicao <- function(tipo_fluxo, #1 - receita, 2- despesa, 3 transferências 
                                            tipo_valor,
                                            tipo_per,
                                            rubricas_sel,
                                            avalia_parc,
                                            data_sel,
                                            height){
  load("todas_series.Rdata")
  library(tidyverse)
  library(zoo)
  
  
  if (tipo_fluxo==1){
    tipo_fluxo_orig <-1
    rubricas<-ExtraiRubricasRaiz(1)
    rubricas<-rubricas[-grep("II.",rubricas)] #Exclui as rubricas de transferência
    rubrica_total <- c("I. RECEITA TOTAL")
    serie_trabalho<- series_temporais_analise_rec
    
  }
  if (tipo_fluxo==2){  
    tipo_fluxo_orig <-2
    rubricas<-ExtraiRubricasRaiz(2)
    serie_trabalho<- series_temporais_analise_desp
    rubrica_total <- c("IV. DESPESA TOTAL")
    
  }
  
  
  if (tipo_fluxo==3){  
    tipo_fluxo_orig <-1
    rubricas<-ExtraiRubricasRaiz(1)
    rubricas<-rubricas[grep("II.",rubricas)] #Permanece apenas as rubricas de transferência
    serie_trabalho<- series_temporais_analise_rec
    rubrica_total <- c("II. TRANSF. POR REPARTIÇÃO DE RECEITA ²")
    
  }
  
  rubricas<-c(rubricas,rubrica_total)
  
  series_grafico<-Trata_Selecao_usuario(serie_trabalho,
                                        tipo_valor,
                                        tipo_per,
                                        rubricas, 
                                        avalia_parc
  )
  
  #converte ts em data.frame
  
  
  series<- data.frame(Rubrica=colnames(series_grafico)[1],Data=as.Date(series_grafico[,1]),Valor=as.numeric(series_grafico[,1]))
  
  for (i in 2:NCOL(series_grafico)){
    series<- rbind(series,data.frame(Rubrica=colnames(series_grafico)[i],Data=as.Date(series_grafico[,i]),Valor=as.numeric(series_grafico[,i])))
  }
  
  rubricas<-rubricas[!(rubricas %in% rubrica_total)]
  
  data<- series[series$Rubrica %in% rubricas & 
                  series$Data== data_sel,]
  
  serie_totalizadora <- series[series$Rubrica == rubrica_total & 
                                 series$Data== data_sel,]
  
  data$Valor <- (data$Valor/serie_totalizadora$Valor)*100
  
  
  
  data=data %>%
    arrange(Valor) %>%
    mutate(Rubrica=factor(Rubrica,Rubrica))
  
  #Verifica se existem rubricas selecionadas que não estão no rol de rubricas raízes
  rubricas_n_raiz <- rubricas_sel[!(rubricas_sel %in% rubricas)]
  rubricas_n_raiz <- rubricas_n_raiz [rubricas_n_raiz != rubrica_total]
  #Para cada rubricanão raiz, acrescentar as respectivas rubricas raiz
  rubricas_raiz <- c(character())
  
  if (length(rubricas_n_raiz>0)){
    for (i in 1:length(rubricas_n_raiz)){
      rubricas_raiz<-c(rubricas_raiz,ExtraiRubricasRaiz(tipo_fluxo_orig,rubricas_n_raiz[i])) }
    rubricas_sel <- c(rubricas_sel,rubricas_raiz)
    
  }
  
  #identificar que texto será exibido em relação ao período
  if (tipo_per %in% c("m","12")){
    texto_periodo <- format(as.yearmon(data_sel),"%m/%Y")
  }
  
  if (tipo_per == "a"){
    texto_periodo <- format(as.yearmon(data_sel),"%Y")
  }
  
  
  if (tipo_per == "t"){
    if (substr(data_sel,6,7) == "01"){
      texto_periodo <- paste0("1º trim ", format(as.yearmon(data_sel),"%Y"))
      
    } else if (substr(data_sel,6,7) == "04")
    {
      texto_periodo <- paste0("2º trim ", format(as.yearmon(data_sel),"%Y"))
    } else
    {
      texto_periodo <- paste0("3º trim ", format(as.yearmon(data_sel),"%Y"))
    }    
  }
  
  
  
  # Plot
  p = ggplot(data, aes(x=data$Rubrica, y=data$Valor)) +
    geom_segment( aes(x=data$Rubrica, xend=data$Rubrica, y=0, yend=data$Valor ), color=ifelse(data$Rubrica %in% rubricas_sel, "orange", "grey"), size=ifelse(data$Rubrica %in% rubricas_sel, 1.3, 0.7) ) +
    geom_point( color=ifelse(data$Rubrica %in% rubricas_sel, "orange", "grey"), size=ifelse(data$Rubrica %in% rubricas_sel, 5, 2) ) +
    theme_light() +
    coord_flip() +
    theme(
      legend.position="none",
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    xlab("") +
    ylab(paste0("% de ",rubrica_total)) +
    ggtitle(paste0("Distribuição Percentual em ",texto_periodo))
  library(plotly)
  
  ggplotly(p,width= 700, height = height)
  
  
}


Busca_URL_Relatorio <- function(data_sel){
  
  library(lubridate)
  library(zoo)
  library(ckanr)
  
  chave_URL<-paste0("url:Nim",format(as.yearmon(data_sel),"%b%Y"))
  
  
  #tb_ckan_rel<-resource_search(chave_URL,url="http://www.tesourotransparente.gov.br/ckan/")
  
  tb_ckan_rel<-resource_search(chave_URL,url="https://apickan.tesouro.gov.br/ckan")
  print(paste("URL",tb_ckan_rel$results[[1]]$url))
  

  #URL_relatorio<-tb_ckan_rel$results[[1]]$url
  
  URL_relatorio<-gsub("http://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",tb_ckan_rel$results[[1]]$url)
  
  #URL_relatorio<-gsub("http://www.tesourotransparente.gov.br","https://www.tesourotransparente.gov.br",tb_ckan_rel$results[[1]]$url)
  URL_relatorio
  
}


