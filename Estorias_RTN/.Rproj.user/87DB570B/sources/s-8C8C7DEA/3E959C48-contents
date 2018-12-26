# plumber.R

library(ckanr)
library(readxl)


#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @png
#* @get /plot
function(){
  rand <- rnorm(100)
  hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b){
  as.numeric(a) + as.numeric(b)
}


#* Retorna o estoque da dívida pública federal
#* @get /estoque_dpf
function(){
  #tb_ckan<-resource_show(id="0402cb77-5e4c-4414-966f-0e87d802a29a",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="0402cb77-5e4c-4414-966f-0e87d802a29a",url="http://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_dpf<-read_xlsx(tmp,sheet = 1,skip = 2,n_max = 5, col_names = FALSE)
  unidade<- as.character(df_dpf[1,NCOL(df_dpf)])
  ult_mes<- as.character(df_dpf[3,NCOL(df_dpf)])
  valor <- as.numeric(as.numeric(df_dpf[5,NCOL(df_dpf)]))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}


#* Retorna as emissões da DPF no ano
#* @get /emissoes_dpf
function(){
  #tb_ckan<-resource_show(id="bf69babd-ac07-40ce-90ff-c8e07ec8c8bf",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="bf69babd-ac07-40ce-90ff-c8e07ec8c8bf",url="http://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_dpf<-read_xlsx(tmp,sheet = 1,skip = 2,n_max = 5, col_names = FALSE)
  unidade<- as.character(df_dpf[1,NCOL(df_dpf)])
  ult_mes<- as.character(df_dpf[3,(NCOL(df_dpf)-1)])
  valor <- as.numeric(as.numeric(df_dpf[5,NCOL(df_dpf)]))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}


#* Retorna o resgate da dívida pública federal
#* @get /resgate_dpf
function(){
  #tb_ckan<-resource_show(id="bf69babd-ac07-40ce-90ff-c8e07ec8c8bf",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="bf69babd-ac07-40ce-90ff-c8e07ec8c8bf",url="http://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_dpf<-read_xlsx(tmp,sheet = 1,skip = 2,n_max = 19, col_names = FALSE)
  unidade<- as.character(df_dpf[1,NCOL(df_dpf)])
  ult_mes<- as.character(df_dpf[3,(NCOL(df_dpf)-1)])
  valor <- as.numeric(as.numeric(df_dpf[19,NCOL(df_dpf)]))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}


#* Retorna o resultado primário do governo central
#* @get /resultado_primario
function(){
  #tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="http://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_rtn<-read_xlsx(tmp,sheet = 2,skip = 2,n_max = 78, col_names = FALSE)
  meses<-as.Date(as.numeric(df_rtn[3,c(2:NCOL(df_rtn))]), origin = "1899-12-30")
  ult_mes <- meses[NROW(meses)]
  pos_ano_corrente <- which(substr(meses,1,4)==substr(ult_mes,1,4))
  pos_ano_corrente <- pos_ano_corrente + 1
  
  unidade<- as.character(df_rtn[1,1])
  
  valor <- as.numeric(sum(as.numeric(df_rtn[74,pos_ano_corrente])))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}


#* Retorna o resultado primário do governo central
#* @get /receita_primaria_liquida
function(){
  #tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="http://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_rtn<-read_xlsx(tmp,sheet = 2,skip = 2,n_max = 37, col_names = FALSE)
  meses<-as.Date(as.numeric(df_rtn[3,c(2:NCOL(df_rtn))]), origin = "1899-12-30")
  ult_mes <- meses[NROW(meses)]
  pos_ano_corrente <- which(substr(meses,1,4)==substr(ult_mes,1,4))
  pos_ano_corrente <- pos_ano_corrente + 1
  
  unidade<- as.character(df_rtn[1,1])
  
  valor <- as.numeric(sum(as.numeric(df_rtn[37,pos_ano_corrente])))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}


#* Retorna o resultado primário do governo central
#* @get /despesa_primaria_total
function(){
  #tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://apickan.tesouro.gov.br/ckan")
  tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="http://www.tesourotransparente.gov.br/ckan/")
  URL_add <- tb_ckan$url
  #URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
  tmp = paste(getwd(),"temp.xlsx")
  tmp = tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp)
  df_rtn<-read_xlsx(tmp,sheet = 2,skip = 2,n_max = 38, col_names = FALSE)
  meses<-as.Date(as.numeric(df_rtn[3,c(2:NCOL(df_rtn))]), origin = "1899-12-30")
  ult_mes <- meses[NROW(meses)]
  pos_ano_corrente <- which(substr(meses,1,4)==substr(ult_mes,1,4))
  pos_ano_corrente <- pos_ano_corrente + 1
  
  unidade<- as.character(df_rtn[1,1])
  
  valor <- as.numeric(sum(as.numeric(df_rtn[38,pos_ano_corrente])))
  #list( valor = valor, unidade ="R$ Bi")
  data.frame("valor" = valor, "unidade" = unidade, "ult_mes" = ult_mes)
  #paste0("R$ ", format(valor, digits=9, decimal.mark=",", big.mark=".",small.mark=".",  small.interval=3), " R$ bilhões")
}
