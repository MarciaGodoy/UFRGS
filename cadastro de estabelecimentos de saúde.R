###Rotina em R
# usando pacote microdatasus (SALDANHA et al, 2019)

######################################################################################
##############################  INICIAL  #############################################
######################################################################################
# PACOTE UTILIZADO PARA DOWNLOAD E PRÉ-PROCESSAMENTO DE DADOS
# "Microdatasus: pacote para download e pré-processamento de microdados do Departamento de Informática do SUS (DATASUS)"
#cff-version: 1.2.0
#message: "If you use this software, please cite it as below."
#authors:
#- family-names: "Saldanha"
#  given-names: "Raphael de Freitas"
#  orcid: "http://orcid.org/0000-0003-0652-8466"
#- family-names: "Bastos"
#  given-names: "Ronaldo Rocha"
#  orcid: "http://orcid.org/0000-0001-9597-5967"
#- family-names: "Barcellos"
#  given-names: "Christovam"
#  orcid: "http://orcid.org/0000-0002-1161-2753"
#title: "Microdatasus: pacote para download e pré-processamento de microdados do Departamento de Informática do SUS (DATASUS)"
#version: 1.0.0
#doi: 10.1590/0102-311X00032419
#date-released: 2019-09-16
#url: "http://ref.scielo.org/dhcq3y"


#definindo diretorio

getwd()
setwd("~/machine learning")

# limpando a memoria
rm(list=ls())

library(usethis)
#usethis::use_github()
#usethis::use_readme_md()

# set the user.name and user.email locally, i.e. for current repo/project
use_git_config(
  user.name = "MarciaGodoy",
  user.email = "marciargodoy@hotmail.com",
)


####################
##INSTALAR PACOTES
#install.packages("read.dbc")
#install.packages("devtools")
#remotes::install_github("rfsaldanha/microdatasus") # apenas 1 vez na maquina
#devtools::install_github("rfsaldanha/microdatasus")
#install.packages("remotes")

###################
# carregando os pacotes 
library(rio) # importar base de dados de diferentes extensoes
library(read.dbc)#ler arquivos sus
library(readxl)#ler excel
library(writexl)#salvar excel
library(tidyr) # comando replace_na
library(dplyr) # manipular base de dados usando %>%
library(lubridate) # trabalhar com data
library(gtsummary) # tabelas
library(gt) # nota de rodapé na tabela
library(gmodels)#tabelas
library(ggplot2) # graficos
library(devtools)
library(tidyverse)
library(data.table)
library(caret)#KNN , CLASSIFICAÇÃO
library(xgboost) #classificacao
library(glmnet)
library(rpart)
library(Matrix)
library(quantmod)
library(lattice)
library(microdatasus) #pacote de extracao de dados do datasus
library(fastDummies) 
library(stargazer)
library(skimr)#faz resumo das variaveis
library(stringr)
library(magrittr)
library(stringr) #para trabalhar com caracteres , extracao 
library(zoo)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(recipes) 
library(recipes)
library(stringr)#variaveis com texto
require(sandwich)
require(msm)
library(plm)


#################
##  IMPORTANDO BASE DE DADOS -webscrapping
#################
#demorado porque há uma
cad_saude <- fetch_datasus(year_start = 2018, month_start=1, year_end = 2020, month_end=12, uf = "RS", information_system = "CNES-ST")
skim(cad_saude)
cad_saude <- cad_saude %>% filter( CODUFMUN == "431490" & QTLEITP1 >= "1" ) %>% 
  select("CNES", "VINC_SUS", "TPGESTAO", "QTLEITP1", "QTLEITP2", "CODUFMUN", "DT_ATUAL", "COMPETEN")

cad_saude <- process_cnes(cad_saude, information_system = "CNES-ST", nomes= TRUE, municipality_data = TRUE)

####inicie aqui
####***********************
  load("~/machine learning/cadastro saude Porto Alegre.RData")
######################

#mnative nome da variavel para fazer merge

table(cad_saude$COMPETEN)#tabela de numero de observacoes



#####obter ano e mes - note que o formato original não era adequado para uso de yearmon, strptime e year(lubridate)
################

cad_saude$COMPETEN <- as.character(cad_saude$COMPETEN)
#####obter ano e mes - note que o formato original não era adequado para uso de yearmon, strptime e year(lubridate)
cad_saude$ano <- str_sub(cad_saude$COMPETEN, 0, 4)
cad_saude$mes <- str_sub(cad_saude$COMPETEN, -2)
cad_saude$mes.nasc <- cad_saude$mes
cad_saude$Ano.nasc <- cad_saude$ano
cad_saude$Ano.nasc <- as.factor(cad_saude$Ano.nasc)
cad_saude$mes.nasc <- as.factor(cad_saude$mes.nasc)

names(cad_saude) # nomes das variaveis
dim(cad_saude) #numero de linhas e colunas
str(cad_saude)
skim(cad_saude) #descricao geral do banco de dados

###alterndo o tipo das variaveis
cad_saude <- cad_saude %>%
  mutate(#categoricas
    
    TPGESTAO =  as.factor(TPGESTAO), 
    VINC_SUS = as.factor(VINC_SUS),
    munResTipo = as.factor(munResTipo),
    munResStatus = as.factor(munResStatus),
    munResUf = as.factor(munResUf),
    munResNome = as.factor(munResNome), 
    CNES = as.factor(CNES),
    
    #classe
    FANTASIA = as.character(FANTASIA),  
    QTLEITP1 = as.numeric(QTLEITP1),
    QTLEITP2 = as.numeric(QTLEITP2),
    munResArea = as.numeric(munResArea))




###gerando artificialmente uma data 
cad_saude$dia = "31"

#cad_saude %>%
cad_saude$datacompleta <-  paste(cad_saude$dia, cad_saude$mes, cad_saude$ano, sep = '-')
cad_saude <- cad_saude%>%
  mutate(datacompleta = as.Date(datacompleta))
class(cad_saude$datacompleta)

#n=nrow(cad_saude)
#cad_saude.plm=pdata.frame(cad_saude,cad_saude$CNES)
#rm(datacompleta)
#attach(cad_saude.plm)
cad_saude$CODESTAB <- cad_saude$CNES_integer
cad_saude <- cad_saude%>%
  mutate(CODESTAB = as.factor(CODESTAB))

####salvando bd2
save(cad_saude, file = "cadastro saude Porto Alegre.RData") #salvando em formato R
write.table(cad_saude, file = "cadastro saude Porto Alegre.csv", sep =",", na="", quote= TRUE, row.names = FALSE) #salvando em formato csv
#############################


####################################################################################################
#### BANCO DE DADOS 2: CADASTRO DE ESTABELECIMENTOS DE SAUDE
