###Rotina em R
# usando pacote microdatasus (SALDANHA et al, 2019)
####NAO USE ACENTOS
######################################################################################
##############################  INICIAL  #############################################
######################################################################################
# PACOTE UTILIZADO PARA DOWNLOAD E PRe-PROCESSAMENTO DE DADOS
# "Microdatasus: pacote para download e pre-processamento de microdados do Departamento de Informatica do SUS (DATASUS)"
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
#title: "Microdatasus: pacote para download e pre-processamento de microdados do Departamento de Informatica do SUS (DATASUS)"
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
library(gt) # nota de rodape na tabela
library(gmodels)#tabelas
library(ggplot2) # graficos
library(devtools)
library(tidyverse)
library(data.table)
library(caret)#KNN , CLASSIFICAÇaO
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



#################
##  IMPORTANDO BASE DE DADOS -webscrapping
#################
dados <- fetch_datasus(year_start = 2018, year_end = 2020, uf = "RS", information_system = "SINASC")
# OBS : para datas
# estrutura =  ddmmaaaa ; format = "%d%m%Y"
# obs: para data, sempre veja como esta a estrutura antes de reclass

dados$DTNASC[1:10] # ddmmaaaa
# verifique formato de data

###nao fazer preprocessamento aqui
###dados <- process_sinasc(dados) # COM PRE PROCESSAMENTO OCORRE ADIÇaO DE VARIAVEIS

names(dados) # nome das variaveis do objeto
dim(dados) # dimensões numero de linhas e colunas
head(dados, n = 10) #apresenta as primeiras linhas do objeto
str(dados)
sumarioRS <- skim(dados)



save(dados, file = "dados_nasc_RS.RData")
# separado por virgula e textos com aspas e celulas NA vazios e nao adicona nomes das linhas como primeira coluna (row.names=FALSE). 
#Pode deslocar a primeira linha na sua planilha SE você NAO USAR este argumento)
write.table(dados, file = "dados_RS_bruto.csv", sep = ",", na = "", quote = TRUE, row.names=FALSE )

####################################################################################################
#rm(cad_saude)

########  SELECIONAR  AS VARIAVEIS DE INTERESSE
####
load ("~/machine learning/dados_nasc_RS.RData")
#######
#hospitais <- as.data.frame(table(sinasc$CODESTAB))
#rm(dados)

#hospitais_poa <- sinasc %>% select("CNES", "FANTASIA", "CODESTAB")

# selecionando as variaveis de interesse e municipio : 
# filtro: Municipio de Residencia (CODMUNRES) == PORTO ALEGRE : 431490

sinasc <- dados %>% filter( CODMUNNASC == "431490" & CODMUNRES =="431490" ) %>% 
  select("CONTADOR", "DTNASC", "CODESTAB", "CODMUNNASC", "CODMUNRES",
         "SEXO", "PESO",  "RACACOR", "SEMAGESTAC","PARIDADE",
         "PARTO", "TPAPRESENT", "APGAR1",
         "IDADEMAE", "IDADEPAI","ESTCIVMAE", "ESCMAE", "ESCMAE2010", "RACACORMAE", 
         "LOCNASC",   
         "GESTACAO", "CONSULTAS","CONSPRENAT",   "GRAVIDEZ", 
         "QTDGESTANT", "QTDPARTNOR", "QTDPARTCES", 
         "TPROBSON",  "KOTELCHUCK" )

#KOTECLCHUCK MEDE A QUALIDADE DA ASSISTENCIA PRENATAL
#GRUPO DE ROBSON CLASSIFICA GRUPOS COM CHANCES DE CESAREANA
table(dados$SEXO) #2 e feminino  no periodo a diferença de nascimento foi de 10 mil ind. sexco masculino a mais



###NOTE O TAMANHO DOS ARQUIVOS - RDTA TEM MENOR TAMANHO E CSV MAIOR


# remover bd1 - REMOVER MAIS TARDE
rm(dados)

##pre processamento
sinasc <- process_sinasc(sinasc) #COM PRE PROCESSAMENTO OCORRE ADIÇaO DE VARIAVEIS

glimpse(sinasc) ###mostra parte do bd

####resume todas as variaveis
sumarioPOA <- skim(sinasc) #fornece descriçao do banco de dados - missings, taxa de completude, maiores valores
#observe que todas as variaveis estao como factor. entao e necessario reclassificar

####
## - RECLASSIFICAR AS VARIAVEIS
####
sinasc <- sinasc %>%
  mutate(#categoricas
    
    ESTCIVMAE =  as.factor(ESTCIVMAE), 
    ESCMAE = as.factor(ESCMAE),
    ESCMAE2010 = as.factor(ESCMAE2010),
    GESTACAO = as.factor(GESTACAO),
    GRAVIDEZ = as.factor(GRAVIDEZ),
    PARTO = as.factor(PARTO),
    RACACORMAE = as.factor(RACACORMAE), 
    RACACOR = as.factor(RACACOR),
    SEXO =  as.factor(SEXO), 
    PARIDADE = as.factor(PARIDADE),
    CONSULTAS = as.factor(CONSULTAS),#classe
    
    #númericas
    IDADEMAE = as.character(IDADEMAE),
    IDADEMAE = as.numeric(IDADEMAE),
    IDADEPAI = as.character(IDADEPAI),
    IDADEPAI = as.numeric(IDADEPAI),
    CONSPRENAT = as.numeric(CONSPRENAT),#número
    SEMAGESTAC = as.numeric(SEMAGESTAC),
    QTDPARTCES = as.numeric(QTDPARTCES),
    QTDGESTANT = as.numeric(QTDGESTANT),
    QTDPARTNOR = as.numeric(QTDPARTNOR),
    TPROBSON = as.numeric(TPROBSON),
    KOTELCHUCK = as.numeric(KOTELCHUCK),
    APGAR1 = as.numeric(KOTELCHUCK),
    PESO = as.numeric(PESO))
#%>% 


# OBS 1: SEXO - 1 = Masculino ; 2 = Feminino
levels(sinasc$LOCNASC)
levels(sinasc$ESTCIVMAE)
levels(sinasc$ESCMAE)
levels(sinasc$GESTACAO)
levels(sinasc$GRAVIDEZ)
levels(sinasc$CONSULTAS)
levels(sinasc$PARTO)
levels(sinasc$RACACORMAE)
levels(sinasc$SEXO)

summary(sinasc$PARTO)
sinasc <- sinasc %>% ##deixou de funcionar - apresenta erro em cesaria
    mutate(PARTO = factor(PARTO, levels  = c("Vaginal", "Cesáreo","Ignorado", "Nao preenchido"))) %>%
  mutate(PARTO = replace_na(PARTO, "Nao preenchido"))
table(sinasc$PARTO)
#sinasc <- sinasc[!is.na(sinasc$PARTO),] #retirando uma observaçao sem dados de idade - 3 observacoes


summary(sinasc$PARTO)

sinasc <- sinasc %>%
  mutate( 
    ESTCIVMAE =  recode(ESTCIVMAE,
                        "1"= "Solteira",
                        "2"= "Casada",
                        "3"= "Viúva",
                        "4"= "Separada judicialmente/divorciada",
                        "5"= "Uniao estavel",
                        "9"= "Ignorado"), 
    
    GESTACAO = recode(GESTACAO,
                      "1"= "Menos de 22 semanas",
                      "2"= "22 a 27 semanas",
                      "3"= "28 a 31 semanas",
                      "4"= "32 a 36 semanas",
                      "5"= "37 a 41 semanas",
                      "6"= "42 semanas e mais",
                      "9"= "Ignorado"),
    GRAVIDEZ = recode(GRAVIDEZ,
                      "1"= "Unica",
                      "2"= "Dupla",
                      "3"= "Tripla ou mais",
                      "9"= "Ignorado"),
    CONSULTAS = recode(CONSULTAS,
                       "1"= "Nenhuma",
                       "2"= "de 1 a 3",
                       "3"= "de 4 a 6",
                       "4"= "7 e mais",
                       "9"= "Ignorado"),
    RACACORMAE = recode(RACACORMAE,
                        "1"= "Branca",
                        "2"= "Preta",
                        "3"= "Amarela",
                        "4"= "Parda",
                        "5"= "Indigena"),
    SEXO =  recode(SEXO, 
                   "0"= "Ignorado",
                   "1"= "Feminino",
                   "2"= "Masculino"),
    PARTO = recode(PARTO,
     #             "1"= "Vaginal" ,
          "2"= "Cesareo",
                "9"= "Ignorado"),
    
    PARIDADE = recode(PARIDADE,
                      "1" = "Multipara",
                      "2" = "Nulipara"))
skim(sinasc$PARTO)

#criando dummies
####DUMMIES E VARIAVEIS DERIVADAS
sinasc$sexo_d <- sinasc$SEXO
# OBS 1: SEXO - 1 = Masculino ; 2 = Feminino
sinasc["sexo_d"][sinasc["sexo_d"] == "0"] <- "" #ignorado fica em branco
sinasc["sexo_d"][sinasc["sexo_d"] == "2"] <- "0" # 2 (fem) transf em 0 
sinasc <- sinasc %>%
  mutate(sexo_d = as.factor(sexo_d)) #as.factor e usado para categorias

# Criar a variavel  Ano de nascimento "Ano.nasc", fazer tabela e grafico
class(sinasc$DTNASC)
sinasc <- sinasc %>% mutate(DTNASC = as.Date(DTNASC))
sinasc$Ano.nasc <- year(sinasc$DTNASC)
sinasc$mes.nasc <- month(sinasc$DTNASC)
sinasc$Ano.nasc <- as.factor(sinasc$Ano.nasc)
sinasc$mes.nasc <- as.factor(sinasc$mes.nasc)

sinasc <- sinasc %>% mutate(DTNASC = as.Date(DTNASC))
sinasc$ano <- year(sinasc$DTNASC)
sinasc$mes <- month(sinasc$DTNASC)
sinasc$ano <- as.factor(sinasc$Ano.nasc)
sinasc$mes <- as.factor(sinasc$mes.nasc)


###gerando artificialmente uma data 
sinasc$dia = "31"

#cad_saude %>%
sinasc$datacompleta <-  paste(sinasc$dia, sinasc$mes, sinasc$ano, sep = '-')
sinasc <- sinasc%>%
  mutate(datacompleta = as.Date(datacompleta))
class(sinasc$datacompleta)


###gerando local de residencia diferente do local de nascimento
sinasc$morafora <- if_else(sinasc$munResNome == "Porto Alegre", "NaO" , "sim")
table(sinasc$morafora)



#PREPARO PARA dummies
sinasc$LOCNASC_d <- sinasc$LOCNASC
# para LOCNASC  ordem "Hospital", "Domicilio", "Outros estabelecimentos de saúde",
# "Outros", "Ignorado
table(sinasc$LOCNASC)
class(sinasc$LOCNASC)
levels(sinasc$LOCNASC)
sinasc <- sinasc  %>%    mutate(LOCNASC = as.factor(LOCNASC))


####o pre-processamento ja fez parte do trabalho ,veja o arquivo dados

sinasc <- sinasc %>%
  mutate(IDADEMAE = as.character(IDADEMAE),
         IDADEMAE = as.numeric(IDADEMAE))



skim(sinasc$IDADEMAE) #observe que ha uma observaçao sem dados
sinasc <- sinasc[!is.na(sinasc$IDADEMAE),] #retirando uma observaçao sem dados de idade
##sinasc["IDADEMAE"][sinasc["IDADEMAE"]== "99"] <- "29" #e POSSIVEL TROCAR 99 PELA IDADE MeDIA

###substituindo missing no cod estabelecimento
sinasc$CODESTAB <-coalesce(sinasc$CODESTAB, "9999") # <- "ignorado"

###gerando codigo ignorado para codestab ausente
#sinasc <- sinasc %>% mutate(CODESTAB = replace_na(PARTO, "9999"))



class(sinasc$CODESTAB)
table(sinasc$CODESTAB)
table(sinasc$PARTO)
levels(sinasc$PARTO)
###sinasc$cesaria <- GERAR DUMMY
sinasc$cesareo <- sinasc$PARTO
#sinasc["cesareo"][sinasc["cesareo"] == "Ignorado"] <- "" # ignorado fica em branco
#sinasc["cesareo"][sinasc["cesareo"] == "Nao preenchido"] <- "" # ignorado fica em branco
table(sinasc$cesareo)
#sinasc["cesareo"][sinasc["cesareo] == "Vaginal"] <- "1" # 
sinasc["cesareo"][sinasc["cesareo"] == "Cesario"] <- "0" # 2 (cesario) transf em 1 

sinasc <- sinasc %>%
  mutate(cesareo = as.factor(cesareo)) #as.factor e usado para categorias

table(sinasc$cesareo)
skim(sinasc$cesareo)

sinasc$idade_cat <- cut(sinasc$IDADEMAE,  # CRIANDO FAIXA ETARIA
                        breaks = c(10, 20, 35, 45, Inf), right = FALSE,
                        labels = c("10 a 19 anos", "20 a 34 anos", "35 a 44 anos", "45 anos ou mais"))


sinasc$LOCNASC <- factor(sinasc$LOCNASC)

sinasc <- sinasc %>% mutate(RACACORMAE = factor(RACACORMAE, 
                                                levels = c("Branca", "Preta",
                                                           "Amarela", "Parda",
                                                           "Indigena", "Outras")))

# Colocar Branco, Amarela ou Indigena na categoria (nao-preta 0) e Preta e Parda na categoria Preta (1)

sinasc$RACACORMAE_d <- sinasc$RACACORMAE
sinasc <- sinasc %>% 
  mutate(RACACORMAE_d = recode(RACACORMAE_d      ,
                               "Preta"= "1"     ,
                               "Parda" = "1"    ,
                               "Branca" = "0"   ,
                               "Amarela" = "0"  ,
                               "Indigena" = "0" ,
                               "Outras" = "0"
  ))

sinasc <- sinasc %>%
  mutate(RACACORMAE_d = as.factor(RACACORMAE_d)) 
class(sinasc$RACACORMAE_d) #fornece o tipo da variavel

sinasc <- sinasc  %>% mutate(ESCMAE2010 =  factor(ESCMAE2010, 
                                                  levels = c("0","1","2", "3", "4","5" ,"9"), 
                                                  labels = c("Nenhuma","Fundamental I",
                                                             "Fundamental II", "Ensino medio","Superior incompleto", 
                                                             "Superior completo", "Ignorado")))

sinasc <- sinasc %>%
  mutate(CODESTAB = as.factor(CODESTAB))

# Criar variavel "Hospital.nasc"; sIM: HOSPITAL E naO :OUTROS LOCAIS
# 
sinasc$Hospital.nasc <- sinasc %>%
  mutate(Hospital.nasc = ifelse(LOCNASC == "Hospital", "Sim", "Nao")) %>%
  mutate( Hospital.nasc = factor(Hospital.nasc, levels = c("Sim", "Nao")) )
summary(sinasc$Hospital.nasc)

sinasc$peso_cat <-  cut(sinasc$PESO,  # CRIANDO categorias de peso
                        breaks = c(0, 2500, 3000, 4000, Inf), right = FALSE,
                        labels = c("Menos de 2500 g", "2500 g a 2999 g",
                                   "3000 g a 3999 g", "4000 g ou mais"))


sinasc$Qpeso <- cut(sinasc$PESO, breaks = c(0, 1000, 1500, 2500, Inf),
                    labels = c("extremo", "muito baixo", "baixo peso", "normal"),
                    include.lowest = T, right = T)
#  PESO > 1500 & peso <= 2500
#extr_bx_peso <- PESO <= 1000

#"1"= "extremo",
#"1"= "muito baixo",
#"1"= "baixo peso",
levels(sinasc$Qpeso)
class(sinasc$Qpeso)
summary(sinasc$Qpeso)

####VARIAVEL A SER EXPLICADA:::: dummy de baixo peso
sinasc$bp <- cut(sinasc$PESO, breaks = c(0, 2500, Inf),
                 labels = c("baixo peso", "normal"),
                 include.lowest = T, right = T)
sinasc <- sinasc %>%
  mutate(bp = recode(bp,
                     "baixo peso" = "1",
                     "normal" = "0" ))

sinasc$KOTELCHUCK <- as.factor(sinasc$KOTELCHUCK)
class(sinasc$KOTELCHUCK)
sinasc$dt_merge ="31"

###sumarioPOA2 <- skim(sinasc) #verifique mudanças   - CORRIGIR ERRO

#save(sinasc, file = "dados_nasc_POA.RData")
save(sinasc, file = "dados_nasc_POA_bruto.RData")
####################
write.csv(sinasc,"dados_POA.csv")


