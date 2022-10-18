###################
####fazendo merge dos arquivos
################################################
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


rn(sinasc)

load("~/machine learning/cadastro saude Porto Alegre.RData")
load("~/machine learning/dados_nasc_POA_bruto.RData")
cad_saude <- cad_saude%>%
  mutate(CODESTAB = as.numeric(CODESTAB))

cad_saude <- cad_saude%>%
  mutate(CNES = as.numeric(CNES))

sinasc <- sinasc%>%
  mutate(CODESTAB = as.numeric(CODESTAB))




sinasc <- sinasc %>% 
  left_join(cad_saude, by = c("CODESTAB"= "CODESTAB", "datacompleta" = "datacompleta"))


##remover os bancos de dados para aumentar memoria disponivel
#rm(cad_saude)
#rm(dados)

#################
#####TABELAS E GRAFICOS
#################
options(OutDec = ",") # decimal com virgula
str(sinasc)
sinasc$munResArea.x <- as.numeric(sinasc$munResArea.x)
class(sinasc$munResArea.x)

#barplot(table(sinasc$Ano.nasc))


# ##CARACTERISTICAS MATERNAS
###### 
#facilita legenda
cidade <- "Porto Alegre"
periodo <- "2018-2020"
periodo <- as.character(periodo)
texto <- paste(cidade, periodo, sep = ',')
############################
#"Porto Alegre, 2018-2020"

#####CARACTERISTICAS MATERNAS
###########################################
####PARTOS POR IDADE E RAÇA DA MAE
#########################################
skim(sinasc$IDADEMAE) #

tab_partos_idade <- as.data.frame(table(sinasc$IDADEMAE))

class(sinasc$idade_cat)
tab_partos_idade_fx <- as.data.frame(table(sinasc$idade_cat))

summary(sinasc$idade_cat)
class(sinasc$idade_cat)

Tab_idademae <- sinasc %>%  group_by(PARTO) %>%
  summarise(Media = mean(IDADEMAE) ,
            DP = sd(IDADEMAE), # desvio-padrao
            Mediana = median(IDADEMAE),
            Q1 = quantile(IDADEMAE, prob = 0.25),
            Q4 = quantile(IDADEMAE, prob = 0.90),
            n = n()
  )
summary(sinasc$RACACORMAE)

summary(sinasc$RACACORMAE_d)
table(sinasc$cesareo)

#################################
### graficos
################################

# Grafico para Faixa etaria da mae

g_1 <- ggplot(sinasc) +
  aes(x = IDADEMAE) +
  geom_bar(fill= "darkgreen") +
  labs(x =  " Grafico 1: Faixa etaria da mae", y = "Frequência", 
       title = "Faixa etaria das maes dos Nascidos Vivos", 
       subtitle = texto ,
       caption = "Fonte: SINASC") +
  theme_minimal() # minimal com linhas de grades suaves

g_1

#idade_cat ~ "Faixa etaria (em anos)"
t_1 <- sinasc %>%
  select(ESTCIVMAE, ESCMAE2010, RACACORMAE, PARIDADE, idade_cat) %>%
  tbl_summary(missing_text = "Sem informaçao", # colocando "Sem informaçao" no NA
              digits = list(all_categorical() ~ c(0,1)), # n sem casa decimal e % com 1 casa decimal
              label = list(ESTCIVMAE ~ "Estado civil",
                           ESCMAE2010 ~ "Escolaridade",
                           RACACORMAE ~ "Raça ou Cor",
                           PARIDADE ~ "Gravidez anterior",
                           idade_cat ~ "Faixa etaria"
              ),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "**Caracteristicas maternas**") %>%
  modify_caption("Tabela 1: Distribuiçao das caracteristicas maternas em Porto Alegre 2018-2020") %>% #testar texto
  modify_footnote(
    all_stat_cols() ~ "Frequência (%)"
  ) %>%
  bold_labels() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: MS/DATASUS - Sistema de Informações sobre Nascidos Vivos - SINASC*"))

t_1

###################
#####PARTO POR ESCOLARIDADE
###################
skim(sinasc$ESCMAE2010)

summary(sinasc$ESCMAE2010)
summary(sinasc$ESCMAE)

#tabela Idade média materna, por nível de escolaridade
Tab_idademae_ESC <- sinasc %>% group_by(ESCMAE2010) %>%
  summarise(Media = mean(IDADEMAE),
            n = n(),''
            #na.rm = TRUE
  )
names(Tab_idademae_ESC) <- c("Escolaridade","Idade média", "N. Nascidos Vivos")

###deixou de funcionar
tb_natalidade <- select(sinasc, IDADEMAE, SEXO)
tab_natalidade <- as.data.frame(table(cut(as.numeric(tb_natalidade$IDADEMAE), breaks = seq(0, 60, by=5), right=F), by = tb_natalidade$SEXO))
#tab_natalidade <- cbind(tab_natalidade, total = rowSums(tab_natalidade))
#tab_natalidade <- rbind(tab_natalidade, total = colSums(tab_natalidade))
#tb_natalidade <- as.data.frame.matrix(tab_natalidade)

##procurar erro
##tabela parto por escolaridade
tab4_2 <-table(sinasc$ESCMAE2010 , sinasc$PARTO)
Total_linha<-margin.table(tab4_2,2)  # O argumento 2 define a marginal da linha
Total_coluna<-margin.table(tab4_2,1) # O argumento 1 define a marginal da coluna
tab4_2_<-rbind(cbind(tab4_2,Total_coluna),c(Total_linha, sum(Total_coluna)))
dimnames(tab4_2_)[[1]][8]<-"Total_linha" 
tab4_2_

tab4_3<-prop.table(tab4_2)
tab4_3
Total_linha<-margin.table(tab4_3,2) 
Total_coluna<-margin.table(tab4_3,1)
tab4_3_<-rbind(cbind(tab4_3,Total_coluna),c(Total_linha, sum(Total_coluna)))
dimnames(tab4_3_)[[1]][8]<-"Total_linha" 
tab4_3_
#http://leg.ufpr.br/~fernandomayer/aulas/ce083-2016-2/05_Analise_exploratoria.html
#https://rpubs.com/EstatBasica/Cap4

###APRESENTAR - se retirar as.data.frame funciona melhor, mas 
tab4_4<- prop.table(tab4_2,2)
tab4_4
Total_linha<-margin.table(tab4_4,2)
Total_coluna<-margin.table(tab4_2,1)/sum(margin.table(tab4_2,1))

tab4_4_<-rbind(cbind(tab4_4,Total_coluna),c(Total_linha, sum(Total_coluna)))
dimnames(tab4_4_)[[1]][8]<-"Total_linha" 
tab4_4_
###47% das mulheres que tem parto cesareo tem 12 anos ou mais de estudo, entquanto no parto vaginal este percentual e de 21,49%
##14% das mulheres que tem parto vaginal tem curso superior completo

###outra forma de apresentaçao
#Tabela 4.4
CrossTable(sinasc$ESCMAE2010 , sinasc$PARTO,prop.r=FALSE, prop.c=TRUE,
           prop.t=FALSE, prop.chisq=FALSE)


CrossTable(sinasc$ESCMAE2010 , sinasc$PARTO,
           prop.r=FALSE,    # Se TRUE, entao retorna as proporções nas linhas
           prop.c=FALSE,    # Se TRUE, entao retorna as proporções nas colunas
           prop.t=FALSE,    # Se TRUE, entao retorna as proporções em relaçao ao total
           prop.chisq=FALSE # Se TRUE, entao retorna a contribuiçao de cada casela para a estatistica de Qui-quadrado
)

#                summarise(Media = mean(CONSPRENAT), na.rm  desconsidera as observaçoes em branco para calculo de media e sd

CrossTable(sinasc$ESCMAE2010 , sinasc$PARTO,
           prop.r=FALSE,    # Se TRUE, entao retorna as proporções nas linhas
           prop.c=FALSE,    # Se TRUE, entao retorna as proporções nas colunas
           prop.t=FALSE,    # Se TRUE, entao retorna as proporções em relaçao ao total
           prop.chisq=FALSE # Se TRUE, entao retorna a contribuiçao de cada casela para a estatistica de Qui-quadrado
)

table(sinasc$PARIDADE) #INDICA SE TEVE GESSTACAO ANTERIOR

CrossTable(sinasc$ESCMAE2010 , sinasc$PARIDADE,
           prop.r=FALSE,    # Se TRUE, entao retorna as proporções nas linhas
           prop.c=FALSE,    # Se TRUE, entao retorna as proporções nas colunas
           prop.t=TRUE,    # Se TRUE, entao retorna as proporções em relaçao ao total
           prop.chisq=FALSE # Se TRUE, entao retorna a contribuiçao de cada casela para a estatistica de Qui-quadrado
)



#######################
###CARACTERISTICAS DO PARTO E RECEM NASCIDO
#########################

table(sinasc$PARTO)
#tb_parto <- select(sinasc, idade_cat, SEXO)
tab_parto <- as.data.frame(table(sinasc$idade_cat, sinasc$SEXO))
tab_parto
names(tab_parto) <- c("Faixa etaria","Sexo do RN", "N. Nascidos Vivos")


###CARACTERISTICAS DO RECEM NASCIDO
table(sinasc$sexo_d)
table(sinasc$SEXO) #observe que ha 21 registros com sexo indefinido

table(sinasc$SEXO)

summary(sinasc$SEXO)

levels(sinasc$peso_cat) #exibe as categorias

class(sinasc$peso_cat)#TIPO DO OBJETO

table(sinasc$bp)
levels(sinasc$bp)
class(sinasc$bp)
library(ggplot2)

#ggplot(data = sinasc, aes(`IDADEMAE`, `PESO`, `CONSPRENAT`, `ESCMAE2010`, `PARIDADE`, `bp`))+ #seleciona os dados e as variaveis para cada eixo, assim como a variavel que determina a cor (value).
# geom_tile(color = "white")+    #definindo a cor do contorno de cada quadrado.
#scale_fill_gradient2(low = "blue", high = "red", mid = "white", #definindo a escala de cor das correlações
#                    midpoint = 0, limit = c(-1,1), space = "Lab", #definindo a escala das correlações
#                   name="Coef. Correlaçao") + #definindo o nome da legenda.
# theme_minimal()+ #tema de fundo do grafico
#coord_fixed() + # coordenadas
#labs(x = "correlaçao", y = "correlaçao") #define o nome para o eixo x e para o eixo y

#conferir

###CARACTERISTICAS DO LOCAL DE NASCIMENTO
hospital_nasc <- as.data.frame(table(sinasc$FANTASIA, sinasc$CNES)) 
hospitais <- as.data.frame(table(sina))
table(sinasc$LOCNASC)

summary(sinasc$LOCNASC_d)

table(sinasc$morafora)
nasc_city <- as.data.frame(table(sinasc$munResNome)) 
#tabela Qtde de NV em Porto Alegre, por cidade de residencia


table(sinasc$FANTASIA)
###verificar no arquivo do crime
sinasc %>% 
  group_by(`FANTASIA`, `PARTO`) %>% 
  arrange(`FANTASIA`, `Ano.nasc`, `mes.nasc`)%>%
  summarise(total = n())

#contando observa??es por enquadramento
df.resumo <-as.data.frame(table (sinasc$FANTASIA, sinasc$`PARTO`))

#contando observa??es crimes por municipio  e mes
totale = sinasc %>% 
  arrange(`Ano.nasc`, `mes.nasc`) %>% 
  group_by(`FANTASIA`, `PARTO`) %>% 
  summarise(total = n())



#crimes por municipio e mes
#transformando linha em coluna m?s
total_resumo2 = totale %>%
  group_by("PARTO", "Ano.nasc","mês") %>%
  pivot_wider(names_from = "mes", values_from = "total")
view(total_resumo2)
print(na.omit(total_resumo2))


######
######ASSISTENCIAL PRENATAL 
class(sinasc$CONSPRENAT)

table(sinasc$CONSPRENAT)
plot(sinasc$CONSPRENAT~sinasc$IDADEMAE)
lines(gam(sinasc$CONSPRENAT, sinasc$IDADEMAE))
boxplot(sinasc$CONSPRENAT~sinasc$idade_cat) #consultas por faixa etaria

###

plot1 <-plot( sinasc$CONSPRENAT, sinasc$IDADEMAE )%>%
  abline(v=mean(sinasc$CONSPRENAT))
abline(h=mean(sinasc$IDADEMAE))# eixo da idade introduz uma reta da idade media

png("plot1%02d.png") #salvando o grafico


#barplot(table(sinasc$KOTELCHUCK), col = "lightblue")

#ggplot(table(sinasc$KOTELCHUCK), col = "lightblue")

ggplot(sinasc, aes(sinasc$CONSPRENAT, sinasc$KOTELCHUCK,color=factor(PARTO))) +
  geom_point()

tab2 <- sinasc %>%
  select( "GESTACAO", "CONSULTAS", "GRAVIDEZ", "PARTO", "SEXO", "PARIDADE", "KOTELCHUCK"
  ) %>%
  tbl_summary(missing_text = "Sem informaçao", # colocando "Sem informaçao" no NA
              digits = list(all_categorical() ~ c(0,1)), # n sem casa decimal e % com 1 casa decimal
              label = list(
                GESTACAO ~ "Duraçao da gestaçao",
                CONSULTAS ~ "Consultas pre-natal",
                GRAVIDEZ ~ "Tipo de gravidez",
                PARTO ~ "Tipo de parto",
                SEXO ~ "Sexo do RN",
                PARIDADE ~ "Gravidez anterior",
                KOTELCHUCK ~ "ind. Kotel"
              ),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "**Caracteristicas**") %>%
  modify_caption("Tabela 2: Distribuiçao das caracteristicas neonatais e 
                 assistenciais dos nascidos vivos no Rio Grande do Sul/ RS em 2018-2020.") %>%
  modify_footnote( all_stat_cols() ~ "Frequência (%)") %>%
  bold_labels() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: MS/SVS/DASIS - Sistema de Informações sobre Nascidos Vivos - SINASC*"))

tab2
####tabela importante
sinasc %>%
  filter( !(PARTO %in% c("Ignorado", "Nao preenchido"))) %>% # sem ign e nao preenc
  mutate( PARTO = factor(PARTO)) %>% 
  select("ESTCIVMAE", "ESCMAE", "RACACORMAE", 
         "GESTACAO", "CONSULTAS", "GRAVIDEZ", "PARTO", "SEXO" ) %>%
  tbl_summary(by= PARTO,
              missing_text = "Sem informaçao", # colocando "Sem informaçao" no NA
              digits = list(all_categorical() ~ c(0,1)), # n sem casa decimal e % com 1 casa decimal
              label = list(ESTCIVMAE ~ "Estado civil",
                           ESCMAE ~ "Escolaridade",
                           RACACORMAE ~ "Raça ou Cor da mae",
                           GESTACAO ~ "Duraçao da gestaçao",
                           CONSULTAS ~ "Consultas pre-natal",
                           GRAVIDEZ ~ "Tipo de gravidez",
                           SEXO ~ "Sexo"
              ),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "**Caracteristicas**") %>%
  modify_caption("Tabela 3: Distribuiçao das caracteristicas maternas, neonatais e 
                 assistenciais dos nascidos vivos segundo Tipo de Parto no Rio Grande do Sul/ RS em 2018-2020") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Tipo de Parto**") %>%
  add_overall(col_label = "**Total**") %>%
  add_p() %>%
  bold_labels() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: MS/SVS/DASIS -
                             Sistema de Informações sobre Nascidos Vivos -
                             SINASC*"))


###SALVANDO O ARQUIVO DE PORTO ALEGRE

#save(sinasc, file = "dados_nasc_POA.RData")

#write.csv(sinasc,"dados_POA.csv")


###SALVANDO O ARQUIVO DE PORTO ALEGRE

#save(sinasc, file = "dados_nasc_POA.RData")

#write.csv(sinasc,"dados_POA.csv")
################################################






#dados.cor = cor(sinasc)
#corrplot(dados.cor, type = "upper", order = "original", tl.col = "black", tl.srt = 45)



#step_knnimpute() para preencher os NAs com seus vizinhos mais próximos 
#e step_dummy() para transformar nossas variaveis categóricas em variaveis dummy.
rec <- sinasc %>%
  recipe(Status ~ .) %>%
  step_knnimpute(all_predictors()) %>% 
  step_dummy(all_predictors(), -all_numeric()) %>% 
  step_center(all_predictors()) %>% 
  step_scale(all_predictors()) %>%
  prep(retain = TRUE)



##featurePlot(x = mags, y = zspec, plot = "scatter")
##http://www.astro.iag.usp.br/~laerte/aga0505_21/R10.html
###### 


sinasc %>%
  select(ESTCIVMAE, ESCMAE, PARTO, SEXO, GRAVIDEZ, morafora, PARIDADE, bp) %>%
  tbl_summary()

#peso.cat ~"Peso ao nascer (em gramas) ###E R R O 


# colocando todos os Ignorados e Nao preenchidos como NA (missing)
# e colocando o texto = "Ignorado ou Nao preenchido"
# nao entram p calculo de %

#idade_cat ~ "Faixa etaria (em anos)",
#peso_cat ~ "Peso ao nascer (em gramas)"

sinasc %>%  
  select("ESTCIVMAE", "ESCMAE", "RACACORMAE", "PARTO",
         "GESTACAO", "CONSULTAS", "GRAVIDEZ",
         "SEXO") %>%
  mutate_all(funs(replace(., .== "Ignorado" | .== "Nao preenchido", NA))) %>%
  mutate(across(where(is.factor) , factor)) %>%
  #filter( !(PARTO %in% c("Ignorado", "Nao preenchido"))) %>% # sem ign e nao preenc
  tbl_summary(by= PARTO,
              missing_text = "Ignorado ou Nao preenchido", # colocando "Sem informaçao" no NA
              digits = list(all_categorical() ~ c(0,1)), # n sem casa decimal e % com 1 casa decimal
              label = list(ESTCIVMAE ~ "Estado civil",
                           ESCMAE ~ "Escolaridade",
                           RACACORMAE ~ "Raça ou Cor da mae",
                           GESTACAO ~ "Duraçao da gestaçao",
                           CONSULTAS ~ "Consultas pre-natal",
                           GRAVIDEZ ~ "Tipo de gravidez",
                           #PARTO ~ "Tipo parto",
                           SEXO ~ "Sexo"
              ),
              statistic = list(all_continuous() ~ "{mean} ({sd})")) %>%
  modify_header(label ~ "**Caracteristicas**") %>%
  modify_caption("Tabela 3: Distribuiçao das caracteristicas maternas, neonatais e 
                 assistenciais dos nascidos vivos segundo Tipo de Parto no Estado do Rio Grande do Sul/ RS em 2018-2019") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Tipo de Parto**") %>%
  add_overall(col_label = "**Total**") %>%
  add_p() %>%
  bold_labels() %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Fonte: MS/SVS/DASIS -
                             Sistema de Informações sobre Nascidos Vivos -
                             SINASC*"))




# Faixa etaria da mae

g1<- ggplot(sinasc) +
  aes(x = IDADEMAE) +
  geom_bar(fill= "darkgreen") +
  labs(x = "Idade", y = "Frequência", 
       title = "Grafico 1: Idade das maes dos Nascidos Vivos", 
       subtitle = texto,
       caption = "Fonte:DATASUS/ SINASC") +
  theme_minimal() #  linhas de grades suaves


###exportando o grafico no formato png
png(filename = "g1.png",width = 8, height = 10, 
    units = "cm",pointsize = 8, "white",res = 1200)
g1
dev.off()

# Grafico de colunas para Tipo de Parto
# inserindo freq absoluta nas colunas

g2 <- ggplot(sinasc, aes(x= PARTO)) +  
  geom_bar(fill = "darkgreen")+ # cor azul escura
  geom_text(aes(label = ..count..), stat = "count", 
            vjust = 1.5, colour = "navyblue", size= 3.5)+ # inserindo freq. absoluta, posicao= 1.5 (dentro da coluna), cor branca e tamanho 3.5
  labs(x = "Tipo de parto", y= "Nº de Nascidos Vivos",
       title= "Grafico 2: Número de NV segundo Tipo de Parto",
       subtitle= texto , 
       caption= "Fonte: SINASC")+
  theme_classic() # classic sem linhas de grades 

###exportando o grafico no formato png
png(filename = "g2.png",width = 12, height = 10, 
    units = "cm",pointsize = 8, "white",res = 1200)
g2
dev.off()


# Grafico de colunas para Tipo de Parto sem Ignorado e Nao preench --- 

g3 <-sinasc %>%
  filter(! PARTO %in% c("Ignorado", "Nao preenchido")) %>%
  ggplot() +
  aes(x= PARTO) +  
  geom_bar(fill = "darkgreen", width= 0.4)+ # Diminuir largura da barra: width=0.4
  geom_text(aes(label = paste("n=", ..count..)), stat = "count", 
            vjust = 1.5, colour = "navyblue", size= 3.5)+ # inserindo freq. absoluta, posicao= 1.5 (dentro da coluna), cor azul e tamanho 3.5
  labs(x = "Tipo de parto", y= "Nº de Nascidos Vivos",
       title= "Grafico 3: Número de NV segundo Tipo de Parto",
       subtitle = texto ,
       caption= "Fonte: SINASC")+
  theme_minimal()+ # minimal com linhas de grades suaves
  theme(text = element_text(size=8)) # tamanho das letras = 12
###exportando o grafico no formato png
png(filename = "g3.png",width = 12, height = 10, 
    units = "cm",pointsize = 8, "white",res = 1200)
g3
dev.off()


#ver# https://ggplot2.tidyverse.org/reference/ggtheme.html


# grafico de colunas para TIPO DE PARTO segundo Fx etaria (sem ignorado e nao preenc)
#sem Ignorado e Nao preench

palette_check(rainbow_pal, plot = TRUE) #ver paleta de cores para pessoas com problemas visuais

##
g4 <- sinasc %>%
  filter(! PARTO %in% c("Ignorado", "Nao preenchido")) %>%
  ggplot() +
  aes(x = PARTO, fill = IDADEMAE) +
  geom_bar(position = "dodge") + # barras lado a lado
  scale_fill_viridis_d(option = "viridis") +  # paleta de cores  ver heat viridis
  labs(x = "Tipo de parto", y= "Nº de Nascidos Vivos",
       title= "Grafico 4: Distribuiçao dos NV segundo a Faixa Etaria materna e Tipo de Parto Ignorado",
       subtitle = texto ,
       fill = "Faixa etaria materna (em anos)",
       caption= "Fonte: SINASC")+
  theme_bw() +
  theme(text = element_text(size=8)) # tamanho das letras = 8
theme(legend.position= "bottom") # legenda embaixo


###exportando o grafico no formato png
png(filename = "g4.png",width = 14, height = 10, 
    units = "cm",pointsize = 8, "white",res = 1200)
g4
dev.off()


# grafico de colunas para TIPO DE PARTO segundo Fx etaria (sem ignorado e nao preenc)
#sem Ignorado e Nao preench


g5 <- sinasc %>%
  filter(! PARTO %in% c("Ignorado", "Nao preenchido")) %>%
  ggplot() +
  aes(x = PARTO, fill = idade_cat) +
  geom_bar(position = "dodge") + # barras lado a lado
  scale_fill_viridis_d(option = "viridis") + # paleta de cores
  labs(x = "Tipo de parto", y= "Nº de Nascidos Vivos",
       title= "Grafico 5: Distribuiçao dos NV segundo a Faixa Etaria materna e Tipo de Parto",
       subtitle = texto ,
       fill = "Faixa etaria materna (em anos)",
       caption= "Fonte: SINASC")+
  theme_bw() +
  theme(legend.position= "top") # legenda 

# posicoes da legenda legend.position: "top", "bottom", "right", "left"

###exportando o grafico no formato png
png(filename = "g5.png",width = 14, height = 10, 
    units = "cm",pointsize = 8, "white",res = 1200)
g5
dev.off()



# mudar cores do scale_fill_viridis_d
# https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html


# Boxplot PESO segundo Tipo de Parto
g6 <- ggplot(sinasc) +
  aes(x = "", y = PESO, fill = PARTO) +
  geom_boxplot() +
  scale_fill_viridis_d() + # cor
  labs(x = "Tipo de parto", y= "Peso",
       title= "Grafico 6: Distribuiçao dos Pesos (em gramas) dos NV segundo Tipo de Parto",
       subtitle = texto,
       fill = "Tipo de Parto",
       caption= "Fonte: SINASC") +
  theme_minimal() +
  theme(legend.position = "top")

###exportando o grafico no formato png
png(filename = "g6.png",width = 14, height = 10, 
    units = "cm",pointsize = 8, "white",res = 1200)
g6
dev.off()

# Boxplot PESO segundo Tipo de Parto 
# sem Ignorado e Nao preench

g7 <- sinasc %>%
  filter(! PARTO %in% c("Ignorado", "Nao preenchido")) %>%
  ggplot() +
  aes(x = "", y = PESO, fill = PARTO) +
  geom_boxplot() +
  scale_fill_hue() + # cor
  labs(x = "Tipo de parto", y= "Peso",
       title= "Grafico 7: Distribuiçao dos Pesos (em gramas) dos NV, segundo Tipo de Parto",
       subtitle = texto ,
       fill = "Tipo de Parto",
       caption= "Fonte: SINASC") +
  theme_minimal() +
  theme(legend.position = "top")

###exportando o grafico no formato png
png(filename = "g7.png",width = 14, height = 10, 
    units = "cm",pointsize = 8, "white",res = 1200)
g7
dev.off()

table(sinasc$PARTO)
# Boxplot (violino) PESO segundo SEXO
# sem ignorado 


g8 <- sinasc %>%
  filter( !(SEXO %in% c("Ignorado")) ) %>%
  ggplot() +
  aes(x = "", y = PESO, fill = SEXO) +
  geom_violin(adjust = 1L, scale = "area") +
  scale_fill_brewer(palette = "Set2") + # cor
  labs(x = "", y= "Peso (em gramas)",
       title= "Grafico 8: Peso ao Nascer, segundo Sexo",
       subtitle = texto ,
       fill = "Sexo", # titulo da legenda
       caption= "Fonte: SINASC") +
  theme_classic()
###exportando o grafico no formato png
png(filename = "g8.png",width = 14, height = 10, 
    units = "cm",pointsize = 8, "white",res = 1200)
g8
dev.off()


# ver corer p comando scale_fill_brewer
# https://ggplot2.tidyverse.org/reference/scale_brewer.html


# Grafico de barras: Distribuiçao da Raça ou Cor materna
g9 <- sinasc %>%
  ggplot() +
  aes(x = RACACORMAE) +
  geom_bar(fill= "lightblue") + # 
  coord_flip() + # coordenada
  labs(x= "Raça ou Cor materna",
       y = "Nº de nascidos vivos", 
       title= "Grafico 9: N. Partos segundo a Raça ou Cor materna",
       subtitle = texto ,
       caption= "Fonte: SINASC") +
  theme_classic(base_size = 12) # muda tamanho das letras

###exportando o grafico no formato png
png(filename = "g9.png",width = 14, height = 10, 
    units = "cm",pointsize = 8, "white",res = 1200)
g9
dev.off()

# Grafico de barras: Distribuiçao da Raça ou Cor materna
# sem NA


tab.raca <- sinasc %>%   
  filter ( !is.na(RACACORMAE) ) %>% 
  group_by(RACACORMAE) %>% tally() %>%
  print()

g10 <- tab.raca %>%
  ggplot() +
  aes(x = reorder(RACACORMAE, n), y = n, goup= 1) +#faz ordenaçao
  geom_bar(stat = "identity", fill= "darkgreen") + 
  coord_flip() + # 
  labs(x= "Raça ou Cor materna",
       y = "Nº de nascidos vivos", 
       title= "Grafico 10: Distribuiçao da Raça ou Cor materna",
       subtitle = texto ,
       caption= "Fonte: DATASUS/SINASC") +
  theme_classic(base_size = 12) # muda tamanho das letras

###exportando o grafico no formato png
png(filename = "g10.png",width = 14, height = 10, 
    units = "cm",pointsize = 8, "white",res = 1200)
g10
dev.off()

# Grafico de barras: Distribuiçao da Raça ou Cor materna
# sem NA
# colocar em o

tab.raca <- sinasc %>%   
  filter ( !is.na(RACACORMAE) ) %>% 
  group_by(RACACORMAE) %>% tally() %>%
  print()

fig1 <- tab.raca %>%
  ggplot() +
  aes(x = reorder(RACACORMAE, - n), y = n, goup= 1) +
  geom_bar(stat = "identity", fill= "darkgreen") +  # cor azul marinho
  coord_flip() + # 
  labs(x= "Raça ou Cor materna",
       y = "Nº de nascidos vivos", 
       title= "Figura 1: Distribuiçao da Raça ou Cor materna",
       subtitle = texto ,
       caption= "Fonte: DATASUS/SINASC") +
  theme_classic(base_size = 12) # muda tamanho das letras

###exportando o grafico no formato png
png(filename = "fig1.png",width = 14, height = 10, 
    units = "cm",pointsize = 8, "white",res = 1200)
fig1
dev.off()

# Grafico de barras: Distribuiçao numero de consultas pre natal 

tab.consultas <- sinasc %>%
  group_by(CONSULTAS) %>%
  tally() %>% print()



fig2 <-ggplot(tab.consultas) +
  aes(x = CONSULTAS, y = n) +
  geom_bar(stat="identity", fill= "darkgreen") + # cor chocolate
  coord_flip() +
  ylim(0, max(tab.consultas$n)+150000) + # ESSE VALOR MUDA DE ACORDO COM VALOR MAXIMO DOS DADOS
  labs(x= "Consultas pre-natal",
       y = "Nº de nascidos vivos", 
       title= "Figura 2: Distribuiçao do Nº de Consultas pre-natal",
       subtitle = texto,
       caption= "Fonte: SINASC") +
  geom_text(aes(label= paste("n =", n)), hjust = - 0.2, 
            size= 4.0, color= "black", fontface = "bold")+ # INSERINDO TEXTO COM CONTAGEM
  theme_classic(base_size = 12)

###exportando o grafico no formato png
png(filename = "fig2.png",width = 12, height = 10, 
    units = "cm",pointsize = 12, "chocolate3",res = 1200)
fig2
dev.off()
#######
# Grafico de pizza / setor: sexo - sem ignorado

tab.sexo <- sinasc %>% filter (! SEXO %in% c("Ignorado")) %>%
  group_by(SEXO) %>%
  tally() %>% print()

tab.sexo <- tab.sexo %>%
  mutate(Porcentagem = scales::percent(n/sum(n))) %>% print()

#ver cores em file:///C:/Users/Marcia/Documents/machine%20learning/Fundamentals_of_Data_Visualization.pdf
library(colorblindcheck)
rainbow_pal = rainbow(n = 7)
rainbow_pal

#grafico de pizza
#labs(x= "",)))




###SALVANDO DADOS DE PORTO ALEGRE - VERIFICAR SE 
# olhar nomes das variaveis
names(sinasc)
# dimensoes do objeto
dim(sinasc) #numero de linhas e colunas
str(sinasc)

##############################################
save(sinasc, file = "dados_nasc_POA.RData")

write.csv(sinasc,"dados_POA.cvs")

###gerando arquivo de teste e treinamento



load(dados_nasc_POA.RData)
####MODELO
summary(model <-  glm(sinasc$CONSPRENAT ~ sinasc$IDADEMAE, family = "poisson"))
plot(sinasc$CONSPRENAT, sinasc$IDADEMAE)
abline(model)


summary(model2 <-  glm(sinasc$CONSPRENAT ~ sinasc$IDADEMAE + sinasc$ESCMAE + sinasc$PARIDADE, family = "poisson"))
#aic reduz a medida que aumenta as variaveis

trainRows <- createDataPartition(sinasc$bp,p=.7,list=FALSE)
trainData <- sinasc[trainRows, ]
testData  <- sinasc[-trainRows, ]


save(trainData, file = "trainData_poa.RData")
save(testData, file = "testData_poa.RData")


write.csv(trainData,"trainData_poa.csv")

write.csv(testData,"testData_poa.csv")


#confusionMatrix(predicao, teste$type)

library(xgboost)



# creating the matrix for training the model
trainData_xg <- xgb.DMatrix(data.matrix(trainData[, colnames(trainData) 'bp']), 
                            label = as.numeric(trainData$bp))

testData_xg <- xgb.DMatrix(data.matrix(testData[, colnames(testData) %ni% 'bp']))

testData_xg <- xgb.DMatrix(data.matrix(testData[, colnames(testData)]))


watchlist <- list(train = trainData_xg, test = testData_xg)


# Define the parameters and cross validate
param <- list("objective" = "reg:linear",
              "eval_metric" = "rmse")
cv.nround <- 5
cv.nfold <- 3
cvMod <- xgb.cv(param=param, data = trainData_xg,
                nfold = cv.nfold, 
                nrounds = cv.nround)
cvMod
data(trainData, package='xgboost')
dtrain <- with(trainData, xgb.DMatrix(data, label = label))
xgb.DMatrix.save(dtrain, 'xgb.DMatrix.data')
dtrain <- xgb.DMatrix('xgb.DMatrix.data')
if (file.exists('xgb.DMatrix.data')) file.remove('xgb.DMatrix.data')

