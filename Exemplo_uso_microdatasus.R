##################################################################################################################
#
# UNIVERSIDADE FEDERAL DO RIO DE JANEIRO
# INSTITUTO DE ESTUDOS EM SA�DE COLETIVA
# Projeto de Extens�o: Apoio �s a��es de Vigil�ncia Epidemiol�gica no enfrentamento da epidemia de COVID-19
# T�tulo: Software livre R e a import�ncia do pacote "microdatasus" para os servi�os de vigil�ncia em sa�de
# Autores: Thaynara Alvarenga Trinxet, Wellington da Silva Gon�alves e Karina da Silva Assis C�rrea.
# Orientadores: Nat�lia Santana Paiva, Antonio Jos� Leal Costa, M�nica Miguel Brochini e Ana Paula Razal Dalvi. 
#
##################################################################################################################

# O pacote  "microdatasus" (Saldanha et. al, 2019) e o software livre e gratuito R s�o ferramentas que otimizam a rotina de an�lises das vigil�ncias em sa�de e 
# podem automatizar e facilitar o manejo dos bancos de dados do Datasus. 
# Desse modo, estas ferramentas permitem que pesquisadores, Secretarias Estaduais e Municipais de Sa�de e Coordenadorias de Aten��o Prim�rias em sa�de analisem indicadores e realizem relat�rios de maneira r�pida e v�lida com o objetivo de monitorar o territ�rio. 
# Al�m disso, � um �timo instrumento para fazer an�lises com rapidez para boletins epidemiol�gicos semanais, por exemplo. 
# Ademais, n�o tem custo com o software, pois � gratuito.

# Neste trabalho, vamos utilizar o SIH (Sistema de Interna��es Hospitalares).
# Dentro do SIH, vamos analisar o estado do Rio de Janeiro (RJ) e suas nterna��es por Influenza com desfecho em morte nos anos de 2008 e 2009 

# 1o passo: mudar o diretorio - ok 
# 2o passo: abrir o script (novo ou existente) - ok
# 3o passo: chamar os pacotes na biblioteca (verificar se precisa instalar!)
library(rio) #ok
library(dplyr) #ok
library(tidyverse) #ok
library(ggplot2) #ok
library(dplyr)#ok
library(lubridate) #ok
library(tableone) #Tabelas
library(ggpubr) #gráficos
library(graphics)
library(lattice)

### PACOTE MICRODATASUS
#Precisa de: Internet e do site do DataSUS funcionando.
#DOWNLOAD: SIH,pois � o que vamos trabalhar. 

### INSTALAR O PACOTE
#Instalar o pacote devtools
install.packages("devtools") #este pacote � necess�rio para chamar o microdatasus
library(devtools) #carregando o pacote

#Ap�s instalar o pacote, vamos instalar o pacote microdatasus
devtools::install_github("rfsaldanha/microdatasus") #dentro do pacote devtools, conseguimos carregar o microdatasus
library(microdatasus) #carregando o microdatasus
# ok! Pacote carregado.

### Agora com o microdatasus baixado, podemos baixar os microdados
# Com a fun��o fetch_datasus conseguimos baixar o banco no ano que queremos e no munic�pio que queremos

SIH_RJ_2008_2009 <- fetch_datasus(year_start = 2008,month_start = "1", year_end = 2009, month_end = "12",
                                  uf = c("RJ"), 
                                  information_system = "SIH-RD")

dados.original.SIH <- SIH_RJ_2008_2009 #atribuimos o banco original a um novo banco para mantermos de reseva
#Aqui conseguimos preservar o banco original caso tenha que voltar nele e n�o precisaremos baixar novamente 

##### EXPLICA��O DA FUN��O:
#O primeiro nome � o que voc� quer dar para seu banco, pode escolher qualquer nome
# <- este simbolo permite que voc� atribua toda a fun��o ao nome do objeto que escolheu 
# fetch_datasus � a fun��o ja nativa do banco que permite fazer a extra��o dos dados
# year_start � o primeiro ano que voc� deseja da an�lise
# month_start � o primeiro m�s que voc� deseja da an�lise
# year_end � o �ltimo ano que voc� deseja da an�lise
# month_end � o �ltimo m�s que voc� deseja da an�lise
# UF � a unidade federativa que deseja 
#information_system � o sistema que deseja analisar, poderia ser tamb�m SIM, SINASC 

## Sempre � recomend�vel visualizar o banco como ele est� antes de come�ar a mexer: 
# 5o passo: olhar o objeto
view(SIH_RJ_2008_2009) 
head(SIH_RJ_2008_2009) 
# 6o passo: olhar dimensao da base, numero de linhas e colunas
dim(SIH_RJ_2008_2009) 
ncol(SIH_RJ_2008_2009) 
nrow(SIH_RJ_2008_2009) 
# 7o passo: olhar nome das variaveis
names(SIH_RJ_2008_2009) 

######## Em seguida, vamos fazer o pr�-processamento, ou seja, higienizar o banco #########

SIH_RJ_2008_2009 <- process_sih(SIH_RJ_2008_2009)

#### OK! Banco higienizado ####

############ 1)	Idade e faixas et�rias:
class(SIH_RJ_2008_2009$IDADE) #VARI�VEL IDADE EST� CHARACTER
table(SIH_RJ_2008_2009$COD_IDADE)
#  5    Anos    Dias   Meses 
# 662 1344079   28802   44844 

#Preciso reclassificar para numeric
SIH_RJ_2008_2009 <- SIH_RJ_2008_2009 %>%
  mutate(IDADE = as.numeric (IDADE))

class(SIH_RJ_2008_2009$IDADE) #ok
table(SIH_RJ_2008_2009$IDADE)

# Dessa forma, precisa colocar como 0 as pessoas que n�o completaram 1 ano ainda
# E incluir isto em uma nova vari�vel.

SIH_RJ_2008_2009$idade <- ifelse(SIH_RJ_2008_2009$COD_IDADE == "Anos", SIH_RJ_2008_2009$IDADE, 0)
# Desse modo, criamos uma nova vari�vel e disse que caso n�o seja o tipo Anos, colocar 0.

SIH_RJ_2008_2009$idade
table(SIH_RJ_2008_2009$idade)
class(SIH_RJ_2008_2009$idade)

summary(SIH_RJ_2008_2009$idade)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.0    20.0    37.0    38.8    58.0    99.0 

sd(SIH_RJ_2008_2009$idade) #desvio padr�o
# 24.20769

# Agora vamos criar as faixas et�rias para trabalhar
# Jovens = 0 a 19 anos
# Adulto = 20 a 59 anos
# Idoso = 60 anos ou +

# Agora precisa criar uma nova vari�vel que vai cortar as idades em faixas et�rias

SIH_RJ_2008_2009 <- SIH_RJ_2008_2009 %>%
  mutate(fx_etaria = cut(idade, breaks = c(0,19,60,Inf), include.lowest = TRUE))

table(SIH_RJ_2008_2009$fx_etaria) #ok
#  [0,19]  (19,60] (60,Inf] 
# 330875   779948   307564

#ok, agora vamos criar o nome das faixas et�rias

SIH_RJ_2008_2009 <- SIH_RJ_2008_2009 %>%
  mutate(fx_etaria = cut(idade, breaks = c(0,19,60,Inf), include.lowest = TRUE,
                         labels = c("Jovem", "Adulto", "Idoso")))

table(SIH_RJ_2008_2009$fx_etaria) 
#  Jovem  Adulto  Idoso 
# 330875  779948 307564


############ 2)	Ver a classifica��o das vari�veis que vamos trabalhar se est�o de forma correta e re classificar se for o caso ::::: O PR�-PROCESSAMENTO FAZ ISSO SOZINHO
#Num�rico ---> Uma vari�vel ou s�rie ser� armazenada como dados num�ricos se os valores 
#forem n�meros ou se os valores contiverem decimais.

#Inteiro ---> O tipo de dados inteiro �, na verdade, um caso especial de dados num�ricos. 
#Os inteiros s�o dados num�ricos sem decimais.

#Character ---> O caractere de tipo de dados � usado ao armazenar texto, conhecido como strings em R. 

#Factor ---> Vari�veis de fator s�o um caso especial de vari�veis de caractere porque
#tamb�m cont�m texto. Normalmente representa uma vari�vel categ�rica. SEXO: F, M

class(SIH_RJ_2008_2009$SEXO) #character
class(SIH_RJ_2008_2009$RACA_COR) #character
class(SIH_RJ_2008_2009$VAL_TOT) #character
class(SIH_RJ_2008_2009$MORTE) #character
class(SIH_RJ_2008_2009$DIAG_PRINC) #character

# Conhecer como as vari�veis e suas categorias est�o dispostas:
table(SIH_RJ_2008_2009$SEXO)
# Feminino Masculino 
# 812124    606263

table(SIH_RJ_2008_2009$RACA_COR)
# Amarela   Branca Ind�gena    Parda    Preta 
# 3692   409362     1357   466985   112093

table(SIH_RJ_2008_2009$MORTE)
# N�o       Sim 
# 1359410   58977 

table(SIH_RJ_2008_2009$DIAG_PRINC)
# A000   A001   A009   A010   .....
# 181    244    101     39

############ 4) Selecionamos apenas as vari�veis que queremos
SIH_RJ <- SIH_RJ_2008_2009 %>%
  select("RACA_COR","SEXO", "fx_etaria", "DIAG_PRINC", "MORTE")

SIH_RJ <- SIH_RJ_2008_2009 %>% dplyr::select("RACA_COR","SEXO", "fx_etaria", "DIAG_PRINC", "MORTE")

# SIH_RJ: Nome do novo banco que vamos criar
# SIH_RJ_2008_2009: Nome do banco original que vamos extrair as vari�veis
# Fun��o select: seleciona as vari�veis que desejamos
# Se atentar de colocar as vari�veis entre parenteses e escrever o correto nome que est�o.
# Caso seja necess�rio, pode utilizar a seguinte fun��o para ver o nome das vari�veis: 
names(SIH_RJ_2008_2009)

############ 3) CID
#A partir da bibiografia vamos avaliar quais os CIDs devem entrar nesta an�lise de Influenza
# Ap�s escolher, fazemor uma nova fun��o para agrupar todos os CIDs em um s� objeto "1"

SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J060"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J090"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J100"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J101"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J108"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J110"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J111"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J118"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J128"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J129"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J156"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J158"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J180"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J181"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J182"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J188"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J189"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J170"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J178"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J130"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J060"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J068"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J140"] <- 1
SIH_RJ$diag2 [SIH_RJ$DIAG_PRINC == "J168"] <- 1

#Ap�s agrupar todos os CIDs no objeto "1", podemos dar um nome a este objeto 
SIH_RJ$diag2 [SIH_RJ$diag2 == 1] <- "Influenza"

#Ap�s estas etapas, os CIDs que n�o escolhemos receberam NA
#Dessa forma, precisamos retir�-los para fazer uma boa apresenta��o
SIH_RJ <- SIH_RJ %>% filter(!is.na(diag2))
SIH_RJ <- SIH_RJ %>% filter(!is.na(RACA_COR))
SIH_RJ <- SIH_RJ %>% filter(!is.na(SEXO))
SIH_RJ <- SIH_RJ %>% filter(!is.na(MORTE))

#Podemos saber a quantidade de NAs que tem nas vari�veis a partir da fun��o:

apply(SIH_RJ, MARGIN = 2, FUN = function(x) sum(is.na(x)))
#RACA_COR         SEXO    fx_etaria   DIAG_PRINC        MORTE tempointerna      VAL_TOT        diag2 
#0            0            0            0            0            0            0            0 

### Agora vamos filtrar para apenas MORTE = SIM, pois vamos analisar apenas os casos que tiveram �bito:
RJ_MORTESIM <- SIH_RJ %>%
  filter(MORTE == "Sim")
#ok!


######## Gr�ficos e Tabelas: 

#Primeira tabela, uma an�lise das vari�veis dos pacientes que evoluiram para �bito tendo como diagn�stico influenza
An�lise1 <- tableStack(c(RACA_COR, SEXO, MORTE, fx_etaria),
                       by = diag2, dataFrame = RJ_MORTESIM, simulate.p.value = T,
                       na.rm = TRUE, total.column = TRUE, percent = c("column"))

An�lise1 #Vendo a tabela 
write.csv(An�lise1, "An�lise1.csv") #foi salvo no meu diret�rio em formato de excel

# Primeiro gr�fico escolhido foi a pir�mide et�ria: 
piramide_sexo_faixaetaria <- RJ_MORTESIM %>% #Atribuir um objeto as vari�veis
  group_by(`fx_etaria`, SEXO) %>%
  tally

piramide_sexo_faixaetaria #objeto criado

windows() #abre uma nova janela para melhor visualizar os gr�ficos

ggplot(piramide_sexo_faixaetaria) + 
  aes(x = factor(`fx_etaria`),
      y = ifelse(test = SEXO == "Feminino",  yes = -n, no = n), 
      fill = SEXO) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = (max(piramide_sexo_faixaetaria$n))* c(-1,1)) +
  labs(y = "Pacientes Hospitalizados com evolu��o para �bito por influenza", x = "", title = "Pacientes Hospitalizados com evolu��o para �bito", subtitle = "por influenza no estado do Rio de Janeiro de 2008 a 2009", caption = "Fonte: SIH-SUS") +
  scale_fill_brewer(palette = "Pastel1") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# ok, pir�mide et�ria pronta!

#Agora para vermos a ra�a/cor dos pacientes hospitalizados com evolu��o para �bito:
# Primeiro cria um objeto dizendo o banco que deve retirar as informa��es e a vari�vel
raca <- RJ_MORTESIM %>%
  group_by(RACA_COR) %>%
  tally  

#Depois colocamos este objeto como porcentagem, pois vamos poder ter uma melhor dimens�o das categorias 
raca$porcent <- round((100 * raca$n / sum(raca$n)),1)

#E ent�o criamos o gr�fico
ggplot(raca, aes(x= reorder(RACA_COR, -porcent), y= porcent)) +
  geom_bar(stat="identity", fill= "paleturquoise3") +
  coord_flip()+ #faz coluna virar barra
  labs( y = "Pacientes Hospitalizados com evolu��o para �bito por influenza no estado do Rio de Janeiro em 2008 e 2009 segundo ra�a/cor", x = "Ra�a/cor", caption = "Fonte: SIH-SUS", options(OutDec=",") )+ 
  ylim(0 , max(raca$porcent)+5) + 
  geom_text(aes(label= paste0(porcent, "%")), hjust= - 0.2, size = 4) +
  theme_classic(base_size = 14) 
#ok, gr�fico pronto!