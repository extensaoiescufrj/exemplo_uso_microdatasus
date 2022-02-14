##################################################################################################################
#
# UNIVERSIDADE FEDERAL DO RIO DE JANEIRO
# INSTITUTO DE ESTUDOS EM SAÚDE COLETIVA
# Projeto de Extensão: Apoio às ações de Vigilância Epidemiológica no enfrentamento da epidemia de COVID-19
# Título: Software livre R e a importância do pacote "microdatasus" para os serviços de vigilância em saúde
# Autores: Thaynara Alvarenga Trinxet, Wellington da Silva Gonçalves e Karina da Silva Assis Côrrea.
# Orientadores: Natália Santana Paiva, Antonio José Leal Costa, Mônica Miguel Brochini e Ana Paula Razal Dalvi. 
#
##################################################################################################################

# O pacote  "microdatasus" (Saldanha et. al, 2019) e o software livre e gratuito R são ferramentas que otimizam a rotina de análises das vigilâncias em saúde e 
# podem automatizar e facilitar o manejo dos bancos de dados do Datasus. 
# Desse modo, estas ferramentas permitem que pesquisadores, Secretarias Estaduais e Municipais de Saúde e Coordenadorias de Atenção Primárias em saúde analisem indicadores e realizem relatórios de maneira rápida e válida com o objetivo de monitorar o território. 
# Além disso, é um ótimo instrumento para fazer análises com rapidez para boletins epidemiológicos semanais, por exemplo. 
# Ademais, não tem custo com o software, pois é gratuito.

# Neste trabalho, vamos utilizar o SIH (Sistema de Internações Hospitalares).
# Dentro do SIH, vamos analisar o estado do Rio de Janeiro (RJ) e suas nternações por Influenza com desfecho em morte nos anos de 2008 e 2009 

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
library(ggpubr) #grÃ¡ficos
library(graphics)
library(lattice)

### PACOTE MICRODATASUS
#Precisa de: Internet e do site do DataSUS funcionando.
#DOWNLOAD: SIH,pois é o que vamos trabalhar. 

### INSTALAR O PACOTE
#Instalar o pacote devtools
install.packages("devtools") #este pacote é necessário para chamar o microdatasus
library(devtools) #carregando o pacote

#Após instalar o pacote, vamos instalar o pacote microdatasus
devtools::install_github("rfsaldanha/microdatasus") #dentro do pacote devtools, conseguimos carregar o microdatasus
library(microdatasus) #carregando o microdatasus
# ok! Pacote carregado.

### Agora com o microdatasus baixado, podemos baixar os microdados
# Com a função fetch_datasus conseguimos baixar o banco no ano que queremos e no município que queremos

SIH_RJ_2008_2009 <- fetch_datasus(year_start = 2008,month_start = "1", year_end = 2009, month_end = "12",
                                  uf = c("RJ"), 
                                  information_system = "SIH-RD")

dados.original.SIH <- SIH_RJ_2008_2009 #atribuimos o banco original a um novo banco para mantermos de reseva
#Aqui conseguimos preservar o banco original caso tenha que voltar nele e não precisaremos baixar novamente 

##### EXPLICAÇÃO DA FUNÇÃO:
#O primeiro nome é o que você quer dar para seu banco, pode escolher qualquer nome
# <- este simbolo permite que você atribua toda a função ao nome do objeto que escolheu 
# fetch_datasus é a função ja nativa do banco que permite fazer a extração dos dados
# year_start é o primeiro ano que você deseja da análise
# month_start é o primeiro mês que você deseja da análise
# year_end é o último ano que você deseja da análise
# month_end é o último mês que você deseja da análise
# UF é a unidade federativa que deseja 
#information_system é o sistema que deseja analisar, poderia ser também SIM, SINASC 

## Sempre é recomendável visualizar o banco como ele está antes de começar a mexer: 
# 5o passo: olhar o objeto
view(SIH_RJ_2008_2009) 
head(SIH_RJ_2008_2009) 
# 6o passo: olhar dimensao da base, numero de linhas e colunas
dim(SIH_RJ_2008_2009) 
ncol(SIH_RJ_2008_2009) 
nrow(SIH_RJ_2008_2009) 
# 7o passo: olhar nome das variaveis
names(SIH_RJ_2008_2009) 

######## Em seguida, vamos fazer o pré-processamento, ou seja, higienizar o banco #########

SIH_RJ_2008_2009 <- process_sih(SIH_RJ_2008_2009)

#### OK! Banco higienizado ####

############ 1)	Idade e faixas etárias:
class(SIH_RJ_2008_2009$IDADE) #VARIÁVEL IDADE ESTÁ CHARACTER
table(SIH_RJ_2008_2009$COD_IDADE)
#  5    Anos    Dias   Meses 
# 662 1344079   28802   44844 

#Preciso reclassificar para numeric
SIH_RJ_2008_2009 <- SIH_RJ_2008_2009 %>%
  mutate(IDADE = as.numeric (IDADE))

class(SIH_RJ_2008_2009$IDADE) #ok
table(SIH_RJ_2008_2009$IDADE)

# Dessa forma, precisa colocar como 0 as pessoas que não completaram 1 ano ainda
# E incluir isto em uma nova variável.

SIH_RJ_2008_2009$idade <- ifelse(SIH_RJ_2008_2009$COD_IDADE == "Anos", SIH_RJ_2008_2009$IDADE, 0)
# Desse modo, criamos uma nova variável e disse que caso não seja o tipo Anos, colocar 0.

SIH_RJ_2008_2009$idade
table(SIH_RJ_2008_2009$idade)
class(SIH_RJ_2008_2009$idade)

summary(SIH_RJ_2008_2009$idade)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.0    20.0    37.0    38.8    58.0    99.0 

sd(SIH_RJ_2008_2009$idade) #desvio padrão
# 24.20769

# Agora vamos criar as faixas etárias para trabalhar
# Jovens = 0 a 19 anos
# Adulto = 20 a 59 anos
# Idoso = 60 anos ou +

# Agora precisa criar uma nova variável que vai cortar as idades em faixas etárias

SIH_RJ_2008_2009 <- SIH_RJ_2008_2009 %>%
  mutate(fx_etaria = cut(idade, breaks = c(0,19,60,Inf), include.lowest = TRUE))

table(SIH_RJ_2008_2009$fx_etaria) #ok
#  [0,19]  (19,60] (60,Inf] 
# 330875   779948   307564

#ok, agora vamos criar o nome das faixas etárias

SIH_RJ_2008_2009 <- SIH_RJ_2008_2009 %>%
  mutate(fx_etaria = cut(idade, breaks = c(0,19,60,Inf), include.lowest = TRUE,
                         labels = c("Jovem", "Adulto", "Idoso")))

table(SIH_RJ_2008_2009$fx_etaria) 
#  Jovem  Adulto  Idoso 
# 330875  779948 307564


############ 2)	Ver a classificação das variáveis que vamos trabalhar se estão de forma correta e re classificar se for o caso ::::: O PRÉ-PROCESSAMENTO FAZ ISSO SOZINHO
#Numérico ---> Uma variável ou série será armazenada como dados numéricos se os valores 
#forem números ou se os valores contiverem decimais.

#Inteiro ---> O tipo de dados inteiro é, na verdade, um caso especial de dados numéricos. 
#Os inteiros são dados numéricos sem decimais.

#Character ---> O caractere de tipo de dados é usado ao armazenar texto, conhecido como strings em R. 

#Factor ---> Variáveis de fator são um caso especial de variáveis de caractere porque
#também contém texto. Normalmente representa uma variável categórica. SEXO: F, M

class(SIH_RJ_2008_2009$SEXO) #character
class(SIH_RJ_2008_2009$RACA_COR) #character
class(SIH_RJ_2008_2009$VAL_TOT) #character
class(SIH_RJ_2008_2009$MORTE) #character
class(SIH_RJ_2008_2009$DIAG_PRINC) #character

# Conhecer como as variáveis e suas categorias estão dispostas:
table(SIH_RJ_2008_2009$SEXO)
# Feminino Masculino 
# 812124    606263

table(SIH_RJ_2008_2009$RACA_COR)
# Amarela   Branca Indígena    Parda    Preta 
# 3692   409362     1357   466985   112093

table(SIH_RJ_2008_2009$MORTE)
# Não       Sim 
# 1359410   58977 

table(SIH_RJ_2008_2009$DIAG_PRINC)
# A000   A001   A009   A010   .....
# 181    244    101     39

############ 4) Selecionamos apenas as variáveis que queremos
SIH_RJ <- SIH_RJ_2008_2009 %>%
  select("RACA_COR","SEXO", "fx_etaria", "DIAG_PRINC", "MORTE")

SIH_RJ <- SIH_RJ_2008_2009 %>% dplyr::select("RACA_COR","SEXO", "fx_etaria", "DIAG_PRINC", "MORTE")

# SIH_RJ: Nome do novo banco que vamos criar
# SIH_RJ_2008_2009: Nome do banco original que vamos extrair as variáveis
# Função select: seleciona as variáveis que desejamos
# Se atentar de colocar as variáveis entre parenteses e escrever o correto nome que estão.
# Caso seja necessário, pode utilizar a seguinte função para ver o nome das variáveis: 
names(SIH_RJ_2008_2009)

############ 3) CID
#A partir da bibiografia vamos avaliar quais os CIDs devem entrar nesta análise de Influenza
# Após escolher, fazemor uma nova função para agrupar todos os CIDs em um só objeto "1"

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

#Após agrupar todos os CIDs no objeto "1", podemos dar um nome a este objeto 
SIH_RJ$diag2 [SIH_RJ$diag2 == 1] <- "Influenza"

#Após estas etapas, os CIDs que não escolhemos receberam NA
#Dessa forma, precisamos retirá-los para fazer uma boa apresentação
SIH_RJ <- SIH_RJ %>% filter(!is.na(diag2))
SIH_RJ <- SIH_RJ %>% filter(!is.na(RACA_COR))
SIH_RJ <- SIH_RJ %>% filter(!is.na(SEXO))
SIH_RJ <- SIH_RJ %>% filter(!is.na(MORTE))

#Podemos saber a quantidade de NAs que tem nas variáveis a partir da função:

apply(SIH_RJ, MARGIN = 2, FUN = function(x) sum(is.na(x)))
#RACA_COR         SEXO    fx_etaria   DIAG_PRINC        MORTE tempointerna      VAL_TOT        diag2 
#0            0            0            0            0            0            0            0 

### Agora vamos filtrar para apenas MORTE = SIM, pois vamos analisar apenas os casos que tiveram óbito:
RJ_MORTESIM <- SIH_RJ %>%
  filter(MORTE == "Sim")
#ok!


######## Gráficos e Tabelas: 

#Primeira tabela, uma análise das variáveis dos pacientes que evoluiram para óbito tendo como diagnóstico influenza
Análise1 <- tableStack(c(RACA_COR, SEXO, MORTE, fx_etaria),
                       by = diag2, dataFrame = RJ_MORTESIM, simulate.p.value = T,
                       na.rm = TRUE, total.column = TRUE, percent = c("column"))

Análise1 #Vendo a tabela 
write.csv(Análise1, "Análise1.csv") #foi salvo no meu diretório em formato de excel

# Primeiro gráfico escolhido foi a pirâmide etária: 
piramide_sexo_faixaetaria <- RJ_MORTESIM %>% #Atribuir um objeto as variáveis
  group_by(`fx_etaria`, SEXO) %>%
  tally

piramide_sexo_faixaetaria #objeto criado

windows() #abre uma nova janela para melhor visualizar os gráficos

ggplot(piramide_sexo_faixaetaria) + 
  aes(x = factor(`fx_etaria`),
      y = ifelse(test = SEXO == "Feminino",  yes = -n, no = n), 
      fill = SEXO) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = abs, limits = (max(piramide_sexo_faixaetaria$n))* c(-1,1)) +
  labs(y = "Pacientes Hospitalizados com evolução para óbito por influenza", x = "", title = "Pacientes Hospitalizados com evolução para óbito", subtitle = "por influenza no estado do Rio de Janeiro de 2008 a 2009", caption = "Fonte: SIH-SUS") +
  scale_fill_brewer(palette = "Pastel1") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# ok, pirâmide etária pronta!

#Agora para vermos a raça/cor dos pacientes hospitalizados com evolução para óbito:
# Primeiro cria um objeto dizendo o banco que deve retirar as informações e a variável
raca <- RJ_MORTESIM %>%
  group_by(RACA_COR) %>%
  tally  

#Depois colocamos este objeto como porcentagem, pois vamos poder ter uma melhor dimensão das categorias 
raca$porcent <- round((100 * raca$n / sum(raca$n)),1)

#E então criamos o gráfico
ggplot(raca, aes(x= reorder(RACA_COR, -porcent), y= porcent)) +
  geom_bar(stat="identity", fill= "paleturquoise3") +
  coord_flip()+ #faz coluna virar barra
  labs( y = "Pacientes Hospitalizados com evolução para óbito por influenza no estado do Rio de Janeiro em 2008 e 2009 segundo raça/cor", x = "Raça/cor", caption = "Fonte: SIH-SUS", options(OutDec=",") )+ 
  ylim(0 , max(raca$porcent)+5) + 
  geom_text(aes(label= paste0(porcent, "%")), hjust= - 0.2, size = 4) +
  theme_classic(base_size = 14) 
#ok, gráfico pronto!