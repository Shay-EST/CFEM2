################################################################################
# Autor: Shayane dos Santos Cordeiro                                          #
# Data:  29 de Setembro de 2021
# Adicionando os códigos dos municípios ao banco de dados CFEM
################################################################################

# Bibliotecas utilizadas
# Bibliotecas utilizadas 
library(sf)
library(geobr)
library(ggplot2)
library(dplyr)
library(maps)

# Limpeza do ambiente environment
# Escolha do diretorio
setwd(" ")
options(scipen = 999)
# Dados
base <- read.csv2("CFEM_Distribuicao.csv")

# Filtrando apenas as linhas que representam municípios 
cfem_d <- base %>% filter(Ente=="Município") 
nrow(base) - nrow(cfem_d)

########################################################################

list_geobr() #IPEA

# Carregados os municípios do estado do Pará no ano de 2020, atualizada.
municipios <- read_municipality(year=2020)
head(municipios)


##########################################################
# Unir os dois bancos de dados
##########################################################

# Renomeia a coluna 
municipios <- rename(municipios, Cod.IBGE    = "code_muni")
municipios <- rename(municipios, NomeEnte    = "name_muni")
municipios <- rename(municipios, SiglaEstado = "abbrev_state")

# Passando as letras para minúsculas
municipios$NomeEnte         <- tolower(municipios$NomeEnte)
municipios$SiglaEstado      <- tolower(municipios$SiglaEstado)
cfem_d$NomeEnte             <- tolower(cfem_d$NomeEnte)
cfem_d$SiglaEstado          <- tolower(cfem_d$SiglaEstado)

# Apresenta as primeiras linhas do banco de dados
head(municipios)
head(cfem_d)

# Unir as bases de dados, permanecendo os municípios da esquerda CFEM.
base_cfem <- left_join(cfem_d,municipios, by =c("NomeEnte","SiglaEstado")) 
head(base_cfem)

# Selecionando as colunas de interesse na base cfem
base_cfem1 <- base_cfem %>% 
  select(Cod.IBGE, Ano, Mês, Ente, SiglaEstado, NomeEnte, 
         TipoDistribuição, Substância, TipoAfetamento,Valor)


#################################################################################
#  Analise dos NAS da união das bases: CFEM e municípios
#################################################################################

# Número de linhas
nrow(cfem_d)
nrow(base_cfem1)
# Não houve perda nem duplicação da informação da CFEM quando o resultado for 0.
nrow(base_cfem1) - nrow(cfem_d)


# Verifica se existe alguma variável que contém linha NA e contalibiza o percentual.
NAS <- round(colSums(is.na(base_cfem1))*100/nrow(base_cfem1),2)
NAS

# Quem são essas variáveis ausentes no cod.IBGE
NAS.1 <- base_cfem1%>% filter(is.na(Cod.IBGE) == TRUE)

## será verificado os nomes dos municípios
NAS.2 <- NAS.1 %>% group_by(NomeEnte) %>% summarise(Soma= sum(Valor))

# Os municípios: grão pará, atílio vivacqua e ***  não foram identificados.
# Após verificar a nomenclatura usando a base municípios, alguns ajustes são necessários:

base_cfem1$NomeEnte[base_cfem1$NomeEnte == "grão pará"]       <- "grão-pará"
base_cfem1$NomeEnte[base_cfem1$NomeEnte == "atílio vivacqua"] <- "atílio vivácqua"

# Exclunindo colunas para aplicar o left_join novamente
base_cfem1 <- select(base_cfem1, -Cod.IBGE)
base_cfem1 <- left_join(base_cfem1,municipios, by =c("NomeEnte","SiglaEstado")) 
base_cfem1 <- base_cfem1 %>% 
  select(Cod.IBGE, Ano, Mês, Ente, SiglaEstado, NomeEnte, 
         TipoDistribuição, Substância, TipoAfetamento,Valor)

# Varificando a presença de algum NA
NAS        <- base_cfem1 %>% filter(is.na(Cod.IBGE) == TRUE)

# Retirando as linhas que não representam municípios identificáveis com *** (6 linhas).
base_cfem2   <- base_cfem1 %>% filter(is.na(Cod.IBGE) != TRUE)
nrow(base_cfem1) - nrow(base_cfem2) # Ok linhas excluidas

## Armazena em um arquivo csv( ) o resultado
write.csv2(base_cfem2, "base_cfem_CodIBGE_para_uniao.csv")
















