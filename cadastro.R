# ------------------------------------------------------------------------------
# Tratamento da base de cadastro da Equatorial
# User: Thiago Pastorelli Rodrigues
# Update: 15/09/2022
# ------------------------------------------------------------------------------

# Periodo: 201601 - 202208
# UF: AL, MA, PA, PI, RS

rm(list=ls())
gc()

library(tidyverse)

# ------------------------------------------------------------------------------

# Painel

inicio <- 2016
fim <- 2022

sorteio <- read.csv2('Desagregado/Data/Temp/Sorteio/sorteio_equatorial.csv') %>% 
  distinct() %>%
  arrange(CONTA_CONTRATO)

df <- NULL
for(j in inicio:fim) {
  for(i in 1:12) {
    df0 <- sorteio %>% mutate(Mes = i, Ano = j)
    df <- rbind(df, df0)
  }
}
 
df <- df %>%
  mutate(Data = Ano * 100 + Mes) %>%
  arrange(UF, CONTA_CONTRATO, Data)

# Selecao

df <- df %>%
  filter(UF != 'AP') %>%
  filter(Data <= 202208)

df <- df %>%
  select(UF, CONTA_CONTRATO, CPF, Data)

# ------------------------------------------------------------------------------

# Cadastro

uf <- c('al', 'ma', 'pa', 'pi', 'rs')

cad <- NULL

i <- 1

for(i in (1:length(uf))){

  # Base de cadastro selecionado
  
  cad0 <- read.csv(paste0('Desagregado/Data/Raw/CPF/Cadastro/',
                          'dados_cadastrais_', uf[i], '.csv'))
  
  cad0 <- cad0 %>%
    mutate(MoveIn = as.numeric(substr(MOVE_IN, 7, 10))*100 + as.numeric(substr(MOVE_IN, 4, 5)))
  
  cad0 <- cad0 %>%
    select(UF, CONTA_CONTRATO, NIVEL_TENSAO, TARIFA_SOCIAL, MoveIn)
  

  # Base de cadastrao completo (recupera CEP)
  
  cad1 <- read.csv(paste0('Desagregado/Data/Raw/Cadastro_completo/csv/',
                          'dados_cadastrais_', uf[i], '.csv'))
  
  cad1 <- cad1 %>% 
    select(CONTA_CONTRATO, CEP, DATA_PRIMEIRA_FATURA)
  
  # Merge
  
  cad0 <- cad0 %>%
    left_join(cad1, by = 'CONTA_CONTRATO')
  
  cad <- rbind(cad, cad0)
  
}

# Tratamento

cad <- cad %>%
  separate(NIVEL_TENSAO, c('NivelTensao', 'TensaoUnidade')) %>%
  mutate(NivelTensao = as.integer(NivelTensao)) %>%
  select(-TensaoUnidade)

cad <- cad %>%
  mutate(PrimeiraFatura = as.numeric(substr(DATA_PRIMEIRA_FATURA, 7, 10)) * 100 +
           as.numeric(substr(DATA_PRIMEIRA_FATURA, 4, 5))) %>%
  select(-DATA_PRIMEIRA_FATURA)

# ------------------------------------------------------------------------------

# Merge painel e cadastro

df <- df %>%
  left_join(cad, by = c('UF', 'CONTA_CONTRATO'))
  
df <- df %>%
  filter(Data >= PrimeiraFatura)
  
saveRDS(df, 'Desagregado/Data/Temp/cadastro.RDS')
  
# ------------------------------------------------------------------------------

  
  
  