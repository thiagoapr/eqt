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

for(i in (1:length(uf))){

  cad0 <- read.csv(paste0('Desagregado/Data/Raw/Cadastro/',
                          'dados_cadastrais_', uf[i], '.csv'))
  
  cad0 <- cad0 %>%
    mutate(MoveIn = as.numeric(substr(MOVE_IN, 7, 10))*100 + as.numeric(substr(MOVE_IN, 4, 5)))
  
  cad0 <- cad0 %>%
    select(UF, CONTA_CONTRATO, NIVEL_TENSAO, TARIFA_SOCIAL, MoveIn)
  
  cad <- rbind(cad, cad0)

}

# Tratamento

cad <- cad %>%
  separate(NIVEL_TENSAO, c('NivelTensao', 'TensaoUnidade')) %>%
  mutate(NivelTensao = as.integer(NivelTensao)) %>%
  select(-TensaoUnidade)

# ------------------------------------------------------------------------------

# Merge painel e cadastro

df <- df %>%
  left_join(cad, by = c('UF', 'CONTA_CONTRATO'))
  
df <- df %>%
  filter(Data >= MoveIn)
  
saveRDS(df, 'Desagregado/Data/Temp/cadastro.RDS')
  
# ------------------------------------------------------------------------------

  
  
  