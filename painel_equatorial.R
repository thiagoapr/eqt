# ------------------------------------------------------------------------------
# Construção do painel da Equatorial: merge base de dados
# User: Thiago Pastorelli Rodrigues
# Update: 15/09/2022
# ------------------------------------------------------------------------------

rm(list=ls())
gc()

library(tidyverse)

# Cadastro 
cad <- readRDS('Desagregado/Data/Temp/cadastro.RDS')

# Cobranca
cob <- readRDS('Desagregado/Data/Temp/cobranca.RDS')

# Merge base de dados
df <- cad %>%
  left_join(cob, by = c('UF', 'CONTA_CONTRATO', 'Data'))

# ------------------------------------------------------------------------------

saveRDS(df, 'Desagregado/Data/painel_equatorial.RDS')

# ------------------------------------------------------------------------------







