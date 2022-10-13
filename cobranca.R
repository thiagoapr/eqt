# ==============================================================================
# Tratamento da base de cobranca da Equatorial
# User: Thiago Pastorelli Rodrigues
# Update: 15/09/2022
# ==============================================================================

rm(list=ls())
gc()
library(tidyverse)

# ------------------------------------------------------------------------------

# AL, MA, PA

uf <- c('al', 'ma', 'pa')

df <- NULL

for(i in (1:length(uf))){

    # Cobranca
    
    cobranca <- read.csv(paste0('Desagregado/Data/Raw/Cobranca/Cobranca/',
                                'historico_cobranca_', uf[i], '.csv'))
    
    cobranca <- cobranca %>%
      mutate(Data = as.numeric(substr(DATA_GERACAO, 1, 6)),
             Unit = 1) 
    
    cobranca <- cobranca %>%
      select(CONTA_CONTRATO, Data, ETAPA_COBRANCA, Unit) %>%
      distinct() %>%
      pivot_wider(names_from = ETAPA_COBRANCA, values_from = Unit)

    # Protesto
    
    protesto <- read.csv(paste0('Desagregado/Data/Raw/Cobranca/Protesto/',
                                'historico_protesto_negativacao_', uf[i], '.csv'))
    
    protesto <- protesto %>%
      mutate(Data = as.numeric(substr(DATA_GERACAO, 1, 6)),
             Unit = 1) %>%
      arrange(CONTA_CONTRATO, Data)
    
    protesto <- protesto %>%
      select(CONTA_CONTRATO, Data, ETAPA_COBRANCA, Unit) %>%
      distinct() %>%
      pivot_wider(names_from = ETAPA_COBRANCA, values_from = Unit)

    # Merge
    
    cobranca <- cobranca %>%
      full_join(protesto, by = c('CONTA_CONTRATO', 'Data'))
    
    cobranca <- cobranca %>%
      mutate(UF = toupper(uf[i]))
    
    df <- plyr::rbind.fill(df, cobranca)

}

# ------------------------------------------------------------------------------

# PI e RS

uf <- c('pi', 'rs')

df2 <- NULL

for(i in (1:length(uf))){

    # Cobranca
    
    cobranca <- read.csv(paste0('Desagregado/Data/Raw/Cobranca/Cobranca/',
                                'historico_cobranca_', uf[i], '.csv'))
    
    cobranca <- cobranca %>%
      mutate(Data = as.numeric(substr(DATA_GERACAO, 7, 10))*100 + as.numeric(substr(DATA_GERACAO, 4, 5)),
             Unit = 1) 
    
    cobranca <- cobranca %>%
      select(CONTA_CONTRATO, Data, ETAPA_COBRANCA, Unit) %>%
      distinct() %>%
      pivot_wider(names_from = ETAPA_COBRANCA, values_from = Unit)
    
    # Protesto
    
    protesto <- read.csv(paste0('Desagregado/Data/Raw/Cobranca/Protesto/',
                                'historico_protesto_negativacao_', uf[i], '.csv'))
    
    protesto <- protesto %>%
      mutate(Data = as.numeric(substr(DATA_GERACAO, 7, 10))*100 + as.numeric(substr(DATA_GERACAO, 4, 5)),
             Unit = 1) 
    
    protesto <- protesto %>%
      select(CONTA_CONTRATO, Data, ETAPA_COBRANCA, Unit) %>%
      distinct() %>%
      pivot_wider(names_from = ETAPA_COBRANCA, values_from = Unit)
    
    # Merge
    
    cobranca <- cobranca %>%
      full_join(protesto, by = c('CONTA_CONTRATO', 'Data'))
    
    cobranca <- cobranca %>%
      mutate(UF = toupper(uf[i]))
      
    df2 <- plyr::rbind.fill(df2, cobranca)

}

# ------------------------------------------------------------------------------

# Merge

df <- plyr::rbind.fill(df, df2)

saveRDS(df, 'Desagregado/Data/Temp/cobranca.RDS')

