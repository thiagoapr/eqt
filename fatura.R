# ==============================================================================
# Tratamento da base de fatura
# User: Thiago Pastorelli Rodrigues
# Update: 14/10/2022
# ==============================================================================

rm(list=ls())
gc()
options(scipen=999)

library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)
library(fastDummies)

# ------------------------------------------------------------------------------
# Leitura e tratamento
# ------------------------------------------------------------------------------

# Leitura

fatura <- read.csv(paste0('Desagregado/Data/Raw/CPF/Fatura_20220922/',
                          'faturamento ', 'MA', '.csv'),
                   dec = ",") %>%
  arrange(CONTA_CONTRATO, MES_COMPETENCIA, DATA_VENCIMENTO)

# Define data de referencia e vencimento

fatura <- fatura %>%
  mutate(Data = as.numeric(substr(MES_COMPETENCIA, 1, 6)),
         Vencimento = as.numeric(substr(DATA_VENCIMENTO, 1, 6))) 

# Agrega os dados em valores unicos por mes e ano

db <- fatura %>%
  ungroup() %>%
  group_by(CONTA_CONTRATO, Vencimento) %>%
  summarise(Data = max(Data, na.rm = TRUE),
            ConsumoMedido = mean(CONSUMO_MEDIDO, na.rm = TRUE),
            ConsumoFaturado = mean(CONSUMO_FATURADO, na.rm = TRUE),
            ValorEmissao = mean(VLR_EMISSAO, na.rm = TRUE),
            ValorFaturado = mean(VLR_ARRECADADO, na.rm = TRUE))

# Pagamento

db <- db %>%
  mutate(Pago = ifelse(ValorFaturado != 0, 1, 0)) %>%
  mutate(Pago = ifelse(ValorFaturado == ValorEmissao, 1, Pago))

# Data table

db <- as.data.table(db)

# ------------------------------------------------------------------------------
# Construcao da matriz de possibilidades de pagamento
# ------------------------------------------------------------------------------

painel <- data.frame(
  Ano = rep(c(2016:2022), each = 12),
  Mes = rep(c(1:12))
) %>%
  mutate(Data = Ano*100 + Mes)

painel <- painel %>% 
  filter(Data >= 201602) %>%
  filter(Data <= 202208)

data <- painel$Data

matriz <- NULL

for(i in 1:length(data)){
  
  matriz0 <- data.frame(Data = data[i],
                        Vencimento = data)
  
  matriz0 <- matriz0 %>%
    filter(Data >= Vencimento)
  
  matriz <- rbind(matriz, matriz0)
  
}

# ------------------------------------------------------------------------------
# Calculo por contrato
# ------------------------------------------------------------------------------

system.time({ 

contrato <- unique(db$CONTA_CONTRATO)

tab <- NULL

for(i in 1:length(contrato)){
  
  tab0 <- db[CONTA_CONTRATO == contrato[i],] # seleciona contrato
  
  tab1 <- matriz %>%
    left_join(tab0, by = c('Data', 'Vencimento'))
  
  tab1 <- tab1 %>%
    mutate(Pago = ifelse(Data == Vencimento & is.na(Pago), 0, Pago)) 
  
  tab1 <- tab1 %>%
    group_by(Vencimento) %>%
    mutate(Pago = na.locf(Pago, na.rm = FALSE))
  
  tab1 <- tab1 %>%
    mutate(DelPago = ifelse(Pago == 1 & is.na(ValorEmissao) & Data != Vencimento, 1, 0))
  
  tab1 <- tab1 %>%
    group_by(Vencimento) %>%
    mutate(ValorEmissao = sum(ValorEmissao, na.rm = TRUE)) 
  
  tab1 <- tab1 %>%
    mutate(Pago = ifelse(ValorEmissao == 0, 1, Pago)) %>% 
    filter(DelPago != 1) 
  
  # ----------------------------------------------------------------------------
  
  # Quantidade de fatura em atraso
  
  tab1 <- tab1 %>%
    mutate(Atraso_N = ifelse(Data != Vencimento & Pago == 0, 1, 0)) 
  
  # Meses atraso
  
  tab1 <- tab1 %>%
    mutate(DataFormat = ymd(paste0(substr(Data, 1, 4), '-',
                                   substr(Data, 5, 6), '-01'))) %>%
    mutate(VencimentoFormat = ymd(paste0(substr(Vencimento, 1, 4), '-',
                                         substr(Vencimento, 5, 6), '-01')))
  
  tab1 <- tab1 %>%
    mutate(Atraso_T = ifelse(Atraso_N == 1, 
                             interval(VencimentoFormat, DataFormat) %/% months(1),
                             0))

  # Valor atrasado
  
  tab1 <- tab1 %>%
    mutate(Atraso_V = ifelse(Atraso_N >= 1, ValorEmissao, 0)) 
  
  # ----------------------------------------------------------------------------
  
  atraso <- tab1 %>%
    ungroup() %>%
    group_by(Data) %>%
    summarise(Atraso_N = sum(Atraso_N, na.rm = TRUE),
              Atraso_T_max = max(Atraso_T, na.rm = TRUE),
              Atraso_T_sum = sum(Atraso_T, na.rm = TRUE),
              Atraso_V = sum(Atraso_V, na.rm = TRUE))
  
  atraso <- atraso %>%
    mutate(Atraso_T_mean = ifelse(Atraso_N > 0, Atraso_T_sum/Atraso_N, Atraso_T_sum)) %>%
    select(-Atraso_T_sum)
  
  corrente <- tab0 %>%
    ungroup() %>%
    group_by(Vencimento) %>%
    summarise(Contrato = max(CONTA_CONTRATO),
              ConsumoMedido = mean(ConsumoMedido, na.rm = TRUE),
              ConsumoFaturado = mean(ConsumoFaturado, na.rm = TRUE),
              ValorEmissao = mean(ValorEmissao, na.rm = TRUE)) %>%
    rename(Data = Vencimento)
  
  # ----------------------------------------------------------------------------
  
  base <- painel %>%
    left_join(corrente, by = 'Data') %>%
    left_join(atraso, by = 'Data') 

  tab <- rbind(tab, base)

}

})

# ------------------------------------------------------------------------------
# Status comercial e status instalacao no mes de referencia
# ------------------------------------------------------------------------------

status <- fatura %>% 
  mutate(Com = STATUS_COMERCIAL) %>%
  mutate(Inst = STATUS_INSTALACAO) %>% 
  mutate(Inst = ifelse(Inst == 'Instalação não suspensa', 'NS', Inst)) %>% 
  mutate(Inst = ifelse(Inst == 'Instalação complet.suspensa', 'CS', Inst)) %>% 
  mutate(Inst = ifelse(Inst == 'Instalação parcial.suspensa', 'PS', Inst)) %>% 
  mutate(Inst = ifelse(Inst == 'Suspensão iniciada', 'SI', Inst)) 

status <- status %>% 
  dummy_cols(select_columns = c("Inst", 'Com'))

status <- status %>% # Agrega pela data de referencia
  ungroup() %>%
  group_by(CONTA_CONTRATO, Data) %>%
  summarise(Inst_CS = max(Inst_CS, na.rm = TRUE),
            Inst_NS = max(Inst_NS, na.rm = TRUE),
            Inst_PS = max(Inst_PS, na.rm = TRUE),
            Inst_SI = max(Inst_SI, na.rm = TRUE),
            Com_CI = max(Com_CI, na.rm = TRUE),
            Com_CP = max(Com_CP, na.rm = TRUE),
            Com_CR = max(Com_CR, na.rm = TRUE),
            Com_DI = max(Com_DI, na.rm = TRUE),
            Com_DS = max(Com_DS, na.rm = TRUE),
            Com_LG = max(Com_LG, na.rm = TRUE),
            Com_PT = max(Com_PT, na.rm = TRUE))           
            
# ------------------------------------------------------------------------------
# Merge e salva o painel final
# ------------------------------------------------------------------------------            

painel_final <- tab %>%
  left_join(status, by = c('Contrato' = 'CONTA_CONTRATO', 'Data'))

saveRDS(painel_final, 'Desagregado/Data/Temp/fatura.RDS')
