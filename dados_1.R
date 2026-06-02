#install.packages(c("rbcb","GetBCBData","dplyr", "ggplot2", "ipeadatar","stargazer", "gridExtra" , "purrr", "lubridate", "mFilter", "tidyverse"))
library(rbcb)
#library(BCB)
library(GetBCBData)
library(dplyr)
library(ggplot2)
library(ipeadatar)
library(stargazer)
library(gridExtra)
library(purrr)
library(lubridate)
library(mFilter)
library(tidyverse)



rm(list = ls())


meta_inflacao <- gbcbd_get_series(13521, first.date = "2003-01-01", last.date = "2025-12-31")
IBC <- gbcbd_get_series(24363, first.date = "2003-01-01", last.date = "2024-12-31") # disponivel apenas a partir de  2003
DIVIDA_LIQUIDA <- gbcbd_get_series(4468, first.date = "2002-01-01", last.date = "2024-12-31")
PIB <- gbcbd_get_series(4380, first.date = "2002-01-01", last.date = "2024-12-31")
SELIC <- gbcbd_get_series(432, first.date = "2003-01-01", last.date = "2024-12-31")
CAMBIO <- gbcbd_get_series(3695, first.date = "2002-09-01", last.date = "2024-12-31")
IPCA <- gbcbd_get_series(433, first.date = "2003-01-01", last.date = "2024-12-31")
PIB_INDUSTRIAL <-  ipeadatar::ipeadata("PAN12_QIIGG12")
EMBI <- ipeadatar::ipeadata("JPM366_EMBI366")
EXPECTATIVA_INFLACAO <- get_market_expectations(type = "monthly", indic  = "IPCA", start_date = "2003-01-01",
  end_date = "2024-12-31" )
#filtrar apenas o ipca e vou escolher a base 0 (antiga metodologia de calculo a nova so tem a partir de  2014)
#EXPECTATIVA_INFLACAO <- EXPECTATIVA_INFLACAO  %>% filter(
 # Indicador == "IPCA", baseCalculo == 0 ,Data > "2003-01-01", Data < "2024-12-31")%>% select(
  #  Indicador,DataReferencia,Data,Media, numeroRespondentes, baseCalculo)%>% mutate(
  #    DataReferencia = as.Date(paste0("01/", DataReferencia),format = "%d/%m/%Y")) %>% rename(
  #      expectativa_ipca_media = Media, ref.date = DataReferencia ) %>% group_by(
   #       ref.date, Data)%>% summarise(
   #         expectativa_ipca_media = mean(
      #        expectativa_ipca_media, na.rm = TRUE))

EXPECTATIVA_INFLACAO <- EXPECTATIVA_INFLACAO %>% mutate(#aqui eu criei uma janela entre a data t e a data t+j e filtrei para pegar so o j, que é 6 meses
  Data = ymd(Data),
  DataReferencia = my(DataReferencia),
  Diff_meses = (year(Data) - year(DataReferencia))*12 +
    (month(Data) - month(DataReferencia))
) %>% filter(abs(Diff_meses) == 12)

EXPECTATIVA_INFLACAO <- EXPECTATIVA_INFLACAO %>% 
  mutate(
    Data = ymd(Data),
    DataReferencia = ymd(DataReferencia),
    Data_mes = floor_date(Data, unit ="month"), #transforma qualquer data no inicio daquele mes
#aqui eu tirei a media com base na data da expectativa e na data da coleta "t"
  )%>% group_by(Data_mes, DataReferencia) %>% summarise(Media =  mean(Media, na.rm = TRUE), groups = "drop")


#cambio e selic não tem dia 1, -> dar um groupby para media
meta_inflacao <- meta_inflacao %>% rename(meta_inflacao = value
) %>% select(meta_inflacao, ref.date
) %>% mutate(ano = year(ref.date)
) %>% crossing(mes = 1:12
) %>% mutate(DataReferencia = as.Date(paste(ano, mes, "01", sep = "-"))
) %>% arrange(DataReferencia
) %>% select(DataReferencia,  meta_anual = meta_inflacao
) %>% mutate(meta_inflacao_mensal = ((1 + meta_anual/100)^(1/12) - 1) * 100
) %>% filter(DataReferencia >= "2003-01-01",DataReferencia < "2026-01-01")


desvio_inflacao_dataframe <- left_join(EXPECTATIVA_INFLACAO, meta_inflacao, by = "DataReferencia") %>% filter(Data_mes >= "2003-01-01", Data_mes < "2025-01-01" ) 
desvio_inflacao_dataframe <- desvio_inflacao_dataframe %>% mutate(desvios = Media - meta_inflacao_mensal)

DIVIDA_LIQUIDA <- DIVIDA_LIQUIDA %>% rename(DIVIDA_LIQUIDA = value) %>% select("DIVIDA_LIQUIDA", "ref.date") %>% filter(format(ref.date, "%d") == "01")
DIVIDA_LIQUIDA <- DIVIDA_LIQUIDA %>%
  mutate(
    ano = year(ref.date),
    mes = month(ref.date),
    # Calcular a dívida do mesmo mês no ano anterior
    divida_ano_anterior = lag(DIVIDA_LIQUIDA, 12),  # 12 meses atrás
    # Calcular a variação em pontos percentuais
    variacao_pp = DIVIDA_LIQUIDA - divida_ano_anterior,
    # Calcular a variação percentual
    variacao_percentual = (DIVIDA_LIQUIDA / divida_ano_anterior - 1) * 100
  ) %>%
  filter(!is.na(variacao_pp))  # Remover os primeiros 12 meses sem dado anterior






IBC <- IBC %>% rename(IBC = value) %>% select("IBC", "ref.date") %>% filter(format(ref.date, "%d") == "01")
 
PIB <- PIB %>% rename(PIB = value) %>% select("PIB", "ref.date")%>% filter(format(ref.date, "%d") == "01")
IPCA <- IPCA %>% rename(IPCA = value) %>% select("IPCA", "ref.date")%>% filter(format(ref.date, "%d") == "01")

EMBI <- EMBI %>% rename(EMBI = value, ref.date = date) %>%
  select("EMBI", "ref.date") %>% 
  mutate(ref.date = floor_date(ref.date, "month")) %>%
  group_by(ref.date) %>%
  summarise(EMBI_media= mean(EMBI, na.rm = TRUE)) %>%
  filter(ref.date >= "2003-01-01")
  

PIB_INDUSTRIAL <- PIB_INDUSTRIAL %>% rename(PIB_INDUSTRIAL = value, ref.date = date) %>%
  select("PIB_INDUSTRIAL", "ref.date") %>% 
  mutate(ref.date = floor_date(ref.date, "month")) %>% filter(ref.date >= "2003-01-01",ref.date < "2025-01-01" )

SELIC <- SELIC %>% filter(format(SELIC$ref.date,"%d") == "01") %>% rename(SELIC = value) %>% select("SELIC", "ref.date")
#SELIC <- SELIC %>% rename(SELIC = value) %>% #nao sei se é a melhor opção
  #select("SELIC", "ref.date") %>%
  #mutate(ref.date = floor_date(ref.date, "month")) %>%
  #group_by(ref.date) %>%
  #summarise(SELIC_media = mean(SELIC, na.rm =TRUE))


#CAMBIO <- CAMBIO %>% rename(CAMBIO = value) %>%
#  select("CAMBIO", "ref.date") %>%
 # mutate(ref.date= floor_date(ref.date, "month")) %>%
 # group_by(ref.date) %>%
 # summarise(CAMBIO_media = mean(CAMBIO, na.rm = TRUE))

CAMBIO <- CAMBIO %>%
  mutate(
   retorno_cambio = ((value / lag(value)) - 1)*100
  ) %>% filter(!is.na(retorno_cambio), ref.date >= "2003-01-01",ref.date < "2025-01-01" )

#juros real -> Taxa Real = (1 + Taxa Selic Mensal) / (1 + IPCA Mensal) - 1
SELIC <- SELIC %>% mutate(
  selic_fator = 1 + SELIC/100
)
IPCA <- IPCA %>% mutate(
  ipca_fator = 1 + IPCA/100
)
SELIC <- SELIC %>% mutate(
  selic_real = (selic_fator / IPCA$ipca_fator)-1)


#lista_dfs <- list(IBC,DIVIDA_LIQUIDA,PIB,SELIC,CAMBIO,IPCA,EMBI, EXPECTATIVA_INFLACAO)
#dados_completos <- reduce(lista_dfs, inner_join, by= "ref.date")

#################################################################
#  agora vou calcular o hiato do PIB industrial
#################################################################

lambda = 14400 # vi que usam esse lambda +para dados mensais
filtro_hp <- hpfilter(PIB_INDUSTRIAL$PIB_INDUSTRIAL, freq = lambda)

#dados_completos$tendencia_PIB <- filtro_hp$trend
#dados_completos$ciclo_PIB <- filtro_hp$cycle
#dados_completos$hiato <- dados_completos$tendencia_PIB - dados_completos$ciclo_PIB
hiato <- as.data.frame(filtro_hp$cycle)%>% rename(hiato = `filtro_hp$cycle`) 
hiato_percentual = hiato$hiato / as.data.frame(filtro_hp$trend) %>% rename(hiato = V1)

#################################################################
# parte dos graficos#
#################################################################

ggplot(data = dados_completos, mapping = aes(x = ref.date, y = IBC))+
  geom_line( col = "turquoise4") +
  labs(title = "IBC: 2003 - 2024", x = "Data", y = "IBC")

ggplot(data = dados_completos, mapping = aes(x = ref.date, y = DIVIDA_LIQUIDA))+
  geom_line( col = "turquoise4") +
  labs(title = "DIVIDA LÍQUIDA: 2003 - 2024", x = "Data", y = "DIVIDA LÍQUIDA")

ggplot(data = dados_completos, mapping = aes(x = ref.date, y = PIB))+
  geom_line( col = "turquoise4") +
  labs(title = "PIB: 2003 - 2024", x = "Data", y = "PIB")

ggplot(data = dados_completos, mapping = aes(x = ref.date, y = SELIC))+
  geom_line( col = "turquoise4") +
  labs(title = "SELIC: 2003 - 2024", x = "Data", y = "SELIC")


ggplot(data = dados_completos, mapping = aes(x = ref.date, y = CAMBIO_media))+
  geom_line( col = "turquoise4") +
  labs(title = "Média mensal do cambio : 2003 - 2024", x = "Data", y = "CAMBIO")



ggplot(data = dados_completos, mapping = aes(x = ref.date, y = IPCA))+
  geom_line( col = "turquoise4") +
  labs(title = "IPCA : 2003 - 2024", x = "Data", y = "IPCA")

ggplot(data = dados_completos, mapping = aes(x = ref.date, y = EMBI_media))+
  geom_line( col = "turquoise4") +
  labs(title = "Média mensal do EMBI : 2003 - 2024", x = "Data", y = "EMBI")

ggplot(data = dados_completos, mapping = aes(x = ref.date, y = expectativa_ipca_media))+
  geom_line( col = "turquoise4") +
  labs(title = "Média mensal da expectativa do IPCA (FOCUS) : 2003 - 2024", x = "Data", y = "EMBI")




