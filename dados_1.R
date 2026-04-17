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

#dados <- get_series(
  #c(
  #  IBC = 24363,
  #  DIVIDA_LIQUIDA = 4468, #Dívida Líquida do Setor Público - Saldos em R$ milhões - Total - Governo Federal e Banco Central
  ##  PIB =  4385, #mensal em milhões
  ##  SELIC = 432,
  ##  CAMBIO = 1,
  #  IPCA = 433),
  #start_date = "2000-01-01",
  #end_date = "2020-12-30"#
#)

meta_inflacao <- gbcbd_get_series(13521, first.date = "2000-01-01", last.date = "2024-12-31")
IBC <- gbcbd_get_series(24363, first.date = "2000-01-01", last.date = "2020-12-31") # disponivel apenas a partir de  2003
DIVIDA_LIQUIDA <- gbcbd_get_series(4468, first.date = "2000-01-01", last.date = "2020-12-31")
PIB <- gbcbd_get_series(4380, first.date = "2000-01-01", last.date = "2024-12-31")
SELIC <- gbcbd_get_series(432, first.date = "2000-01-01", last.date = "2020-12-31")
CAMBIO <- gbcbd_get_series(1, first.date = "2000-01-01", last.date = "2020-12-31")
IPCA <- gbcbd_get_series(433, first.date = "2000-01-01", last.date = "2024-12-31")
EMBI <- ipeadatar::ipeadata("JPM366_EMBI366")
EXPECTATIVA_INFLACAO <- get_market_expectations(type = "monthly", indicator = "IPCA")
#filtrar apenas o ipca e vou escolher a base 0 (antiga metodologia de calculo a nova so tem a partir de  2014)
EXPECTATIVA_INFLACAO <- EXPECTATIVA_INFLACAO  %>% filter(
  Indicador == "IPCA", baseCalculo == 0 ,Data > "2000-01-01", Data < "2020-12-31")%>% select(
    Indicador,DataReferencia,Data,Media, numeroRespondentes, baseCalculo)%>% mutate(
      DataReferencia = as.Date(paste0("01/", DataReferencia),format = "%d/%m/%Y")) %>% rename(
        expectativa_ipca_media = Media, ref.date = DataReferencia ) %>% group_by(
          ref.date, Data)%>% summarise(
            expectativa_ipca_media = mean(
              expectativa_ipca_media, na.rm = TRUE))

#cambio e selic não tem dia 1, -> dar um groupby para media
meta_inflacao <- meta_inflacao %>% rename(meta_inflacao = value
) %>% select(meta_inflacao, ref.date
) %>% mutate(ano = year(ref.date)
) %>% crossing(mes = 1:12
) %>% mutate(data = as.Date(paste(ano, mes, "01", sep = "-"))
) %>% arrange(data
) %>% select(data,  meta_anual = meta_inflacao
) %>% mutate(meta_inflacao_mensal = ((1 + meta_anual/100)^(1/12) - 1) * 100)

inflacao_menos_meta <- data.frame(desvio_da_meta = IPCA$value - meta_inflacao$meta_inflacao_mensal, data = IPCA$ref.date)


IBC <- IBC %>% rename(IBC = value) %>% select("IBC", "ref.date") %>% filter(format(ref.date, "%d") == "01")
DIVIDA_LIQUIDA <- DIVIDA_LIQUIDA %>% rename(DIVIDA_LIQUIDA = value) %>% select("DIVIDA_LIQUIDA", "ref.date") %>% filter(format(ref.date, "%d") == "01") 
PIB <- PIB %>% rename(PIB = value) %>% select("PIB", "ref.date")%>% filter(format(ref.date, "%d") == "01")
IPCA <- IPCA %>% rename(IPCA = value) %>% select("IPCA", "ref.date")%>% filter(format(ref.date, "%d") == "01")

EMBI <- EMBI %>% rename(EMBI = value, ref.date = date) %>%
  select("EMBI", "ref.date") %>% 
  mutate(ref.date = floor_date(ref.date, "month")) %>%
  group_by(ref.date) %>%
  summarise(EMBI_media= mean(EMBI, na.rm = TRUE))

SELIC <- SELIC %>% filter(format(SELIC$ref.date,"%d") == "01") %>% rename(SELIC = value) %>% select("SELIC", "ref.date")
#SELIC <- SELIC %>% rename(SELIC = value) %>% #nao sei se é a melhor opção
  #select("SELIC", "ref.date") %>%
  #mutate(ref.date = floor_date(ref.date, "month")) %>%
  #group_by(ref.date) %>%
  #summarise(SELIC_media = mean(SELIC, na.rm =TRUE))


CAMBIO <- CAMBIO %>% rename(CAMBIO = value) %>%
  select("CAMBIO", "ref.date") %>%
  mutate(ref.date= floor_date(ref.date, "month")) %>%
  group_by(ref.date) %>%
  summarise(CAMBIO_media = mean(CAMBIO, na.rm = TRUE))


lista_dfs <- list(IBC,DIVIDA_LIQUIDA,PIB,SELIC,CAMBIO,IPCA,EMBI, EXPECTATIVA_INFLACAO)
dados_completos <- reduce(lista_dfs, inner_join, by= "ref.date")

#################################################################
#  agora vou calcular o hiato do PIB
#################################################################

lambda = 14400 # vi que usam esse lambda +para dados mensais
filtro_hp <- hpfilter(PIB$PIB, freq = lambda)
#dados_completos$tendencia_PIB <- filtro_hp$trend
#dados_completos$ciclo_PIB <- filtro_hp$cycle
#dados_completos$hiato <- dados_completos$tendencia_PIB - dados_completos$ciclo_PIB
hiato <- as.data.frame(filtro_hp$cycle)%>% rename(hiato = `filtro_hp$cycle`)

#################################################################
# parte dos graficos#
#################################################################

ggplot(data = dados_completos, mapping = aes(x = ref.date, y = IBC))+
  geom_line( col = "turquoise4") +
  labs(title = "IBC: 2003 - 2020", x = "Data", y = "IBC")

ggplot(data = dados_completos, mapping = aes(x = ref.date, y = DIVIDA_LIQUIDA))+
  geom_line( col = "turquoise4") +
  labs(title = "DIVIDA LÍQUIDA: 2003 - 2020", x = "Data", y = "DIVIDA LÍQUIDA")

ggplot(data = dados_completos, mapping = aes(x = ref.date, y = PIB))+
  geom_line( col = "turquoise4") +
  labs(title = "PIB: 2003 - 2020", x = "Data", y = "PIB")

ggplot(data = dados_completos, mapping = aes(x = ref.date, y = SELIC))+
  geom_line( col = "turquoise4") +
  labs(title = "SELIC: 2003 - 2020", x = "Data", y = "SELIC")


ggplot(data = dados_completos, mapping = aes(x = ref.date, y = CAMBIO_media))+
  geom_line( col = "turquoise4") +
  labs(title = "Média mensal do cambio : 2003 - 2020", x = "Data", y = "CAMBIO")



ggplot(data = dados_completos, mapping = aes(x = ref.date, y = IPCA))+
  geom_line( col = "turquoise4") +
  labs(title = "IPCA : 2003 - 2020", x = "Data", y = "IPCA")

ggplot(data = dados_completos, mapping = aes(x = ref.date, y = EMBI_media))+
  geom_line( col = "turquoise4") +
  labs(title = "Média mensal do EMBI : 2003 - 2020", x = "Data", y = "EMBI")

ggplot(data = dados_completos, mapping = aes(x = ref.date, y = expectativa_ipca_media))+
  geom_line( col = "turquoise4") +
  labs(title = "Média mensal da expectativa do IPCA (FOCUS) : 2003 - 2020", x = "Data", y = "EMBI")

#ggplot(dados_completos, aes(x = ref.date)) +
 # geom_line(aes(y = PIB, color = "PIB")) +
 # geom_line(aes(y = tendencia_PIB, color = "Tendência (HP)")) +
 # labs(title = "PIB vs Tendência (Filtro HP) : 2003 - 2020",
     #  y = "PIB", x = "Data")

#ggplot(dados_completos, aes(x = ref.date)) +
  #geom_line(aes(y = tendencia_PIB, color = "Tendência (HP)")) +
  #geom_line(aes(y = ciclo_PIB, color = "Ciclo")) +
  #labs(title = "Ciclo vs Tendência do PIB (Filtro HP) : 2003 - 2020",
     #  y = "PIB", x = "Data")

#ggplot(data = dados_completos, mapping = aes(x = ref.date, y = hiato))+
  #geom_line( col = "turquoise4") +
  #labs(title = "Hiato do PIB (filtro hp): 2003 - 2020", x = "Data", y = "Hiato")

#ggplot(dados_completos, aes(x = ref.date)) +
  #geom_line(aes(y = PIB, color = "PIB")) +
 # geom_line(aes(y = hiato, color = "hiato")) +
  #labs(title = "PIB vs Hiato do PIB (Filtro HP) : 2003 - 2020",
   #    y = "PIB", x = "Data")



