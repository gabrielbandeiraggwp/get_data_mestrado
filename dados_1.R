library(rbcb)
library(GetBCBData)
library(dplyr)
library(ggplot2)
library(ipeadatar)

rm(list = ls())

dados <- get_series(
  c(
    IBC = 24363,
    DIVIDA_LIQUIDA = 4468, #Dívida Líquida do Setor Público - Saldos em R$ milhões - Total - Governo Federal e Banco Central
    PIB =  4385, #mensal em milhões
    SELIC = 432,
    CAMBIO = 1,
    IPCA = 433),
  start_date = "2000-01-01",
  end_date = "2020-12-30"
)

IBC <- gbcbd_get_series(24363, first.date = "2000-01-01", last.date = "2020-12-31")
DIVIDA_LIQUIDA <- gbcbd_get_series(4468, first.date = "2000-01-01", last.date = "2020-12-31")
PIB <- gbcbd_get_series(4385, first.date = "2000-01-01", last.date = "2020-12-31")
SELIC <- gbcbd_get_series(432, first.date = "2000-01-01", last.date = "2020-12-31")
CAMBIO <- gbcbd_get_series(1, first.date = "2000-01-01", last.date = "2020-12-31")
IPCA <- gbcbd_get_series(433, first.date = "2000-01-01", last.date = "2020-12-31")

EMBI <- ipeadatar::ipeadata("JPM366_EMBI366")
