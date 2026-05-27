library(urca)


#teste adf e kpss para desvio da inflação
ggplot(data= desvio_inflacao_dataframe, mapping = aes(x = Data_mes, y = desvios))+
  geom_line(color = "turquoise4") +
  labs(title = "Desvios da meta de inflação : 2003 - 2024")
adf_inflacao <- ur.df(desvio_inflacao_dataframe$desvios, type =c("drift"), lags = 6, selectlags = "AIC")
kpss_inflacao <- ur.kpss(desvio_inflacao_dataframe$desvios, type = "mu", lags = "long")
summary(adf_inflacao) #estacionaria
summary(kpss_inflacao) #estacionaria
sd(desvio_inflacao_dataframe$desvios)
acf(desvio_inflacao_dataframe$desvios, plot = TRUE)
pacf(desvio_inflacao_dataframe$desvios, plot = TRUE)

#teste adf e kpss para hiato do produto
ggplot(data= hiato, mapping = aes(x = seq_along(hiato), y =hiato))+
  geom_line(color = "turquoise4") +
  labs(title = "Hiato : 2003 - 2024", y = "HIATO")
adf_hiato <- ur.df(hiato_percentual$hiato, type =c("drift"), lags = 6, selectlags = "AIC")
kpss_hiato <- ur.kpss(hiato_percentual$hiato, type = "mu", lags = "long") #obs^o tau é equvalente a "trend"
summary(adf_hiato) #aparentemente estacionaria
summary(kpss_hiato) #estacionaria
sd(hiato_percentual$hiato)
acf(hiato_percentual$hiato, plot = TRUE)
pacf(hiato_percentual$hiato, plot = TRUE)

#teste adf e kpss para cambio
ggplot(data= CAMBIO, mapping = aes(x = ref.date, y = retorno_cambio))+
  geom_line(color = "turquoise4") +
  labs(title = "Taxa de câmbio - Livre - Dólar americano (compra) ", y = "Fim de período - mensal : 2003 - 2024")
adf_cambio <- ur.df(CAMBIO$retorno_cambio, type =c("drift"), lags = 6, selectlags = "AIC")
kpss_cambio <- ur.kpss(CAMBIO$retorno_cambio, type = "mu", lags = "long") #obs^o tau é equvalente a "trend"
summary(adf_cambio)
summary(kpss_cambio) 
sd(CAMBIO$retorno_cambio)
acf(CAMBIO$retorno_cambio, plot = TRUE)
pacf(CAMBIO$retorno_cambio, plot = TRUE)

#teste adf e kpss para o EMBI
ggplot(data= EMBI, mapping = aes(x = ref.date, y = EMBI_media))+
  geom_line(color = "turquoise4") +
  labs(title = "EMBI : 2003 - 2024", y = "EMBI")
adf_EMBI <- ur.df(EMBI$EMBI_media, type =c("drift"), lags = 6, selectlags = "AIC")
kpss_EMBI <- ur.kpss(EMBI$EMBI_media, type = "tau", lags = "long") #obs^o tau é equvalente a "trend"
summary(adf_EMBI) 
summary(kpss_EMBI)
sd(EMBI$EMBI_media)
acf(EMBI$EMBI_media, plot = TRUE)
pacf(EMBI$EMBI_media, plot = TRUE)

#teste adf e kpss para a dívida líquida do setor publico
ggplot(data= DIVIDA_LIQUIDA, mapping = aes(x = ref.date, y = variacao_percentual))+
  geom_line(color = "turquoise4") +
  labs(title = "Dívida Líquida : 2003 - 2024", y = "Divida Líquida")
adf_divida <- ur.df(DIVIDA_LIQUIDA$variacao_percentual, type =c("trend"), lags = 6, selectlags = "AIC")
kpss_divida <- ur.kpss(DIVIDA_LIQUIDA$variacao_percentual, type = "mu", lags = "long") #obs^o tau é equvalente a "trend"
summary(adf_divida) #aparentemente estacionaria
summary(kpss_divida) #estacionaria
sd(DIVIDA_LIQUIDA$variacao_percentual)
acf(DIVIDA_LIQUIDA$variacao_percentual, plot = TRUE)
pacf(DIVIDA_LIQUIDA$variacao_percentual, plot = TRUE)
