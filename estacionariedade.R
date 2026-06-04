library(urca)
library(tseries)
library(aTSA)


#teste adf e kpss para desvio da inflação
ggplot(data= desvio_inflacao_dataframe, mapping = aes(x = Data_mes, y = desvios))+
  geom_line(color = "turquoise4") +
  labs(title = "Desvios da meta de inflação : 2003 - 2024")
adf_inflacao <- ur.df(desvio_inflacao_dataframe$desvios, type =c("drift"))
kpss_inflacao <- ur.kpss(desvio_inflacao_dataframe$desvios, type = "mu")
pp_inflacao <-  ur.pp(desvio_inflacao_dataframe$desvios, 
                    type = "Z-tau",      
                    model = "constant")           


summary(pp_inflacao)
summary(adf_inflacao) 
summary(kpss_inflacao) 

sd(desvio_inflacao_dataframe$desvios)
acf(desvio_inflacao_dataframe$desvios, plot = TRUE)
pacf(desvio_inflacao_dataframe$desvios, plot = TRUE)

#teste adf e kpss para hiato do produto
ggplot(data= hiato, mapping = aes(x = seq_along(hiato), y =hiato))+
  geom_line(color = "turquoise4") +
  labs(title = "Hiato : 2003 - 2024", y = "HIATO")
adf_hiato <- ur.df(hiato$hiato, type =c("drift"))
kpss_hiato <- ur.kpss(hiato$hiato, type = "mu")
pp_hiato <- ur.pp(hiato$hiato, 
                    type = "Z-tau",      
                    model = "constant") 
summary(adf_hiato) #
summary(kpss_hiato) #
summary(pp_hiato)
sd(hiato$hiato)
acf(hiato$hiato, plot = TRUE)
pacf(hiato$hiato, plot = TRUE)

#teste adf e kpss para cambio
ggplot(data= CAMBIO, mapping = aes(x = ref.date, y = value))+
  geom_line(color = "turquoise4") +
  labs(title = "Taxa de câmbio - Livre - Dólar americano (compra) ", y = "Fim de período - mensal : 2003 - 2024")
adf_cambio <- ur.df(CAMBIO$value, type =c("trend"))
kpss_cambio <-ur.kpss(CAMBIO$value, type = "tau")
pp_cambio <- ur.pp(CAMBIO$value, 
                    type = "Z-tau",      
                    model = "trend") 
summary(adf_cambio)
summary(kpss_cambio)
summary(pp_cambio)
sd(CAMBIO$value)
acf(CAMBIO$value, plot = TRUE)
pacf(CAMBIO$value, plot = TRUE)

#teste adf e kpss para o EMBI
ggplot(data= EMBI, mapping = aes(x = ref.date, y = EMBI_media))+
  geom_line(color = "turquoise4") +
  labs(title = "EMBI : 2003 - 2024", y = "EMBI")
adf_EMBI <- ur.df(EMBI$EMBI_media, type =c("drift"))
kpss_EMBI <- ur.kpss(EMBI$EMBI_media, type = "mu")
summary(adf_EMBI) 
summary(kpss_EMBI)
sd(EMBI$EMBI_media)
acf(EMBI$EMBI_media, plot = TRUE)
pacf(EMBI$EMBI_media, plot = TRUE)

#teste adf e kpss para a dívida líquida do setor publico
ggplot(data= DIVIDA_LIQUIDA, mapping = aes(x = ref.date, y = DIVIDA_LIQUIDA))+
  geom_line(color = "turquoise4") +
  labs(title = "Dívida Líquida : 2003 - 2024", y = "Divida Líquida")
adf_divida <- ur.df(DIVIDA_LIQUIDA$DIVIDA_LIQUIDA, type =c("trend"))
kpss_divida <-ur.kpss(DIVIDA_LIQUIDA$DIVIDA_LIQUIDA, type = "tau")
pp_divida <- ur.pp(DIVIDA_LIQUIDA$DIVIDA_LIQUIDA, 
                    type = "Z-tau",      
                    model = "trend")
summary(adf_divida) #
summary(kpss_divida) #
summary(pp_divida)
sd(DIVIDA_LIQUIDA$DIVIDA_LIQUIDA)
acf(DIVIDA_LIQUIDA$DIVIDA_LIQUIDA, plot = TRUE)
pacf(DIVIDA_LIQUIDA$DIVIDA_LIQUIDA, plot = TRUE)

#teste adf e kpss para a Selic defasada (t - 1)
ggplot(data= SELIC, mapping = aes(x = ref.date, y = SELIC_defasado))+
  geom_line(color = "turquoise4") +
  labs(title = "selica: 2003 - 2024", y = "selic")
adf_selic <- ur.df(SELIC$SELIC_defasado, type =c("trend"))
kpss_selic <- ur.kpss(SELIC$SELIC_defasado, type = "tau") #obs^o tau é equvalente a "trend"
pp_selic <- ur.pp(SELIC$SELIC_defasado, 
                    type = "Z-tau",      
                    model = "trend")
summary(adf_selic) #
summary(kpss_selic)
summary(pp_selic)
sd(SELIC$SELIC_defasado)
acf(SELIC$SELIC_defasado, plot = TRUE)
pacf(SELIC$variacao_percentual, plot = TRUE)


#teste adf e kpss para comodities
ggplot(data= COMODITIES, mapping = aes(x = ref.date, y = value))+
  geom_line(color = "turquoise4") +
  labs(title = "comodities : 2003 - 2024", y = "comodities")
adf_COMODITIES <- ur.df(COMODITIES$value, type =c("trend"))
kpss_COMODITIES <- ur.kpss(COMODITIES$value, type = "tau") #obs^o tau é equvalente a "trend"
pp_COMODITIES <- ur.pp(COMODITIES$value, 
                    type = "Z-tau",      
                    model = "trend")
summary(adf_COMODITIES) #
summary(kpss_COMODITIES)
summary(pp_COMODITIES)
sd(COMODITIES$value)
acf(COMODITIES$value, plot = TRUE)
pacf(COMODITIES$value, plot = TRUE)
