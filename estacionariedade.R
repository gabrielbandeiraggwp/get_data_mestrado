library(urca)


#teste adf e kpss para desvio da inflação
ggplot(data= inflacao_menos_meta, mapping = aes(x = data, y = desvio_da_meta))+
  geom_line(color = "turquoise4") +
  labs(title = "Desvios da meta de inflação : 2000 - 2024")
adf_inflacao <- ur.df(inflacao_menos_meta$desvio_da_meta, type =c("none"), lags = 6, selectlags = "AIC")
kpss_inflacao <- ur.kpss(inflacao_menos_meta$desvio_da_meta, type = "mu", lags = "long")
summary(adf_inflacao) #estacionaria
summary(kpss_inflacao) #estacionaria
sd(inflacao_menos_meta$desvio_da_meta)
acf(inflacao_menos_meta$desvio_da_meta, plot = TRUE)
pacf(inflacao_menos_meta$desvio_da_meta, plot = TRUE)

#teste adf para hiato do produto
ggplot(data= hiato, mapping = aes(x = seq_along(hiato), y =hiato))+
  geom_line(color = "turquoise4") +
  labs(title = "Hiato : 2000 - 2024", y = "HIATO")
adf_hiato <- ur.df(hiato$hiato, type =c("trend"), lags = 6, selectlags = "AIC")
kpss_hiato <- ur.kpss(hiato$hiato, type = "tau", lags = "long") #obs^o tau é equvalente a "trend"
summary(adf_hiato) #aparentemente estacionaria
summary(kpss_hiato) #estacionaria
sd(hiato$hiato)
acf(hiato$hiato, plot = TRUE)
pacf(hiato$hiato, plot = TRUE)
