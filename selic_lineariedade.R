library(tsDyn)      # Para modelos TAR
library(lmtest)     # Para testes de diagnóstico
library(car)        # Para testes Wald
library(forecast)   # Para testes de autocorrelação


model1_tar <- setar(SELIC$SELIC, 
                    m = 2,           # ordem AR (defasagens)
                    d = 1,           # variável threshold
                    steps = 1,       # passos à frente
                    thDelay = 0)     # delay do threshold

summary(model1_tar)





r2_tar <- function(modelo) {
  residuos <- residuals(modelo)
  y <- modelo$model[,1]
  rss <- sum(residuos^2)
  tss <- sum((y - mean(y))^2)
  r2 <- 1 - (rss/tss)
  return(round(r2, 3))
}

r2_model1 <- r2_tar(model1_tar)






# Teste de Breusch-Godfrey para autocorrelação
# H₀: não há autocorrelação de ordem p
bg_test <- bgtest(model1_tar, order = 4)  # ordem 4 para dados trimestrais
lm_stat <- bg_test$statistic
lm_pvalue <- bg_test$p.value




# Teste RESET de Ramsey
# H₀: modelo está corretamente especificado
reset_test <- resettest(model1_tar, power = 2:3, type = "fitted")
reset_stat <- reset_test$statistic
reset_pvalue <- reset_test$p.value




# Para modelo linear vs não-linear (TAR)
# Primeiro, estimar modelo AR linear
modelo_ar <- ar(data$Selic, order.max = 2, method = "ols")

# Comparar com modelo TAR usando teste de Wald

# Alternativa: testar não-linearidade diretamente
# Usando o teste de Tsay (pacote `nonlinearTseries`)
install.packages("nonlinearTseries")
library(nonlinearTseries)

nonlinearity_test <- terasvirta.test(data$Selic, p = 2, d = 1)