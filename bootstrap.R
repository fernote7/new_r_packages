## 1. Carregar os pacotes necessários
 
library(sidrar)
library(tidyverse)
library(forecast)
 
## 2. Importar os dados da taxa de desocupação da PNAD Contínua mensal
 
pnad <- sidrar::get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")
 
## 3. Criar série de tempo
 
pnad_ts <- ts(pnad$Valor, start = c(2012,3), freq = 12)
 
## 4. Definir amostras de treino e de teste
 
pnad_treino <- window(pnad_ts, end = c(2016,7))
 
pnad_teste <- window(pnad_ts, start = c(2016,8))
 
## 5. Definir o número de séries geradas via bootstrap (utilizei apenas 10 para deixar mais rápido)
 
k <- 10
 
## 6. Computar as séries através de "blocked bootstrap" e definir como série de tempo
 
set.seed(1)
 
pnad_boot <- forecast::bld.mbb.bootstrap(pnad_treino, k) %>%
 
purrr::map(.f = ts, start = c(2012,3), freq = 12)
 
## 7. Computar as previsões pontuais das séries pelo método auto.arima 
 
aa_fc <- function(x){forecast(auto.arima(x, max.d = 1), n = 24)[["mean"]]} 
 
pnad_boot_fc <- purrr::map(.x = pnad_boot, .f = aa_fc) 
 
## 8. Separar as previsões da série original e pelo método bagging 
## (a série original é sempre a primeira da lista) 
 
fc_original <- pnad_boot_fc[[1]] 
 
fc_bagged <- pnad_boot_fc %>% purrr::reduce(`+`) %>% `/`(k) 
 
## 9. Comparar os dois 
 
accuracy(fc_original, pnad_teste) 
accuracy(fc_bagged, pnad_teste) 
