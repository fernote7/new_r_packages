## 1. Carregar pacotes necessários
 
library(tidyverse)
library(forecast)
library(timetk)
 
## 1.2. Simular séries estacionárias e não-estacionárias a partir de uma distribuição normal. 
## Em seguida, vamos colocá-las no formato de série de tempo (ts).
 
set.seed(1)
 
mu <- list(-2, 0, 2, 4, 6) # média
 
sigma <- list(1, 2, 3, 4, 5) # desvio-padrão
 
series <- purrr::map2_dfc(mu, sigma, rnorm, n = 100) %>% dplyr::mutate_all(funs(timetk::tk_ts))
 
## 1.3. Definir séries V1 e V3 como não-estacionárias
 
series_aux <- series %>% dplyr::mutate_at(vars(V1,V3), funs(cumsum))

## 2.1. Criar a função ur_map para generalizar a função ndiffs.
 
testes <- list(kpss = "kpss", pp = "pp", adf = "adf")
 
ur_map <- function(x) purrr::map(testes, function(y){forecast::ndiffs(x, alpha = 0.05, y)})
 
## 2.2 Criar um tibble com o resultado para cada teste sobre cada série e
## adicionar uma coluna "Diferenciar" com valor "SIM" se a série for não-estacionária 
## em ao menos 2 testes e "NÃO" caso contrário.
 
## OBS: para formalizar este critério, eu considerei que as séries são I(0) ou I(1), 
## isto é, ou não precisam de nenhuma diferenciação (estacionária) ou precisam de 
## apenas 1 diferenciação. Esse é o padrão em séries econômicas.
 
series_ndiffs <- series_aux %>%
 
purrr::map(.f = ur_map) %>%
 
plyr::ldply(bind_rows) %>%
 
dplyr::mutate(Diferenciar = ifelse(kpss + adf + pp >= 2, "SIM", "NÃO"))

## 3.1. Retornar o nome das séries que são não-estacionárias.
 
series_labs <- series_ndiffs %>%
 
dplyr::filter(Diferenciar == "SIM") %>%
 
dplyr::select(.id)
 
## 3.2. Criar função auxiliar para diferenciar
 
dif <- function(x){x-dplyr::lag(x)}
 
## 3.3. Diferenciar as séries não-estacionárias
 
series_dif <- series_aux %>%
 
dplyr::mutate_at(vars(series_labs$.id), funs(dif))
