### 1. Carregar pacotes necessários e importar dados

### 1.1 Carregar pacotes 

library(sidrar)
library(tidyverse)
library(sweep)
library(lubridate)
library(formattable)

### 1.2 Importar dados do IPCA por grupos através da API do Sidra/IBGE.

### Em seguida, vamos apenas renomear algumas colunas para nomes mais apropriados.

ipca_grupos <- sidrar::get_sidra(api = "/t/1419/n1/all/v/63/p/all/c315/7170,7445,7486,7558,7625,7660,7712,7766,7786/d/v63%202")

ipca_grupos_aux <- ipca_grupos %>% 
  
  dplyr::select(Data = `Mês (Código)`, Grupo = `Geral, grupo, subgrupo, item e subitem`, Valor)

#

ipca_grupos_nest <- ipca_grupos_aux %>%
  
  dplyr::group_by(Grupo) %>%
  
  tidyr::nest(.key = grupos_nest)

#

arima_nest <- function(x){arima(x$Valor, order = c(1,0,0))}

ipca_grupos_arima <- ipca_grupos_nest %>%
  
  dplyr::mutate(grupos_arima = purrr::map(.x = grupos_nest, .f = arima_nest))

#

ipca_grupos_table <- ipca_grupos_arima %>%
  
  dplyr::mutate(Coeficientes = purrr::map(.x = grupos_arima, .f = sweep::sw_tidy)) %>%
  
  dplyr::select(Grupo, Coeficientes) %>%
  
  tidyr::unnest() %>%
  
  dplyr::filter(term == "ar1") %>%
  
  dplyr::select(Grupo, Coeficiente = estimate) %>%
  
  dplyr::arrange(desc(Coeficiente))

