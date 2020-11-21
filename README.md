
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cjf

<!-- badges: start -->

[![R build
status](https://github.com/jjesusfilho/cjf/workflows/R-CMD-check/badge.svg)](https://github.com/jjesusfilho/cjf/actions)
<!-- badges: end -->

Baixa e organiza a jurisprudência dos tribunais federais

# Instalação

``` r
remotes::install_github("jjesusfilho/cjf")
```

# Utilização

Para baixar a jurisprudência de um determinado tribunal, informe os
parâmetros como você faria na página do Conselho da Justiça Federal:

``` r
dir.create("stf")
cjf_baixar_cjsg(livre = "indígena",
                tribunal = "stf",
                data_inicial = "01/01/2020",
                data_final = "20/11/2020",
                diretorio = "stf")

```
