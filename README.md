# Modelo epidemiológico para Santa Catarina (epiCata)

Pacote R baseado no modelo epidemiológico adaptado para a realidade do estado de Santa Catarina.

Para saber mais detalhes ou caso reutilize esse código, veja e cite nosso artigo (submetido para revista):

> Avelar, P. H. da C., Lamb, L. C., Tsoka, S., & Cardoso-Silva, J. (2021). Weekly Bayesian modelling strategy to predict deaths by COVID-19: a model and case study for the state of Santa Catarina, Brazil. Retrieved from http://arxiv.org/abs/2104.01133

Os relatórios dos nossos estudos com o modelo são divulgados semanalmente pelo Social Good Brasil no link: https://socialgoodbrasil.org.br/modelo-epidemiologico/

Para entender como pacotes R funcionam, leia o livro on-line gratuito: http://r-pkgs.had.co.nz/


## Como compilar

O jeito mais fácil é [usar o RStudio](https://support.rstudio.com/hc/en-us/articles/200486508-Building-Testing-and-Distributing-Packages). Abra o projeto (pasta `epiCata`) e faça um build do pacote com o atalho `Ctrl + Shift + B`.

Caso queira rodar na linha de comando, abra um terminal do `R` dentro das pastas `epiCataPlot` e `epiCata` e digite:

```
    devtools::build()
    devtools::install()
```

Assim, os pacotes `epiCataPlot` e `epiCata` estarão disponíveis:

```
    library(epiCataPlot)
    library(epiCata)
```

## Como rodar o modelo

Para reproduzir os resultados do modelo epidemiológico para o dia 10/07, rode os comandos do script `run_model.R`, ou na linha de comando:

```

library(epiCata)

allowed_locations <-
  c("SC_ESTADO",
    "SC_MAC_FOZ_DO_RIO_ITAJAI",
    "SC_MAC_PLANALTO_NORTE_E_NORDESTE",
    "SC_MAC_GRANDE_OESTE",
    "SC_MAC_GRANDE_FLORIANOPOLIS",
    "SC_MAC_SUL",
    "SC_MAC_ALTO_VALE_DO_ITAJAI",
    "SC_MAC_MEIO_OESTE_E_SERRA_CATARINENSE",
    "SC_MUN_JOINVILLE",
    "SC_MUN_ITAJAI",
    "SC_MUN_FLORIANOPOLIS",
    "SC_MUN_BLUMENAU",
    "SC_MUN_CRICIUMA",
    "SC_MUN_LAGES",
    "SC_MUN_CHAPECO"
)

model_output <-
  run_epidemiological_model(mode="FULL",
                            allowed_locations=allowed_locations,
                            reference_date="2020-07-20")
for(location_name in model_output$stan_list$available_locations){
  make_three_panel_plot(location_name, model_output)
}

make_all_forecast_plots(model_output)
```

