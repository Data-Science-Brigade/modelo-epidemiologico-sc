# Plots para o Modelo epidemiológico de Santa Catarina (epiCataPlot)

Pacote R para plotar informações de um modelo baseado no modelo epidemiológico do Imperial College London, adaptado para a realidade do estado de Santa Catarina.

Para entender como pacotes R funcionam, leia o livro on-line gratuito: http://r-pkgs.had.co.nz/


## Como compilar

O jeito mais fácil é [usar o RStudio](https://support.rstudio.com/hc/en-us/articles/200486508-Building-Testing-and-Distributing-Packages). Abra o projeto (pasta `epiCata`) e faça um build do pacote com o atalho `Ctrl + Shift + B`.

Caso queira rodar na linha de comando, abra um terminal do `R` e digite:

```
    devtools::build()
    devtools::install()
```

Assim, o pacote `epiCataPlot` estará disponível:

```
    library(epiCataPlot)
```
