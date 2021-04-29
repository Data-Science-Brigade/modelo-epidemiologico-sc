# Modelo epidemiológico para Santa Catarina (epiCata)

Pacote R baseado no modelo epidemiológico do [Imperial College London](https://github.com/ImperialCollegeLondon/covid19model), adaptado para a realidade do estado de Santa Catarina.

Os relatórios dos nossos estudos com o modelo são divulgados semanalmente pelo Social Good Brasil no link: https://socialgoodbrasil.org.br/modelo-epidemiologico/

Para entender como pacotes R funcionam, leia o livro on-line gratuito: http://r-pkgs.had.co.nz/


## Como compilar

### Linux

Caso se esteja num ambiente Linux (debian), talvez seja necessário instalar os seguintes pacotes do sistema:

```
sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev libv8-dev
```

### Dependências

Lembre-se de instalar as seguintes dependências antes de instalar o projeto:

* optparse==1.6.6
* devtools==2.4.0
* usethis==2.0.1 (dependência do devtools)

E, para garantir que as versões das bibliotecas instaladas foram as mesmas com as quais o modelo foi testado, instale os seguintes pacotes:
 
* rstan==2.19.3
* zoo==1.8.0
* bayesplot==1.7.2
* tidyverse==1.3.0
* dplyr==1.0.0
* tidyr==1.1.0
* EnvStats==2.3.1
* cowplot==1.0.0
* ggrepel==0.8.2
* scales==1.1.1
* ggplot2==3.3.2
* optparse==1.6.6
* slider==0.1.0

Caso queira, após instalar os pacotes optparse, devtools e usethis, pode-se rodar o script `install_dependencies.R` com o comando do terminal `R -f install_dependencies.R`, que ele todos os pacotes acima serão instalados na versão especificada.

### Compilando

O jeito mais fácil é [usar o RStudio](https://support.rstudio.com/hc/en-us/articles/200486508-Building-Testing-and-Distributing-Packages). Abra o projeto (pasta `epiCata`) e faça um build do pacote com o atalho `Ctrl + Shift + B`.

Caso queira rodar na linha de comando, abra um terminal do `R` dentro das pastas `epiCataPlot` e `epiCata` e, com o pacote `devtools` instalado, digite:

```
    devtools::build()
    devtools::install(upgrade = "never") # O upgrade aqui está "never" para não sobrescrever as versões já instaladas dos pacotes
```

Assim, os pacotes `epiCataPlot` e `epiCata` estarão disponíveis:

```
    library(epiCataPlot)
    library(epiCata)
```

## Como rodar o modelo

### Dados necessários

Para rodar o modelo com os parâmetros padrões, deve se ter os seguintes arquivos extras na raiz do projeto (i.e., junto dos arquivos `run_*.R`

#### deaths.csv

É o arquivo que contém o número de casos e óbitos por data por cidade, com as informações adicionais de qual Macrorregião aquela cidade pertence e o código do município de acordo com o IBGE.

O arquivo deve ser parecido com:

```
nom_regional,cod_municipio_ibge,nom_municipio,data_ocorrencia,casos,obitos
PLANALTO NORTE E NORDESTE,4218301,TRES BARRAS,2020-01-01,0,0

```

Caso queira, pode-se salvar o arquivo original como `deaths_unclean.csv` e, com uma instalação do python 3 com a biblioteca `pandas` (>=1.0.0, <2.0.0) instalada, então rodar o seguinte comando no terminal:

```
python3 clean_deaths.py
```

Isso irá "limpar" dados ruidosos, fazendo com que cada cidade tenha sua regional ser a regional mais comum para aquela cidade, trocará typos comuns na digitação do ano (2002, 2022, 2200, 2202, 2220, 2222) para o ano de 2020, e colocará ocorrências nos anos de 201X, 200X e 199X como ocorrências na data de 2020-01-01.

Isto é feito pois o modelo, caso seja alimentado com dados indicando um range de dados muito grande (e.g. 1990-2021), irá consumir toda a memória do computador e não irá rodar.

#### Global_Mobility_Report.csv

O arquivo com os dados de mobilidade disponibilizado pelo Google durante a pandemia. Para baixar esse arquivo basta rodar o script `get_google_mobility.sh` se estiver num sistema linux, tendo o programa `wget` instalado.

Caso se queira, pode-se baixar o mesmo arquivo acessando o link (https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv).

O arquivo deve ser parecido com:

```
country_region_code,country_region,sub_region_1,sub_region_2,metro_area,iso_3166_2_code,census_fips_code,place_id,date,retail_and_recreation_percent_change_from_baseline,grocery_and_pharmacy_percent_change_from_baseline,parks_percent_change_from_baseline,transit_stations_percent_change_from_baseline,workplaces_percent_change_from_baseline,residential_percent_change_from_baseline
BR,Brazil,,,,,,ChIJzyjM68dZnAARYz4p8gYVWik,2020-02-15,5,4,-5,8,6,0
```

#### onset_to_death.csv

Esse arquivo define a distribuição de onset-to-death (quantos dias uma pessoa que vai à óbito demora desde o início dos sintomas até o óbito).

O arquivo deve ser parecido com (não use esses valores para o arquivo):

```
"count","avg_days","std_days","coeff_variation"
2,3,1,0.333333333333333
```

O valor `count` indica de quantas amostras foram retirados aqueles valores, o `avg_days` indica a média do onset-to-death, o `std_days` o desvio padrão, e o `coeff_variation` é o desvio padrão dividido pela média.

Caso se tenha um arquivo indicando, linha a linha, a data de início dos sintomas e a data de óbito de pacientes, parecido com:

```
id,dat_inicio_sintomas,data_sintomas_ou_hospital,dat_obito,data_onset_death
1,2020-01-21,2020-01-02,2020-01-22,2
```

Coloque esse arquivo com o nome `raw_onset_to_death.csv` dentro da raiz e rode o comando `R -f onset_to_death.R` que o arquivo `onset_to_death.csv` será gerado, ignorando casos que tenham tido um onset-to-death de zero dias.

#### Outros dados

Outros dados necessários já estão disponibilizados nesse repositório, como os arquivos:

* `IFR.csv`
* `pop_and_regions.csv`
* `serial_interval.csv`

### Rodando o modelo

Para reproduzir os resultados do modelo epidemiológico para o dia 10/08, rode os comandos do script `run_regions.R` ou `run_health_regions.R`.

Uma lista completa das opções que se pode passar para os scripts para rodar o modelo pode ser vista usando o comando `Rscript run_regions.R --help` our  `Rscript run_health_regions.R --help`. As principais opções são:

* `-m`: O modo com o qual o modelo será rodado. O padrão é DEBUG, e deve ser alterado para DEVELOP ou para FULL para usar alguns valores pré-definidos, ou então personalizar o valor com as opções abaixo.
* `-k`: Caso seja um valor maior do que 1, os valores usados do google mobility serão primeiro passados por um filtro de média móvel com o número de dias indicados. Por exemplo, com `-k 7` se usaria uma média móvel de 7 dias. Note que para o comportamento esperado acontecer, não podem haver dias faltantes.
* `-i`: O número de iterações (totais) que o modelo será amostrado por. Por exemplo, com `-i 3000` o modelo terá 3000 amostragens feitas, tanto de aquecimento quanto amostragens "reais". Esse valor supercede o valor pré-definido pelo modo do modelo.
* `-w`: O número de iterações (dentro das iterações totais) que serão usadas para fins de "aquecimento" do modelo (e não deveriam ser usadas como parte dos resultados. Deve ser menor que o valor passado para `-i` ou o valor padrão definido pelo modo. Por exemplo `-w 2400` faz com que 2400 das iterações rodadas sejam iterações de warmup.
* `-c`: O número de correntes que se utilizará para fazer o sampling. Se recomenda usar mais do que uma para verificar a convergência. Por exemplo, `-c 4` indica que serão rodadas 4 correntes.
* `-t`: A profundidade máxima da árvore de busca do MCMC, por exemplo `-t 8` indica que a profundidade máxima será 8. Valores muito altos podem fazer com que o modelo demore para rodar e valores muito baixos podem piorar a performance do modelo. Se possível, evite usar valores abaixo de 8.
* `-r`: A data de referência, no formato "YYYY-MM-DD". Essa data normalmente deve ser 1 dia após a data que se tem dados disponíveis. Por exemplo, para rodar para 10 de agosto de 2020, se usaria `-r "2020-08-10"`
* `-e`: O caminho de modelo anterior para se inicializar os priors do modelo com os valores do modelo anterior. Por exemplo `~/results/2020_08_10/2020_08_10_modelo.Rdata` inicializaria o modelo com o salvo em `2020_08_10_modelo.Rdata`
* `-y`: Se o modelo deve ser rodado de maneira semanal ou não. `-y TRUE` indica que o modelo deve ter os dados agregados de maneira semanal. **ATENÇÃO**: Se usado dessa forma os dados devem terminar sempre no domingo, e a data de referência deve ser sempre uma segunda-feira.
* `-v`: Se deve se rodar o modelo de maneira verbosa ou não. `-v TRUE` indica que o modelo deve ser rodado de maneira verbosa.

Por exemplo, para rodar o modelo para a data de 17 de agosto de 2020, com alguns padrões do setting DEVELOP, mas com 1200 iterações, das quais 600 são de warmup, com 4 correntes, com a profundidade da árvores máxima setada em 8 e fazendo a média móvel de 7 dias do google mobility, deve se rodar o seguinte comando:

```
Rscript run_health_regions.R -m DEVELOP -k 7 -i 1200 -w 600 -c 4 -t 8 -r "2020-10-17"
```
