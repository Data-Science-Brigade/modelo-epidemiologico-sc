# epiCata 

Pacote R com o código fonte do modelo epidemiologico desenvolvido pela Data Science Brigade (DSB) <img alt="Data Science Brigade" width="15px" src="https://blog.dsbrigade.com/content/images/2020/07/dsb-escudo.png" />, adaptado para a realidade do estado de Santa Catarina -- Brasil.

Este é um estudo da DSB em parceria com a Universidade Federal do Rio Grande do Sul <img alt="UFRGS" width="15px" src="https://user-images.githubusercontent.com/896254/122652835-5e364500-d117-11eb-971b-4817a672acac.png"/> e o King's College London <img alt="King's College London" width="15px" src="https://yt3.ggpht.com/ytc/AAUvwnjyVU6LCRFsb7P3jPMB6gewcjP7gh1vnb28XdhF=s88-c-k-c0x00ffffff-no-rj" />.

O modelo probabilístico é baseado no modelo de [Flaxman et al 2020 (Imperial College London)](https://doi.org/10.1038/s41586-020-2405-7) publicado na Nature em junho de 2020, sendo que o principal diferencial consiste em levar em conta a tendência da curva de casos confirmados -- e não apenas a curva de óbitos confirmados -- para estimar e prever o número de óbitos por COVID-19. 

Ao aplicar esse novo modelo semanalmente às [macrorregiões de saúde de Santa Catarina](https://www.saude.sc.gov.br/index.php/informacoes-gerais-documentos/sala-de-leitura/revista-catarinense-de-saude-da-familia-1/5114-5-edicao-da-revista/file), vimos que as predições de óbitos por COVID-19 para as próximas quatro semanas tinham um erro bem menor comparado ao modelo original:

![](https://user-images.githubusercontent.com/896254/122653086-c3d70100-d118-11eb-8348-4382705a7c21.png)

Para saber mais detalhes técnicos e caso venha a utilizar e/ou escrever sobre esse estudo, leia e cite nosso artigo:

> Avelar, P. H. da C., Lamb, L. C., Tsoka, S., & Cardoso-Silva, J. (2021). Weekly Bayesian modelling strategy to predict deaths by COVID-19: a model and case study for the state of Santa Catarina, Brazil. (preprint) http://arxiv.org/abs/2104.01133

# Tutoriais

Primeiros passos com o código do modelo epidemiológico e como pensar sobre o problema:

- [00 - Setup do Ambiente](tutoriais/PT-BR/Tutorial00.html)
- [01 - Preprocessamento](tutoriais/PT-BR/Tutorial01.html)
- [02 - Modelo base e uma única localização](tutoriais/PT-BR/Tutorial02.html)
