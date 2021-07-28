require(tidyverse)
require(ggplot2)
require(EnvStats)

# ########################################
# DEATHS
# ########################################

# 1st variant
# composed by 3 similar waves 
g1_1 <- EnvStats::rgammaAlt(2000, mean=8, cv=0.12)
hi1_1 <- hist(g1_1,breaks = seq(min(g1_1),max(g1_1), l = (length(g1_1)/8 + 1) )) 
g1_2 <- EnvStats::rgammaAlt(2000, mean=8, cv=0.08)
hi1_2 <- hist(g1_2,breaks = seq(min(g1_2),max(g1_2), l = (length(g1_2)/8 + 1) )) 
g1_3 <- EnvStats::rgammaAlt(2000, mean=8, cv=0.10)
hi1_3 <- hist(g1_3,breaks = seq(min(g1_3),max(g1_3), l = (length(g1_3)/8 + 1) )) 

overlap <- 50

deaths1_1 <-c(hi1_1$counts, seq(0,0,length.out=length(hi1_2$counts)-overlap), seq(0,0,length.out=length(hi1_3$counts)-overlap) )
deaths1_2 <- c(seq(0,0,length.out=length(hi1_1$counts)-overlap),hi1_2$counts, seq(0,0,length.out=length(hi1_3$counts)-overlap) )
deaths1_3 <- c(seq(0,0,length.out=length(hi1_1$counts)-overlap), seq(0,0,length.out=length(hi1_2$counts)-overlap),hi1_3$counts )

deaths1 <- deaths1_1 + deaths1_2 + deaths1_3
length(deaths1)

# 2nd variante
# Composed by 1 wave, more lethal than the first variant
g2_1 <- EnvStats::rgammaAlt(2000, mean=8, cv=0.12)
hi2_1 <- hist(g2_1,breaks = seq(min(g2_1),max(g2_1), l = (length(g2_1)/16 + 1) ))

deaths2 <- c(seq(0,0,length.out=length(hi1_1$counts)-overlap),
              seq(0,0,length.out=length(hi1_2$counts)-overlap),
              seq(0,0,length.out=(length(hi1_3$counts)/4)-20),
              hi2_1$counts, seq(0,0,length.out=(length(hi1_3$counts)/4)+19 ) 
          )

length(deaths2)

# Combining variants
deaths_comb <- c(deaths1+ deaths2)

# Plots
x1 <- c(1:length(deaths1))
plot_df <- data.frame(x=x1,y1=deaths1, y2=deaths2, ycomb=deaths_comb)


ggplot(plot_df, aes(x=x))+
    geom_col(aes(y=y2), fill='#9C5998', alpha=0.65) +
    geom_col(aes(y=y1), fill='#596B9C', alpha = 0.8)+
    theme_minimal()+
    ggtitle("Deaths per variant")+
    xlab("Days") + ylab("Deaths")

ggsave("multivariants_deaths.png",  plot = last_plot())
    
ggplot(plot_df, aes(x=x))+  
    geom_col(aes(y=ycomb), fill='#AC83C4', alpha = 0.8)+
    theme_minimal()+
    ggtitle("Deaths")+
    xlab("Days") + ylab("Deaths")

ggsave( "comb_deaths.png",  plot = last_plot())

# ########################################
# CASES
# ########################################
# Hypothesis: daily number of cases adn deaths follow the same distribution.

# 1st variant: mortality rate of 1%
cases1 <-deaths1*100

# 2nd variant: mortality rate of 7%
cases2 <- deaths2*100/7

# Combining variants
cases_comb <- cases1+cases2
  
# Plots  
plot_df_cases <- data.frame(x=x1,y1=casoe1, y2=casos2, ycomb=cases_comb)


ggplot(plot_df_casos, aes(x=x))+
    geom_col(aes(y=y1), fill='#596B9C', alpha = 0.8)+
    geom_col(aes(y=y2), fill='#9C5998', alpha=1) +
    theme_minimal()+
    ggtitle("Cases per variant")+
    xlab("Days") + ylab("Cases")

ggsave( "multivariants_cases.png",  plot = last_plot())

ggplot(plot_df_casos, aes(x=x))+  
    geom_col(aes(y=ycomb), fill='#AC83C4', alpha = 0.8) +
    theme_minimal()+
    ggtitle("Cases")+
    xlab("Days") + ylab("Cases")

ggsave( "comb_cases.png",  plot = last_plot())

