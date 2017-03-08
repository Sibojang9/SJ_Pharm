library(dplyr)
library(ggplot2)
library(readr)
library(grid)
library(gridExtra)
library(stringr)
library(vpc)

 

'C:\\Users\\SiboJ\\Desktop\\PK601.prn'


### aaa <-rename(sim,c("IPRED"='DV')) 
#--------------------
library(VPC)

# read simulation
Osim <- na.omit(as.data.frame(sapply(read_table('C:\\Users\\SiboJ\\Desktop\\ERPK700.prn',skip=1), as.numeric)))


sim <- Osim

#sim <- sim[sim$FLAG ==1,]
sim <- sim[sim$FLAG ==2,] 
sim <- sim[sim$FLAG ==3,] 

#--------------------
# read observation
Oobs <- na.omit(as.data.frame(sapply(read_csv('C:\\Users\\SiboJ\\Desktop\\Addult-PK.csv'), as.numeric)))
 
obs<- Oobs 

str(obs)
#obs  <-  obs[obs$Flag ==1,]
obs  <-  obs[obs$Flag ==2,]
#obs  <-  obs[obs$Flag ==3,]


obs<-  obs[obs$CMT ==2,]

bin_manual <- seq(0,25,0.5)

page_title <- c('Wang\'s model simulation-IR')
page_title <- c('Wang\'s model simulation-ER')
page_title <- c('Wang\'s model simulation-other')

 
page_title <- c('ER best-fit model')

p1<-vpc(sim,obs,show=list(obs_dv = T,
                          obs_ci = F,
                          obs_median =F,
                          sim_median=TRUE,
                          pi=TRUE),
        bins=bin_manual ,
        vpc_theme = create_vpc_theme (list(
          obs_size=2,bin_separators_color = NA )  ))+
  scale_x_continuous("Time (hours)") +
  scale_y_continuous(expression('APAP ('*mu*'mol/L)')) +
    ggtitle(page_title)


