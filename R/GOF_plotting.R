fname <- c('ER PK')

#------------------------
# process clipboard
aa <- c('C:\Models\PK_SUM\Only_IR\PK316.mod')

dd  <- read.table('clipboard', sep ="@",header=F, fill=TRUE, as.is=TRUE)
aa <- dd[1,1]


cc <- strsplit(aa, "\\\\")
ee <- cc[[1]][length(cc[[1]])]
 prn_name <-sub('mod','prn',aa)
pic_name <-str_sub(ee,1,-5)
#---------------------------------


oridf <- na.omit(as.data.frame(sapply(read_table(prn_name,skip=1), as.numeric)))

### subpo
 
tab <- oridf  # ;[oridf$CMT==4,]
tab <- tab[!tab$DV==0,]


tab1<- tab

###### 
#--
ipredlim     <- range(tab1$IPRED) * 1.05
dvlim        <- range(tab1$DV) * 1.05
lim1         <- range(c(ipredlim, dvlim))
reslim       <- range(tab1$CWRES) * 1.05


###########
 Sibotheme <- theme(plot.title=element_text(size=18),axis.text.y = element_text(colour="black"), axis.title=element_text(size=18))

#DV_IPRED
plot1 =
  ggplot(data=tab1, aes(y=DV,x=IPRED)) +
  geom_abline(intercept=0, slope=1, linetype="solid", size=0.5) +
  geom_point(col='blue', shape=16) +
  stat_smooth(method="lm", se=FALSE, size=0.5, col="orangered", linetype="solid") +
  scale_x_continuous("Individual predictions",limits=lim1) +
  scale_y_continuous("Observed",limits=lim1) +
  ggtitle("Individual predictions versus concentrations")+
   Sibotheme 


#DV_PRED
plot2 =
  ggplot(data=tab1, aes(y=DV,x=PRED)) +
  geom_abline(intercept=0, slope=1, linetype="solid", size=0.5) +
  geom_point(col="blue", shape=16) +
  stat_smooth(method="lm", se=FALSE, size=0.5, col="orangered", linetype="solid") +
  scale_x_continuous("Population Predictions",limits=lim1) +
  scale_y_continuous("Observed",limits=lim1) +
  ggtitle("Population predictions versus concentrations")+
  Sibotheme 


#Residues vs. PRED
plot3 =
  ggplot(data=tab1, aes(y=CWRES,x=PRED)) +
  geom_hline(yintercept=-1.96, linetype="dashed") +
  geom_hline(yintercept=1.96, linetype="dashed") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_point(col="blue", shape=16) +
  stat_smooth(method="lm", se= FALSE,size=0.5,linetype="solid", col="orangered") +
  scale_x_continuous("Population predictions") + 
  scale_y_continuous("CWRES") +
  ggtitle("Conditional weighted residuals versus population predictions")+
  Sibotheme 

plot4 =
  ggplot(data=tab1, aes(y=CWRES,x=TIME)) +
  geom_hline(yintercept=-1.96, linetype="dashed") +
  geom_hline(yintercept=1.96, linetype="dashed") +
  geom_hline(yintercept=0, linetype="solid") +
  geom_point(col="blue", shape=16) +
  stat_smooth(method="lm", se= FALSE, size=0.5, linetype="solid", col="orangered") +
  scale_x_continuous("Time after dose") + 
  scale_y_continuous("CWRES") +
  ggtitle("Conditional weighted residuals versus time")+
  Sibotheme 



#------------------
 
grid.arrange(plot1, plot2, plot3, plot4, ncol=2, top=textGrob("fname", gp=gpar(fontsize=24, font=1,cex=0.6)))

#####
filepath <- paste('c:/Plots',"/",pic_name,'.png',sep="")

ggsave (filepath, width=4,height =3,units='in', dpi =300)


# open created file
shell.exec(filepath)  


