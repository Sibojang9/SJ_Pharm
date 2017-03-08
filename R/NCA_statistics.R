
# write_csv(final_result,'NCA_result.csv')
path <- 'N:\\1_Sibo_code\\R_Script\\4_Proj\\Observ_6_2_2016\\NCA_result.csv'


  re  <- read_csv(path,  na=".", comment = "#")

aucIR <- re$aucinf[re$FLAG %in% c(1)] 
aucER <- re$aucinf[re$FLAG %in% c(2)] 

t.test(aucIR,aucER)
t.test(aucIR,mu=281)

t.test(aucER,mu=281)

#test auc####################
dose <- c( 15,20)
auc <- c(66,83)
model  <-lm( auc ~ dose) 
newdose <-    data.frame(dose=80)
predict(model, newdose ) 

shapiro.test(aucIR)
shapiro.test(aucER)
var.test(aucIR ,aucER ) 

