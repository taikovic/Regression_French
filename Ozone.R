library(ggplot2)

#dir()
ozone<-read.table("./work/Prog/Regression_French/Data_Set/Dataset_ozone.txt", header=TRUE,sep=";",dec=",")
#print(ozone)
ggplot(ozone, aes(x=T12,y=maxO3)) + geom_point() + xlab("T12")+ ylab("maxO3")
#implementer la regression lineaire: linear model: lm
reg_sim<-lm(maxO3~T12, data=ozone)

# lire resultats: 
summary(reg_sim)

# Visuliser la droite de regression: 
ggplot(ozone,aes(x=T12,y=maxO3))+ geom_point() + stat_smooth(method="lm", se=FALSE)+xlab("T12") + ylab("maxO3")

# Residus: 
ozone$residus_s<-reg_sim$residuals

ggplot(ozone, aes(x=residus_s)) + geom_histogram(binwidth=10, aes(y=..density..))+
  ggtitle("Histogramme") + xlab("Residus") + ylab("")

# Prediction: 

a_prevoir<-data.frame(T12=19)
maxO3_prev<-predict(reg_sim,a_prevoir)
round(maxO3_prev,digits=1)

