##################################
# Solution exercises session 1.2 #
##################################

# Use the function setwd to set the working directory to 
# the folder where you stored the file BP.txt

setwd("C:\\Users\\lucp2169\\Documents\\FLAMES\\Cursussen\\Basic parametric statistics\\versie 2019\\Session 1")

BP<-read.table("BP.txt",
 header=T,sep="\t")
str(BP)

BP$RiskSystolic<-factor(BP$RiskSystolic)
BP$RiskDiastolic<-factor(BP$RiskDiastolic)
str(BP)

# Question a

tab<-table(BP$Sex)
tabp<-prop.table(tab)
ft <- rbind(tab, tabp)
ft <- t(ft)
colnames(ft) <- c("freq","percent")
round(ft, 4)

# Question b

tab<-table(BP$RiskSystolic)
tabp<-prop.table(tab)
ft <- rbind(tab, tabp, cumsum(tab), cumsum(tabp))
ft <- t(ft)
colnames(ft) <- c("freq","percent",
                  "cumul freq","cumul percent")
round(ft, 4)

tab<-table(BP$RiskDiastolic)
tabp<-prop.table(tab)
ft <- rbind(tab, tabp, cumsum(tab), cumsum(tabp))
ft <- t(ft)
colnames(ft) <- c("freq","percent",
                  "cumul freq","cumul percent")
round(ft, 4)

# Question c

boxplot(BP$KorotkoffLASys~BP$Sex,
 main="Boxplot Systolic by Sex",
 ylab="Korotkoff Systolic")
boxplot(BP$KorotkoffLADias~BP$Sex,
 main="Boxplot Diastolic by Sex",
 ylab="Korotkoff Diastolic")

# Question d

boxplot(BP$KorotkoffLASys~BP$RiskSystolic,
 main="Boxplot Systolic by Risk factor",
 ylab="Korotkoff Systolic")
boxplot(BP$KorotkoffLADias~BP$RiskDiastolic,
 main="Boxplot Diastolic by Risk factor",
 ylab="Korotkoff Diastolic")

# Question e

tapply(BP$KorotkoffLASys, BP$Sex, mean)
tapply(BP$KorotkoffLASys, BP$Sex, sd)

SystolicMen<-BP$KorotkoffLASys[BP$Sex=="M"]
SystolicWomen<-BP$KorotkoffLASys[BP$Sex=="F"]
var.test(SystolicMen,SystolicWomen)				
 # Equal variances can be assumed
t.test(SystolicMen,SystolicWomen,var.equal=T)


tapply(BP$KorotkoffLADias, BP$Sex, mean)
tapply(BP$KorotkoffLADias, BP$Sex, sd)

DiaStolicMen<-BP$KorotkoffLADias[BP$Sex=="M"]
DiaStolicWomen<-BP$KorotkoffLADias[BP$Sex=="F"]
var.test(DiaStolicMen,DiaStolicWomen)				
 # Equal variances can be assumed
t.test(DiaStolicMen,DiaStolicWomen,var.equal=T)

# Question g

differencesys<-BP$KorotkoffLASys-BP$LongTermMeanSys
differencedias<-BP$KorotkoffLADias-BP$LongTermMeanDias

boxplot(differencesys,
 main="Difference in Systolic Blood Pressure with long term mean",
 ylab="Difference")
boxplot(differencedias,
 main="Difference in Diastolic Blood Pressure with long term mean",
 ylab="Difference")

t.test(BP$KorotkoffLASys,BP$LongTermMeanSys,
 paired=T)
t.test(BP$KorotkoffLADias,BP$LongTermMeanDias,
 paired=T)
