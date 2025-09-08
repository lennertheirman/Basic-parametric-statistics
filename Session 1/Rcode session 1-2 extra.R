#################################
# R session 1.2: Extra exercise #
#################################

# Use the function setwd to set the working directory to 
# the folder where you stored the file tbad.txt
setwd("C:\\Users\\lucp2169\\Documents\\FLAMES\\Cursussen\\Basic parametric statistics")

# Select the file "TBAD.txt" and check the variables
tbad<-read.table("tbad.txt",header=T,sep="\t",dec=".")	
str(tbad)

# Secondary.specialty, Certification.level, Gender, 
# Medical.school and Residence should be factors
# and add labels
tbad$Secondary.specialty<-factor(tbad$Secondary.specialty,
 levels=0:1,labels=c("no","yes"))
tbad$Certification.level<-factor(tbad$Certification.level)
tbad$Gender=factor(tbad$Gender,
 levels=0:1,labels=c("man","woman"))
tbad$Medical.school<-factor(tbad$Medical.school,
 levels=0:1,labels=c("USA","Foreign"))
tbad$Residence<-factor(tbad$Residence,
 levels=0:1,labels=c("USA","Foreign"))
str(tbad)

# Frequency tables and bar plot for Certification level
tab<-table(tbad$Certification.level)
tabp<-prop.table(tab)
ft <- rbind(tab, tabp, cumsum(tab), cumsum(tabp))			
ft <- t(ft)
colnames(ft) <- c("freq","percent",
                  "cumul freq","cumul percent")
round(ft, 2)

barplot(tab,main="Barplot for Certification level", 
 xlab="Certification level", ylab="Frequency")

# Descriptive measures for number of patients per month
summary(tbad$Total.patients.per.month)
sd(tbad$Total.patients.per.month)

# Hist & boxplot for number of patients per month
hist(tbad$Total.patients.per.month,
  main="Histogram of number of patients per month",
  xlab="Number of patients per month",
  ylab="Frequency")
boxplot(tbad$Total.patients.per.month,
 main="Boxplot of number of patients per month")
boxplot(tbad$Total.patients.per.month,
 ylim=c(0,4000),
 main="Boxplot of number of patients per month")

#descr & boxplot for experience by gender
tapply(tbad$Years.of.experience, 
 tbad$Gender, summary)
boxplot(tbad$Years.of.experience ~ tbad$Gender,
 main="Boxplot of Years of experience,\n by gender")

#experience by medical school
boxplot(tbad$Years.of.experience ~ tbad$Medical.school,
 main="Boxplot of Years of experience,\n by Medical school")
YoeUSA = tbad$Years.of.experience[tbad$Medical.school=="USA"]
YoeFor = tbad$Years.of.experience[tbad$Medical.school=="Foreign"]
var.test(YoeUSA,YoeFor)
t.test(YoeUSA,YoeFor,var.equal=T)

#cost less than 60, for pediatrics
summary(tbad$Total.average.costs.per.patient.per.month[tbad$Specialty=="pd"])
length(tbad$Total.average.costs.per.patient.per.month[tbad$Specialty=="pd"])
t.test(tbad$Total.average.costs.per.patient.per.month[tbad$Specialty=="pd"],
mu=60,alternative="less")
