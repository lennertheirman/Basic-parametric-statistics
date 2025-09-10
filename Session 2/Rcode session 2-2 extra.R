#################################
# R session 2.2: Extra exercise #
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

# 1-way anova experience by specialties
boxplot(tbad$Years.of.experience~tbad$Specialty,
 main="Boxplot of Years of Experience by Specialty")
lmanova <- lm(Years.of.experience~Specialty, 
  data=tbad)
anova(lmanova)
pairwise.t.test(tbad$Years.of.experience, tbad$Specialty, 
 p.adj="none")
pairwise.t.test(tbad$Years.of.experience, tbad$Specialty, 
 p.adj="bonferroni")
aovanova <- aov(Years.of.experience ~ Specialty, data=tbad)
TukeyHSD(aovanova)
library(DTK)					
TK.test(tbad$Years.of.experience, tbad$Specialty)	
DTK.test(tbad$Years.of.experience, tbad$Specialty)	


# 2-way anova experience by specialties and gender
lmanova <- lm(Years.of.experience~Specialty+Gender+Specialty*Gender, 
  data=tbad)
anova(lmanova)
lmanova <- lm(Years.of.experience~Specialty+Gender, 
  data=tbad)
anova(lmanova)

# two-way anova based on health.txt
health<-read.table("Health.txt",
 header=T,sep=",")	
str(health)

lm1=lm(exercise ~ gender+bmi+gender:bmi,data=health)
anova(lm1)
lm1a=lm(exercise ~ gender+bmi,data=health)
anova(lm1a)
lm1b=lm(exercise ~ gender,data=health)
anova(lm1b)

lm2=lm(exercise ~ gender+profession+gender:profession,data=health)
anova(lm2)

lm3=lm(exercise ~ bmi+profession+bmi:profession,data=health)
anova(lm3)
lm3a=lm(exercise ~ bmi+profession,data=health)
anova(lm3a)
lm3b=lm(exercise ~ profession,data=health)
anova(lm3b)

lm4=lm(exercise ~ bmi+children+bmi:children,data=health)
anova(lm4)
lm4a=lm(exercise ~ bmi+children,data=health)
anova(lm4a)
lm4b=lm(exercise ~ children,data=health)
anova(lm4b)

lm5=lm(exercise ~ profession+children+profession:children,data=health)
anova(lm5)
lm5a=lm(exercise ~ profession+children,data=health)
anova(lm5a)
