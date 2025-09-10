##################################
# Solution exercises session 2.2 #
##################################

# Use the function setwd to set the working directory to 
# the folder where you stored the file
setwd("C:\\Users\\lucp2169\\Documents\\FLAMES\\Cursussen\\Basic parametric statistics")

radiation<-read.table("radiation.txt",header=T,sep="\t")	
str(radiation)

radiation$FrequencyMHz<-factor(radiation$FrequencyMHz)
radiation$Distancemm<-factor(radiation$Distancemm)
radiation$Angleo<-factor(radiation$Angleo)

str(radiation)

# Question a
#############

# First we do an informal check 
# using boxplots and summary numbers:

 # Angle
 tapply(radiation$MeasuredSignalmV,
  radiation$Angleo,summary)
 tapply(radiation$MeasuredSignalmV,
  radiation$Angleo,sd)
 tapply(radiation$MeasuredSignalmV,
  radiation$Angleo,t.test)

 boxplot(radiation$MeasuredSignalmV~
  radiation$Angleo,
  main="Boxplots Measured Signal by Angle",
  xlab="Angle",
  ylab="Measured Signal (mV)")

 # Frequency
 tapply(radiation$MeasuredSignalmV,
  radiation$FrequencyMHz,summary)
 tapply(radiation$MeasuredSignalmV,
  radiation$FrequencyMHz,sd)
 tapply(radiation$MeasuredSignalmV,
  radiation$FrequencyMHz,t.test)

 boxplot(radiation$MeasuredSignalmV~radiation$FrequencyMHz,
  main="Boxplots Measured Signal by Frequency",
  xlab="Frequency (Mhz)",
  ylab="Measured Signal (mV)")

# Formalize findings from boxplots and summary numbers:

 # Angle
 lmanova <- lm(MeasuredSignalmV ~ Angleo, 
  data=radiation)
 anova(lmanova)

 # Frequency
 lmanova <- lm(MeasuredSignalmV ~ FrequencyMHz, 
  data=radiation)
 anova(lmanova)


# Question b
#############

tapply(radiation$MeasuredSignalmV,
 radiation$Distancemm,summary)
tapply(radiation$MeasuredSignalmV,
 radiation$Distancemm,sd)
tapply(radiation$MeasuredSignalmV,
 radiation$Distancemm,t.test)

boxplot(radiation$MeasuredSignalmV~
 radiation$Distancemm,
 main="Boxplots Measured Signal by Distance",
 xlab="Distance (mm)",
 ylab="Measured Signal (mV)")

lmanova <- lm(MeasuredSignalmV ~ Distancemm,
 data=radiation)
anova(lmanova)

# Question c
#############

library(DTK)

aovanova <- aov(MeasuredSignalmV ~ Distancemm,
 data=radiation)
TukeyHSD(aovanova)
DTK.test(radiation$MeasuredSignalmV,
 radiation$Distancemm)
