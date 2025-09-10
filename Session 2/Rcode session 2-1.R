###############################################
# R session 2.1: Comparing more than 2 groups #
###############################################

## Introduction

# Use the function setwd to set the working directory to 
# the folder where you stored the file
setwd("C:\\Users\\lucp2169\\Documents\\FLAMES\\Cursussen\\Basic parametric statistics")

sound<-read.table("sound.txt",
 header=T,sep="\t")
str(sound)

# Exploring the data
# Summary numbers & confidence intervals
 tapply(sound$score,sound$group,
  summary)
 tapply(sound$score,sound$group,
  sd)
 tapply(sound$score,sound$group,
  t.test)

 boxplot(sound$score~sound$group,
  main="Boxplots Scores by Group",
  xlab="Group",
  ylab="Scores")

## One-way anova
### Levene's test will be discussed in part 3
# Option 1
lmanova <- lm(score ~ group, data=sound)
lmanova
anova(lmanova)
summary(lmanova)
tapply(sound$score,sound$group,mean)
# Option 2 
# aov calls lm function, but different print & summary
aovanova <- aov(score ~ group, data=sound)
aovanova
anova(aovanova)
summary(aovanova)
# Option 3 (which allows for unequal variances)
oneanova <- oneway.test(score ~ group, 
 var.equal=TRUE, #default = FALSE
 data=sound)
oneanova
# anova(oneanova) # not possible

## Paired anova or block design
prices<-read.table("prices.txt",
 header=T,sep="\t")	
str(prices)

lmanova <- lm(price ~ store + product, 
 data=prices)
anova(lmanova)

#Extra:
interaction.plot(x.factor = prices$product, 
                 trace.factor = prices$store, 
                 response = prices$price, fun = mean, 
                 legend = FALSE,
                 xlab = "Product", ylab="Price",
                 lty=1:4, col = c("blue","red","green","black"))
legend("topright",c("Carrefour","Colruyt","Delhaize","Match"),
       col=c("blue","red","green","black"),lty=1:4)

## Multi-way Anova
machines<-read.table("machines.txt",
 header=T,sep="\t")	
str(machines)

interaction.plot(x.factor = machines$Material, 
                 trace.factor = machines$Machine, 
                 response = machines$Production, fun = mean, 
                 legend = FALSE,
                 xlab = "Machine", ylab="Production",
                 lty=c(1,2), col = c("blue","red"))
legend("topright",c("Machine 1","Machine 2"),
       col=c("blue","red"),lty=1:2)


lmanova<-lm(Production~
 Machine + Material + Material:Machine,
  # Machine*Material #abbreviation
 data=machines)
anova(lmanova)


## Post-Hoc Analyses
# LSD testing: 
pairwise.t.test(sound$score, sound$group, 
 p.adj="none")		
# Bonferroni correction: 
pairwise.t.test(sound$score, sound$group, 
 p.adj="bonferroni")	

pairwise.t.test(machines$Production, machines$Machine:machines$Material,
                p.adj="bonferroni")	

# Tukey correction (needs aov-object):
aovanova <- aov(score ~ group, data=sound)
TukeyHSD(aovanova)

aovanova <- aov(Production ~ Machine + Material + Machine:Material, data=machines)
anova(aovanova)
TukeyHSD(aovanova)				

# Tukey-Kramer correction
 # if package "DTK" is not installed yet, 
 # type in command: 
 # install.packages("DTK")
library(DTK)					
TK.test(sound$score, sound$group)		

# Dunnett's Modified Tukey-Kramer 
# is alternative for Games-Howell
DTK.test(sound$score, sound$group)		
