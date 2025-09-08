#####################################################################
# R session 1.1: Descriptives, graphs, one and two sample inference #
#####################################################################

# Use the function setwd to set the working directory to 
# the folder where you stored the files tbad.txt and 
# measures.txt
setwd("C:\\Users\\lucp2169\\Documents\\FLAMES\\Cursussen\\Basic parametric statistics")

# Select the file "TBAD.txt" and check the variables
tbad<-read.table("tbad.txt",header=T,sep="\t",dec=".")	
str(tbad)

# Change structure to correct ones: 
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

## Categorical variables
# Frequency tables and plots for nominal/ordinal data:
tab<-table(tbad$Specialty)
tabp<-prop.table(tab)
ft <- rbind(tab, tabp, cumsum(tab), cumsum(tabp))			
# The cumulatives are given so you know how to do it, 
# but are of course only of interest 
# when you work with ordinal data
ft <- t(ft)
colnames(ft) <- c("freq","percent",
                  "cumul freq","cumul percent")
round(ft, 2)

barplot(tab,main="Barplot for Specialty", 
 xlab="Specialty", ylab="Frequency")
pie(tab,main="Pie chart for Specialty",col=1:7)

## Numerical variables
# Histogram
hist(tbad$Years.of.experience,main="",
 xlab="Experience (years)",
 ylab="Frequency",col="blue");box()
 # optional
 hist(tbad$Years.of.experience,main="",
  xlab="Experience (years)",
  ylab="Frequency",col="blue",
  breaks=20);box()

# Summary numbers for location, spread
summary(tbad$Years.of.experience)

 # Central tendency
  # Calculate mean of variable:
 mean(tbad$Years.of.experience)		
  # Calculate median of variable:
 median(tbad$Years.of.experience)		
  # Calculate mode of variable
  # Table needed to calculate mode: (not of primary interest)
 tab<-table(tbad$Years.of.experience)	
 as.numeric(names(tab)[tab==max(tab)])	

 # Spread
  # Calculate range of variable:
 max(tbad$Years.of.experience)-min(tbad$Years.of.experience)	
  # Calculate standard deviation of variable:
 sd(tbad$Years.of.experience)		
  # Calculate inter quartile range of variable: 
  # several algorithms to calculate quantiles 
  # (see help(IQR) to choose type)
  # e.g. type=6 for SPSS-like IQR
 IQR(tbad$Years.of.experience)		
 IQR(tbad$Years.of.experience,type=6)
   
 # Relative standing and outliers
 average<-mean(tbad$Total.average.costs.per.patient.per.month)
 s<-sd(tbad$Total.average.costs.per.patient.per.month)
  ## Empirical rule:
  # Interval containing about 70% of the data 
  # (in unimodal, symmetric case)
 c(average-s,average+s)			
  # Interval containing about 95% of the data 
  # (in unimodal, symmetric case)
 c(average-2*s,average+2*s)			
  # Interval containing about 99% of the data 
  # (in unimodal, symmetric case)
 c(average-3*s,average+3*s)			

 hist(tbad$Total.average.costs.per.patient.per.month,
  main="Histogram of average cost per month",
  xlab="Total average cost per month",
  ylab="Frequency")

  # Calculate 1st quantile:
 Q1<-quantile(tbad$Total.average.costs.per.patient.per.month,
  probs=0.25)		
  # Calculate 2nd quantile:
  # median(tbad$Total.average.costs.per.patient.per.month) #OR
 Q2<-quantile(tbad$Total.average.costs.per.patient.per.month,
  probs=0.5)		
  # Calculate 3rd quantile:
 Q3<-quantile(tbad$Total.average.costs.per.patient.per.month,
  probs=0.75)		
  ## Boxplot rule:
  # Interval that captures exactly 50% of the data:
 c(Q1,Q3)															
  # Interval that captures about 95% of the data:
 c(Q2-1.5*IQR(tbad$Total.average.costs.per.patient.per.month),
  Q2+1.5*IQR(tbad$Total.average.costs.per.patient.per.month))	
  # Interval that captures about 99% of the data
 c(Q1-1.5*IQR(tbad$Total.average.costs.per.patient.per.month),
  Q3+1.5*IQR(tbad$Total.average.costs.per.patient.per.month))	

 boxplot(tbad$Total.average.costs.per.patient.per.month,
  main="Boxplot of average cost per month")
  # zoom in on lower part
 boxplot(tbad$Total.average.costs.per.patient.per.month,
  main="Boxplot of average cost per month",
  ylim=c(-100,250))

## One sample inferences: the mean
# For purpose of illustration: select subset of data, containing
# only doctors with specialty Family Practice
costPatientFp<-tbad$Total.average.costs.per.patient.per.month[tbad$Specialty=="fp"]
# Read confidence interval and compare with total average cost
t.test(costPatientFp)						
# By default test for mu=0
# Add argument to perform t-test for mu=103  
t.test(costPatientFp,mu=103)	
# By default test is two-sided				
# Add argument to perform one-sided test
t.test(costPatientFp,mu=103,alternative='less')		
# Note that confidence interval changes!

## A continuous variable over categories
# use ~ to create multiple boxplots
boxplot(tbad$Total.average.costs.per.patient.per.month~tbad$Specialty,
 ylab="Average cost per patient per month")
boxplot(tbad$Total.average.costs.per.patient.per.month~tbad$Specialty,
 ylab="Average cost per patient per month",
 ylim=c(-100,420))

# summary numbers computed by category:
tapply(tbad$Total.average.costs.per.patient.per.month, 
 tbad$Specialty, summary)
# std. dev. by category:
tapply(tbad$Total.average.costs.per.patient.per.month, 
 tbad$Specialty, sd)

## Two sample inferences: the mean
# Select costs only for doctors who studied in USA
costUSA<-tbad$Total.average.costs.per.patient.per.month[tbad$Medical.school=="USA"]
# Select costs only for doctors who studied in foreign country
costFor<-tbad$Total.average.costs.per.patient.per.month[tbad$Medical.school=="Foreign"]
# Check equality of variances:
# remark difference F-test <-> Levene's test!!! 
# (cfr class 3)
var.test(costUSA,costFor)			
# Change var.equal option, depending on result of variance test
t.test(costUSA,costFor, var.equal=F)

# Select costs only for doctors without secondary specialty
costSecSpec0<-tbad$Total.average.costs.per.patient.per.month[tbad$Secondary.specialty=="no"]
# Select costs only for doctors with secondary specialty
costSecSpec1<-tbad$Total.average.costs.per.patient.per.month[tbad$Secondary.specialty=="yes"]
# Check equality of variances:
# remark difference F-test <-> Levene's test!!! 
# (cfr class 3)
var.test(costSecSpec0,costSecSpec1)
# Change var.equal option, depending on result of variance test
# based on Levene's test (see Part 3) we assume equal variances
t.test(costSecSpec0,costSecSpec1, var.equal=T)		# Change var.equal option, depending on result of variance test

## Paired t-test
measures<-read.table("MEASURES.TXT",sep="\t",header=T) 
# Perform a paired t-test for dependent measures:
t.test(measures$Before,measures$After,paired=T)		


