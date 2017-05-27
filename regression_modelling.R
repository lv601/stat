# 1) Look into the data set swiss of the Swiss socioeconomic survey from 1888. 
#        Determine the ’best’ model for describing education when ignoring the examination results.
#        Use goodnees of fit, F -tests, AIC or BIC for argumentation. 
# 2) Look into the Pima Indian data from package MASS. Visualise and explore the data appropriately. 

#     2) Determine the ’best’ model for describing glucose level glu. Use goodnees of fit, F -tests, 
#        AIC or BIC for argumentation.
# 1)
#Description: Standardized fertility measure and socio-economic indicators for each of 47 French-speaking provinces of Switzerland at about 1888.
summary(swiss)
head(swiss)
cor(swiss)
fivenum(swiss)
boxplot(swiss)
swiss[4]
#Genf hat 53% -> kein Ausreißer, Wert plausibel, ist schlechter Hebelpunkt wird entfernt
edu=swiss[,4]
edu[45,] = NA
#create full model
fulllm=lm(edu[,1]~swiss[,1]+swiss[,2]+swiss[,5]+swiss[,6])
summary(fulllm)
#summary tells us that infant. mort is not significant for explaining edu
lm1=lm(edu[,1]~swiss[,1]+swiss[,2]+swiss[,5])
summary(lm1)
plot(lm1)lm1=lm(edu[,1]~swiss[,1]+swiss[,2]+swiss[,5])
#cath in facotr variable with cutoff 50%
cath=swiss[,5]
cathfactor = as.numeric(cath > 50)
#create lm and lmfull with cath factor
lmFactor=lm(edu[,1]~swiss[,1]+swiss[,2]+cathfactor)
fulllmFactor=lm(edu[,1]~swiss[,1]+swiss[,2]+cathfactor+swiss[,6])
summary(lmFactor); summary(fulllmFactor);
#Use info criterion to det which model is best, lower score = better score
AIC(lm1,fulllm); BIC(lm1,fulllm);
AIC(lmFactor,fulllmFactor); BIC(lmFactor,fulllmFactor);
#Examination ist correliert mit Agriculture und Fertility
# 2)
library(MASS)
# ###########################################################################################################################################
# Look into the help for the anscombe data in R. Compare the graphs and fitted regressions and interpret what is happening here.

require(stats); require(graphics)
summary(anscombe)

##-- now some "magic" to do the 4 regressions in a loop:
ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  ## or   ff[[2]] <- as.name(paste0("y", i))
  ##      ff[[3]] <- as.name(paste0("x", i))
  mods[[i]] <- lmi <- lm(ff, data = anscombe)
  print(anova(lmi))
}

## See how close they are (numerically!)
sapply(mods, coef)
lapply(mods, function(fm) coef(summary(fm)))

## Now, do what you should have done in the first place: PLOTS
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
for(i in 1:4) {
  ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
  plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
       xlim = c(3, 19), ylim = c(3, 13))
  abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)

#Bilder
#Bild eins zeigt eine "schöne" Kalibirierung die Punkte streuen um die Gerade 
#Bild zwei zeigt Daten welche für das verwendete nicht in Ordnung sind. Es ist sehr weit weg von linear. 
#   Eine exponentielle Funktion wäre denke ich besser geeignet, da die Punkte am Ende nochmals nach unten abfallen,
#   bringt auch eine logarithimsche Skalierung der Achsen keine weiteren Vorteile.
#Bild drei zeigt eine schöne Kalibrierungsgerade, welche durch einen Leverage Punkt ausgehebelt wird. Dadurch 
#   weicht vorallem bei den oberen Punkten die Gerade stärker von den Punkten ab, was sich negativ auf die 
#   prediction von weiteren Werten auswirkt.
#Bild vier zeigt schlecht streuende Werte. Es wurden auf der y Achse größere Abweichungen gefunden als diese durch 
#   x Werte erklärt werden konnten. Hinzu kommt ein einziger Hebelpunkt welche ausserhalb liegt. Von diesen Werten
#   sollte / dürfte keine Kalibrierung erstellt werden.

# ###########################################################################################################################################
# A research study estimated that under a certain condition, the probability that a subject would be referred for heart catheterization
# was 0.906 for whites and 0.847 for blacks. 
# * press release about the study stated that the odds of referral for cardiac catheterization 
#   for blacks are 60% of the odds for whites. Explain how they obtained 60% more accurately, 57% 
# * An Associated Press story later described the study and said ŚŚDoctors were only 60% as likely to order cardiac catheterizati
#   on for blacks as for whites.ŠŠ Explain what is wrong with this interpretation. Give the correct percentage for this interpretation
#   In stating results to the general public, it is better to use the relative risk than the odds ratio. It is simpler to understand
#   and less likely to be misinterpreted. For details, see New Engl.J.Med.341:279-283,1999

oddsratio = ((0.847)/(1-0.847)) / ((0.906)/(1-0.906))
#Probability has been calculated with Oddratio P/(1-P) / P/(1-P)

rr = 0.847/0.906; rr; 1-rr

#Afroamerikaner haben eine 6.5% niedrigere Chance eine Herzkatheter-Beahndlung zu bekommen


# ###########################################################################################################################################
# Use the Pima Indian data from package MASS. 
# * Visualise and explore the data appropriately. 
# * Fit a logistic regression model. 
# * Is there a significant dependence between bmi and glu? 
# * Explain the odds ratio interpretation and ’percentual’ increase of the chance for diabetes for the significant explanatory variables.
# ###########################################################################################################################################

# Loading data set
install.packages("MASS")
library("MASS")
pima <- Pima.tr

# Exploring and visualising
table(pima$type)
barplot(table(pima$type), main = "Diabetes in Pima Indian Women")
summary(pima)

summary(subset(pima, pima$type == "Yes"))
summary(subset(pima, pima$type == "No"))

hist(pima$npreg)
hist(pima$glu)
hist(pima$bp)
hist(pima$skin)
hist(pima$bmi)
hist(pima$ped)
hist(pima$age)

boxplot(pima$npreg)
boxplot(pima$glu)
boxplot(pima$bp)
boxplot(pima$skin)
boxplot(pima$bmi)
boxplot(pima$ped)
boxplot(pima$age)

library(corrplot)
pima_numeric <- pima[, -c(8)]
cor_pima <- cor(pima_numeric)
corrplot(cor_pima, method="number")

full_model <- glm(type ~.,family=binomial(link='logit'),data=pima)
summary(model)
plot(model)

# Reducing the model leads to a smaller AIC
reduced_model <- glm(pima$type~pima$npreg+pima$glu+pima$bmi+pima$ped,family=binomial(link='logit'))
summary(reduced_model)
plot(reduced_model)

# Testing for dependence between bmi and glu
plot(pima$bmi, pima$glu)
cov(pima$glu, pima$bmi)
cor(pima$glu, pima$bmi)
l <- lm(pima$glu ~ pima$bmi)
abline(l, col="blue")

# Odds ratio interpretation