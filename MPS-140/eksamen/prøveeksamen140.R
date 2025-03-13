#prøveeksamen mps140
mydata <- haven::read_sav("13365587/Replication data for Gottenborg 2022.sav")
mydata <- mydata[complete.cases(mydata),]

t.test(mydata$Engasjement~mydata$Kjønn)

plot( mydata$Autonomi,mydata$Engasjement)

sim.mod <- lm(mydata$Engasjement~mydata$Autonomi)
performance::check_model(sim.mod, check=c("linearity", "homogeneity", "normality"))

mult.mod <- lm(mydata$Engasjement~mydata$Autonomi+mydata$Kjønn+mydata$Alder)
performance::check_model(mult.mod, check=c("linearity", "homogeneity", "normality"))

mult.mod %>% summary


aa <- mydata[, 21:ncol(mydata)]

cor.plot(aa)

library(lavaan);library(tidyverse)
model <- "ENG =~ Eng1+Eng2+Eng3+Eng4+Eng5+Eng6"
f <- cfa(model, data=aa)#confirmatory factor analysis
head(standardizedsolution(f),6)

model <- "Arb =~ Arbp1+Arbp2+Arbp3"
f <- cfa(model, data=aa)#confirmatory factor analysis
head(standardizedsolution(f),5)

## utbrenthet
utbr = aa %>% select(matches("Overb|Kyn"))#10 items
f <- fa(utbr, 2)#spesifiserer 2 faktorer
f$loadings



## 

fa.parallel(aa)
