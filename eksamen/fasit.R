mydata <- readRDS("datafil.rds")
table(complete.cases(mydata))

mydata <- mydata[complete.cases(mydata),  ]

summary(mydata$Alder)
