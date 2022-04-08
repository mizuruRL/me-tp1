travel_insurance <- read.csv("travel_insurance.csv", na.strings=c("", "NA"))

netBreaks <- nclass.Sturges(travel_insurance$Net.Sales)
commissionBreaks <- nclass.Sturges(travel_insurance$Commision..in.value.)
ageBreaks <- 25
print(netBreaks)
print(commissionBreaks)
print(ageBreaks)
range(travel_insurance$Age)

h1 <- hist(travel_insurance$Net.Sales, plot = FALSE)
h1$breaks

netGroups <- cut(travel_insurance$Net.Sales, seq(-389, 810, l = netBreaks))
commissionGroups <- cut(travel_insurance$Commision..in.value., seq(0, 283.5, l = commissionBreaks))
ageGroups <- cut(travel_insurance$Age, seq(0, 120, l = ageBreaks))

#freq abs
netFA <- table(netGroups)
commFA <- table(commissionGroups)
ageFA <- table(ageGroups)

print(netFA)
print(commFA)
print(ageFA)
