library(psych)

travel_insurance <- read.csv("travel_insurance.csv", na.strings=c("", "NA"))

netGroups <- cut(travel_insurance$Net.Sales, seq(-400, 900, l = 14))
commissionGroups <- cut(travel_insurance$Commision..in.value., seq(0, 300, l = 16))
ageGroups <- cut(travel_insurance$Age, seq(0, 120, l = 25))

#freq abs
netAbs <- table(netGroups)
commAbs <- table(commissionGroups)
ageAbs <- table(ageGroups)
agencyAbs <- table(travel_insurance$Agency)
destinationAbs <- table(travel_insurance$Destination)
productAbs <-  table(travel_insurance$Product.Name)

netRel <- prop.table(netAbs)
commRel <- prop.table(commAbs)
ageRel <- prop.table(ageAbs)
agencyRel <- prop.table(agencyAbs)
destinationRel <- prop.table(destinationAbs)
productRel <- prop.table(productNameAbs)

net <- cbind("Frequência Absoluta" = netAbs, "Frequência Relativa" = netRel, "Frequência Relativa %" = netRel*100, "Frequência Abs. Acumulada" = cumsum(netRel), "Frequência Rel.% Acumulada" = cumsum(netRel*100))
comm <- cbind("Frequência Absoluta" = commAbs, "Frequência Relativa" = commRel, "Frequência Relativa %" = commRel*100, "Frequência Abs. Acumulada" = cumsum(commRel), "Frequência Rel.% Acumulada" = cumsum(commRel*100))
age <- cbind("Frequência Absoluta" = ageAbs, "Frequência Relativa" = ageRel, "Frequência Relativa %" = ageRel*100, "Frequência Abs. Acumulada" = cumsum(ageRel), "Frequência Rel.% Acumulada" = cumsum(ageRel*100))
agency <- cbind("Frequência Absoluta" = agencyAbs, "Frequência Relativa" = agencyRel, "Frequência Relativa %" = agencyRel*100, "Frequência Abs. Acumulada" = cumsum(agencyRel), "Frequência Rel.% Acumulada" = cumsum(agencyRel*100))
destination <- cbind("Frequência Absoluta" = destinationAbs, "Frequência Relativa" = destinationRel, "Frequência Relativa %" = destinationRel*100, "Frequência Abs. Acumulada" = cumsum(destinationRel), "Frequência Rel.% Acumulada" = cumsum(destinationRel*100))
product <- cbind("Frequência Absoluta" = productAbs, "Frequência Relativa" = productRel, "Frequência Relativa %" = productRel*100, "Frequência Abs. Acumulada" = cumsum(productRel), "Frequência Rel.% Acumulada" = cumsum(productRel*100))

View(net)
View(comm)
View(age)
View(agency)
View(destination)
View(product)
