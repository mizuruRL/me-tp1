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
claimAbs <- table(travel_insurance$Claim)

netRel <- prop.table(netAbs)
commRel <- prop.table(commAbs)
ageRel <- prop.table(ageAbs)
agencyRel <- prop.table(agencyAbs)
destinationRel <- prop.table(destinationAbs)
productRel <- prop.table(productAbs)
claimRel <- prop.table(claimAbs)

net <- cbind("Frequência Absoluta" = netAbs, "Frequência Relativa" = netRel, "Frequência Relativa %" = netRel*100, "Frequência Abs. Acumulada" = cumsum(netRel), "Frequência Rel.% Acumulada" = cumsum(netRel*100))
comm <- cbind("Frequência Absoluta" = commAbs, "Frequência Relativa" = commRel, "Frequência Relativa %" = commRel*100, "Frequência Abs. Acumulada" = cumsum(commRel), "Frequência Rel.% Acumulada" = cumsum(commRel*100))
age <- cbind("Frequência Absoluta" = ageAbs, "Frequência Relativa" = ageRel, "Frequência Relativa %" = ageRel*100, "Frequência Abs. Acumulada" = cumsum(ageRel), "Frequência Rel.% Acumulada" = cumsum(ageRel*100))
agency <- cbind("Frequência Absoluta" = agencyAbs, "Frequência Relativa" = agencyRel, "Frequência Relativa %" = agencyRel*100, "Frequência Abs. Acumulada" = cumsum(agencyRel), "Frequência Rel.% Acumulada" = cumsum(agencyRel*100))
destination <- cbind("Frequência Absoluta" = destinationAbs, "Frequência Relativa" = destinationRel, "Frequência Relativa %" = destinationRel*100, "Frequência Abs. Acumulada" = cumsum(destinationRel), "Frequência Rel.% Acumulada" = cumsum(destinationRel*100))
product <- cbind("Frequência Absoluta" = productAbs, "Frequência Relativa" = productRel, "Frequência Relativa %" = productRel*100, "Frequência Abs. Acumulada" = cumsum(productRel), "Frequência Rel.% Acumulada" = cumsum(productRel*100))
claim <- cbind("Frequência Absoluta" = claimAbs, "Frequência Relativa" = claimRel, "Frequência Relativa %" = claimRel*100, "Frequência Abs. Acumulada" = cumsum(claimRel), "Frequência Rel.% Acumulada" = cumsum(claimRel*100))

View(net)
View(comm)
View(age)
View(agency)
View(destination)
View(product)
View(claim)

summary(travel_insurance$Net.Sales)
summary(travel_insurance$Commision..in.value.)
summary(travel_insurance$Agency)
summary(travel_insurance$Destination)
summary(travel_insurance$Product.Name)
summary(travel_insurance$Claim)


boxplot(travel_insurance$Age,
        outline = FALSE,
        main = "Diagrama de extremos e quartis Idade",
        ylab = "Idade",
        col = c("#c8ade6")
)

boxplot(travel_insurance$Age ~ travel_insurance$Claim,
        outline = FALSE,
        main = "Diagrama de extremos e quartis Idade/Seguro Acionado",
        ylab = "Idade",
        xlab = "Seguro Acionado",
        col = c("#c8ade6")
)

boxplot(travel_insurance$Net.Sales ~ travel_insurance$Agency,
        outline = FALSE,
        main = "Diagrama de extremos e quartis Vendas Liquidas/Agência",
        ylab = "Vendas Líquidas",
        xlab = "Agências",
        col = c("#c8ade6"),
        las = 2
)

boxplot(travel_insurance$Net.Sales ~ travel_insurance$Claim,
        outline = FALSE,
        main = "Diagrama de extremos e quartis Idade",
        ylab = "Vendas Líquidas",
        xlab = "Seguro Acionado",
        col = c("#c8ade6"),
)
