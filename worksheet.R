if (!require(psych))
        install.packages("psych")
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

subset(destinationAbs, destinationAbs > 100)

print(destinationAbs)


netRel <- prop.table(netAbs)
commRel <- prop.table(commAbs)
ageRel <- prop.table(ageAbs)
agencyRel <- prop.table(agencyAbs)
destinationRel <- prop.table(destinationAbs)
productRel <- prop.table(productAbs)
claimRel <- prop.table(claimAbs)

colnames(destination)

net <- cbind("Frequência Absoluta" = netAbs, "Frequência Relativa" = netRel, "Frequência Relativa %" = netRel*100, "Frequência Abs. Acumulada" = cumsum(netAbs), "Frequência Rel.% Acumulada" = cumsum(netRel*100))
comm <- cbind("Frequência Absoluta" = commAbs, "Frequência Relativa" = commRel, "Frequência Relativa %" = commRel*100, "Frequência Abs. Acumulada" = cumsum(commAbs), "Frequência Rel.% Acumulada" = cumsum(commRel*100))
age <- cbind("Frequência Absoluta" = ageAbs, "Frequência Relativa" = ageRel, "Frequência Relativa %" = ageRel*100, "Frequência Abs. Acumulada" = cumsum(ageAbs), "Frequência Rel.% Acumulada" = cumsum(ageRel*100))
agency <- cbind("Frequência Absoluta" = agencyAbs, "Frequência Relativa" = agencyRel, "Frequência Relativa %" = agencyRel*100, "Frequência Abs. Acumulada" = cumsum(agencyAbs), "Frequência Rel.% Acumulada" = cumsum(agencyRel*100))
destination <- cbind("Frequência Absoluta" = destinationAbs, "Frequência Relativa" = destinationRel, "Frequência Relativa %" = destinationRel*100, "Frequência Abs. Acumulada" = cumsum(destinationAbs), "Frequência Rel.% Acumulada" = cumsum(destinationRel*100))
product <- cbind("Frequência Absoluta" = productAbs, "Frequência Relativa" = productRel, "Frequência Relativa %" = productRel*100, "Frequência Abs. Acumulada" = cumsum(productAbs), "Frequência Rel.% Acumulada" = cumsum(productRel*100))
claim <- cbind("Frequência Absoluta" = claimAbs, "Frequência Relativa" = claimRel, "Frequência Relativa %" = claimRel*100, "Frequência Abs. Acumulada" = cumsum(claimAbs), "Frequência Rel.% Acumulada" = cumsum(claimRel*100))

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
summary(travel_insurance$Age)


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
        main = "Diagrama de extremos e quartis Vendas Líq./Seguro Acionado",
        ylab = "Vendas Líquidas",
        xlab = "Seguro Acionado",
        col = c("#c8ade6")
)

describe(travel_insurance$Net.Sales)
describe(travel_insurance$Commision..in.value.)
describe(travel_insurance$Age)

describeBy(travel_insurance$Net.Sales, group = travel_insurance$Claim, mat = TRUE, digits = 2)
describeBy(travel_insurance$Net.Sales, group = travel_insurance$Agency, mat = TRUE, digits = 2)
describeBy(travel_insurance$Net.Sales, group = ageGroups, mat = TRUE, digits = 2)
describeBy(travel_insurance$Net.Sales, group = travel_insurance$Destination, mat = TRUE, digits = 2)
describeBy(travel_insurance$Net.Sales, group = travel_insurance$Product.Name, mat = TRUE, digits = 2)
describeBy(travel_insurance$Net.Sales, group = commissionGroups, mat = TRUE, digits = 2)
describeBy(travel_insurance$Commision..in.value., group = travel_insurance$Claim, mat = TRUE, digits = 2)
describeBy(travel_insurance$Commision..in.value., group = travel_insurance$Agency, mat = TRUE, digits = 2)
describeBy(travel_insurance$Commision..in.value., group = ageGroups, mat = TRUE, digits = 2)
describeBy(travel_insurance$Commision..in.value., group = travel_insurance$Destination, mat = TRUE, digits = 2)
describeBy(travel_insurance$Commision..in.value., group = travel_insurance$Product.Name, mat = TRUE, digits = 2)
describeBy(travel_insurance$Commision..in.value., group = netGroups, mat = TRUE, digits = 2)

hist(travel_insurance$Age,
        main = "Histograma Idade",
        xlab = "Idade",
        ylab = "Total Clientes",
        ylim = c(0, 30000),
        col = c("#c8ade6")
)

hist(travel_insurance$Commision..in.value.,
        main = "Histograma valor de comissões",
        xlab = "Valor de Comissão",
        ylab = "Total de vendas",
        ylim = c(0, 60000),
        col = c("#c8ade6")
)

hist(travel_insurance$Net.Sales,
        main = "Histograma de vendas líquidas",
        xlab = "Vendas em líquido",
        ylab = "Total de vendas",
        ylim = c(0, 60000),
        col = c("#c8ade6")
        )

default_margins <- par("mar")
margins <- default_margins + c(11.5, 0, 0, 0)
par(mar = margins)

barplot(productAbs,
        main = "Gráfico de barras total de Produtos",
        ylab = "Total de Produtos",
        las = 2,
        col = c("#c8ade6")
)

par(mar = default_margins)

pieRel <- subset(destinationRel, destinationRel > 0.035)

pie(pieRel,
        labels = paste0(round(100 * pieRel, 2), "%"),
        main = "10 destinos mais frequentes",
        clockwise = TRUE,
        col = rainbow(10)
)
legend("bottomleft", legend = c("Australia", "China", "Hong Kong", "India", "Indonesia", "Malaysia", "Philippines", "Singapore", "Thailand", "United States"), fill = rainbow(10))
