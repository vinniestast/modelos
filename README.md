#Regressão Linear - Programação#
nome = c("Vinicius","Lia", "Luiz","Rico", "Ana", "Carol", "Duda", "Marcia", "Igor", "João")
kWh = c(160,200,201,199,180,155,210,196,209,177)
familia = c(2,3,2,1,5,3,1,4,2,1)
metroq = c(50,66,70,65,59,78,62,55,70,66)
eledomest = c(5,6,7,3,8,4,11,8,5,9)
banco_de_dados <-data.frame(nome,kWh,familia,metroq,eledomest)
banco_de_dados
barplot(kWh)
barplot(familia)
barplot(eledomest)
summary(banco_de_dados)
regressao <- lm(kWh~familia+eledomest)
regressao
summary(lm(kWh~familia+eledomest))
plot(lm(kWh~familia+eledomest))
