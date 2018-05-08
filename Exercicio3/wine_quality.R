set.seed(1234567890)
library(neuralnet)
library(hydroGOF)
library(leaps)
library(arules)

dados <- read.csv("/Users/MarcoSilva/Dropbox/UNIVERSIDADE/3º Ano/2º Semestre/Sistemas de Representação de Conhecimento e Raciocínio/Trabalho de Grupo/winequality-white.csv",header=TRUE,sep=";",dec=".")

colMax <- function(data) sapply(data,max,na.rm=TRUE)
maiores <- colMax(dados)

dados$fixed.acidity <- dados$fixed.acidity/maiores[1]
dados$volatile.acidity <- dados$volatile.acidity/maiores[2]
dados$citric.acid <- dados$citric.acid/maiores[3]
dados$residual.sugar <- dados$residual.sugar/maiores[4]
dados$chlorides <- dados$chlorides/maiores[5]
dados$free.sulfur.dioxide <- dados$free.sulfur.dioxide/maiores[6]
dados$total.sulfur.dioxide <- dados$total.sulfur.dioxide/maiores[7]
dados$density <- dados$density/maiores[8]
dados$pH <- dados$pH/maiores[9]
dados$sulphates <- dados$sulphates/maiores[10]
dados$alcohol <- dados$alcohol/maiores[11]
dados$quality <- dados$quality/maiores[12]

treino <- dados[1:3673, ]
teste <- dados[3674:4898, ]

funcao <- quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide +	total.sulfur.dioxide + density +	pH	+ sulphates +	alcohol
selecao <- regsubsets(funcao,dados,nvmax = 5)
summary(selecao)

formula <- quality ~ alcohol + sulphates + chlorides + volatile.acidity

# discretize aqui

rnawine <- neuralnet(formula, teste, hidden = c(20,10), lifesign = "full", linear.output = FALSE, threshold = 0.1)

plot(rnawine, rep = "best")

# definir variaveis de input para teste
teste.01 <- subset(teste, select = c("alcohol", "sulphates", "chlorides", "volatile.acidity"))

# testar a rede com os novos casos
rnawine.resultados <- compute(rnawine, teste.01)

# imprimir resultados
resultados <- data.frame(atual = teste$quality, previsao=rnawine.resultados$net.result)

# imprimir resultados arredondados
#resultados$previsao <- round(resultados$previsao, digits = 0)
View(resultados)

#calcular o RMSE
rmse(c(teste$quality),c(resultados$previsao))
