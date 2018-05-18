set.seed(1234567890)
library(neuralnet)
library(hydroGOF)
library(leaps)
library(arules)

# Carrega o dataset do ficheiro csv
dados <- read.csv("C:\\Users\\DiogoAfonsoSilvaCost\\Desktop\\Repos\\SRCR_1718\\Exercicio3\\winequality-red.csv",header=TRUE,sep=";",dec=".")

# Retorna os m?ximos de todas as colunas
colMax <- function(data) sapply(data,max,na.rm=TRUE)

# Função que transforma os dados para alimentar as RNA's
convert <- function(dados) {
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
  
  return(dados)
}



# Função que retorna uma rede neuronal
rede_neuronal <- function(formula, treino, hidden, teste_vars, teste) {
  
  rna_wine <- neuralnet(formula, treino, hidden, lifesign = "full", linear.output = TRUE, threshold = 0.01)
  
  # Computa??o da rede com casos de teste
  rna_wine$resultados <- compute(rna_wine, teste_vars)
  
  rna_wine$resultados_comp <- data.frame(atual = teste$quality, previsao = rna_wine$resultados$net.result)
  # atual <- round(rna_wine$resultados_comp$atual, digits = 1)
  # previsao <- round(rna_wine$resultados_comp$previsao, digits = 1)
  # rna_wine$resultados_comp_round <- data.frame(atual = atual, previsao = previsao)
  
  # C?lculo do RMSE
  rna_wine$rmse <- rmse(c(teste$quality),c(rna_wine$resultados_comp$previsao))
  
  return(rna_wine)
}

# discretize aqui

# Transforma os dados
dados <- convert(dados)

# Cria um conjunto com os dados de treino
treino <- dados[1:534, ]

# Cria um conjunto com os dados de teste
teste <- dados[535:1600, ]

# Sele??o das componentes mais significativas
funcao <- quality ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + free.sulfur.dioxide +	total.sulfur.dioxide + density +	pH	+ sulphates +	alcohol
# selecao <- regsubsets(funcao,dados,nvmax = 5)
# summary(selecao)

formula <- quality ~ alcohol + free.sulfur.dioxide  + residual.sugar + volatile.acidity + density + pH

# definir variaveis de input para teste
teste.01 <- subset(teste, select = c("alcohol", "free.sulfur.dioxide", "residual.sugar", "volatile.acidity", "density", "pH"))

# Topologia das camadas interm?dias
hidden <- c(3,2)

rna_wine_output <- rede_neuronal(formula, treino, hidden, teste.01, teste)

# Visualiza??o gr?fica da rede neuronal
plot(rna_wine_output, rep = "best")

# Visualiza??o do valor RMSE
View(rna_wine_output$rmse)
View(rna_wine_output$resultados_comp)


