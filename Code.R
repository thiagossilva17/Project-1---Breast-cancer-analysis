#install.packages("mclust")
library(mclust)
library(ggplot2)

BD <- wdbc
?wdbc

#Verificando dimensoes do dataframe
dim(BD)

#Visualizando data frame
#View(BD)

#verficando proporcao de dados belignos e malignos
table(BD$Diagnosis)
prop.table(table(BD$Diagnosis))

#Limpando

#Verificando se há algum valor NA no dataset
anyNA(BD)

#Porcentagem de valores NAS em cada coluna
NAS <- round(colSums(is.na(BD))*100/nrow(BD), 2)
#VER TODOS NAS
NAS

#


BD <- BD[,-1]

box.plot <- function(x){
  if(is.numeric(BD[, x])){
    ggplot(BD, aes_string('Diagnosis', x)) +
      geom_boxplot( fill=c('#009900','#FF3300')) +
      ggtitle(paste('Diagnósticos por ', x))
  }
}

col.names = names(BD)
sapply(col.names, box.plot)

str(BD)
library(corrplot)
corrplot(cor(BD[,-1]), method= "color")



library(caret)

#Criando matriz com linhas dos dados de treino - 70%
set.seed(1)
filtro <- createDataPartition(y=BD$Diagnosis, p=0.7, list=FALSE)

#dados de treino e teste
treino <- BD[filtro,]
teste <- BD[-filtro,]

#Criando o modelo
set.seed(1)
modelo <- train(Diagnosis ~ ., data=treino, method="glmnet", tuneLenght=4, trControl = trainControl(method="cv", number = 5))

#Precisao no modelo de treino
mean(modelo$resample$Accuracy)

#Prevendo dados no modelo de teste
Prev <- predict(modelo, teste)
#View(data.frame(teste$Diagnosis, Prev))

library(gmodels)

#Visualizando matriz de confusao
CrossTable(Prev, teste$Diagnosis, dnn = c("Previsto", "Real"), prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, prop.c = FALSE)

#Caret
#Verificando acuracia
confusionMatrix(Prev, teste$Diagnosis, dnn = c("Previsto", "Real"))


#ROC/ AUC

#Calculando Probabilidades de ser beligno ou maligno
PrevProb <- predict(modelo, teste, type="prob")

#Visualizando probabilidades
View(round(PrevProb, 2))

#Precisa-se armazenar apenas uma das colunas de vetores, neste caso as probabilidades dos belignos
PrevProb <- PrevProb$B

#install.packages("pROC")
library(pROC)

#Sensibilidade = 1
#TPR = sensitivity = TP/ (TP+FN)

#Especificidade = 1
#FPR= 1- Specificity = FP/ (FP+TN)
  
#Curva ROC
#Calculando os valores com base nos dados de teste em função da probabilidade
ROC <- roc(teste$Diagnosis ~ PrevProb, levels= c("M", "B"))

?roc

plot(ROC)

ROC$auc

#Criando data frame com sensibilidade, especificidade e thresholds arrendondando pela segunda casa
View(round(data.frame(ROC$sensitivities, ROC$specificities, ROC$thresholds),2))

#Resultado do modelo
confusionMatrix(Prev, teste$Diagnosis, dnn=c("Previsto", "Real"))

#Diminuindo o numero de falsos positivos para tumores belignos

#Visualizando novamente as melhores combinacoes de especificidade e sensibilidades 
#para que ocorra o menor número possível de de falsos tumores belignos
View(round(data.frame(ROC$sensitivities, ROC$specificities, ROC$thresholds),2))

#Ordenando por especificidade conseguimos visualizar a combinacao que zeraria os falsos positivos,
#porem o modelo perderia muita sensibilidade ficando em torno de apenas 56%
ROC$thresholds[111]

#Armazenando o vetor de probabilidades em outra variavel
PrevT <- PrevProb

#Substituindo as probabilidades acima de 99,45% por "B" (tumores belignos)
#Dessa forma, um tumor sera considerado beligno apenas se apresentar probabilidade superior a 99,45%
PrevT[PrevT>ROC$thresholds[111]] <- "B"
#Todas as probabilidades que nao foram identificadas como "B" (beligno) serao "M" (maligno)
PrevT[PrevT != "B"] <- "M"
#Convertendo para fator
PrevT <- as.factor(PrevT)

#Resultado do modelo com thereshold ajustado
confusionMatrix(PrevT, teste$Diagnosis, dnn = c("Previsto", "Real"))
#A precisao do modelo caiu 24%, porem reduzimos ao maximo o numero de acusacoes de tumores belignos que na verdade sao malignos


#Tentando aumentar precisao do modelo, aceitando o minimo de 1 acusacao errada para os tumores belignos

#Filtrando dados com sensibilidade acima de 0.89 e a maior combinacao possivel de especificidade
View(round(data.frame(ROC$sensitivities, ROC$specificities, ROC$thresholds),2))



#Encontramos o vetor 75, com sensibilidade de 0.89 e especificidade de 0.98
#Substituindo no modelo temos
ROC$thresholds[75]

PrevT <- PrevProb
PrevT[PrevT>ROC$thresholds[75]] <- "B"
PrevT[PrevT != "B"] <- "M"
PrevT <- as.factor(PrevT)

#Resultado do modelo com thereshold ajustado
confusionMatrix(PrevT, teste$Diagnosis, dnn = c("Previsto", "Real"))

#Melhoramos a precisao do modelo em 20% aceitando 1 caso de falso positivo para tumores belignos
#Aceitaremos este modelo com a acuracia de 92%
