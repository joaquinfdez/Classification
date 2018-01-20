library(RXKCD)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(data.table)
library(SnowballC)
library(textstem)

maximo_etiquetado <- function(positive, negative, neutral)
{
  result <- array(0:0,c(2,1)) 
  if(positive > negative && positive > neutral)
  {
    result[1,1] <- as.String("positive")
    result[2,1] <- as.numeric(positive)
    
  }
  else if (negative > positive && negative > neutral)
  {
    result[1,1] <- as.String("negative")
    result[2,1] <- as.numeric(negative)
  }
  else 
  {
    result[1,1] <- as.String("neutral")
    result[2,1] <- as.numeric(neutral)
  }

  return(result)
}

maximo_categorico <- function(positive, negative, neutral, dataset)
{
  if(positive > negative && positive > neutral)
  {
    result <- as.String("positive")
  }
  else if (negative > positive && negative > neutral)
  {
    result <- as.String("negative")
  }
  else if (neutral > positive && neutral > negative)
  {
    result <- as.String("neutral")
  }
  else
  {
    result <- as.String("empate")
  }
  return(result)
}

herramientas <- c("BingSentiment", "CoreNLPSentiment", "MCSentiment", "MicrosoftSentiment", "SentiStrSentiment")
tecnicas = c("knn", "J48", "svmLinear", "rpart", "C5.0")
corpus <- c("Airlines", "ClintonTrump", "Food", "Movies", "TripAdvisor", "TripAdvisor2", "Twitter")

aux <- data.frame(matrix(ncol = 2, nrow = 500))

colnames(aux)[1] = "ID"
colnames(aux)[2] = "EnsembleSentiment"

for(i in 1:length(herramientas))
{
  herramienta <- herramientas[i]
  # herramienta <- "BingSentiment"
  for(t in 1:length(corpus))
  {
    tema = corpus[t]
    # tema <- "Airlines"
    for(j in 1:length(tecnicas))
    {
      aux <- data.frame(matrix(ncol = 2, nrow = 500))
      
      colnames(aux)[1] = "ID"
      colnames(aux)[2] = "EnsembleSentiment"
    # j = 1
      algoritmo = tecnicas[j]
      prob_c50 <- read.csv(file = toString(paste("./data/ensemble/probabilidades", herramienta, tema,"C5.0 .csv", sep = " ")))
      prob_j48 <- read.csv(file = toString(paste("./data/ensemble/probabilidades", herramienta, tema,"J48 .csv", sep = " ")))
      prob_knn <- read.csv(file = toString(paste("./data/ensemble/probabilidades", herramienta, tema,"knn .csv", sep = " ")))
      prob_rpart <- read.csv(file = toString(paste("./data/ensemble/probabilidades", herramienta, tema,"rpart .csv", sep = " ")))
      prob_svm <- read.csv(file = toString(paste("./data/ensemble/probabilidades", herramienta, tema,"svmLinear .csv", sep = " ")))
      indice = 1
      for(k in 1:nrow(prob_c50))
      {
        # k = 1
        valores <- array(0:0,c(2,5)) 
        positivo = prob_c50$positive[k]
        negativo = prob_c50$negative[k]
        neutral = prob_c50$neutral[k]
        valores[,1] <- maximo_etiquetado(positivo, negativo, neutral)
        
        positivo = prob_j48$positive[k]
        negativo = prob_j48$negative[k]
        neutral = prob_j48$neutral[k]
        valores[,2] <- maximo_etiquetado(positivo, negativo, neutral)
        
        positivo = prob_knn$positive[k]
        negativo = prob_knn$negative[k]
        neutral = prob_knn$neutral[k]
        valores[,3] <- maximo_etiquetado(positivo, negativo, neutral)
        
        positivo = prob_rpart$positive[k]
        negativo = prob_rpart$negative[k]
        neutral = prob_rpart$neutral[k]
        valores[,4] <- maximo_etiquetado(positivo, negativo, neutral)
        
        positivo = prob_svm$positive[k]
        negativo = prob_svm$negative[k]
        neutral = prob_svm$neutral[k]
        valores[,5] <- maximo_etiquetado(positivo, negativo, neutral)
        
        # print(valores)
        
        positive = 0
        negative = 0
        neutral = 0
        
        for(a in 1:5)
        {
          
          if( as.String(valores[1,a]) == as.String("positive"))
          {
            positive = positive + 1
          } 
          else if (as.String(valores[1,a]) == as.String("negative")){
            negative = negative + 1
          }
          else
          {
            neutral = neutral + 1
          }
        }
        
        max <- maximo_categorico(positive, negative, neutral)
        
        if(max == "empate")
        {
          positive = array(0:0,c(2,1)) 
          negative = array(0:0,c(2,1)) 
          neutral = array(0:0,c(2,1)) 
          
          for(a in 1:5)
          {
          # a = 1
            if( as.String(valores[1,a]) == as.String("positive"))
            {
              positive[1,1] = as.String("positive")
              positive[2,1] = as.numeric(positive[2,1]) +  as.numeric(valores[2,a])
            }
            else if (as.String(valores[1,a]) == as.String("negative")){
              negative[1,1] = as.String("negative")
              negative[2,1] = as.numeric(negative[2,1]) +  as.numeric(valores[2,a])
            }
            else
            {
              neutral[1,1] = as.String("neutral")
              neutral[2,1] = as.numeric(neutral[2,1]) +  as.numeric(valores[2,a])
          }
          }
          
          maximo_en_caso_empate <- maximo_etiquetado(positive[2,1], negative[2,1], neutral[2,1])
          resultado = as.String(maximo_en_caso_empate[1,1])
        }
        else
        {
          resultado <- max
        }
        
        # print(c("Ensamble: ", resultado))
        aux$ID[k] = indice
        aux$EnsembleSentiment[k] = toString(resultado) 
        
        indice = indice + 1
        
      }
      
      write.table(aux, file = toString(paste("./data/ensemble/ensembleALG", herramienta, tema ,".csv", sep = " ")), row.names = FALSE, sep = ",")
        
    }
  }
}

