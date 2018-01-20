library(RXKCD)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(data.table)
library(SnowballC)
library(textstem)

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
    result <- as.String("neutral")
  }
  return(result)
}

herramientas <- c("BingSentiment", "CoreNLPSentiment", "MCSentiment", "MicrosoftSentiment", "SentiStrSentiment")
tecnicas = c("knn", "J48", "svmLinear", "rpart", "C5.0")
corpus <- c("Airlines", "ClintonTrump", "Food", "Movies", "TripAdvisor", "TripAdvisor2", "Twitter")

for(t in 1:length(corpus))
{
  tema = corpus[t]
  print(tema)
  # tema <- "Airlines"
  
  # *********************************************************************************
  #   Creamos nuestra matriz de diversidad
  # *********************************************************************************
  matriz_diversidad <- data.frame(matrix(data = 0, nrow = 6, ncol = 6), stringsAsFactors=FALSE)

  colnames(matriz_diversidad)[1] <- "BingSentiment"
  colnames(matriz_diversidad)[2] <- "CoreNLPSentiment"
  colnames(matriz_diversidad)[3] <- "MCSentiment"
  colnames(matriz_diversidad)[4] <- "MicrosoftSentiment"
  colnames(matriz_diversidad)[5] <- "SentiStrSentiment"
  colnames(matriz_diversidad)[6] <- "ensembleSAM"
  
  rownames(matriz_diversidad)[1] <- "knn"
  rownames(matriz_diversidad)[2] <- "J48"
  rownames(matriz_diversidad)[3] <- "svmLinear"
  rownames(matriz_diversidad)[4] <- "rpart"
  rownames(matriz_diversidad)[5] <- "c5.0"
  rownames(matriz_diversidad)[6] <- "ensembleALG"
  
  
  for(i in 1:length(herramientas))
  {
    herramienta <- herramientas[i]
    # herramienta <- "BingSentiment"
    # print(herramienta)
    for(j in 1:length(tecnicas))
    {
      algoritmo = tecnicas[j]
      print(algoritmo)
      # algoritmo = "J48"

      probabilidad <- read.csv(file = toString(paste("./data/ensemble/probabilidades", herramienta, tema, algoritmo,".csv", sep = " ")))
      
      Messages <- read.csv(file = toString(paste("./data/datasets/aprendizaje/dataset_train", herramienta, tema ,".csv", sep = " ")))
      colnames(Messages)[1:(ncol(Messages)-1)] <- paste0("X", 1:(ncol(Messages)-1)) #Cambiamos los nombres por variables X1,X2, ..., Xn
      colnames(Messages)[ncol(Messages)] = "Sentiment"
      Messages$Sentiment <- factor(Messages$Sentiment, c("positive", "neutral", "negative"))
      
      matriz_diversidad[6,6] = nrow(probabilidad) #El número de mensajes que hay 
      
      ensembleSAMs <- read.csv(file = toString(paste("./data/ensemble/ensembleSAMsEvaluadas", tema,".csv", sep = " ")))
      ensembleALG <- read.csv(file = toString(paste("./data/ensemble/ensembleALG", herramienta, tema, ".csv", sep = " ")))
      matriz_diversidad[j, 6]=0
      for(k in 1:nrow(probabilidad))
      {
        resultado_fila = maximo_categorico(probabilidad$positive[k], probabilidad$negative[k], probabilidad$neutral[k])
        # *********************************************************************************
        #   Para cada mensaje comparamos el valor obtenido por el modelo de cada 
        #        técnica y lo comparamos con la herramienta
        # *********************************************************************************
        if(as.String(resultado_fila)==as.String(Messages$Sentiment[k]))
        {
          # print(c("Compara: ", resultado_fila, as.String(Messages$Sentiment[k])))
          # print("Entra")
          matriz_diversidad[j, i] = matriz_diversidad[j, i] + 1
          # print(matriz_diversidad[j, i])
        }
        # *********************************************************************************
        #   Para cada mensaje comparamos el valor obtenido por el modelo de cada 
        #        técnica y lo comparamos con el ensemble de herramientas
        # *********************************************************************************
        
        if(as.String(resultado_fila)==as.String(ensembleSAMs$EnsembleSentiment[k]))
        {
          # print(c("Compara: ", resultado_fila, as.String(ensembleSAMs$EnsembleSentiment[k])))
          # print("Entra")
          matriz_diversidad[j, 6] = matriz_diversidad[j, 6] + 1
          # print(matriz_diversidad[j, 6])
        }
      }
    }
    
    for(k in 1:nrow(probabilidad))
    {
      # *********************************************************************************
      #   Para cada mensaje comparamos el valor obtenido por cada herramienta de sentimietno
      #        y lo comparamos con el ensemble de las técnicas
      # *********************************************************************************
      if(as.String(ensembleALG$EnsembleSentiment[k])==as.String(Messages$Sentiment[k]))
      {
        matriz_diversidad[6, i] = matriz_diversidad[6, i] + 1
      }
    }
  }
  write.table(matriz_diversidad, file = toString(paste("./data/result/diversidad", tema ,".csv", sep = " ")), row.names = TRUE, col.names = TRUE, sep = ",")
  
}