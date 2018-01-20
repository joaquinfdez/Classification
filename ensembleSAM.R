library(RXKCD)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
library(textstem)

maximo <- function(positive, negative, neutral, dataset)
{
  if(positive > negative && positive > neutral)
  {
    result <- "positive"
  }
  else if (negative > positive && negative > neutral)
  {
    result <- "negative"
  }
  else if (neutral > positive && neutral > negative)
  {
    result <- "neutral"
  }
  else
  {
    result <- "empate"
  }
  return(result)
}

enumera = 1

dataset <- read.csv(file = toString(paste("./data/Dataset.csv")))

resultado <- data.frame(matrix(ncol = 3, nrow = nrow(dataset)), stringsAsFactors=FALSE)

colnames(resultado)[1] = "ID"
colnames(resultado)[2] = "Corpus"
colnames(resultado)[3] = "EnsembleSentiment"

for(i in 1:nrow(dataset))
{
  positive = 0
  negative = 0
  neutral = 0
  
  for(j in 3:7)
  {
    if( as.String(dataset[i,j]) == as.String("positive"))
    {
      positive = positive + 1
    } 
    else if (as.String(dataset[i,j]) == as.String("negative")){
      negative = negative + 1
    }
    else
    {
      neutral = neutral + 1
    }
    
    # Hayamos el máximo de los 3
    result <- maximo(positive, negative, neutral, dataset)
    
    # En caso de empate desempatamos
    if(result == "empate")
    {
      media = 0
      for(k in 8:12)
      {
        media = media + dataset[i,k]
      }
      media = media/5
      
      if(media>=0 && media <=0.4)
      {
        result <- "negative"
      }
      else if (media>0.4 && media<0.6)
      {
        result <- "neutral"
      }
      else
      {
        result <- "positive"
      }
    }
    # comillas <- cat("\"")
    # texto <- paste0(comillas,dataset$Text[i], comillas)
    # Guardamos los datos en nuestro resultado
    # resultado[i,1] <- as.String(texto)
    resultado[i,1] <- enumera
    resultado[i,2] <- as.String(dataset$Corpus[i])
    resultado[i,3] <- result
  }
  enumera = enumera + 1
}

write.table(resultado, file = toString("./data/ensemble/ensembleSAMs.csv"), row.names = FALSE, sep = ",")

