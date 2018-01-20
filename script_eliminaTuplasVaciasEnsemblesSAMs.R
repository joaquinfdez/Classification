library(RXKCD)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)
library(textstem)

corpus <- c("Airlines", "ClintonTrump", "Food", "Movies", "TripAdvisor", "TripAdvisor2", "Twitter")
ensembleDatasetTotal <- read.csv("./data/ensemble/ensembleSAMs.csv")


programa = "BingSentiment"

for(t in 1:length(corpus))
{
  tema = corpus[t]
  resultado = data.frame(matrix(ncol = 3, nrow = 500))
  colnames(resultado)[1] = "ID"
  colnames(resultado)[2] = "Corpus"
  colnames(resultado)[3] = "EnsembleSentiment"
  
  ensembleCorpus = data.frame(matrix(ncol = 3, nrow = 500))
  colnames(ensembleCorpus)[1] = "ID"
  colnames(ensembleCorpus)[2] = "Corpus"
  colnames(ensembleCorpus)[3] = "EnsembleSentiment"
  
  indice = 1
  # tema = "Airlines"
  Messages <- read.csv(file = toString(paste("./data/datasets/dataset", programa, tema ,".csv", sep = " ")), stringsAsFactors=FALSE)
  
  ensembleCorpus$ID = ensembleDatasetTotal$ID[ensembleDatasetTotal$Corpus == tema]
  ensembleCorpus$Corpus = ensembleDatasetTotal$Corpus[ensembleDatasetTotal$Corpus == tema]
  ensembleCorpus$EnsembleSentiment = ensembleDatasetTotal$EnsembleSentiment[ensembleDatasetTotal$Corpus == tema]
  
  for(x in 1:nrow(Messages))
  {
    todo_cero = TRUE;
    for(y in 1:(ncol(Messages)-1))
    {
      if(Messages[x,y] != 0)
      {
        todo_cero = FALSE;
      }
    }
    
    if(todo_cero == TRUE)
    {
      print(c("Hay tuplas vacias", tema, "fila: ", x))
    }
    else if(todo_cero == FALSE)
    {
      for(a in 1:ncol(Messages))
      {
        resultado[indice,1] <- ensembleCorpus$ID[x]
        resultado[indice,2] <- as.String(tema)
        resultado[indice,3] <- as.String(ensembleCorpus$EnsembleSentiment[x])
      }
      
      indice = indice + 1
    }
  }
  
  write.table(resultado, file = toString(paste("./data/ensemble/ensembleSAMsEvaluadas", tema ,".csv", sep = " ")), row.names = FALSE, sep = ",")

}