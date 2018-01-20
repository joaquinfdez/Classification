dataset <- read.csv(file = "./data/Dataset.csv")

corpus <- c("Airlines", "ClintonTrump", "Food", "Movies", "TripAdvisor", "TripAdvisor2", "Twitter")

palabras = 20

ruta_relativa = paste0("./data/datasets/dataset_", palabras)

for(t in 1:length(corpus))
{
# t = 1
  print(c("Tema: ", corpus[t]))
  
  tema <- toString(corpus[t])
  
  # Preparamos nuestro dataframe con la columna sentimiento
  Messages <- read.csv(file = toString(paste(ruta_relativa, tema ,".csv", sep = " ")), stringsAsFactors=FALSE)
  colnames(Messages)[1:(ncol(Messages))] <- paste0("X", 1:(ncol(Messages))) #Cambiamos los nombres por variables X1,X2, ..., Xn
  
  # Messages[1,(ncol(Messages)+1)] = "Sentiment"
  
  # *************************************************************************
  
  Messages$Sentiment <- dataset$BingSentiment[dataset$Corpus == tema]
  
  write.table(Messages, file = toString(paste("./data/datasets/dataset BingSentiment", tema ,".csv", sep = " ")), row.names = FALSE, sep = ",")

  
  # *************************************************************************
  
  Messages$Sentiment <- dataset$CoreNLPSentiment[dataset$Corpus == tema]
  
  write.table(Messages, file = toString(paste("./data/datasets/dataset CoreNLPSentiment", tema ,".csv", sep = " ")), row.names = FALSE, sep = ",")
  
    
  # *************************************************************************
  
  Messages$Sentiment <- dataset$MicrosoftSentiment[dataset$Corpus == tema]
  
  write.table(Messages, file = toString(paste("./data/datasets/dataset MicrosoftSentiment", tema ,".csv", sep = " ")), row.names = FALSE, sep = ",")
  
  # *************************************************************************
  
  Messages$Sentiment <- dataset$MCSentiment[dataset$Corpus == tema]
  
  write.table(Messages, file = toString(paste("./data/datasets/dataset MCSentiment", tema ,".csv", sep = " ")), row.names = FALSE, sep = ",")
  
  # *************************************************************************
  
  Messages$Sentiment <- dataset$SentiStrSentiment[dataset$Corpus == tema]
  
  write.table(Messages, file = toString(paste("./data/datasets/dataset SentiStrSentiment", tema ,".csv", sep = " ")), row.names = FALSE, sep = ",")
  
}
  