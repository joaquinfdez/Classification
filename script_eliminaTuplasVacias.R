corpus <- c("Airlines", "ClintonTrump", "Food", "Movies", "TripAdvisor", "TripAdvisor2", "Twitter")
program <- c("BingSentiment", "CoreNLPSentiment", "MCSentiment", "MicrosoftSentiment", "SentiStrSentiment")
for(t in 1:length(corpus))
{
  for(j in 1:length(program))
  {
    tema = corpus[t]
    programa = program[j]
    aux = data.frame()
    indice = 1
  # tema = "Movies"
  # programa = "CoreNLPSentiment"
    Messages <- read.csv(file = toString(paste("./data/datasets/dataset", programa, tema ,".csv", sep = " ")), stringsAsFactors=FALSE)
    
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
          aux[indice, a] = Messages[x, a]
        }
        
        indice = indice + 1
      }
    }
    
    write.table(aux, file = toString(paste("./data/datasets/aprendizaje/dataset_train", programa, tema ,".csv", sep = " ")), row.names = FALSE, sep = ",")
    
  }
}