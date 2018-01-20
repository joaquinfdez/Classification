Messages <- read.csv(file = "./data/Dataset.csv", stringsAsFactors = FALSE)
resultado = data.frame(matrix(ncol = 1, nrow = 500))
colnames(resultado)[1] = "ID"
corpus <- c("Airlines", "ClintonTrump", "Food", "Movies", "TripAdvisor", "TripAdvisor2", "Twitter")


for(i in 1:length(corpus))
{
# i = 2
  ensemble <- read.csv(file = toString(paste("./data/ensemble/ensembleSAMsEvaluadas", corpus[i], ".csv", sep = " ")))
  
  cont = 1
  
  for(j in 1:50)
  {
    indice = ensemble$ID[j] #Recorremos las frases con las que trabajamos, para no coger las que hemos eliminado del conjunto inicial
    resultado$ID[cont] = toString(Messages$Text[indice])
    print(Messages$Text[indice])
    
    cont = cont + 1
  }
  write.table(resultado, file = toString(paste("./data/evaluar_a_mano", corpus[i] ,".csv", sep = " ")), row.names = FALSE, sep = ";;;")
  
}