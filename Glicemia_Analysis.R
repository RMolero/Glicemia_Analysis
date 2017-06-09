# system.time(source("Glicemia_Analysis.R"))

library(lubridate)

My_path <- "Glicemia.csv"
My_data <- read.csv(My_path, stringsAsFactors = F)
My_nrow = nrow(My_data)

My_data$Data <- mdy(My_data$Data)
My_data$Data <- My_data$Data - My_data[1,]$Data
My_data$Dia.semana <- as.factor(My_data$Dia.semana)

My_data[My_data$Tipo=="Jejum",]$Tipo <- "Janta anterior"
My_data[My_data$Tipo=="Almoço",]$Tipo <- "Café antes almoço"
My_data[My_data$Tipo=="Jantar",]$Tipo <- "Almoço antes jantar"

Frutas <- c("uva", "melancia", "laranja", "manga", "banana", "mamao", "suco")
Nuts <- c("castanha", "amendoin", "caju", "para", "nozes")
Doces <- c("m&m", "chocolate", "bala", "bis")
Gordura <- c("linguica", "maionese", "mortadela")
Leite <- c("leite", "pó", "cafe", "achocolatado")
Lanche_integral <- c("integral")
Low_carb <- c("peixe", "sopa", "omelete", "alface", "tomate",
                       "legumes", "berin", "creme", "diet", "salsicha", "couve", "brocolis")
High_carb <- c("pizza", "arroz", "nhoque", "lasanha", "macarrao", "frances")
Fritura <- c("pastel", "bolinho", "empad", "risol", "frit", "chips", "ovo")
Nada <- c("nada")

My_list <- list(Frutas = Frutas,
                Nuts = Nuts,
                Doces = Doces, 
                Leite = Leite,
                Gordura = Gordura,
                Lanche_integral = Lanche_integral,
                Low_carb = Low_carb,
                High_carb = High_carb,
                Fritura = Fritura,
                Nada = Nada)

for (name in names(My_list))
{
  My_count = 0
  for (word in unlist(My_list[name])) {
    My_vec <- grepl(word, My_data[,4])
    My_vec <- My_vec | grepl(word, My_data[,5])
    if (My_count == 0){
      My_data[,name] <- as.numeric(My_vec)
    }
    else{
      My_data[,name] <- My_data[,name] | as.numeric(My_vec)
    }
    # cat("Nome: ", word," encontrado ", sum(as.numeric(My_vec))," vezes \n")
    My_count <- My_count + 1
  }
}

My_ncol <- ncol(My_data)
My_data$soma <- rowSums(My_data[,7:My_ncol])
My_data$soma <- as.factor(My_data$soma)

My_data <- My_data[,-(4:5)]

cat("Modelo completo...")
My_model <- lm(Glicemia ~ ., My_data)
print(summary(My_model))

rm(Doces, Fritura, Frutas, High_carb, Lanche_integral, Leite, Low_carb, My_list, My_ncol, My_nrow, My_path,
   name, word)

My_data_frame <- data.frame(My_model$coefficients)
write.csv(My_data_frame, "Saida.csv")
