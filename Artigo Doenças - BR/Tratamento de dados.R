
#Bibliotecas
library(readr)
library(tidyverse)
library(stringdist)
library(readxl)
library(writexl)
library(readr)


dados <- read_excel("BD/MAPA.xls")


###### Minhas Funções ###########
# Converte string padroes em numero 
converte_nomes_para_numero <- function(x,vetor) {
  nomes_corretos <- c(vetor)
  
  # cria um vetor para armazenar as cidades convertidas
  nomes_convertidos <- vector(mode = "numeric", length = length(x))
  
  # itera sobre cada cidade e encontra a correspondência mais próxima
  for (i in seq_along(x)) {
    nomes_atual <- x[i]
    
    # se a cidade atual estiver nas cidades corretas, converte para seu índice
    if (nomes_atual %in% nomes_corretos) {
      nomes_convertidos[i] <- match(nomes_atual, nomes_corretos)
    } else {
      # caso contrário, encontra a cidade correta com menor distância de edição
      distancias <- sapply(nomes_corretos, function(cidade_correta) adist(nomes_atual, cidade_correta))
      melhor_correspondencia <- nomes_corretos[which.min(distancias)]
      nomes_convertidos[i] <- match(melhor_correspondencia, nomes_corretos)
    }
  }
  
  # retorna o vetor com as cidades convertidas
  return(nomes_convertidos)
}

# Encontra duplicatas
encontrar_duplicatas_lista <- function(x) {
  duplicatas <- duplicated(x)
  return(list(unique(x[duplicatas]), duplicatas))
}

table(dados$Semestre)
