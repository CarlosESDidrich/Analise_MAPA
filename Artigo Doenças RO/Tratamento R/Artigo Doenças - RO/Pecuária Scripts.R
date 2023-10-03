
#Bibliotecas
library(readr)
library(tidyverse)
library(stringdist)
library(readxl)
library(writexl)


cidade <-  read_excel("C:/Users/carlo/Documents/Projetos/Artigo Doenças RO/Tratamento R/Artigo Doenças - RO/Banco de dados/MAPA/Doença_cidade_MAPA.xlsx")
head(cidade$MUNICIPIO)



### TRatamento nomes cidade ####
converte_cidades_para_numero <- function(x) {
  cidades_corretas <- c("Porto Velho",
                        "Guajará-Mirim",
                        "Vilhena",
                        "São Francisco do Guaporé",
                        "Nova Mamoré",
                        "Machadinho d'Oeste",
                        "São Miguel do Guaporé",
                        "Alta Floresta d'Oeste",
                        "Ji-Paraná",
                        "Candeias do Jamari",
                        "Pimenta Bueno",
                        "Pimenteiras do Oeste",
                        "Chupinguaia",
                        "Governador Jorge Teixeira",
                        "Costa Marques",
                        "Espigão d'Oeste",
                        "Ariquemes",
                        "Itapuã do Oeste",
                        "Alto Alegre dos Parecis",
                        "Cujubim",
                        "Cacoal",
                        "Seringueiras",
                        "Campo Novo de Rondônia",
                        "Buritis",
                        "Vale do Anari",
                        "Corumbiara",
                        "Alvorada d'Oeste",
                        "Jaru",
                        "Cerejeiras",
                        "Alto Paraíso",
                        "Parecis",
                        "Theobroma",
                        "Ouro Preto do Oeste",
                        "Cacaulândia",
                        "Monte Negro",
                        "Presidente Médici",
                        "Rio Crespo",
                        "Nova Brasilândia d'Oeste",
                        "Rolim de Moura",
                        "Colorado do Oeste",
                        "Cabixi",
                        "Santa Luzia d'Oeste",
                        "Mirante da Serra",
                        "Vale do Paraíso",
                        "Castanheiras",
                        "Novo Horizonte do Oeste",
                        "Urupá",
                        "Nova União",
                        "Ministro Andreazza",
                        "Primavera de Rondônia",
                        "São Felipe d'Oeste",
                        "Teixeirópolis")
  
  # cria um vetor para armazenar as cidades convertidas
  cidades_convertidas <- vector(mode = "numeric", length = length(x))
  
  # itera sobre cada cidade e encontra a correspondência mais próxima
  for (i in seq_along(x)) {
    cidade_atual <- x[i]
    
    # se a cidade atual estiver nas cidades corretas, converte para seu índice
    if (cidade_atual %in% cidades_corretas) {
      cidades_convertidas[i] <- match(cidade_atual, cidades_corretas)
    } else {
      # caso contrário, encontra a cidade correta com menor distância de edição
      distancias <- sapply(cidades_corretas, function(cidade_correta) adist(cidade_atual, cidade_correta))
      melhor_correspondencia <- cidades_corretas[which.min(distancias)]
      cidades_convertidas[i] <- match(melhor_correspondencia, cidades_corretas)
    }
  }
  
  # retorna o vetor com as cidades convertidas
  return(cidades_convertidas)
}

dados <-cidade %>% 
  mutate("Municipio_N" = converte_cidades_para_numero(cidade$MUNICIPIO))


# Tratamneto dos diágnosticos ####
## Teste de duplicatas para encontrar as doenças presentes no DF e ajudar a vizualização

encontrar_duplicatas_lista <- function(x) {
  duplicatas <- duplicated(x)
  return(list(unique(x[duplicatas]), duplicatas))
}


#diags <- encontrar_duplicatas_lista(cidade$DIAGNOSTICO)
#diags<-data.frame(print(diags[[1]]))
#Salvar em excel para analisar 
#write.table(diags, file='Diagnostico.xlsx', sep=';', dec=',', row.names=FALSE)


#Juntado e montadno DF completo tratado
dados$DIAGNOSTICO<- toupper(dados$DIAGNOSTICO)  
dados<- dados %>% 
  arrange(dados$DIAGNOSTICO)

doenças <-  read_excel("C:/Users/carlo/Documents/Projetos/Artigo Doenças RO/Tratamento R/Artigo Doenças - RO/Banco de dados/Diagnosticos/Diagnostico Completo.xlsx")

dados<-dados %>% 
  mutate("Diagnostico_N" = doenças$Doença_N)

## Separados no excel
doenças <-  read_excel("C:/Users/carlo/Documents/Projetos/Artigo Doenças RO/Tratamento R/Artigo Doenças - RO/Banco de dados/Diagnosticos/Doenças.xlsx")
#nao_doenças <-  read_excel("Banco de dados/Diagnosticos/Não Doenças.xlsx")








## DF para analisar as Doenças em RO####
doenças<- tibble(doenças$Diagnostico_N)
colnames(doenças)<-c("Diagnostico_N")

## Filtrando Apenas doenças 
doenças <-dados %>% semi_join(doenças, by =  'Diagnostico_N')
doenças <-doenças %>% 
  select(MES_ANO,UF,Municipio_N,Diagnostico_N,QUANTIDADE)






### Separar os valores da coluna "data" em "ano" e "mes" ####
doenças$MES_ANO <- as.character(doenças$MES_ANO)
doenças <- separate(doenças, col = MES_ANO, into = c("ano", "mes","dia"), sep = "-")


#Remove o dia#
doenças <- doenças %>% 
  select(ano,mes,UF,Municipio_N,Diagnostico_N,QUANTIDADE)
# Resultado
doenças
#write.table(doenças, file='Diagnostico_dataJunto.csv', sep=';', dec=',', row.names=FALSE)

table(doenças$mes)

doenças$UF <- doenças$UF %>% 
                  recode("RO" = 1)
  
#write.table(doenças, file='Diagnostico_mes.csv', sep=',', dec=',', row.names=FALSE)

table(doenças$Diagnostico_N)
  
