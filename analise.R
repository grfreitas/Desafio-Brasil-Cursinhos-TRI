#leitura dos arquivos
dia1 = read.csv(file = "C:/Users/Gabs/Desktop/dados/dia1.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
dia2 = read.csv(file = "C:/Users/Gabs/Desktop/dados/dia2.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)
dados = read.csv(file = "C:/Users/Gabs/Desktop/dados/dados.csv", header = TRUE, sep = ";", stringsAsFactors=FALSE)

#tabela gabarito reformulada utilizando excel
gabEsp = read.csv(file = "C:/Users/Gabs/Desktop/dados/gabEsp.csv", header = FALSE, sep = ",", stringsAsFactors=FALSE)
gabEsp = t(gabEsp)
gabEsp = gabEsp[-c(1),]
gabIng = read.csv(file = "C:/Users/Gabs/Desktop/dados/gabIng.csv", header = FALSE, sep = ",", stringsAsFactors=FALSE)
gabIng = t(gabIng)
gabIng = gabIng[-c(1),]

#padroniza��o
colnames(dia1)[1]  <- "inscricao"
colnames(dia2)[1]  <- "inscricao"
colnames(dados)[1] <- "inscricao"

#cria��o de tabelas gerais
respostas = merge(dia1, dia2, by = "inscricao")

tabelaGeral = merge(dados, respostas, by = "inscricao")
colnames(tabelaGeral)[1] <- "Inscri��o"
colnames(tabelaGeral)[2] <- "C�digo do Curso"
colnames(tabelaGeral)[3] <- "Data de Nascimento"
colnames(tabelaGeral)[4] <- "Curso Pretendido"
colnames(tabelaGeral)[5] <- "Exerce Trabalho Remunerado?"
colnames(tabelaGeral)[6] <- "Presen�a"
colnames(tabelaGeral)[7] <- "Renda Familiar per Capita em S�larios M�nimos"
colnames(tabelaGeral)[8] <- "Dist�ncia do Cursinho em Minutos"
tabelaGeral[9] <- NULL
colnames(tabelaGeral)[9] <- "Reda��o"
colnames(tabelaGeral)[10] <- "Idioma"
tabelaGeral = tabelaGeral[,-c(11:ncol(tabelaGeral))]

#cria��o de tabela com acertos bin�rios para modelo IRT

respostas = merge(dia1, dia2, by = "inscricao")
respostas = respostas[-c(1:3, 95)]
for(i in 1:nrow(respostas)){
  if(respostas[i,1] == "ESP"){
    for(j in 2:ncol(respostas)){
      if(respostas[i,j] != gabEsp[1,j-1]){
        respostas[i,j] <- as.numeric(0)
      }
      else{ 
        respostas[i,j] <- as.numeric(1)
      }
    }
  }
  else{
    for(j in 2:ncol(respostas)){
      if(respostas[i,j] != gabIng[1,j-1]){
        respostas[i,j] <- as.numeric(0)
      }
      else{ 
        respostas[i,j] <- as.numeric(1)
      }
    }
  }
}
resultadosBin <- respostas[-c(1)]
acertos = data.matrix(resultadosBin)


tabelaGeral$"Escore Total" <- rowSums(acertos)
tabelaGeral$'Escore Linguagens' <- rowSums(acertos[,1:45])
tabelaGeral$'Escore Humanas' <- rowSums(acertos[,46:90])
tabelaGeral$'Escore Matem�tica' <- rowSums(acertos[,91:135])
tabelaGeral$'Escore Natureza' <- rowSums(acertos[,136:180])
tabelaGeral$'Presen�a' <- as.numeric(sub("%", "", tabelaGeral$Presen�a))/100
tabelaGeral[,7] <- as.numeric(sub(",", "\\.", tabelaGeral[,7]))

library(zoo)
tabelaGeral$Presen�a <- na.fill(tabelaGeral$Presen�a, as.numeric(3))

for(i in 1:nrow(tabelaGeral)){
  if(tabelaGeral[i,5] == "Não"){
    tabelaGeral[i,5] <- as.numeric(0)
  } else if(tabelaGeral[i,5] == "Sim"){
    tabelaGeral[i,5] <- as.numeric(1)
  } else
    tabelaGeral[i,5] <- as.numeric(2)
}

curso7 <- tabelaGeral[-c(1:262, 299:344),]

library(ggplot2)

for(i in 1:nrow(curso7)){
  if(curso7[i,6] >= 0.6){
    curso7[i,16] <- 'Alta Presen�a'
  } else if(tabelaGeral[i,5] < 0.6){
    curso7[i,16] <- 'Baixa Presen�a'
  }
}

p <- ggplot(curso7, aes(x=curso7[,6], 
                        y=curso7[,11], 
                        col=curso7[,16])) + geom_point() + geom_smooth(method="lm", se=TRUE)
p + labs(colour = "Presen�a") + labs(x = "Frequ�ncia em Porcentagem") + labs(y = "Escore Bruto")