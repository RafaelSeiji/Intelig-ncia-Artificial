#cria��o da assinatura da fun��o com um dataset, uma query a ser pesquisada e
#um valor k a ser utilizado
knn <- function(dataset, query, k){
  
  #Pesquisa de n�mero de colunas no dataset
  classId = ncol(dataset)
  
  #Ela faz um c�lculo de dist�ncia em cima das colunas sem
  #a �ltima(que no caso seria a de classifica��o), tamb�m convertendo
  #palavras/string para n�meros
  E = apply(dataset, 1, function(row){
    #Euclidiana
    sqrt(sum((query - as.numeric(row[1:(classId-1)]))^2))
    #Manhattan
    #sum(abs(query - as.numeric(row[1:(classId-1)])))
  })

  #Apos o calculo, temos uma ordena��o do resultado crescente (menor para
  #maior) retornando o numero da linha mais proxima da query buscada de
  #acordo com a distancia calculada
  ids = sort.list(E, dec=F)[1:k]
  
  #Com base nos ids filtrados, separamos as classes das mesmas
  classes = dataset[ids, classId]
  
  #Linha para separadar as classes unicas recebidas
  U = unique(classes)
  
  #Vetor de numeros de possibilidades do comprimento U
  R = rep(0, length(U))
  
  #Contagem de quantas vezes as classes apareceram no vetor U
  for (i in 1:length(U)){
    R[i] = sum(U[i] == classes)
  }
  
  #Retorna o id com a maior contagem feita no for
  id = which.max(R)
  
  ret = list()
  ret$U = U
  ret$R = R
  
  #Resposta da classe com base no id mais votado
  ret$class = as.character(U[id])
  
  return(ret)
}
