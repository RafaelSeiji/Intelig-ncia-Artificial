require(nnet)
require(tseriesChaos)

f <- function(net) {
  return ( 1/(1+exp(-net)) )
}

df_dnet <- function(f_net) {
  return (f_net * (1 - f_net) )
}


mlp.architecture <- function(input.length = 13,
                             hidden.length = 10,
                             output.length = 5,
                             my.f = f,
                             my.df_dnet = df_dnet) {
  
  model = list()
  model$input.length = input.length
  model$hidden.length = hidden.length
  model$outputlength = output.length
  
  
  model$hidden = matrix(runif(min=-0.5, max=0.5, 
                              hidden.length*(input.length+1)), 
                                    nrow = hidden.length, ncol=input.length+1)
  
  model$output = matrix(runif(min=-0.5, max=0.5, 
                               output.length*(hidden.length+1)), 
                                    nrow = output.length, ncol= hidden.length+1)
  
  model$f = my.f
  model$df_dnet = my.df_dnet
  
  return (model)
}

mlp.forward <- function(model, Xp) {
  
  #Hidden layer
  net_h_p = model$hidden %*% c(Xp,1)
  f_net_h_p = model$f(net_h_p)
  
  #Output layer
  net_o_p = model$output %*% c(as.numeric(f_net_h_p),1)
  f_net_o_p = model$f(net_o_p)
  
  #Results
  ret = list()
  ret$net_h_p = net_h_p
  ret$net_o_p = net_o_p
  ret$f_net_h_p = f_net_h_p
  ret$f_net_o_p = f_net_o_p
  
  return(ret)
}

mlp.backpropagation <- function(model, dataset, eta=0.1, threshold=0.45) {
  
  sqerror = 2 * threshold
  counter = 0
  while (sqerror > threshold & counter < 500)  {
    sqerror = 0
    cat("counter: ", counter, "\n")
    
    # Treinando com cada exemplo de meu conjunto X dadas as classes em Y
    for (p in 1:nrow(dataset)) {
      Xp = as.numeric(dataset[p, 1:model$input.length])
      Yp = as.numeric(dataset[p, (model$input.length+1):ncol(dataset)])
      results = mlp.forward(model, Xp)
      Op = results$f_net_o_p
      error = Yp - Op
      # Calculando erro quadrático
      sqerror = sqerror + sum(error^2)
      
      # Calculando delta da camada de saída para um padrão
      delta_o_p = error * model$df_dnet(results$f_net_o_p)

    
      
      # Calculando delta da camada escondida para um padrão
      w_o_kj = model$output[,1:model$hidden.length]
      delta_h_p = as.numeric(model$df_dnet(results$f_net_h_p)) *
                        (as.numeric(delta_o_p) %*% w_o_kj)
      
      # Atualizando a camada de saída
      model$output = model$output + 
        eta*(delta_o_p%*%as.vector(c(results$f_net_h_p,1)))
      
      # Atualizando a camada escondida
      model$hidden = model$hidden +
        eta*(t(delta_h_p)%*%as.vector(c(Xp, 1)))
      
    }
    
    sqerror = sqerror / nrow(dataset)
    cat("Average squared error: ", sqerror, "\n")
    counter = counter +1
  }
  ret = list()
  ret$model = model
  ret$counter = counter
  
  return (ret)
}

