f <- function(net, epsilon=0.5) {
  if (net > epsilon)
    return (1)
  return (0)
}

perceptron.train <- function(dataset, max_it = 20,eta=0.1, threshold=1e-2) {
  
  classId = ncol(dataset)
  X = dataset[,1:(classId-1)]
  Y = dataset[,classId]
  # w1 w2 theta
  weights = runif(min=-0.5, max=0.5, n=ncol(X)+1)
  cat("Pesos iniciais= ",weights, "\n")
  squaredError = 2*threshold
  t=1
  while (squaredError > threshold & t<max_it) {
    cat("t= ", t, "\n")
    squaredError = 0
    for (i in 1:nrow(X)) {
      example = as.vector(ts(X[i,]))
      e = Y[i]
      
      # Saída do meu neurônio
      # example = c(0, 0)
      # c(0, 0, 1) %*% weights
      # c(0, 0, 1) %*% c(w1, w2, theta)
      # net = 0*w1 + 0*w2 + 1*theta
      y = f(c(example, 1) %*% weights)
      
      # Erro
      E = e - y
      squaredError = squaredError + E^2
      
      # training...
      # w_1(t+1) = w_1(t) -eta * dE^2/dw_1
      #	dE^2/dw_1 = 2 * (e - y) * -Xp1
      # w_2(t+1) = w_2(t) -eta * dE^2/dw_2
      #	dE^2/dw_2 = 2 * (e - y) * -Xp2
      # theta(t+1) = theta(t) -eta * dE^2/dtheta
      #	dE^2/dtheta = 2 * (e - y) * -1
      
      dE2dweights = 2 * E * -c(example, 1)
      weights = weights - eta * dE2dweights
      print(weights)
    }
    squaredError = squaredError / nrow(X)
    cat("Squared error = ", squaredError, "\n")
    t=t+1
  }
  cat("Pesos finais= ",weights, "\n")
  return (weights)
}

perceptron.run <- function(X, Y, weights) {
  
  cat("#expected\tobtained\n")
  for (i in 1:nrow(X)) {
    example = as.vector(ts(X[i,]))
    e = as.numeric(ts(Y[i]))
    
    # net
    net = c(example, 1) %*% weights
    y = f(net)
    
    cat(e, "\t", y, "\n")
  }
}

perceptron.test <- function(x, w) {
  
  net= c(x,1) %*% w
  return (f(net))
  
}


