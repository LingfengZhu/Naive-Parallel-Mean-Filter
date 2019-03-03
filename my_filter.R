my_filter = function(X, k) { # X is the matrix and k is the size
  n = dim(X)[1]
  m = dim(X)[2]
  pad_matrix = matrix(0, n+2*k, m+2*k)
  pad_matrix[(k+1):(n+k), (k+1):(m+k)] = X
  
  output = matrix(NA, nrow = n, ncol = m)
  for (i in (k+1):(n+k)) {
    for (j in (k+1):(m+k)) {
      moving_matrix = pad_matrix[(i-k):(i+k), (j-k):(j+k)]
      output[(i-k),(j-k)] = sum(moving_matrix)/((1+2*k)^2)
    }
  }
  return(output)
}