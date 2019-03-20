# exported from https://github.com/shuxiaoc/agRank
# commit 6d0806c52a91a7f41c9cb768bc9e09e94ee88518
# modifications: make adherence optional
sgdPL <- function(data, mu, sigma, rate, adherence = TRUE, maxiter = 1000,
                 tol = 1e-9, start, decay = 1.1){

  #let m be the number of varieties,
  #let n be the number of farmers.
  #data is an n*m matrix,
  #data(i, j) represents the rank of variety i by farmer j
  #the entry where varieties are not included is 0

  #rate is the step size/learning rate


  #helper function
  #return the value of the function to be minimized
  #as well as the gradient w.r.t. index-th observation
  targetPL <- function(index, score, adherence, data, mu, sigma){
    #let m be the number of varieties,
    #let n be the number of farmers.
    #data is an n*m matrix,
    #data(i, j) represents the rank of variety i by farmer j
    #the entry where varieties are not included is 0

    #(mu, sigma) are parameters of the normal prior

    nobs <- nrow(data)
    nvar <- ncol(data)
    colnames(data) <- 1:nvar #assign labels to varieties

    #the first nvar element is the gradient for score
    #the last nobs element is the gradient for adherence
    gradient <- rep(0, nvar + nobs)
    #initialize
    inv_sigma <- solve(sigma)
    target_value <-
        as.numeric(0.5 * (t(score - mu) %*% inv_sigma %*% (score - mu)))
    gradient[1:nvar] <- 1 / nobs * inv_sigma %*% (score - mu)

    #loop over all observations
    for(i in 1:nobs){

      #calculate the ranking of the form A>B>C...
      ranks <- data[i, ][data[i, ] != 0]
      ranking <- as.numeric(names(sort(ranks)))

      #the length of i-th observation
      nrank <- length(ranks)


      #loop over all pairwise comparisons
      for(j in 1:(nrank - 1)){

        sum_temp <- 0
        sum_temp2 <- 0

        for(k in (j + 1):nrank){

          #ranking[j] wins over ranking[k]
          win <- ranking[j]
          lose <- ranking[k]

          sum_temp <- sum_temp +
              exp(-adherence[i] * (score[win] - score[lose]))
          sum_temp2 <- sum_temp2 +
              exp(-adherence[i] * (score[win] - score[lose])) *
              (-score[win] + score[lose])

        }

        #update the value of the target function
        target_value <- target_value + log(1 + sum_temp)

        if(index == i){
          #update the gradient w.r.t. score
          after_j <- ranking[(j + 1):nrank] #varieties ranked after variety j
          gradient[after_j] <- gradient[after_j] +
              (adherence[i] / (1 + sum_temp)) *
              exp(-adherence[i] * (score[win] - score[after_j]))
          gradient[win] <- gradient[win] -
              (adherence[i] / (1 + sum_temp)) * sum_temp

          #update the gradient w.r.t. adherence
          gradient[nvar + i] <- gradient[nvar + i] +
              (1 / (1 + sum_temp)) * sum_temp2

        }

      }


    }

    return(list(value = target_value, gradient = gradient))


  }






  nobs <- nrow(data)
  nvar <- ncol(data)
  colnames(data) <- 1:nvar #assign labels to varieties
  inv_sigma <- solve(sigma)

  #initialize
  niter <- 0
  #the first nvar element is the score
  #the last nobs element is the adherence
  if (adherence | length(start) == (nobs + nvar)){
    param <- start
  } else param <- c(start, rep.int(1, nobs))
  target <- rep(0, niter)

  flag <- TRUE
  #loop until the convergence criteria are met
  while(flag){

    for(i in 1:nobs){
      niter <- niter + 1
      score_temp <- param[1:nvar]
      adherence_temp <- param[(nvar + 1):(nvar + nobs)]
      #evaluate the log-posterior as well as the gradient
      #only used for small dataset (where we want to decide the learning rate)
      #if used for big dataset, where we don't want to
      #evaluate log-posterior everytime, the function should be modified
      res_temp <- targetPL(i, score_temp, adherence_temp, data, mu, sigma)

      #store the value of the target function
      target[niter] <- res_temp[[1]]

      #extract the gradient
      gradient <- res_temp[[2]]

      #update the parameters
      if (adherence){
        param <- param - rate * gradient
      } else param[1:nvar] <- param[1:nvar] - rate * gradient[1:nvar]

      #check the convergence criteria: square of the change of target values
      if(niter > 1){
        if((target[niter] - target[niter - 1]) ^ 2 < tol | niter > maxiter){
          flag <- FALSE
          break
        }

        #update learning rate if the target value don't decrease
        if((target[niter - 1] - target[niter]) / target[niter - 1] < 0){
          rate <- rate / decay
        }
      }


    }

  }

  return(list(value = target, niter = niter, score = param[1:nvar],
              adherence = param[(nvar + 1):(nvar + nobs)]))

}
