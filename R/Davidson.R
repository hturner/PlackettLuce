## Algorithm as in Davidson 1970
Davidson <- function(i, j, r_ij, w_ij, w_ji, t_ij, maxit = 500){
    n <- max(c(i, j))
    R <- length(i)
    nt <- sum(t_ij)
    theta <- (2*nt)/(sum(w_ij + w_ji + t_ij) - nt)
    s <- numeric(n)
    for (obj in 1:n){
        s[obj] <- c(2*w_ij %*% (i == obj) + 2*w_ji %*% (j == obj) +
                        t_ij %*% (i == obj | j == obj))
    }
    gamma <- rep(1/n, n)
    for (k in 1:maxit){
        gamma2 <- numeric(n)
        resi <- r_ij*(2 + theta*sqrt(gamma[j]/gamma[i]))/(
            gamma[i] + gamma[j] + theta*sqrt(gamma[i]*gamma[j]))
        resj <- r_ij*(2 + theta*sqrt(gamma[i]/gamma[j]))/(
            gamma[i] + gamma[j] + theta*sqrt(gamma[i]*gamma[j]))
        for (obj in 1:n){
            gamma2[obj] <- s[obj]/(sum(resi[i == obj]) + sum(resj[j == obj]))
        }
        gamma2 <- gamma2/sum(gamma2)
        theta2 <- nt/sum(r_ij*sqrt(gamma2[i]*gamma2[j])/(
            gamma2[i] + gamma2[j] + theta*sqrt(gamma2[i]*gamma2[j])))
        if (isTRUE(all.equal(log(gamma), log(gamma2)))) break
        gamma <- gamma2
        theta <- theta2
    }
    structure(c(theta2, gamma2), iter = k)
}
