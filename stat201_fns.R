ht_prop <- function(n, B, p0, seed = 1){
  ret <- rep(NA , B)
  for(b in 1:B){
    samp <- sample(c(1, 0), n, replace = T, prob = c(p0, 1-p0))
    ret[b] <- mean(samp == 1)
  }
  ret
}

ht_independence <- function(n1, n2, n3, n4, B, seed = 1){
  # n1: number belonging to group 1 of variable 1
  # n2: number belonging to group 2 of variable 1
  # n3: number belonging to group 1 of variable 2
  ret <- rep(NA , B)
  cards <- c(rep(1, n1), rep(0, n2))
  for(b in 1:B){
    samp <- sample(cards)
    x_n3 <- samp[1:n3]
    x_n4 <- samp[-c(1:n3)]
    ret[b] <- mean(x_n3 == 1) - mean(x_n4 == 1)
  }
  # return different in proportions (group 1 - group 2) for variable 2
  ret
}

bootstrap_prop <- function(n, B, x, seed = 1){
  ret <- rep(NA, B)
  for(b in 1:B){
    xsamp <- sample(x, n, replace = T)
    ret[b] <- mean(xsamp == 1)
  }
  ret
}

bootstrap_mean <- function(n, B, x, seed = 1){
  ret <- rep(NA, B)
  for(b in 1:B){
    xsamp <- sample(x, n, replace = T)
    ret[b] <- mean(xsamp)
  }
  ret
}

bootstap_diff_props <- function(n1, n2, x1, x2, B, seed = 1){
  ret <- rep(NA, B)
  for(b in 1:B){
    x1samp <- sample(x1, n1, replace = T)
    x2samp <- sample(x2, n2, replace = T)
    ret[b] <- mean(x1samp == 1) - mean(x2samp == 1)
  }
  ret
}





