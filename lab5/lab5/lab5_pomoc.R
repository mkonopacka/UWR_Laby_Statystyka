M <- 50

# zadanie 1 -----------------------------------------------
T_phi <- function(phi, x, y){
  z <- c(x, y)
  m <- length(x)
  n <- length(y)
  N <- n+m
  s <- sqrt(m*n/N)
  m1 <- mean(phi((rank(z)[1:m] - 0.5)/N))
  m2 <- mean(phi((rank(z)[(m+1):N] - 0.5)/N))
  s * (m1 - m2)
}

phi1 <- function(u){
  sqrt(3)*(2*u - 1)
}
phi2 <- function(u){
  sqrt(48)*(0.25 - abs(u - 0.5))
}
wilc <- function(x, y){
  T_phi(phi1, x, y)^2}
ab <- function(x, y){
  T_phi(phi2, x, y)^2
}
lep <- function(x, y){
  wilc(x, y) + ab(x, y)
}
ks <- function(x, y){
  z <- c(x, y)
  m <- length(x)
  n <- length(y)
  N <- n+m
  s <- sqrt(m*n/N)
  s*ks.test(x, y)$statistic
}

A <- matrix(0, 10000, 4)
set.seed(1)
for (i in 1:10000){
  x <- rnorm(M, 0, 1)
  y <- rnorm(M, 0, 1)
  A[i, 1] <- wilc(x, y)
  A[i, 2] <- ab(x, y)
  A[i, 3] <- lep(x, y)
  A[i, 4] <- ks(x, y)
}

wilc_test <- quantile(A[, 1], 0.95)
ab_test <- quantile(A[, 2], 0.95)
lep_test <- quantile(A[,3], 0.95)
ks_test <- quantile(A[,4], 0.95)

c(wilc_test, qchisq(0.95, 1))
c(ab_test, qchisq(0.95, 1))
c(lep_test, qchisq(0.95, 2))
c(ks_test, qKS())

# zadanie 2 ---------------------------

zad2 <- function(rozklad, mu2){
  set.seed(1)
  
  wilc_odrzucone <- 0
  ab_odrzucone <- 0
  lep_odrzucone <- 0
  ks_odrzucone <- 0
  
  for(i in 1:10000){
    x <- rozklad(M, 0, 1)
    y <- rozklad(M, mu2, 1)
    if (wilc(x, y) > wilc_test){wilc_odrzucone <- wilc_odrzucone + 1}
    if (ab(x, y) > ab_test){ab_odrzucone <- ab_odrzucone + 1}
    if (lep(x, y) > lep_test){lep_odrzucone <- lep_odrzucone + 1}
    if (ks(x, y) > ks_test){ks_odrzucone <- ks_odrzucone + 1}
  }
  c(wilc_odrzucone, ab_odrzucone, lep_odrzucone, ks_odrzucone)/10000
}

m_norm <- matrix(0, 7, 4)
for (k in 1:7){m_norm[k, ] <- zad2(rnorm, k/5)}
m_norm

x <- 1:7/5
plot(x, m_norm[,1], 'l', col = 'red', ylim = c(0, 1), main = "Wykres dla kolejnych wartości mi \n Rozkład normalny")
lines(x, m_norm[,2], 'l', col = 'blue')
lines(x, m_norm[,3], 'l', col = 'green')
lines(x, m_norm[,4], 'l', col = 'orange')
legend('right', c('Test Wilcoxona', "Test Asnari - Bradley'a", "Test Lepage'a", "Test Kolmogorova - Smirnova"), xpd=TRUE, col = par('red', 'blue', 'green', 'orange'))

m_logis <-matrix(0, 7, 4)
for (k in 1:7){m_logis[k, ] <- zad2(rlogis, k/5)}
m_logis

x <- 1:7/5
plot(x, m_logis[,1], 'l', col = 'red', ylim = c(0,1), main = "Wykres dla kolejnych wartości mi \n Rozkład logistyczny")
lines(x, m_logis[,2], 'l', col = 'blue')
lines(x, m_logis[,3], 'l', col = 'green')
lines(x, m_logis[,4], 'l', col = 'orange')
legend('topleft', c('Test Wilcoxona', "Test Asnari - Bradley'a", "Test Lepage'a", "Test Kolmogorova - Smirnova"), xpd=TRUE, col = par('red', 'blue', 'green', 'orange'))

m_cauchy <- matrix(0, 7, 4)
for (k in 1:7){m_cauchy[k, ] <- zad2(rcauchy, (k-1)/2)}
m_cauchy

x <- 0:6/2
plot(x, m_cauchy[,1], 'l', col = 'red', ylim = c(0, 1), main = "Wykres dla kolejnych wartości mi \n Rozkład Cauchy'ego")
lines(x, m_cauchy[,2], 'l', col = 'blue')
lines(x, m_cauchy[,3], 'l', col = 'green')
lines(x, m_cauchy[,4], 'l', col = 'orange')
legend('right', c('Test Wilcoxona', "Test Asnari - Bradley'a", "Test Lepage'a", "Test Kolmogorova - Smirnova"), xpd=TRUE, col = par('red', 'blue', 'green', 'orange'))

# zadanie 3

zad3 <- function(rozklad, s2){
  set.seed(1)
  
  wilc_odrzucone <- 0
  ab_odrzucone <- 0
  lep_odrzucone <- 0
  ks_odrzucone <- 0
  
  for(i in 1:10000){
    x <- rozklad(M, 0, 1)
    y <- rozklad(M, 0, s2)
    if (wilc(x, y) > wilc_test){wilc_odrzucone <- wilc_odrzucone + 1}
    if (ab(x, y) > ab_test){ab_odrzucone <- ab_odrzucone + 1}
    if (lep(x, y) > lep_test){lep_odrzucone <- lep_odrzucone + 1}
    if (ks(x, y) > ks_test){ks_odrzucone <- ks_odrzucone + 1}
  }
  c(wilc_odrzucone, ab_odrzucone, lep_odrzucone, ks_odrzucone)/10000
}
m_norm <- matrix(0, 7, 4)
for (i in 1:7){m_norm[i, ] <- zad3(rnorm, (i+1)/2)}
m_norm

x <- (2:8)/2
plot(x, m_norm[,1], 'l', col = 'red', ylim = c(0,1), main = "Wykres dla kolejnych wartości sigma \n Rozkład normalny")
lines(x, m_norm[,2], 'l', col = 'blue')
lines(x, m_norm[,3], 'l', col = 'green')
lines(x, m_norm[,4], 'l', col = 'orange')
legend(2.3, 0.3, c('Test Wilcoxona', "Test Asnari - Bradley'a", "Test Lepage'a", "Test Kolmogorova - Smirnova"), xpd=TRUE, col = par('red', 'blue', 'green', 'orange'))

m_logis <-matrix(0, 7, 4)
for (k in 1:7){m_logis[k, ] <- zad3(rlogis, (k+1)/2)}
m_logis

x <- (2:8)/2
plot(x, m_logis[,1], 'l', col = 'red', ylim = c(0,1), main = "Wykres dla kolejnych wartości sigma \n Rozkład logistyczny")
lines(x, m_logis[,2], 'l', col = 'blue')
lines(x, m_logis[,3], 'l', col = 'green')
lines(x, m_logis[,4], 'l', col = 'orange')
legend(2.3, 0.3, c('Test Wilcoxona', "Test Asnari - Bradley'a", "Test Lepage'a", "Test Kolmogorova - Smirnova"), xpd=TRUE, col = par('red', 'blue', 'green', 'orange'))

m_cauchy <- matrix(0, 7, 4)
for (k in 1:7){m_cauchy[k, ] <- zad3(rcauchy, k)}
m_cauchy

x <- 1:7
plot(x, m_cauchy[,1], 'l', col = 'red', ylim = c(0,1), main = "Wykres dla kolejnych wartości sigma \n Rozkład Cauchy'ego")
lines(x, m_cauchy[,2], 'l', col = 'blue')
lines(x, m_cauchy[,3], 'l', col = 'green')
lines(x, m_cauchy[,4], 'l', col = 'orange')
legend(2.3, 0.3, c('Test Wilcoxona', "Test Asnari - Bradley'a", "Test Lepage'a", "Test Kolmogorova - Smirnova"), xpd=TRUE, col = par('red', 'blue', 'green', 'orange'))

# zadanie 4

zad4 <- function(rozklad, mu2, s2){
  set.seed(1)
  
  wilc_odrzucone <- 0
  ab_odrzucone <- 0
  lep_odrzucone <- 0
  ks_odrzucone <- 0
  
  for(i in 1:10000){
    x <- rozklad(M, 0, 1)
    y <- rozklad(M, mu2, s2)
    if (wilc(x, y) > wilc_test){wilc_odrzucone <- wilc_odrzucone + 1}
    if (ab(x, y) > ab_test){ab_odrzucone <- ab_odrzucone + 1}
    if (lep(x, y) > lep_test){lep_odrzucone <- lep_odrzucone + 1}
    if (ks(x, y) > ks_test){ks_odrzucone <- ks_odrzucone + 1}
  }
  c(wilc_odrzucone, ab_odrzucone, lep_odrzucone, ks_odrzucone)/10000
}
m_norm <- matrix(0, 7, 4)
for (i in 1:7){m_norm[i, ] <- zad4(rnorm, i/5, (i+1)/2)}
m_norm

a <- 1:7
b <- (2:8)/2
x <- a + b
plot(x, m_norm[,1], 'l', col = 'red', ylim = c(0,1), main = "Wykres dla kolejnych wartości mi i sigma \n Rozkład normalny")
lines(x, m_norm[,2], 'l', col = 'blue')
lines(x, m_norm[,3], 'l', col = 'green')
lines(x, m_norm[,4], 'l', col = 'orange')
legend('bottomright', c('Test Wilcoxona', "Test Asnari - Bradley'a", "Test Lepage'a", "Test Kolmogorova - Smirnova"), xpd=TRUE, col = par('red', 'blue', 'green', 'orange'))

m_logis <- matrix(0, 7, 4)
for (i in 1:7){m_logis[i, ] <- zad4(rlogis, i/5, (i+1)/2)}
m_logis

a <- 1:7
b <- (2:8)/2
x <- a + b
plot(x, m_logis[,1], 'l', col = 'red', ylim = c(0,1), main = "Wykres dla kolejnych wartości mi i sigma \n Rozkład logistyczny")
lines(x, m_logis[,2], 'l', col = 'blue')
lines(x, m_logis[,3], 'l', col = 'green')
lines(x, m_logis[,4], 'l', col = 'orange')
legend('bottomright', c('Test Wilcoxona', "Test Asnari - Bradley'a", "Test Lepage'a", "Test Kolmogorova - Smirnova"), xpd=TRUE, col = par('red', 'blue', 'green', 'orange'))

m_cauchy <- matrix(0, 7, 4)
for (k in 1:7){m_cauchy[k, ] <- zad4(rcauchy, (k-1)/2, k)}
m_cauchy

a <- 0:6/2
b <- 1:7
x <- a + b
plot(x, m_cauchy[,1], 'l', col = 'red', ylim = c(0,1), main = "Wykres dla kolejnych wartości mi i sigma \n Rozkład Cauchy'ego")
lines(x, m_cauchy[,2], 'l', col = 'blue')
lines(x, m_cauchy[,3], 'l', col = 'green')
lines(x, m_cauchy[,4], 'l', col = 'orange')
legend('bottomright', c('Test Wilcoxona', "Test Asnari - Bradley'a", "Test Lepage'a", "Test Kolmogorova - Smirnova"), xpd=TRUE, col = par('red', 'blue', 'green', 'orange'))