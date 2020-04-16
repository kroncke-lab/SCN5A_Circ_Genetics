# Calculate the 95% credible interval for penetrance of LQT3 and BrS1

n <- 5 # total number of carriers (example)
brs <- 1  # total number of carriers with BrS1 (example)
lqt <- 3  # total number of carriers with LQT3 (example)
abrs0=0.32 # prior for BrS1
alqt0=0.11 # prior for LQT3
beta0=1 # sets the prior for unaffected

cred_lqt3 <- qbeta( c(.025, .975), lqt+alqt0, n-lqt+beta0)
cred_brs <- qbeta( c(.025, .975), brs+abrs0, n-brs+beta0)

100*(cred_lqt3)
100*(cred_brs)
