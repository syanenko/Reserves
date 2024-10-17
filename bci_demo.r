# 
# https://peter.solymos.org/vegan/reference/diversity.html
# 
# source("C:/Projects/__Garden/papers/biodiversity/reserves/bci_demo.r")
#
library ("vegan")

data(BCI, BCI.env)
## data(BCI)

cat("\n-Default\n")
H <- diversity(BCI)
print(H)

cat("\n-Simpson\n")
simp <- diversity(BCI, "simpson")
print(simp)

## Fisher alpha
cat("\n-Inv Simpson\n")
invsimp <- diversity(BCI, "inv")
print(invsimp)

## Unbiased Simpson
cat("\n-Unbiased Simpson\n")
unbias.simp <- simpson.unb(BCI)
print(unbias.simp)

## Fisher
cat("\n-Fisher alpha\n")
alpha <- fisher.alpha(BCI)
print(alpha)

## Plot all
pairs(cbind(H, simp, invsimp, unbias.simp, alpha), pch="+", col="blue")

## Species richness (S) and Pielou's evenness (J):
cat("\n-Species richness\n")
S <- specnumber(BCI) ## rowSums(BCI > 0) does the same...
print(S)

cat("\n-Pielou's evenness\n")
J <- H/log(S)
print(J)

## beta diversity defined as gamma/alpha - 1:
## alpha is the average no. of species in a group, and gamma is the
## total number of species in the group
(alpha <- with(BCI.env, tapply(specnumber(BCI), Habitat, mean)))
cat("\n-Alpha\n")
print(alpha)

(gamma <- with(BCI.env, specnumber(BCI, Habitat)))
cat("\n-Gamma\n")
print(gamma)

cat("\n-Beta\n")
print(gamma/alpha - 1)

## similar calculations with Shannon diversity
(alpha <- with(BCI.env, tapply(diversity(BCI), Habitat, mean))) # average
cat("\n-Alpha (Shannon)\n")
print(alpha)

(gamma <- with(BCI.env, diversity(BCI, groups=Habitat))) # pooled
cat("\n-Gamma (Shannon)\n")
print(gamma)

## additive beta diversity based on Shannon index
cat("\n-Beta (Shannon)\n")
print(gamma-alpha)
