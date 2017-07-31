library(psychotree)
library(sandwich)

data("Topmodel2007", package = "psychotree")
preference <- Topmodel2007$preference

# plfit vs btmodel
btmod <- btmodel(preference, ref = "Anja")

G <- as.grouped_rankings(preference)
plmod <- plfit(G, ref= "Anja", estfun = TRUE, object = TRUE)

## coef
all.equal(coef(btmod), coef(plmod)[-6])

## estfun - will only agree if last level taken as ref in btmodel
E1 <- estfun(btmod)
E2 <- plmod$estfun
all.equal(unname(E1), unname(E2))

## worth and vcov - vcov will only agree if first level taken as ref in btmodel
btmod <- btmodel(preference, ref = "Barbara")

w1 <- worth(btmod, log = FALSE, ref = 1)
w2 <- worth(plmod$object, log = FALSE, ref = 1)

all.equal(coef(w1), coef(w2))

all.equal(attr(w1, "vcov"), attr(w2, "vcov"), 1e-6)

# pltree vs brtree
bt_tree <- bttree(preference ~ ., data = Topmodel2007, minsize = 5, ref = "Anja")

tm_tree <- pltree(G ~ ., data = Topmodel2007[, -1], minsize = 15, ref = "Anja",
                  verbose = TRUE)

all.equal(itempar(bt_tree), itempar(tm_tree))
