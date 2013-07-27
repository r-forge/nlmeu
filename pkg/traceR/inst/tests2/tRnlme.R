           
library(traceR)

library(nlme)
## Changes in selected exported functions of "nlme" namespace will be made
# NmsE <- getNamespaceExports("nlme")  # 107
## Changes in functions of "nlme" namespace will be made
oNms <- ls(getNamespace("nlme"))       # 579
traceRedit(oNms, "nlme")
detach(package:nlme)

library(nlme)
head(lme.formula, n = 15)

traceR.on()
fm1 <- lme(distance ~ age, data = Orthodont) # random is ~ age
head(.traceRmap)
# traceR.optim()
dim(.traceRmap)
traceR.off()

unloadNamespace("nlme")
detach(package:traceR)
detach(package:SOAR)
sessionInfo()

