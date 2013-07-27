
library(traceR)

library(lme4)

nms <- ls(loadNamespace("lme4"))

nms.rm  <- c("plot.coef.mer", "plot.ranef.mer", "qqmath.ranef.mer", "terms.mer")

mtch <- match(nms.rm, nms)
nms <- nmsAll[-mtch]

traceRedit(nms, ns = "lme4", verbose = TRUE)  # verbose = TRUE
detach(package:lme4)


library(lme4)
traceR.on()
fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
traceR.report("lmerStaticReport.html", staticTree = TRUE)
traceR.report("lmerCollapsableReport.html", staticTree = TRUE)
traceR.off()
Remove(list = Objects())

unloadNamespace("lme4")
detach(package:traceR)
detach(package:SOAR)
sessionInfo()
