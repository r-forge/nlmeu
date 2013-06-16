melt.data.frame.Rtraced <-
function (data, id.vars, measure.vars, variable_name = "variable", 
    na.rm = !preserve.na, preserve.na = TRUE, ...) 
{
   .functionLabel <- "mdf"                     # Function label (recommended)
   .traceRinit <- attr(options()$traceR, "init")
   .traceR <-  if (is.null(.traceRinit)) function(...){} else .traceRinit      

    .traceR(1, lbl="-> START")                 # <=== .traceR() 
    if (!missing(preserve.na)) 
        message("Use of preserve.na is now deprecated, please use na.rm instead")
    var <- reshape::melt_check(data, id.vars, measure.vars)
    if (length(var$measure) == 0) {
        .traceR(10, "if10", store = FALSE)     # <===
        return(data[, var$id, drop = FALSE])
    }
    ids <- data[, var$id, drop = FALSE]
    .traceR(2, lbl = "ids")                    # <===
    df <- do.call("rbind", lapply(var$measure, function(x) {
        .traceR(21, lbl = paste("x_", x, sep = ""))  # <===
        data.frame(ids, x, data[, x])
    }))
    names(df) <- c(names(ids), variable_name, "value")
    .traceR(5)                                 # <===
    df[[variable_name]] <- factor(df[[variable_name]], unique(df[[variable_name]]))
    if (na.rm) {
        df <- df[!is.na(df$value), , drop = FALSE]
    }
    .traceR(6, lbl = "END<-")                     # <===
    rownames(df) <- NULL
    df
}

library(traceR)

library(SOAR)
library(reshape)
traceRsetup()
head(melt.data.frame.Rtraced(tips))             # Function melt.data.frame is traced
.traceRmap                                      # data frame with summary of traceR execution
names(r_1)                                      # named list r_1 in .R_Cache contains 6 objects 
names(r_2)
r_2$var
r_3
names(r_6)
r_6$variable_name
head(r_6$df)
traceRcleanup()

