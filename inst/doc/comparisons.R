## ---- echo = FALSE, results = "hide", message = FALSE--------------------
require("emmeans")
knitr::opts_chunk$set(collapse = TRUE, fig.width = 4.5)

## ------------------------------------------------------------------------
pigs.lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
pigs.emmGrid.s <- emmeans(pigs.lm, "source")
pairs(pigs.emmGrid.s)

## ----fig.height = 1.5----------------------------------------------------
plot(pigs.emmGrid.s, comparisons = TRUE)

## ------------------------------------------------------------------------
cld(pigs.emmGrid.s)

## ------------------------------------------------------------------------
coef(pairs(pigs.emmGrid.s))

## ------------------------------------------------------------------------
pigs.emmGrid.p <- emmeans(pigs.lm, "percent")
ply <- contrast(pigs.emmGrid.p, "poly")
ply

coef(ply)

## ------------------------------------------------------------------------
org.aov <- aov(sales1 ~ day + Error(store), data = oranges,
               contrasts = list(day = "contr.sum"))
org.emml <- emmeans(org.aov, consec ~ day)
org.emml

## ------------------------------------------------------------------------
skip_comp.emmc <- function(levels, skip = 1, reverse = FALSE) {
    if((k <- length(levels)) < skip + 1)
        stop("Need at least ", skip + 1, " levels")
    coef <- data.frame()
    coef <- as.data.frame(lapply(seq_len(k - skip - 1), function(i) {
        sgn <- ifelse(reverse, -1, 1)
        sgn * c(rep(0, i - 1), 1, rep(0, skip), -1, rep(0, k - i - skip - 1))
    }))
    names(coef) <- sapply(coef, function(x)
        paste(which(x == 1), "-", which(x == -1)))
    attr(coef, "adjust") = "fdr"   # default adjustment method
    coef
}

## ------------------------------------------------------------------------
skip_comp.emmc(1:5)

skip_comp.emmc(1:5, skip = 0, reverse = TRUE)

## ------------------------------------------------------------------------
contrast(org.emml[[1]], "skip_comp", skip = 2, reverse = TRUE)

## ------------------------------------------------------------------------
LF <- contrast(pigs.emmGrid.s, 
               list(lambda1 = c(1, 2, 0), lambda2 = c(0, 3, -2)),
               offset = c(-7, 1))
confint(LF, adjust = "bonferroni")

## ------------------------------------------------------------------------
pairs(pigs.emmGrid.s, type = "lp")

pairs(pigs.emmGrid.s, type = "response")

