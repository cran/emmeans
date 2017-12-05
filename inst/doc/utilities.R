## ---- echo = FALSE, results = "hide", message = FALSE--------------------
require("emmeans") 
knitr::opts_chunk$set(collapse = TRUE,
fig.width = 4.5) 

## ------------------------------------------------------------------------
pigs.lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
pigs.emm.s <- emmeans(pigs.lm, "source")
pigs.emm.s

## ------------------------------------------------------------------------
pigs.emm.s <- update(pigs.emm.s, infer = c(TRUE, TRUE), null = log(35))
pigs.emm.s

## ----eval = FALSE--------------------------------------------------------
#  emmeans(pigs.lm, "source", options = list(infer = c(TRUE, TRUE), null = log(35)))

## ------------------------------------------------------------------------
get_emm_option("emmeans")

## ------------------------------------------------------------------------
get_emm_option("contrast")

## ------------------------------------------------------------------------
get_emm_option("ref_grid")

## ------------------------------------------------------------------------
emm_options(emmeans = list(type = "response"),
            contrast = list(infer = c(TRUE, TRUE)))

## ------------------------------------------------------------------------
pigs.anal.p <- emmeans(pigs.lm, consec ~ percent)
pigs.anal.p

## ------------------------------------------------------------------------
options(emmeans = NULL)

## ------------------------------------------------------------------------
rbind(pairs(pigs.emm.s), pigs.anal.p[[2]])

## ------------------------------------------------------------------------
update(pigs.anal.p[[2]] + pairs(pigs.emm.s), adjust = "mvt")

## ------------------------------------------------------------------------
pigs.emm.s[2:3]

## ------------------------------------------------------------------------
pigs.emm.ss <- add_grouping(pigs.emm.s, "type", "source",
                            c("animal", "vegetable", "animal"))
str(pigs.emm.ss)

## ------------------------------------------------------------------------
emmeans(pigs.emm.ss, pairwise ~ type)

