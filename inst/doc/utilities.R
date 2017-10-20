## ---- echo = FALSE, results = "hide", message = FALSE--------------------
require("emmeans") 
knitr::opts_chunk$set(collapse = TRUE,
fig.width = 4.5) 

## ------------------------------------------------------------------------
pigs.lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
pigs.emmGrid.s <- emmeans(pigs.lm, "source")
pigs.emmGrid.s

## ------------------------------------------------------------------------
pigs.emmGrid.s <- update(pigs.emmGrid.s, infer = c(TRUE, TRUE), null = log(35))
pigs.emmGrid.s

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
rbind(pairs(pigs.emmGrid.s), pigs.anal.p[[2]])

## ------------------------------------------------------------------------
update(pigs.anal.p[[2]] + pairs(pigs.emmGrid.s), adjust = "mvt")

## ------------------------------------------------------------------------
pigs.emmGrid.s[2:3]

## ------------------------------------------------------------------------
pigs.emmGrid.ss <- add_grouping(pigs.emmGrid.s, "type", "source",
                            c("animal", "vegetable", "animal"))
str(pigs.emmGrid.ss)

## ------------------------------------------------------------------------
emmeans(pigs.emmGrid.ss, pairwise ~ type)

