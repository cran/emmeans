## ---- echo = FALSE, results = "hide", message = FALSE--------------------
require("emmeans")
knitr::opts_chunk$set(collapse = TRUE, fig.width = 4.5)
options(show.signif.starts = FALSE)

## ------------------------------------------------------------------------
qt(c(.9, .95, .975), df = Inf)
qnorm(c(.9, .95, .975))

## ----eval = FALSE--------------------------------------------------------
#  emmeans:::convert_workspace()

