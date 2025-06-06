% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.R, R/test.R
\name{summary.emmGrid}
\alias{summary.emmGrid}
\alias{confint.emmGrid}
\alias{test}
\alias{test.emmGrid}
\alias{predict.emmGrid}
\alias{as.data.frame.emmGrid}
\alias{[.summary_emm}
\title{Summaries, predictions, intervals, and tests for \code{emmGrid} objects}
\usage{
\method{summary}{emmGrid}(object, infer, level, adjust, by,
  cross.adjust = "none", type, df, calc, null, delta, side, frequentist,
  bias.adjust = get_emm_option("back.bias.adj"), sigma, ...)

\method{confint}{emmGrid}(object, parm, level = 0.95, ...)

test(object, null, ...)

\method{test}{emmGrid}(object, null = 0, joint = FALSE, verbose = FALSE,
  rows, by, status = FALSE, ...)

\method{predict}{emmGrid}(object, type, interval = c("none", "confidence",
  "prediction"), level = 0.95,
  bias.adjust = get_emm_option("back.bias.adj"), sigma, ...)

\method{as.data.frame}{emmGrid}(x, row.names = NULL, optional,
  check.names = TRUE, destroy.annotations = FALSE, ...)

\method{[}{summary_emm}(x, ..., as.df = FALSE)
}
\arguments{
\item{object}{An object of class \code{"emmGrid"} (see \link{emmGrid-class})}

\item{infer}{A vector of one or two logical values. The first determines
whether confidence intervals are displayed, and the second determines
whether \emph{t} tests and \emph{P} values are displayed. If only one value
is provided, it is used for both.}

\item{level}{Numerical value between 0 and 1. Confidence level for confidence
intervals, if \code{infer[1]} is \code{TRUE}.}

\item{adjust}{Character value naming the method used to adjust \eqn{p} values
or confidence limits; or to adjust comparison arrows in \code{plot}. See
the P-value adjustments section below.}

\item{by}{Character name(s) of variables to use for grouping into separate 
tables. This affects the family of tests considered in adjusted \emph{P}
values.}

\item{cross.adjust}{Character: \eqn{p}-value adjustment method to 
additionally apply \emph{across} 
the \code{by} groups. See the section on P-value adjustments for details.}

\item{type}{Character: type of prediction desired. This only has an effect if
there is a known transformation or link function. \code{"response"} 
specifies that the inverse transformation be applied. \code{"mu"} (or 
equivalently, \code{"unlink"}) is usually the same as \code{"response"},
but in the case where the model has both a link function and a response 
transformation, only the link part is back-transformed. Other valid values 
are \code{"link"}, \code{"lp"}, and \code{"linear.predictor"}; these are
equivalent, and request that results be shown for the linear predictor,
with no back-transformation. The default is \code{"link"}, unless the 
\code{"predict.type"} option is in force; see \code{\link{emm_options}},
and also the section below on transformations and links.}

\item{df}{Numeric. If non-missing, a constant number of degrees of freedom to
use in constructing confidence intervals and \emph{P} values (\code{NA}
specifies asymptotic results).}

\item{calc}{Named list of character value(s) or formula(s).
The expressions in \code{char} are evaluated and appended to the
summary, just after the \code{df} column. The expression may include
any names up through \code{df} in the summary, any additional names in 
\code{object@grid} (such as \code{.wgt.} or \code{.offset.}), or any
earlier elements of \code{calc}.}

\item{null}{Numeric. Null hypothesis value(s), on the linear-predictor scale,
against which estimates are tested. May be a single value used for all, or
a numeric vector of length equal to the number of tests in each family
(i.e., \code{by} group in the displayed table).}

\item{delta}{Numeric value (on the linear-predictor scale). If zero, ordinary
tests of significance are performed. If positive, this specifies a
threshold for testing equivalence (using the TOST or two-one-sided-test
method), non-inferiority, or non-superiority, depending on \code{side}. See
Details for how the test statistics are defined.}

\item{side}{Numeric or character value specifying whether the test is
left-tailed (\code{-1}, \code{"-"}, \code{"<"}, \code{"left"}, or
\code{"nonsuperiority"}); right-tailed (\code{1}, \code{"+"}, \code{">"},
\code{"right"}, or \code{"noninferiority"}); or two-sided (\code{0},
\code{2}, \code{"!="}, \code{"two-sided"}, \code{"both"},
\code{"equivalence"}, or \code{"="}). See the special section below for
more details.}

\item{frequentist}{Ignored except if a Bayesian model was fitted. If missing
or \code{FALSE}, the object is passed to \code{\link{hpd.summary}}. Otherwise, 
a logical value of \code{TRUE} will have it return a frequentist summary.}

\item{bias.adjust}{Logical value for whether to adjust for bias in
back-transforming (\code{type = "response"}). This requires a valid value of 
\code{sigma} to exist in the object or be specified.}

\item{sigma}{Error SD assumed for bias correction (when 
\code{type = "response"} and a transformation
is in effect), or for constructing prediction intervals. If not specified,
\code{object@misc$sigma} is used, and a warning is issued if it is not found
or not valid.
\emph{Note:} \code{sigma} may be a vector, but be careful that it correctly
corresponds (perhaps after recycling) to the order of the reference grid.}

\item{...}{Optional arguments such as \code{scheffe.rank} 
(see \dQuote{P-value adjustments}). 
In \code{confint.emmGrid}, 
\code{predict.emmGrid}, and 
\code{test.emmGrid}, these arguments are passed to
\code{summary.emmGrid}.}

\item{parm}{(Required argument for \code{confint} methods, but not used)}

\item{joint}{Logical value. If \code{FALSE}, the arguments are passed to 
\code{\link{summary.emmGrid}} with \code{infer=c(FALSE, TRUE)}. If \code{joint = 
TRUE}, a joint test of the hypothesis L beta = null is performed, where L 
is \code{linfct(object)} and beta is the vector of fixed effects estimated 
by \code{object@betahat}. This will be either an \emph{F} test or a 
chi-square (Wald) test depending on whether degrees of freedom are 
available. See also \code{\link{joint_tests}}.}

\item{verbose}{Logical value. If \code{TRUE} and \code{joint = TRUE}, a table
of the effects being tested is printed.}

\item{rows}{Integer values. The rows of L to be tested in the joint test. If
missing, all rows of L are used. If not missing, \code{by} variables are
ignored.}

\item{status}{logical. If \code{TRUE}, a \code{note} column showing status
flags (for rank deficiencies and estimability issues) is displayed even 
when empty. If \code{FALSE}, the column is included only if there are 
such issues.}

\item{interval}{Type of interval desired (partial matching is allowed): 
\code{"none"} for no intervals,
  otherwise confidence or prediction intervals with given arguments, 
  via \code{\link{confint.emmGrid}}. 
  Note: prediction intervals are not available
  unless the model family is \code{"gaussian"}.}

\item{x}{object of the given class}

\item{row.names}{passed to \code{\link{as.data.frame}}}

\item{optional}{required argument, but ignored in \code{as.data.frame.emmGrid}}

\item{check.names}{passed to \code{\link{data.frame}}}

\item{destroy.annotations}{Logical value. If \code{FALSE}, an object of class
\code{summary_emm} is returned (which inherits from \code{data.frame}),
but if displayed, details like confidence levels, P-value adjustments, 
transformations, etc. are also shown. But unlike the result
of \code{summary}, the number of digits displayed
is obtained from \code{getOption("digits")} rather than using the
optimal digits algorithm we usually use. Thus, it is formatted more like a 
regular data frame, but with any annotations and groupings still intact.
If \code{TRUE} (not recommended), a \dQuote{plain vanilla} data frame is 
returned, based on \code{row.names} and \code{check.names}.}

\item{as.df}{Logical value. With \code{x[..., as.df = TRUE]}, the result is
object is coerced to a \code{\link{data.frame}} before the subscripting 
is applied. With \code{as.df = FALSE}, the result is
returned as a \code{summary_emm} object when possible.}
}
\value{
\code{summary.emmGrid}, \code{confint.emmGrid}, and
  \code{test.emmGrid} return an object of class \code{"summary_emm"}, which
  is an extension of \code{\link{data.frame}} but with a special \code{print}
  method that displays it with custom formatting. For models fitted using
  MCMC methods, the call is diverted to \code{\link{hpd.summary}} (with 
  \code{prob} set to \code{level}, if specified); one may
  alternatively use general MCMC summarization tools with the 
  results of \code{as.mcmc}.

\code{predict} returns a vector of predictions for each row of \code{object@grid}.

The \code{as.data.frame} method returns an object that inherits 
  from \code{"data.frame"}.
}
\description{
These are the primary methods for obtaining numerical or tabular results from
an \code{emmGrid} object. \code{summary.emmGrid} is the general function for
summarizing \code{emmGrid} objects. It also serves as the print method for
these objects; so for convenience, \code{summary()} arguments may be included
in calls to functions such as \code{\link{emmeans}} and
\code{\link{contrast}} that construct \code{emmGrid} objects. Note that by
default, summaries for Bayesian models are diverted to
\code{\link{hpd.summary}}.
}
\details{
\code{confint.emmGrid} is equivalent to \code{summary.emmGrid with 
infer = c(TRUE, FALSE)}. The function \code{test.emmGrid}, when called with 
\code{joint = FALSE}, is equivalent to \code{summary.emmGrid} with \code{infer = c(FALSE, TRUE)}. 

With \code{joint = TRUE}, \code{test.emmGrid} calculates the Wald test of the
hypothesis \code{linfct \%*\% bhat = null}, where \code{linfct} and
\code{bhat} refer to slots in \code{object} (possibly subsetted according to
\code{by} or \code{rows}). An error is thrown if any row of \code{linfct} is
non-estimable. It is permissible for the rows of \code{linfct} to be linearly
dependent, as long as \code{null == 0}, in which case a reduced set of 
contrasts is tested. Linear dependence and nonzero \code{null} cause an 
error. The returned object has an additional \code{"est.fcns"} attribute, which
is a list of the linear functions associated with the joint test.
}
\note{
In doing testing and a transformation and/or link is in force, any
  \code{null} and/or \code{delta} values specified must always be on the
  scale of the linear predictor, regardless of the setting for `type`. If
  \code{type = "response"}, the null value displayed in the summary table 
  will be back-transformed from the value supplied by the user. But the
  displayed \code{delta} will not be changed, because there (often) is
  not a natural way to back-transform it.

When we have \code{type = "response"}, and \code{bias.adj = TRUE},
  the \code{null} value displayed in the output is both back-transformed
  and bias-adjusted, leading to a rather non-intuitive-looking null value.
  However, since the tests themselves are performed on the link scale,
  this is the response value at which a *P* value of 1 would be obtained.

The default \code{show} method for \code{emmGrid} objects (with the
  exception of newly created reference grids) is \code{print(summary())}.
  Thus, with ordinary usage of \code{\link{emmeans}} and such, it is
  unnecessary to call \code{summary} unless there is a need to
  specify other than its default options.

If a data frame is needed, \code{summary}, \code{confint},
  and \code{test} serve this need. \code{as.data.frame} routes to
  \code{summary} by default; calling it with \code{destroy.annotations = TRUE}
  is not recommended for exactly that reason.
  If you want to see more digits in the output, use
  \code{print(summary(object), digits = ...)}; and if you \emph{always} want
  to see more digits, use \code{emm_options(opt.digits = FALSE)}.
}
\section{Defaults}{

  The \code{misc} slot in \code{object} may contain default values for
  \code{by}, \code{calc}, \code{infer}, \code{level}, \code{adjust}, 
  \code{type}, \code{null}, \code{side}, and \code{delta}. 
  These defaults vary depending
  on the code that created the object. The \code{\link{update}} method may be
  used to change these defaults. In addition, any options set using 
  \samp{emm_options(summary = ...)} will trump those stored in the object's 
  \code{misc} slot.
}

\section{Transformations and links}{

  With \code{type = "response"}, the transformation assumed can be found in
  \samp{object@misc$tran}, and its label, for the summary is in
  \samp{object@misc$inv.lbl}. Any \eqn{t} or \eqn{z} tests are still performed
  on the scale of the linear predictor, not the inverse-transformed one.
  Similarly, confidence intervals are computed on the linear-predictor scale,
  then inverse-transformed. 
  
  Be aware that only univariate transformations and links are
  supported in this way. Some multivariate transformations are supported by 
  \code{\link{mvregrid}}.
}

\section{Bias adjustment when back-transforming}{

  When \code{bias.adjust} is \code{TRUE}, then back-transformed estimates
  are adjusted by adding 
  \eqn{0.5 h''(u)\sigma^2}, where \eqn{h} is the inverse transformation and
  \eqn{u} is the linear predictor. This is based on a second-order Taylor
  expansion. There are better or exact adjustments for certain specific
  cases, and these may be incorporated in future updates.
  
  Note: In certain models, e.g., those with non-gaussian families,
  \code{sigma} is initialized as \code{NA}, and so by default, bias adjustment
  is skipped and a warning is issued. You may override this by specifying a
  value for \code{sigma}. However, \emph{with ordinary generalized linear models,
  bias adjustment is inappropriate} and you should not try to do it. With GEEs and GLMMs,
  you probably should \emph{not} use \code{sigma(model)}, and instead you should create an
  appropriate value using the estimated random effects, e.g., from \code{VarCorr(model)}.
  An example is provided in the \dQuote{transformations} vignette.
}

\section{P-value adjustments}{

  The \code{adjust} argument specifies a multiplicity adjustment for tests or
  confidence intervals. This adjustment always is applied \emph{separately}
  to each table or sub-table that you see in the printed output (see
  \code{\link{rbind.emmGrid}} for how to combine tables). If there are non-estimable
  cases in a \code{by} group, those cases are \emph{excluded} before determining
  the adjustment; that means there could be different adjustments in different groups.
  
  The valid values of \code{adjust} are as follows:
  \describe{
  \item{\code{"tukey"}}{Uses the Studentized range distribution with the number
    of means in the family. (Available for two-sided cases only.)}
  \item{\code{"scheffe"}}{Computes \eqn{p} values from the \eqn{F}
    distribution, according to the Scheffe critical value of
    \eqn{\sqrt{rF(\alpha; r, d)}}{sqrt[r*qf(alpha, r, d)]}, where \eqn{d} is
    the error degrees of freedom and \eqn{r} is the rank of the set of linear
    functions under consideration. By default, the value of \code{r} is
    computed from \code{linfct(object)} for each by group; however, if the
    user specifies an argument matching \code{scheffe.rank}, its value will
    be used instead. Ordinarily, if there are \eqn{k} means involved, then
    \eqn{r = k - 1} for a full set of contrasts involving all \eqn{k} means, and
    \eqn{r = k} for the means themselves. (The Scheffe adjustment is available
    for two-sided cases only.)}
  \item{\code{"sidak"}}{Makes adjustments as if the estimates were independent
    (a conservative adjustment in many cases).}
  \item{\code{"bonferroni"}}{Multiplies \eqn{p} values, or divides significance
    levels by the number of estimates. This is a conservative adjustment.}
  \item{\code{"dunnettx"}}{Uses our own\emph{ad hoc} approximation to the 
    Dunnett distribution for a family of estimates having pairwise
    correlations of \eqn{0.5} (as is true when comparing treatments with a
    control with equal sample sizes). The accuracy of the approximation
    improves with the number of simultaneous estimates, and is much faster
    than \code{"mvt"}. (Available for two-sided cases only.)}
  \item{\code{"mvt"}}{Uses the multivariate \eqn{t} distribution to assess the
    probability or critical value for the maximum of \eqn{k} estimates. This
    method produces the same \eqn{p} values and intervals as the default
    \code{summary} or \code{confint} methods to the results of
    \code{\link{as.glht}}. In the context of pairwise comparisons or comparisons
    with a control, this produces \dQuote{exact} Tukey or Dunnett adjustments,
    respectively. However, the algorithm (from the \pkg{mvtnorm} package) uses a
    Monte Carlo method, so results are not exactly repeatable unless the same
    random-number seed is used (see \code{\link[base:Random]{set.seed}}). As the family
    size increases, the required computation time will become noticeable or even
    intolerable, making the \code{"tukey"}, \code{"dunnettx"}, or others more
    attractive.}
  \item{\code{"none"}}{Makes no adjustments to the \eqn{p} values.}
  } %%%%%%%%%%%%%%%% end \describe {}

  For tests, not confidence intervals, the Bonferroni-inequality-based adjustment
  methods in \code{\link{p.adjust}} are also available (currently, these
  include \code{"holm"}, \code{"hochberg"}, \code{"hommel"},
  \code{"bonferroni"}, \code{"BH"}, \code{"BY"}, \code{"fdr"}, and
  \code{"none"}). If a \code{p.adjust.methods} method other than
  \code{"bonferroni"} or \code{"none"} is specified for confidence limits, the
  straight Bonferroni adjustment is used instead. Also, if an adjustment method
  is not appropriate (e.g., using \code{"tukey"} with one-sided tests, or with
  results that are not pairwise comparisons), a more appropriate method
  (usually \code{"sidak"}) is substituted.

  In some cases, confidence and \eqn{p}-value adjustments are only approximate
  -- especially when the degrees of freedom or standard errors vary greatly
  within the family of tests. The \code{"mvt"} method is always the correct
  one-step adjustment, but it can be very slow. One may use
  \code{\link{as.glht}} with methods in the \pkg{multcomp} package to obtain
  non-conservative multi-step adjustments to tests.
  
  \emph{Warning:} Non-estimable cases are \emph{included} in the family to which adjustments
  are applied. You may wish to subset the object using the \code{[]} operator
  to work around this problem.
  
  The \code{cross.adjust} argument is a way of specifying a multiplicity
  adjustment across the \code{by} groups (otherwise by default, each group is
  treated as a separate family in regards to multiplicity adjustments). It
  applies only to \eqn{p} values. Valid options are one of the
  \code{p.adjust.methods} or \code{"sidak"}. This argument is ignored unless
  it is other than \code{"none"}, there is more than one \code{by} group, and
  they are all the same size. Under those conditions, we first use
  \code{adjust} to determine the within-group adjusted \eqn{p} values.
  Imagine each group's adjusted \eqn{p} values arranged in side-by-side
  columns, thus forming a matrix with the number of columns equal to the
  number of \code{by} groups. Then we use the \code{cross.adjust} method to
  further adjust the adjusted \eqn{p} values in each row of this matrix. Note
  that an \emph{overall} Bonferroni (or Sidak) adjustment is obtainable by
  specifying \emph{both} \code{adjust} and \code{cross.adjust} as
  \code{"bonferroni"} (or \code{"sidak"}). However, less conservative (but
  yet conservative) overall adjustments are available when it is possible to
  use an \dQuote{exact} within-group method (e.g., \code{adjust = "tukey"}
  for pairwise comparisons) and \code{cross.adjust} as a conservative
  adjustment. [\code{cross.adjust} methods other than \code{"none"}, 
  \code{"bonferroni"}, or \code{"sidak"} do not seem advisable, but other 
  \code{p.adjust} methods are available if you can make sense of them.]
}

\section{Tests of significance, nonsuperiority, noninferiority, or equivalence}{

  When \code{delta = 0}, test statistics are the usual tests of significance.
  They are of the form 
  \samp{(estimate - null)/SE}. Notationally: 
  \describe{
    \item{Significance}{\eqn{H_0: \theta = \theta_0}  versus \cr
       \eqn{H_1: \theta < \theta_0} (left-sided), or\cr
      \eqn{H_1 \theta > \theta_0} (right-sided), or\cr
      \eqn{H_1: \theta \ne \theta_0} (two-sided)\cr
      The test statistic is\cr
      \eqn{t = (Q - \theta_0)/SE}\cr 
      where \eqn{Q} is our estimate of \eqn{\theta};
      then left, right, or two-sided \eqn{p} values are produced, 
      depending on \code{side}.}
  }
  When \code{delta} is positive, the test statistic depends on \code{side} as
  follows.
  \describe{
  \item{Left-sided (nonsuperiority)}{\eqn{H_0: \theta \ge \theta_0 + \delta}
    versus \eqn{H_1: \theta < \theta_0 + \delta}\cr 
    \eqn{t = (Q - \theta_0 - \delta)/SE}\cr 
    The \eqn{p} value is the lower-tail probability.}
  \item{Right-sided (noninferiority)}{\eqn{H_0: \theta \le \theta_0 - \delta}
    versus \eqn{H_1: \theta > \theta_0 - \delta}\cr 
    \eqn{t = (Q - \theta_0 + \delta)/SE}\cr
    The \eqn{p} value is the upper-tail probability.}
  \item{Two-sided (equivalence)}{\eqn{H_0: |\theta - \theta_0| \ge \delta}
    versus \eqn{H_1: |\theta - \theta_0| < \delta}\cr
    \eqn{t = (|Q - \theta_0| - \delta)/SE}\cr
    The \eqn{p} value is the \emph{lower}-tail probability.\cr
    Note that \eqn{t} is the maximum of \eqn{t_{nonsup}} and \eqn{-t_{noninf}}. 
    This is equivalent to choosing the less 
    significant result in the two-one-sided-test (TOST) procedure.}
  } %%%%%%%%%%%% end \describe{}
}

\section{Non-estimable cases}{

  When the model is rank-deficient, each row \code{x} of \code{object}'s
  \code{linfct} slot is checked for estimability. If \code{sum(x*bhat)}
  is found to be non-estimable, then the string \code{NonEst} is displayed for the
  estimate, and associated statistics are set to \code{NA}. 
  The estimability check is performed
  using the orthonormal basis \code{N} in the \code{nbasis} slot for the null
  space of the rows of the model matrix. Estimability fails when
  \eqn{||Nx||^2 / ||x||^2} exceeds \code{tol}, which by default is
  \code{1e-8}. You may change it via \code{\link{emm_options}} by setting
  \code{estble.tol} to the desired value.
  
  See the warning above that non-estimable cases are still included when
  determining the family size for \emph{P}-value adjustments.
}

\section{Warning about potential misuse of P values}{

  Some in the statistical and scientific community argue that
  the term \dQuote{statistical significance} should be completely abandoned, and
  that criteria such as \dQuote{p < 0.05} never be used to assess the
  importance of an effect. These practices can be too misleading and are prone to abuse.
  See \href{../doc/basics.html#pvalues}{the \dQuote{basics} vignette} for more
  discussion.
}

\examples{
warp.lm <- lm(breaks ~ wool * tension, data = warpbreaks)
warp.emm <- emmeans(warp.lm, ~ tension | wool)
warp.emm    # implicitly runs 'summary'

confint(warp.emm, by = NULL, level = .90)

# --------------------------------------------------------------
pigs.lm <- lm(log(conc) ~ source + factor(percent), data = pigs)
pigs.emm <- emmeans(pigs.lm, "percent", type = "response")
summary(pigs.emm)    # (inherits type = "response")
summary(pigs.emm, calc = c(n = ".wgt."))  # Show sample size

# For which percents is EMM non-inferior to 35, based on a 10\% threshold?
# Note the test is done on the log scale even though we have type = "response"
test(pigs.emm, null = log(35), delta = log(1.10), side = ">")

con <- contrast(pigs.emm, "consec")
test(con)

test(con, joint = TRUE)

# default Scheffe adjustment - rank = 3
summary(con, infer = c(TRUE, TRUE), adjust = "scheffe")

# Consider as some of many possible contrasts among the six cell means
summary(con, infer = c(TRUE, TRUE), adjust = "scheffe", scheffe.rank = 5)

# Show estimates to more digits
print(test(con), digits = 7)

# --------------------------------------------------------------
# Cross-adjusting P values
prs <- pairs(warp.emm)   # pairwise comparisons of tension, by wool
test(prs, adjust = "tukey", cross.adjust = "bonferroni")

# Same comparisons taken as one big family (more conservative)
test(prs, adjust = "bonferroni", by = NULL)

}
\seealso{
\code{\link{hpd.summary}}
}
