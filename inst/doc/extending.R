### R code from vignette source 'extending.rnw'

###################################################
### code chunk number 1: extending.rnw:41-43
###################################################
options(show.signif.stars=FALSE, prompt="R> ", continue="   ")
set.seed(271828)


###################################################
### code chunk number 2: extending.rnw:54-55 (eval = FALSE)
###################################################
## help("extending-emmeans", package="emmeans")


###################################################
### code chunk number 3: extending.rnw:63-67
###################################################
fake = expand.grid(rep = 1:5, A = c("a1","a2"), B = c("b1","b2","b3"))
fake$y = c(11.46,12.93,11.87,11.01,11.92,17.80,13.41,13.96,14.27,15.82,
           23.14,23.75,-2.09,28.43,23.01,24.11,25.51,24.11,23.95,30.37,
           17.75,18.28,17.82,18.52,16.33,20.58,20.55,20.77,21.21,20.10)


###################################################
### code chunk number 4: extending.rnw:73-78
###################################################
library(MASS)
fake.rlm = rlm(y ~ A * B, data = fake)

library(emmeans)
emmeans(fake.rlm, ~B | A)


###################################################
### code chunk number 5: extending.rnw:84-85
###################################################
fake.lts = ltsreg(y ~ A * B, data = fake)


###################################################
### code chunk number 6: extending.rnw:90-91
###################################################
emmeans:::recover_data.lm


###################################################
### code chunk number 7: extending.rnw:94-95
###################################################
recover_data.lqs = emmeans:::recover_data.lm


###################################################
### code chunk number 8: extending.rnw:98-100
###################################################
rec.fake = recover_data(fake.lts)
head(rec.fake)


###################################################
### code chunk number 9: extending.rnw:114-115
###################################################
args(emmeans:::emm_basis.lm)


###################################################
### code chunk number 10: extending.rnw:122-123
###################################################
MASS:::predict.lqs


###################################################
### code chunk number 11: extending.rnw:127-138
###################################################
emm_basis.lqs = function(object, trms, xlev, grid, ...) {
    m = model.frame(trms, grid, na.action = na.pass, xlev = xlev)
    X = model.matrix(trms, m, contrasts.arg = object$contrasts)
    bhat = coef(object)
    Xmat = model.matrix(trms, data=object$model)
    V = rev(object$scale)[1]^2 * solve(t(Xmat) %*% Xmat)
    nbasis = matrix(NA)
    dfargs = list(df = nrow(Xmat) - ncol(Xmat))
    dffun = function(k, dfargs) dfargs$df
    list(X=X, bhat=bhat, nbasis=nbasis, V=V, dffun=dffun, dfargs=dfargs)
}


###################################################
### code chunk number 12: extending.rnw:142-143
###################################################
emmeans(fake.lts, ~ B | A)


###################################################
### code chunk number 13: extending.rnw:154-155 (eval = FALSE)
###################################################
## nbasis = estimability::nonest.basis(Xmat)


###################################################
### code chunk number 14: extending.rnw:176-179
###################################################
form = ~ data$x + data[[5]]
base::all.vars(form)
emmeans::.all.vars(form)


###################################################
### code chunk number 15: extending.rnw:186-187
###################################################
.get.offset(terms(~ speed + offset(.03*breaks)), head(warpbreaks))


###################################################
### code chunk number 16: extending.rnw:204-228
###################################################
recover_data.rsm = function(object, data, mode = c("asis", "coded", "decoded"), ...) {
    mode = match.arg(mode)
    cod = rsm::codings(object)
    fcall = object$call
    if(is.null(data))
        data = emmeans::recover_data(fcall, delete.response(terms(object)), object$na.action, ...)
    if (!is.null(cod) && (mode == "decoded")) {
        pred = cpred = attr(data, "predictors")
        trms = attr(data, "terms")
        data = rsm::decode.data(rsm::as.coded.data(data, formulas = cod))
        for (form in cod) {
            vn = all.vars(form)
            if (!is.na(idx <- grep(vn[1], pred))) {
                pred[idx] = vn[2]
                cpred = setdiff(cpred, vn[1])
            }
        }
        attr(data, "predictors") = pred
        new.trms = update(trms, reformulate(c("1", cpred)))   # excludes coded variables
        attr(new.trms, "orig") = trms       # save orig terms as an attribute
        attr(data, "terms") = new.trms
    }
    data
}


###################################################
### code chunk number 17: extending.rnw:240-264
###################################################
emm_basis.rsm = function(object, trms, xlev, grid, 
                         mode = c("asis", "coded", "decoded"), ...) {
    mode = match.arg(mode)
    cod = rsm::codings(object)
    if(!is.null(cod) && mode == "decoded") {
        grid = rsm::coded.data(grid, formulas = cod)
        trms = attr(trms, "orig")   # get back the original terms we saved
    }
    
    m = model.frame(trms, grid, na.action = na.pass, xlev = xlev)
    X = model.matrix(trms, m, contrasts.arg = object$contrasts)
    bhat = as.numeric(object$coefficients) 
    V = emmeans::.my.vcov(object, ...)
    
    if (sum(is.na(bhat)) > 0)
        nbasis = estimability::nonest.basis(object$qr)
    else
        nbasis = estimability::all.estble
    dfargs = list(df = object$df.residual)
    dffun = function(k, dfargs) dfargs$df

    list(X = X, bhat = bhat, nbasis = nbasis, V = V, 
         dffun = dffun, dfargs = dfargs, misc = list())
}


###################################################
### code chunk number 18: extending.rnw:273-279 (eval = FALSE)
###################################################
## if (requireNamespace("emmeans", quietly = TRUE)) {
##     importFrom("emmeans", "recover_data", "emm_basis")
##     importFrom("estimability", "all.estble", "nonest.basis")
##     S3method(recover_data, rsm)
##     S3method(emm_basis, rsm)
## }


###################################################
### code chunk number 19: extending.rnw:289-291
###################################################
library("rsm")
example("rsm")   ### (output is not shown) ###


###################################################
### code chunk number 20: extending.rnw:295-297
###################################################
emmeans(CR.rs2, ~ x1 * x2, mode = "coded", 
        at = list(x1 = c(-1, 0, 1), x2 = c(-2, 2)))


###################################################
### code chunk number 21: extending.rnw:300-301
###################################################
codings(CR.rs1)


###################################################
### code chunk number 22: extending.rnw:304-306
###################################################
emmeans(CR.rs2, ~ Time * Temp, mode = "decoded", 
        at = list(Time = c(80, 85, 90), Temp = c(165, 185)))


