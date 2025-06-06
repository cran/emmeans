##############################################################################
#    Copyright (c) 2012-2019 Russell V. Lenth                                #
#                                                                            #
#    This file is part of the emmeans package for R (*emmeans*)              #
#                                                                            #
#    *emmeans* is free software: you can redistribute it and/or modify       #
#    it under the terms of the GNU General Public License as published by    #
#    the Free Software Foundation, either version 2 of the License, or       #
#    (at your option) any later version.                                     #
#                                                                            #
#    *emmeans* is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of          #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
#    GNU General Public License for more details.                            #
#                                                                            #
#    You should have received a copy of the GNU General Public License       #
#    along with R and *emmeans*.  If not, see                                #
#    <https://www.r-project.org/Licenses/> and/or                            #
#    <http://www.gnu.org/licenses/>.                                         #
##############################################################################

# contrast() and related functions (previously in with emmeans code)

### 'contrast' S3 generic and method
#' Contrasts and linear functions of EMMs
#' 
#' These methods provide for follow-up analyses of \code{emmGrid} objects:
#' Contrasts, pairwise comparisons, tests, and confidence intervals. They may
#' also be used to compute arbitrary linear functions of predictions or EMMs.
#'
#' @export
contrast = function(object, ...)
    UseMethod("contrast")

#' @rdname contrast
#' @param object An object of class \code{emmGrid}
#' @param method Character value giving the root name of a contrast method (e.g.
#'   \code{"pairwise"} -- see \link{emmc-functions}). Alternatively, a function
#'   of the same form, or a named \code{list} of coefficients (for a contrast or
#'   linear function) that must each conform to the number of results in each
#'   \code{by} group. In a multi-factor situation, the factor levels are
#'   combined and treated like a single factor.
#' @param interaction Character vector, logical value, or list. If this is specified,
#'   \code{method} is ignored. See the \dQuote{Interaction contrasts} section
#'   below for details.
#' @param by Character names of variable(s) to be used for ``by'' groups. The
#'   contrasts or joint tests will be evaluated separately for each combination
#'   of these variables. If \code{object} was created with by groups, those are
#'   used unless overridden. Use \code{by = NULL} to use no by groups at all.
#' @param offset,scale Numeric vectors of the same length as each \code{by} group.
#'   The \code{scale} values, if supplied, multiply their respective linear estimates, and
#'   any \code{offset} values are added. Scalar values are also allowed. 
#'   (These arguments are ignored when \code{interaction} is specified.)
#' @param name Character name to use to override the default label for contrasts
#'   used in table headings or subsequent contrasts of the returned object.
#' @param options If non-\code{NULL}, a named \code{list} of arguments to pass
#'   to \code{\link{update.emmGrid}}, just after the object is constructed.
#' @param type Character: prediction type (e.g., \code{"response"}) -- added to
#'   \code{options}
#' @param adjust Character: adjustment method (e.g., \code{"bonferroni"}) --
#'   added to \code{options}
#' @param simple Character vector or list: Specify the factor(s) \emph{not} in
#'   \code{by}, or a list thereof. See the section below on simple contrasts.
#' @param combine Logical value that determines what is returned when
#'   \code{simple} is a list. See the section on simple contrasts.
#' @param ratios Logical value determining how log and logit transforms are
#'   handled. These transformations are exceptional cases in that there is a
#'   valid way to back-transform contrasts: differences of logs are logs of
#'   ratios, and differences of logits are odds ratios. If \code{ratios = TRUE}
#'   and summarized with \code{type = "response"}, \code{contrast} results are
#'   back-transformed to ratios whenever we have true contrasts (coefficients
#'   sum to zero). For other transformations, there is no natural way to
#'   back-transform contrasts, so even when summarized with \code{type = "response"},
#'   contrasts are computed and displayed on the linear-predictor scale. Similarly, 
#'   if \code{ratios = FALSE}, log and logit transforms are treated in the same way as
#'   any other transformation.
#' @param parens character or \code{NULL}. If a character value, the labels for levels
#'   being contrasted are parenthesized if they match the regular expression in 
#'   \code{parens[1]} (via \code{\link{grep}}). The default is \code{emm_option("parens")}.
#'   Optionally, \code{parens} may contain second and third elements specifying
#'   what to use for left and right parentheses (default \code{"("} and \code{")"}).
#'   Specify \code{parens = NULL} or \code{parens = "a^"} (which won't match anything)
#'   to disable all parenthesization.
#' @param enhance.levels character or logical. 
#'   If character, the levels of the named factors that are contrasted are enhanced 
#'   by appending the name of the factor; e.g., if a factor named \code{"trt"} has
#'   levels \code{A} and \code{B}, a \code{trt} comparison is labeled \code{trtA - trtB}.
#'   If \code{enhance.levels} is logical, then if \code{TRUE} (the default), 
#'   only factors with numeric levels are enhanced; and of
#'   course if \code{FALSE}, no levels are enhanced.
#'   The levels of \code{by} variables are not enhanced, and any 
#'   names of factors that don't exist are silently ignored. 
#'   To enhance the labels beyond what is done here, change them
#'   directly via \code{\link[=update.emmGrid]{levels<-}}.
#' @param wts The \code{wts} argument for some contrast methods. You should omit
#'   this argument unless you want unequal \code{wts}. Otherwise we recommend
#'   specifying \code{wts = NA} which instructs that \code{wts} be obtained from
#'   \code{object}, \emph{separately} for each \code{by} group. If numerical
#'   \code{wts} are specified, they must
#'   conform to the number of levels in each \code{by} group, and those same weights
#'   are used in each group. 
#'
#' @param ... Additional arguments passed to other methods
#'
#' @return \code{contrast} and \code{pairs} return an object of class
#'   \code{emmGrid}. Its grid will correspond to the levels of the contrasts and
#'   any \code{by} variables. The exception is that an \code{\link{emm_list}}
#'   object is returned if \code{simple} is a list and \code{combine} is
#'   \code{FALSE}.
#'
#' @section Pairs method: The call \code{pairs(object)} is equivalent to
#'   \code{contrast(object, method = "pairwise")}; and \code{pairs(object,
#'   reverse = TRUE)} is the same as \code{contrast(object, method =
#'   "revpairwise")}.
#'
#' @section Interaction contrasts: When \code{interaction} is specified,
#'   interaction contrasts are computed. Specifically contrasts are generated
#'   for each factor separately, one at a time; and these contrasts are applied
#'   to the object (the first time around) or to the previous result
#'   (subsequently). (Any factors specified in \code{by} are skipped.) The final
#'   result comprises contrasts of contrasts, or, equivalently, products of
#'   contrasts for the factors involved. Any named elements of \code{interaction}
#'   are assigned to contrast methods; others are assigned in order of
#'   appearance in \code{object@levels}. The contrast factors in the resulting 
#'   \code{emmGrid} object are ordered the same as in \code{interaction}.
#'   
#'   \code{interaction} may be a character vector or list of valid contrast
#'   methods (as documented for the \code{method} argument). If the vector or
#'   list is shorter than the number needed, it is recycled. Alternatively, if
#'   the user specifies \code{contrast = TRUE}, the contrast specified in
#'   \code{method} is used for all factors involved.
#'   
#' @section Simple contrasts:
#'   \code{simple} is essentially the complement of \code{by}: When
#'   \code{simple} is a character vector, \code{by} is set to all the factors in
#'   the grid \emph{except} those in \code{simple}. If \code{simple} is a list,
#'   each element is used in turn as \code{simple}, and assembled in an
#'   \code{"emm_list"}. To generate \emph{all} simple main effects, use
#'   \code{simple = "each"} (this works unless there actually is a factor named
#'   \code{"each"}). Note that a non-missing \code{simple} will cause \code{by}
#'   to be ignored.
#'   
#'   Ordinarily, when \code{simple} is a list or \code{"each"}, the return value
#'   is an \code{\link{emm_list}} object with each entry in correspondence with
#'   the entries of \code{simple}. However, with \code{combine = TRUE}, the
#'   elements are all combined into one family of contrasts in a single
#'   \code{\link[=emmGrid-class]{emmGrid}} object using
#'   \code{\link{rbind.emmGrid}}.. In that case, the \code{adjust} argument sets
#'   the adjustment method for the combined set of contrasts.
#'
#' @note When \code{object} has a nesting structure (this can be seen via
#'   \code{str(object)}), then any grouping factors involved are forced into
#'   service as \code{by} variables, and the contrasts are thus computed
#'   separately in each nest. This in turn may lead to an irregular grid in the
#'   returned \code{emmGrid} object, which may not be valid for subsequent
#'   \code{emmeans} calls.
#'
#' @method contrast emmGrid
#' @export
#'
#' @examples
#' warp.lm <- lm(breaks ~ wool*tension, data = warpbreaks)
#' (warp.emm <- emmeans(warp.lm, ~ tension | wool))
#' 
#' contrast(warp.emm, "poly")    # inherits 'by = "wool"' from warp.emm
#' 
#' ### Custom contrast coefs (we already have wool as 'by' thus 3 means to contrast)
#' contrast(warp.emm, list(mid.vs.ends = c(-1,2,-1)/2, lo.vs.hi = c(1,0,-1)))
#' 
#' pairs(warp.emm)
#' 
#' # Effects (dev from mean) of the 6 factor combs, with enhanced levels:
#' contrast(warp.emm, "eff", by = NULL, 
#'     enhance.levels = c("wool", "tension"))  
#'     
#' pairs(warp.emm, simple = "wool") # same as pairs(warp.emm, by = "tension")
#' 
#' # Do all "simple" comparisons, combined into one family
#' pairs(warp.emm, simple = "each", combine = TRUE)
#' 
#' \dontrun{
#' 
#' ## Note that the following are NOT the same:
#' contrast(warp.emm, simple = c("wool", "tension"))
#' contrast(warp.emm, simple = list("wool", "tension"))
#' ## The first generates contrasts for combinations of wool and tension
#' ##   (same as by = NULL)
#' ## The second generates contrasts for wool by tension, and for 
#' ##   tension by wool, respectively.
#' }
#'
#' # An interaction contrast for tension:wool
#' tw.emm <- contrast(warp.emm, interaction = c(tension = "poly", wool = "consec"), 
#'                    by = NULL)
#' tw.emm          # see the estimates
#' coef(tw.emm)    # see the contrast coefficients
#' 
#' # Use of scale and offset
#' #   an unusual use of the famous stack-loss data...
#' mod <- lm(Water.Temp ~ poly(stack.loss, degree = 2), data = stackloss)
#' (emm <- emmeans(mod, "stack.loss", at = list(stack.loss = 10 * (1:4))))
#' # Convert results from Celsius to Fahrenheit:
#' confint(contrast(emm, "identity", scale = 9/5, offset = 32))
#' 
contrast.emmGrid = function(object, method = "eff", interaction = FALSE, 
                        by, offset = NULL, scale = NULL, name = "contrast", 
                        options = get_emm_option("contrast"), 
                        type, adjust, simple, combine = FALSE, ratios = TRUE, 
                        parens, enhance.levels = TRUE, wts, ...) 
{
    if (!missing(type))
        options = as.list(c(options, predict.type = type))
    
    if(!missing(simple))
        return(.simcon(object, method = method, interaction = interaction,
                      offset = offset, scale = scale, name = name, options = options,
                      type = type, simple = simple, combine = combine, 
                      adjust = adjust, parens = parens, ...))
    if(missing(by)) 
        by = object@misc$by.vars
    if(length(by) == 0) # character(0) --> NULL
        by = NULL
    
    nesting = object@model.info$nesting
    if (!is.null(nesting) || !is.null(object@misc$display))
        return (.nested_contrast(rgobj = object, method = method, interaction = interaction, by = by, adjust = adjust, type = type, offset = offset, ...))
    
    orig.grid = object@grid[, , drop = FALSE]
    orig.grid[[".wgt."]] = orig.grid[[".offset."]] = NULL
    
    if (is.logical(interaction) && interaction)
        interaction = method
    if (!is.logical(interaction)) { # i.e., interaction is not FALSE
        if(missing(adjust))
            adjust = "none"
        vars = names(object@levels)
        k = length(vars)
        if(!is.null(by)) {
            vars = c(setdiff(vars, by), by)
            k = k - length(by)
        }
        nms = names(interaction)
        interaction = as.list(rep(interaction, k)[1:k])
        names(interaction) = c(nms, rep("", k))[1:k]
        nms = vars[1:k]
        if (is.null(names(interaction)))
            names(interaction) = nms
        else {
            unnamed = which(!(names(interaction) %in% nms))
            names(interaction)[unnamed] = setdiff(nms, names(interaction))
            nms = names(interaction)
        }
        
        
        # if (!is.character(interaction))
        #     stop("interaction requires named contrast function(s)")
        ### by = NULL why was this here before ???
        tcm = NULL
        for (i in k:1) {
            if (is.character(interaction[[i]]))
                nm = paste(nms[i], interaction[[i]], sep = "_")
            else
                nm = paste(nms[i], "custom", sep = "_")
            pos = which(vars == nms[i])
            object = contrast.emmGrid(object, interaction[[i]], by = vars[-pos], 
                                      name = nm, enhance.levels = FALSE, ...)
            if(is.null(tcm))
                tcm = object@misc$con.coef
            else
                tcm = object@misc$con.coef %*% tcm
            vars[pos] = nm
        }
        object = update(object, by = by, adjust = adjust, silent = TRUE) 
          ### removed `...` here Nov 2019 because a `mode` arg gets matched with `model.info`
          ### when passed via formula lhs in `emmeans()`
        object@misc$is.new.rg = NULL
        object@misc$orig.grid = orig.grid
        object@misc$con.coef = tcm
        object = .update.options(object, options, ...)
        return(object)
    }
    
    # else we have a regular contrast (not interaction)
    linfct = object@linfct[, , drop = FALSE]
    args = g = object@grid[, , drop = FALSE]
    args[[".offset."]] = NULL 
    args[[".wgt."]] = NULL # ignore auxiliary stuff in labels, etc.
    
    # figure out any enhancement of factor levels
    if (is.logical(enhance.levels)) { # convert to character
        if(enhance.levels)
            enhance.levels = names(args[sapply(args, .is.num)])               
        else # FALSE
            enhance.levels = character(0)
    }
    enhance.levels = intersect(setdiff(enhance.levels, by), names(args))
    for (nm in enhance.levels)
        args[[nm]] = paste0(nm, args[[nm]])
    
    if (!is.null(by)) {
        by.rows = .find.by.rows(args, by)
        ulen = unique(sapply(by.rows, length))
        if (length(ulen) > 1)
            stop ("`by` groups are of irregular size;\n  currently not supported except with nested structures")
        bylevs = args[, by, drop=FALSE]
        all.args = args
        args = args[by.rows[[1]], , drop=FALSE]
        for (nm in by) {
            args[[nm]] = NULL
            all.args[[nm]] = NULL
        }
        all.levs = do.call("paste", c(unname(all.args), sep = get_emm_option("sep")))   # keep all levels in case we have permutations of them
    }
    args = unname(args)
    args$sep = get_emm_option("sep")
    levs = do.call("paste", args)  # NOTE - these are levels for the first (or only) by-group
    if (length(levs) == 0)   # prevent error when there are no levels to contrast
        method = "eff"
    if(is.null(by))
        all.levs = levs
    
    # parenthesize levels if they contain spaces or operators
    rawlevs = levs  # save orig ones
    if(missing(parens))
        parens = get_emm_option("parens")
    if(is.character(parens) && length(idx <- grep(parens[1], all.levs)) > 0) {
        if(length(parens) < 3)
            parens = c(parens, "(", ")")
        all.levs[idx] = paste0(parens[2], all.levs[idx], parens[3])
        idx = grep(parens[1], levs)
        if (length(idx) > 0)
            levs[idx] = paste0(parens[2], levs[idx], parens[3])
    }
    attr(levs, "raw") = rawlevs # so we can recover original levels when needed
    
    variable.cmat = FALSE
    if (is.list(method)) {
        cmat = as.data.frame(method, optional = TRUE)
        # I have no clue why they named that argument 'optional',
        # but setting it to TRUE keeps it from messing up the names
        method = function(levs, ...) cmat
    }
    else if (is.character(method)) {
        fn = paste(method, "emmc", sep=".")
        if (exists(fn, mode = "function")) 
            method = get(fn) 
        else if (exists(fn, mode = "function", envir = parent.frame()))
            method = get(fn, envir = parent.frame()) 
        else 
            stop(paste("Contrast function '", fn, "' not found", sep=""))
        if(!missing(wts) && any(is.na(wts))) {
            variable.cmat = TRUE
            all.wts = weights(object)
            wts = if(!is.null(by))   rep(1, length(levs))
                  else               all.wts
        }
        if(missing(wts))
            wts = rep(1, length(levs))
    }

    # case like in old lsmeans, contr = list
    else if (!is.function(method))
        stop("'method' must be a list, function, or the basename of an '.emmc' function")
    
    # Get the contrasts; this should be a data.frame
    cmat = method(levs, wts = wts, ...)
    if (!is.data.frame(cmat))
        stop("Contrast function must provide a data.frame")
    else if(ncol(cmat) == 0) {
        cmat = data.frame(`(nothing)` = rep(NA, nrow(args)), check.names = FALSE)
        adjust = "none"
    }
    # warning("No contrasts were generated! Perhaps only one emmean is involved.\n",
    #      "  This can happen, for example, when your predictors are not factors.")
    else if (nrow(cmat) != nrow(args))
        stop("Nonconforming number of contrast coefficients")
    tcmat = t(cmat)
    if (!is.null(scale)) {
        if (length(scale) %in% c(1, nrow(tcmat)))
            tcmat = tcmat * scale
        else
            stop("'scale' length of ", length(scale), 
                 " does not conform with ", nrow(tcmat), " contrasts", call. = FALSE)
    }
    else
        scale = 1
    
    # do some bookkeeping whereby we get NAs only when NA rows get nonzero weight
    NAflag = linfct[, 1] * 0
    NAflag[is.na(linfct[, 1])] = 1
    linfct[is.na(linfct)] = 0 # now we don't have any NAs but we know where to put them back later
    .NArows = function(mat, flag) # utility for flagging bad rows (any nonzero coef applied to an NA)
        apply(mat, 1, function(x, f) any(x * f != 0), flag)
    
    if (is.null(by)) {
        linfct = tcmat %*% linfct
        linfct[.NArows(tcmat, NAflag), ] = NA # put back the NAs where they belong
        grid = data.frame(.contrast.=names(cmat))
        if (hasName(object@grid, ".offset."))
            grid[[".offset."]] = t(cmat) %*% object@grid[[".offset."]]
        by.rows = list(seq_along(object@linfct[ , 1]))
    }
    
    # NOTE: The kronecker thing here depends on the grid being regular.
    # Irregular grids are handled by .nested_contrast
    else {
        tcmat = kronecker(.diag(rep(1,length(by.rows))), tcmat)
        if(variable.cmat) { # we have to replace each block
            rowidx = seq_len(ncol(cmat))  # note we're working with transpose
            colidx = seq_len(nrow(cmat))
            for(i in seq_along(by.rows)) {
                wts = all.wts[by.rows[[i]]]
                tcmat[rowidx, colidx] = t(method(levs, wts = wts, ...)) * scale
                rowidx = rowidx + ncol(cmat)
                colidx = colidx + nrow(cmat)
            }
        }
        linfct = tcmat %*% linfct[unlist(by.rows), , drop = FALSE]
        linfct[.NArows(tcmat, NAflag[unlist(by.rows)]), ] = NA
        tmp = expand.grid(con = names(cmat), by = seq_len(length(by.rows)), stringsAsFactors = FALSE)###unique(by.id))
        # check if levs have different orderings in subsequent by groups
        for (i in 1 + seq_along(by.rows[-1])) { 
            j = by.rows[[i]]
            if (any(all.levs[j] != levs)) {
                cm = method(all.levs[j], wts = wts, ...)
                tmp$con[seq_along(cm) + length(cm)*(i-1)] = names(cm)
            }
        }
        grid = data.frame(.contrast. = factor(tmp$con, levels = unique(tmp$con)))
        n.each = ncol(cmat)
        row.1st = sapply(by.rows, function(x) x[1])
        xlevs = list()
        for (v in by)
            xlevs[[v]] = rep(bylevs[row.1st, v], each=n.each)
        grid = cbind(grid, data.frame(xlevs, check.names = FALSE))
        if (hasName(object@grid, ".offset."))
            grid[[".offset."]] = tcmat %*% object@grid[unlist(by.rows), ".offset."]
    }
    
    # Rename the .contrast. column -- ordinarily to "contrast",
    # but otherwise a unique variation thereof
    con.pat = paste("^", name, "[0-p]?", sep = "")
    n.prev.con = length(grep(con.pat, names(grid)))
    con.col = grep("\\.contrast\\.", names(grid))
    con.name = paste(name, 
                     ifelse(n.prev.con == 0, "", n.prev.con), sep="")
    names(grid)[con.col] = con.name
    
    row.names(linfct) = NULL
    misc = object@misc
    misc$initMesg = NULL
    misc$estName = "estimate"
    if (!is.null(et <- attr(cmat, "type")))
        misc$estType = et
    else {
        is.con = all(abs(sapply(cmat, sum)) < .001)
        misc$estType = ifelse(is.con, "contrast", "prediction")
    }
    misc$methDesc = attr(cmat, "desc")
    misc$famSize = ifelse(is.null(fs <- attr(cmat, "famSize")), length(by.rows[[1]]), fs)
    misc$pri.vars = setdiff(names(grid), c(by, ".offset.",".wgt."))
    if (missing(adjust)) adjust = attr(cmat, "adjust")
    if (is.null(adjust)) adjust = "none"
    if (!is.null(attr(cmat, "offset")))
        offset = attr(cmat, "offset")
    if (!is.null(offset)) {
        if(!hasName(grid, ".offset."))
            grid[[".offset."]] = 0
        grid[[".offset."]] = grid[[".offset."]] + rep(offset, length(by.rows))
    }
    if (!is.null(fs <- attr(cmat, "famSize")))
        misc$famSize = fs
    misc$is.new.rg = FALSE
    misc$adjust = adjust
    misc$infer = c(FALSE, TRUE)
    misc$by.vars = by
    if(!is.na(misc$estType) && misc$estType == "pairs") # internal flag to keep track of original by vars for paired comps
        misc$.pairby = paste(c("", by), collapse = ",")
    # save contrast coefs
    by.cols = seq_len(ncol(tcmat))
    if(!is.null(by.rows))
        by.cols[unlist(by.rows)] = by.cols # gives us inverse of by.rows order
    misc$orig.grid = orig.grid  # save original grid
    misc$con.coef = tcmat[ , by.cols, drop = FALSE] # save contrast coefs
    # test that each set of coefs sums to 0
    true.con = all(abs(apply(cmat, 2, function(.) sum(.) / (1.0e-6 + max(abs(.))))) < 1.0e-6) 
    if(is.na(true.con)) # prevent error when there are no contrasts
        true.con = FALSE
    if(true.con)
        misc$sigma = NULL   # sigma surely no longer makes sense

    # zap the transformation info except in special cases
    if (!is.null(misc$tran)) {
        misc$orig.tran = .fmt.tran(misc)
        if (ratios && true.con && misc$orig.tran %in% c("log", "log2", "log10", "logit", "log.o.r.")) {
            misc$log.contrast = TRUE      # remember how we got here; used by summary
            misc$orig.inv.lbl = misc$inv.lbl
            if (misc$tran == "logit") {
                misc$inv.lbl = "odds.ratio"
                misc$tran = "log.o.r."
                misc$tran.mult = misc$tran.offset = NULL
            }
            else if (misc$tran == "log.o.r.") {
                # leave everything as-is. Once an odds ratio, always an odds ratio
            }
            else {
                misc$inv.lbl = "ratio"
                ### (stays at log, log2, log10)   misc$tran = "log"
                misc$tran.mult = misc$tran.offset = NULL
            }
        }
        else {
            misc$initMesg = c(misc$initMesg, 
                              paste("Note: contrasts are still on the", misc$orig.tran, "scale. Consider using\n",
                              "     regrid() if you want contrasts of back-transformed estimates."))
             misc$tran = misc$tran.mult = misc$tran.offset = NULL
        }
    }
    
    # ensure we don't inherit inappropriate settings
    misc$null = misc$delta = misc$side = misc$calc = NULL
    
    object@roles$predictors = "contrast"
    levels = list()
    for (nm in setdiff(names(grid), ".offset."))
        levels[[nm]] = unique(grid[[nm]])
    
    ### bypass new as we're not re-classing    result = new("emmGrid", object, linfct = linfct, levels = levels, grid = grid, misc = misc)
    result = as(object, "emmGrid")
    result@linfct = linfct
    result@levels = levels
    result@grid = grid
    result@misc = misc
    result@roles$predictors = setdiff(names(result@levels), result@roles$multresp)
    
    .update.options(result, options, ...)
}


# Add-on to support `simple` argument - the complement of `by`
# e.g., with factors A, B, C, simple = "A" <==> by = c("B", "C")
# Note `by` is an argument so that it can be ignored and never duplicated
# If `simple` is a list, we run this on each element
# If simple = "each", we run it on each factor in the grid
# We handle `adjust`` ourselves rather than passing it to `contrast``
.simcon = function(object, ..., simple, by, combine = FALSE, adjust) {
    if (is.list(simple)) {
        if(is.null(names(simple)))
            names(simple) = sapply(simple, function(.) 
                paste("simple contrasts for", paste0(., collapse = "*")))
        result = lapply(simple, function(.) 
            .simcon(object, ..., simple = .))
        class(result) = c("emm_list", "list")
        if(combine)
            result = do.call(rbind.emmGrid, result)
    }
    else if ((length(simple) == 1) && (simple == "each") && !("each" %in% names(object@levels))) {
        result = .simcon(object, ..., 
            simple = as.list(names(object@levels)), combine = combine)
    }
    else {
        facs = names(object@levels)
        by = setdiff(facs, simple)
        if (length(by) == 0) by = NULL
        result = contrast.emmGrid(object, ..., by = by)
    }
    if (!missing(adjust)) {
        if (is.list(result)) {
            for (i in seq_along(result))
                result[[i]] = update(result[[i]], adjust = adjust)
        }
        else
            result = update(result, adjust = adjust)
    }
    result
}


# pairs method

#' @rdname contrast 
#' @param x An \code{emmGrid} object
#' @param reverse Logical value - determines whether to use \code{"pairwise"} (if \code{TRUE}) or \code{"revpairwise"} (if \code{FALSE}).
#' @importFrom graphics pairs
#' @export
pairs.emmGrid = function(x, reverse = FALSE, ...) {
    object = x # for my sanity
    if (reverse)
        contrast(object, method = "revpairwise", ...)
    else
        contrast(object, method = "pairwise", ...)
}


# coef method - returns contrast coefficients
#' @rdname contrast 
#' @return \code{coef} returns a \code{data.frame} containing the "parent" object's grid, 
#' along with columns named \code{c.1, c.2, ...} containing the contrast coefficients
#' used to produce the linear functions embodied in the object. \code{coef()} only
#' returns coefficients if \code{object} is the result of a call to \code{contrast()},
#' and the parent object is the object that was handed to \code{contrast}. This
#' is most useful for understanding interaction contrasts.
#'
#' @export
#' @importFrom stats coef
#' @method coef emmGrid
coef.emmGrid = function(object, ...) {
    if (is.null(cc <- object@misc$con.coef)) {
        message("No contrast coefficients are available")
        return (NULL)
    }
    cc = as.data.frame(t(cc))
    rownames(cc) = NULL
    names(cc) = paste("c", seq_len(ncol(cc)), sep = ".")
    cbind(object@misc$orig.grid, cc)
}

#' @rdname contrast
#' @importFrom stats weights
#' @return \code{weights} returns the weights stored for each row of \code{object},
#'    or a vector of 1s if no weights are saved.
#' @method weights emmGrid
#' @export
weights.emmGrid = function(object, ...) {
    if(is.null(rtn <- object@grid[[".wgt."]]))
        rtn = rep(1, nrow(object@grid))
    if(!is.null(object@misc$display))
        rtn = rtn[object@misc$display]
    rtn
}

