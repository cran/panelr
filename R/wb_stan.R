#' @title Bayesian estimation of within-between models
#' @description A near-equivalent of [wbm()] that instead uses Stan,
#'   via \pkg{rstan} and \pkg{brms}.
#' @inheritParams wbm
#'
#' @param model.cor Do you want to model residual autocorrelation?
#'   This is often appropriate for linear models (`family = gaussian`).
#'   Default is FALSE to be consistent with [wbm()], reduce
#'   runtime, and avoid warnings for non-linear models.
#' @param fit_model Fit the model? Default is TRUE. If FALSE, only the model
#'   code is returned.
#' @param chains How many Markov chains should be used? Default is 3, to leave
#'   you with one unused thread if you're on a typical dual-core machine.
#' @param iter How many iterations, including warmup? Default is 2000, leaving
#'   1000 per chain after warmup. For some models and data, you may need quite
#'   a few more.
#' @param scale Standardize predictors? This can speed up model fit. Default
#'   is FALSE.
#' @param save_ranef Save random effect estimates? This can be crucial for
#'   predicting from the model and for certain post-estimation procedures.
#'   On the other hand, it drastically increases the size of the resulting
#'   model. Default is FALSE.
#' @param ... Additional arguments passed on to [brms::brm()]. This can include
#'   specification of priors.
#' @return A `wbm_stan` object, which is a list containing a `model` object
#'   with the `brm` model and a `stan_code` object with the model code.
#'
#'   If `fit_model = FALSE`, instead a list is returned containing a `stan_code`
#'   object and a `stan_data` object, leaving you with the tools you need to
#'   run the model yourself using `rstan`.
#'
#' @author Jacob A. Long
#' @details See [wbm()] for details on the formula syntax, model types,
#'   and some other stuff.
#' @examples
#' \dontrun{
#'  data("WageData")
#'  wages <- panel_data(WageData, id = id, wave = t)
#'  model <- wbm_stan(lwage ~ lag(union) + wks | blk + fem | blk * lag(union),
#'            data = wages, chains = 1, iter = 2000)
#'  summary(model)
#' }
#' @export
#' @rdname wbm_stan
#' @seealso [wbm()]
#'
#' @importFrom stats as.formula gaussian terms

wbm_stan <- function(formula, data, id = NULL, wave = NULL, model = "w-b",
                   detrend = FALSE, use.wave = FALSE, wave.factor = FALSE,
                   min.waves = 2, model.cor = FALSE, family = gaussian,
                   fit_model = TRUE, balance.correction = FALSE,
                   dt.random = TRUE, dt.order = 1,
                   chains = 3, iter = 2000, scale = FALSE, save_ranef = FALSE,
                   interaction.style = c("double-demean", "demean", "raw"),
                   weights = NULL, offset = NULL, ...) {

  if (!requireNamespace("brms")) {
    stop_wrap("You must have the brms package installed to use wbm_stan.")
  }
  if (getOption("stan-warning", FALSE) == FALSE &
      "package:brms" %nin% search()) {
    msg_wrap("If model compilation fails, please run 'library(brms)' and 
             try again.")
    options("stan-warning" = TRUE)
  }
  
  the_call <- match.call()
  the_call[[1]] <- substitute(wbm_stan)
  the_env <- parent.frame()
  
  if (any(c(detrend, balance.correction))) {
    if (!requireNamespace("tidyr") | !requireNamespace("purrr")) {
      stop_wrap("To use the 'detrend' or 'balance_correction' arguments, you 
                must have the 'tidyr' and 'purrr' packages installed.")
    }
  }
  
  formula <- Formula::Formula(formula)
  interaction.style <- match.arg(interaction.style,
                                 c("double-demean", "demean", "raw"))
  
  # Send to helper function for data prep
  prepped <- wb_prepare_data(formula = formula, data = data, id = id,
                             wave = wave, model = model, detrend = detrend,
                             use.wave = use.wave, wave.factor = wave.factor,
                             min.waves = min.waves,
                             balance_correction = balance.correction,
                             dt_random = dt.random, dt_order = dt.order,
                             weights = UQ(enquo(weights)),
                             offset = UQ(enquo(offset)), 
                             demean.ints = interaction.style == "double-demean",
                             old.ints = interaction.style == "demean")
  
  e <- prepped$e
  pf <- prepped$pf
  data <- e$data
  wave <- prepped$wave
  id <- prepped$id
  dv <- prepped$dv
  weights <- prepped$weights
  offset <- prepped$offset

  if (wave.factor == TRUE) {
    data[[wave]] <- as.factor(data[[wave]])
  }

  fin_formula <- formula_esc(e$fin_formula, c(e$int_means, e$within_ints,
                                              e$cross_ints, pf$v_info$meanvar,
                                              pf$varying, pf$constants, dv))

  names(data) <- make.names(names(data))
  
  # Use helper function to generate formula to pass to lme4
  fin_formula <- prepare_lme4_formula(fin_formula, pf, data, use.wave, wave,
                                      id, c(e$int_means, e$within_ints),
                                      e$cross_ints, dv)  

  # TODO: test this
  # Give brms the weights in the desired formula syntax
  if (!is.null(weights)) {
    weights <- make.names(weights) # Also give syntactically valid name for wts
    lhs <- paste(dv, "~")
    new_lhs <- paste0(dv, " | weights(", weights, ") ~ ")
    fin_formula <- sub(lhs, new_lhs, as.character(deparse(fin_formula)),
                       fixed = TRUE)
  }

  if (model.cor == TRUE) {
    cor_append <- paste0("+ arma(time = ", wave, ",", " gr = ", id, 
                         ", p = 1, q = 0, cov = FALSE)")
    fin_formula <- paste(as.character(deparse(fin_formula)), cor_append)
  } 
  
  fin_formula <- brms::brmsformula(fin_formula)
  
  ints <- e$cross_ints

  if (scale == TRUE) {

    scale_names <- names(data)
    scale_names <- scale_names[!(names(data) %in% c(id, wave, dv, weights))]
    data <- jtools::gscale(x = scale_names, data = data, n.sd = 1,
                           binary.inputs = "0/1")

  }

  # Give users the option to just get code + data for this
  if (fit_model == TRUE) {

    model <- brms::brm(fin_formula,
                       data = data,
                       chains = chains, iter = iter,
                       family = family,
                       save_ranef = save_ranef, ...)

    out <- list(model = model, data = data, fin_formula = fin_formula,
                dv = dv, id = id, wave = wave,
                num_distinct = prepped$num_distinct,
                varying = pf$varying, model = model,
                stab_terms = e$stab_terms,
                max_wave = prepped$maxwave, min_wave = prepped$minwave,
                ints = ints, model_cor = model.cor)

    class(out) <- "wbm_stan"

    return(out)

  } else {

    standat <- brms::make_standata(fin_formula,
                       data = data,
                       chains = chains, iter = iter,
                       family = family,
                       save_ranef = save_ranef, ...)

    stancode <-
      brms::make_stancode(fin_formula,
                          data = data, chains = chains, iter = iter,
                          family = family, save_ranef = save_ranef, ...)

    return(list(stan_data = standat, stan_code = stancode))

  }

}

#' @export
#'
#'

summary.wbm_stan <- function(object, ...) {

  summary(object$model)

}


# summary.wb_stan <- function(x, ...) {
#
#   cat("MODEL INFO:\n")
#   cat("Entities:", x$model$dims$ngrps[x$id], "\n")
#   cat("Time periods:", length(unique(x$data[,x$wave])), "\n")
#   cat("Dependent variable:", x$dv, "\n")
#   cat("Model type: Linear mixed effects\n")
#
#   # Name the estimator
#   est_name <- x$estimator
#   if (x$estimator == "w-b") {est_name <- "within-between"}
#   if (x$estimator == "stability") {
#     est_name <- "within-between with between-entity time trends"
#   }
#   cat("Estimator: ", est_name, "\n\n", sep = "")
#
#   cat("MODEL FIT:\n")
#   cat("AIC =", summary(x$model)$AIC, "\n")
#   cat("BIC =", summary(x$model)$BIC, "\n\n")
#
#   coefs <- summary(x$model)$tTable
#   coefs <- coefs[,c("Value","Std.Error","t-value","p-value")]
#   pvals <- coefs[,"p-value"]
#   coefs <- round(coefs, 3)
#   coefs <- cbind(coefs, rep(0, nrow(coefs)))
#   colnames(coefs) <- c("Est.", "S.E.", "t-value", "p", "")
#
#   sigstars <- c()
#   for (y in 1:nrow(coefs)) {
#     if (pvals[y] > 0.1) {
#       sigstars[y] <- ""
#     } else if (pvals[y] <= 0.1 & pvals[y] > 0.05) {
#       sigstars[y] <- "."
#     } else if (pvals[y] > 0.01 & pvals[y] <= 0.05) {
#       sigstars[y] <- "*"
#     } else if (pvals[y] > 0.001 & pvals[y] <= 0.01) {
#       sigstars[y] <- "**"
#     } else if (pvals[y] <= 0.001) {
#       sigstars[y] <- "***"
#     }
#   }
#
#   coefs[,5] <- sigstars
#   coefs <- as.table(coefs)
#
#   if (length(x$varying) > 0) {
#
#     cat("WITHIN EFFECTS:\n")
#     print(coefs[rownames(coefs) %in% x$varying,])
#     cat("\n")
#
#     coefs <- coefs[!(rownames(coefs) %in% x$varying),]
#     if (length(x$stab_terms) > 0) {
#
#       stabs <- coefs[rownames(coefs) %in% x$stab_terms,]
#       coefs <- coefs[!(rownames(coefs) %in% x$stab_terms),]
#
#     }
#
#   }
#
#   cat("BETWEEN EFFECTS:\n")
#   print(coefs)
#   cat("\n")
#
#   if (x$estimator == "stability") {
#
#     cat("BETWEEN-ENTITY TIME TRENDS:\n")
#     print(stabs)
#     cat("\n")
#
#   }
#
#   if (x$model_cor == TRUE) {
#     ar1 <- coef(x$model$modelStruct$corStruct, unconstrained = FALSE)
#     cat("Autocorrelation estimate =", round(ar1,3), "\n")
#   }
#
# }
