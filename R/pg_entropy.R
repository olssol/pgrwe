#' Estimate Adjusted Mean of Outcome
#'
#' Estimate adjusted mean of outcome by entropy balancing and operation
#' difference if mean outcome was reported in a clinical study
#'
#' @param target_stats List of summary statistics in a target patient population
#' @param dta_ext External data dataframe
#' @param v_y Column name in the external data dataframe corresponding to
#'     outcome
#' @param reported Reported mean outcome of the target patient population
#' @param nbs Number of bootstraps
#' @param ... Optional parameters for entropy balancing
#'
#' @examples
#' rwe_pgoal_est(ex_stats, ex_rwd, v_y = "dead_6mths",  nbs = 10)
#'
#' @export
#'
rwe_pgoal_est <- function(target_stats, dta_ext, v_y = "Y", reported = NULL,
                          nbs = 100, ...) {

    f_rst <- function(cur_dta) {
        ents      <- rwe_margin_entropy(target_stats, cur_dta, ...)
        adj_stats <- ents$stats
        raw_y     <- mean(cur_dta[[v_y]])
        adj_y     <- sum(cur_dta[[v_y]] * ents$weights)

        edelta     <- NULL
        if (!is.null(reported))
            edelta <- reported / adj_y

        list(adj_stats = adj_stats,
             raw_y     = raw_y,
             adj_y     = adj_y,
             edelta    = edelta)
    }

    ## original results
    rst_org <- f_rst(dta_ext)

    ## bootstrap results
    rst_bs <- NULL
    if (nbs > 0) {
        n_pt <- nrow(dta_ext)
        for (i in seq_len(nbs)) {
            print(i)
            inx     <- sample(1:n_pt, n_pt, replace = TRUE)
            bs_dta  <- dta_ext[inx, ]

            cur_rst <- tryCatch(
            {
                f_rst(bs_dta)
            },
            error = function(e) {
                NULL
          })

            if (is.null(cur_rst))
                next

            rst_bs <- rbind(rst_bs,
                            c(raw_y  = cur_rst$raw_y,
                              adj_y  = cur_rst$adj_y,
                              edelta = cur_rst$edelta))
        }
    }

    ## return
    rst <- list(rst_org  = rst_org,
                rst_bs   = rst_bs,
                reported = reported)

    rst
}


#' Calculate entropy balancing score
#'
#' Calculate entropy balancing score
#'
#' @inheritParams rwe_pgoal
#' @param tol Tolerance level for entropy balancing optimization
#' @param max_it Maximum number of iterations
#' @param print_level Verbose or not
#'
#' @return A list that includes weights (entropy balancing weights), stats
#'     (summary statistics based on weighted patients), convergence status, etc.
#'
#' @examples
#' rwe_margin_entropy(ex_stats, ex_rwd)
#'
#' @export
#'
rwe_margin_entropy <- function(target_stats, dta_ext,
                               tol = 1e-8, max_it = 10000,
                               print_level = 0) {

    line.searcher <- function(Base.weight, Co.x, Tr.total,
                              coefs, Newton, ss) {
        weights.temp <- c(exp(Co.x %*% (coefs - (ss * Newton))))
        weights.temp <- weights.temp * Base.weight
        Co.x.agg     <- c(weights.temp %*% Co.x)
        maxdiff      <- max(abs(Co.x.agg - Tr.total))
        return(maxdiff)
    }

    x_m         <- rwe_extract_stats_covx(target_stats, dta_ext)
    covx        <- cbind(1, x_m$covx)
    n_ext       <- nrow(covx)
    tr_total    <- c(n_ext, x_m$constraints * n_ext)
    base_weight <- rep(1, n_ext)
    coefs       <- rep(0, ncol(covx))

    ## optimize
    converged   <- FALSE
    for (iter in 1:max_it) {
        weights_temp <- c(exp(covx %*% coefs))
        weights_ebal <- weights_temp * base_weight
        covx_agg     <- c(weights_ebal %*% covx)
        gradient     <- covx_agg - tr_total

        if (1 == iter)
            init_stats <- covx_agg


        if (max(abs(gradient)) < tol) {
            converged <- TRUE
            break
        }

        if (print_level >= 2) {
            cat("Iteration", iter, "maximum deviation is =",
                format(max(abs(gradient)), digits = 4), "\n")
        }

        hessian  <- t(covx) %*% (weights_ebal * covx)
        Coefs    <- coefs
        newton   <- solve(hessian, gradient)
        coefs    <- coefs - newton

        loss.new <- line.searcher(Base.weight = base_weight,
                                  Co.x = covx,
                                  Tr.total = tr_total,
                                  coefs = coefs,
                                  Newton = newton, ss = 1)

        loss.old <- line.searcher(Base.weight = base_weight,
                                  Co.x = covx,
                                  Tr.total = tr_total,
                                  coefs = Coefs,
                                  Newton = newton, ss = 0)

        if (is.nan(loss.new) |
            loss.old <= loss.new) {
            ss.out <- optimize(line.searcher, lower = 1e-05,
                               upper = 1, maximum = FALSE,
                               Base.weight = base_weight,
                               Co.x = covx, Tr.total = tr_total, coefs = Coefs,
                               Newton = newton)

            coefs <- Coefs - ss.out$minimum * solve(hessian, gradient)
        }
    }

    ## return
    weights <- weights_ebal / n_ext
    stats   <- rbind(as.numeric(init_stats)[-1] / n_ext,
                     x_m$constraints,
                     as.numeric(weights %*% covx)[-1])
    rownames(stats) <- c("raw", "target", "adjusted")

    list(maxdiff      = max(abs(gradient)),
         coefs        = coefs,
         weights      = weights,
         stats        = stats,
         converged    = converged)
}

#' Get estimating equation
#'
#' Extract summary statistics from a data frame based on existing statistics
#'
#' @param target_stats Summary statistics to be extracted
#' @param dta Data frame with subject level data
#'
#' @return List of summary statistics
#'
#' @examples
#' rwe_extract_stats(ex_stats, ex_rwd)
#'
#' @export
#'
rwe_extract_stats_covx <- function(target_stats, dta,
                                   cont_stat = c("mean", "ex2")) {

    ## continuous covariates summary statistics
    cont_stat <- match.arg(cont_stat, several.ok = TRUE)

    f_disc <- function(cur_y, stats) {

        xlev  <- stats$values
        probs <- stats$probs

        c_rst <- NULL
        c_m   <- NULL
        for (i in seq_len(length(xlev) - 1)) {
            x     <- xlev[i]
            c_rst <- cbind(c_rst,
                           as.numeric(x == cur_y))
            c_m   <- c(c_m, probs[i])
        }

        list(c_rst, c_m)
    }

    f_cont <- function(cur_y, stats, cont_stat) {
        c_rst <- NULL
        c_m   <- NULL
        for (i in cont_stat) {
            cur_v <- stats[[i]]
            if (is.null(cur_v))
                next;

            if ("mean" == i) {
                y <- cur_y
            } else if ("ex2" == i) {
                y <- y^2
            }

            c_rst <- cbind(c_rst, y)
            c_m   <- c(c_m, cur_v)
        }

        list(c_rst, c_m)
    }

    f_quan <- function(cur_y, stats) {
        quants <- stats$quants

        c_m   <- quants[, "quants"]
        c_rst <- NULL
        for (i in seq_len(nrow(quants))) {
            qt    <- quants[i, "x_quants"]
            c_rst <- cbind(c_rst,
                           as.numeric(cur_y <= qt))
        }

        list(c_rst, c_m)
    }

    rst_x <- NULL
    rst_m <- NULL
    for (i in seq_len(length(target_stats))) {
        cur_v   <- names(target_stats)[i]
        cur_s   <- target_stats[[i]]
        cur_y   <- dta[[cur_v]]

        if (!is.null(cur_s$scale))
            cur_y <- cur_y * cur_s$scale

        cur_rst <- switch(
            cur_s$type,
            discrete   = f_disc(cur_y, cur_s),
            continuous = f_cont(cur_y, cur_s, cont_stat),
            quants     = f_quan(cur_y, cur_s)
        )

        rst_x <- cbind(rst_x, cur_rst[[1]])
        rst_m <- c(rst_m, cur_rst[[2]])
    }

    ## return
    list(covx        = rst_x,
         constraints = rst_m)
}


#' Estimate Performance Goal
#'
#' Different estimates of the outcomes for deriving performance goals.
#'
#' @param target_stats List of summary statistics in a target patient population
#' @param exist_stats List of summary statistics in an existing clinical study
#' @param exist_ey Expectation of the outcome reported by the existing clinical study
#'
#' @param dta_ext External data dataframe
#' @param v_y Column name in the external data dataframe corresponding to
#'     outcome
#' @param nbs Number of bootstraps
#' @param ... Optional parameters for entropy balancing
#'
#' @examples
#' rst <- rwe_pgoal(target_stats = ex_stats,
#'                  exist_stats  = ex2_stats,
#'                  exist_ey     = 0.1,
#'                  dta_ext      = ex_rwd,
#'                  v_y          = "dead_6mths")
#' print(rst$est_pg)
#'
#'
#' @export
#'
rwe_pgoal <- function(target_stats, exist_stats, exist_ey,
                      dta_ext, v_y = "Y", nbs = 100, ...) {

    est_target <- rwe_pgoal_est(target_stats, dta_ext, v_y = v_y,
                                nbs = nbs, ...)

    est_exist <- rwe_pgoal_est(exist_stats, dta_ext, v_y = v_y,
                               reported = exist_ey, nbs = nbs, ...)

    raw <- c(est_target$rst_org$raw_y,
             est_target$rst_bs[, 'raw_y'])

    adj <- c(est_target$rst_org$adj_y,
             est_target$rst_bs[, 'adj_y'])

    edelta <- c(est_exist$rst_org$edelta,
                est_exist$rst_bs[, "edelta"])

    rec    <- adj * edelta

    ## results
    rst    <- apply(rbind(raw, adj, edelta, rec),
                    1,
                    function (x) {
                        c(x[1],
                          quantile(x[-1], c(0.025, 0.975), na.rm = TRUE)
                          )
                    })
    rst <- t(rst)
    colnames(rst) <- c("Estimate", "LowerBound", "UpperBound")
    rownames(rst) <- c("Unadjusted",
                       "Selection Difference Adjusted",
                       "Operation Difference",
                       "Recommended")

    ## return
    list(est_pg     = rst,
         est_target = est_target,
         est_exist  = est_exist)
}
