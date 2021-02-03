require(pgrwe)

g_cols <- c("Unadjusted" = "blue", "Adjusted" = "red")

expit <- function(x) {
    ex <- exp(x)
    ex / (1 + ex)
}

simu_data <- function(n, p_age = c(60, 5), p_disease = 0.5,
                      beta = c(-4, 0.1, -8), seed = 1000) {
    old_seed <- set.seed(seed)
    age      <- rnorm(n, mean = p_age[1], sd = p_age[2])
    disease  <- rbinom(n, size = 1, prob = p_disease)
    x_mat    <- cbind(1, age, disease)

    prob     <- apply(x_mat, 1, function(x) expit(sum(x * beta)))
    y        <- rbinom(n, 1, prob = prob)

    set.seed(old_seed)

    data.frame(y       = y,
               age     = age,
               disease = as.character(disease),
               weights = 1 / n)
}

set_sum_stat <- function(p_age = c(60, 5), p_disease = 0.5) {
    list(age = list(type = "continuous",
                    mean = p_age[1],
                    ex2  = p_age[2]^2 + p_age[1]^2),
         disease = list(type   = "discrete",
                        values = c("1", "0"),
                        probs  = c(p_disease,
                                   1 - p_disease))
         )
}

get_weights <- function(stats, dat) {
    ents <- rwe_margin_entropy(stats, dat,
                               tol = 1e-6, max_it = 10000,
                               print_level = 0)

    dat$weights <- ents$weights
    stats      <- ents$stats[c(1, 3), ]
    stats[, 2] <- sqrt(stats[, 2] - stats[, 1]^2)

    colnames(stats) <- c("Age_Mean", "Age_SD", "Disease_Prob")
    rownames(stats) <- c("RWD: Observed", "RWD: Adjusted")

    list(stats = stats,
         dat   = dat)
}

get_posterior <- function(y, weights, a = 0.5, b = 0.5) {
    n   <- length(y)
    rst <- rbind(c(a + sum(y),
                   b + sum(1 - y)))

    ab <- c(a + n * sum(y * weights),
            b + n * sum((1 - y) * weights))

    rst <- rbind(rst, ab)
    rst
}

plot_age <- function(dat, adj = F, cols = g_cols) {
    rst <- ggplot(data = dat, aes(x = age)) +
        stat_density(aes(color = names(cols)[1]),
                     position = "identity",
                     geom = "line") +
        scale_colour_manual(name = "", values = cols) +
        theme_bw()

    if (adj)
        rst <- rst +
            stat_density(aes(color  = names(cols)[2],
                             weight = weights),
                         position = "identity",
                         geom = "line")

    rst
}

plot_disease <- function(dat, adj = F, cols = g_cols) {
    rst <- ggplot(data = dat, aes(x = disease)) +
        geom_bar(aes(color  = names(cols)[1],
                     y = (..count..) / sum(..count..)),
                 position = position_nudge(x = - 0.11),
                 width = 0.2,
                 fill = cols[1],
                 alpha = 0.2) +
        scale_colour_manual(name = "", values = cols) +
        theme_bw()

    if (adj)
        rst <- rst +
            geom_bar(aes(color  = names(cols)[2],
                         weight = weights),
                     position = position_nudge(x = 0.1),
                     width = 0.2,
                     fill = cols[2],
                     alpha = 0.2)

    rst
}

plot_posterior <- function(dat, ...,
                           truth = NULL,
                           adj = F, cols = g_cols) {

    ab <- get_posterior(dat$y, dat$weights, ...)

    if (!adj)
        ab <- ab[-2, , drop = F]

    x  <- seq(0, 1, by = 0.01)
    rst <- NULL
    for (i in seq_len(nrow(ab))) {
        y   <- dbeta(x, ab[i, 1], ab[i, 2])
        rst <- rbind(
            rst,
            data.frame(
                x = x,
                y = y,
                group = names(cols)[i]
            )
        )
    }

    plt <- ggplot(data = rst, aes(x = x, y = y)) +
        geom_line(aes(col = group)) +
        scale_colour_manual(name = "", values = cols) +
        theme_bw()

    if (!is.null(truth) & adj) {
        plt <- plt +
            geom_vline(xintercept = truth,
                       col   = "brown",
                       linetype = 2) +
            geom_text(aes(truth, 1, label = "TRUTH"),
                      col = "brown")
    }

    plt
}

plot_weights <- function(dat, adj = F) {
    rst <- ggplot(data = dat, aes(x = age, y = weights)) +
        geom_hline(yintercept = 1 / nrow(dat)) +
        theme_bw()

    if (adj)
        rst <- rst +
            geom_point(aes(color = disease))

    rst
}
