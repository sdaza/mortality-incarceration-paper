
##############################
# health and incarceration
# utility functions
# author: sebastian daza
##############################

library(texreg)
library(survminer)


# compute dispersion for INLA model
get_dispersion <- function(model, data, variable) {

    mu <- model$summary.fitted.values[, "mean"]
    E <- (data[, variable, with = FALSE] - mu) / sqrt(mu)
    N <- nrow(data)
    p <- nrow(model$summary.fixed)

    return(sum(E^2) / (N - p))
}

# create cox-snell residual plot
plot_coxsnell <- function(model, dat) {
  r <- model$residual
  rr <- dat$died - r
  fit <- survfit(Surv(rr, dat$died) ~ 1)
  S <- fit$surv
  T <- fit$time
  t0 <- seq(0, max(T), 0.05)
  S0 <- exp(-t0)

  plot(T,S, xlim = range(T), ylim = c(0,1), xlab = "time",
       ylab = "Cox-Snell residual values", pch='.')
  lines(t0, S0)

  return(print("cox-snell plot done!"))
}


# create plots of cumulative incidence
getComparisonPlot <- function(model, data1, data2, fun = "event",
                              pos1 = c(38, 40), pos2 = c(0.5, 0.09),
                              xlab = "\n Years", ylab = "Cumulative event\n") {
    # get survfit objects
    sf1 <- survfit(model, newdata = data1)
    sf0 <- survfit(model, newdata = data2)

    # get surv_summary format
    sf0 <- survminer:::.apply_surv_func(surv_summary(sf0), fun = fun)

    # first plot
    plot1 <- ggsurvplot(sf1, data = data1, fun = fun,
                        censor = FALSE, color = "red")

    # add curve
    plot1 <- plot1$plot + geom_step(data = sf0,
                                    mapping = aes(x = time, y = surv),
                                    color = "blue") +
                                    survminer:::.geom_confint(data = sf0,
                                    mapping = aes(ymin = lower, ymax = upper),
                                    fill = "blue", alpha = 0.3)

    # format
    plot1 <- plot1 + annotate("text", x = pos1, y = pos2,
                              label = c('bold("Prison")', 'bold("No prison")'), parse = TRUE,
                              col  = c("red", "blue"), size = c(4,4)) +
                              labs(x = xlab, y = ylab) + ylim(0,1)

    # return
    return(plot1)

}


# extension for svycoxph objects (survival package)
extract.svycoxph <- function(model, include.events = TRUE, include.nobs = TRUE, ...) {
    s <- summary(model)
    coefficient.names <- rownames(s$coefficients[, 1])
    coefficients <- s$coefficients[, 1]
    if (is.null(s$used.robust)) {
        standard.errors <- s$coef[, 4]
        z <- coefficients / standard.errors
        significance <- 2*pnorm(-abs(z))
    } else {
        standard.errors <- s$coef[, 3]
        z <- coefficients / standard.errors
        significance <- 2*pnorm(-abs(z))
    }

    gof <- numeric()
    gof.names <- character()
    gof.decimal <- logical()
    if (include.events == TRUE) {
        gof <- c(gof, event)
        gof.names <- c(gof.names, "Num.\ events")
        gof.decimal <- c(gof.decimal, FALSE)
    }
    if (include.nobs == TRUE) {
        gof <- c(gof, n)
        gof.names <- c(gof.names, "Num.\ obs.")
        gof.decimal <- c(gof.decimal, FALSE)
    }

    tr <- createTexreg(
        coef.names = coefficient.names,
        coef = coefficients,
        se = standard.errors,
        pvalues = significance,
        gof.names = gof.names,
        gof = gof,
        gof.decimal = gof.decimal
        )

    return(tr)
}

setMethod("extract", signature = className("svycoxph", "survival"),
    definition = extract.coxph)

