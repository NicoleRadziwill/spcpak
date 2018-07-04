# From https://stackoverflow.com/questions/30956558/implementing-additional-standard-run-rules-with-r-and-qcc

nelsonr1 <- function(x, m = mean(x), s = sd(x)) {
    # Nelson's QC rule 1: detect values outside + or -3 sd
    which(abs((x - m) / s) >= 3)
}

nelsonr2 <- function(x, m = mean(x), minrun = 9) {
    # Nelson's QC rule 2: detect runs of >= 9 points on the same side of the mean
    n <- length(x)
    counts <- sign(x - m)
    result <- counts
    for (runlength in 2:minrun)
        result <- result + c(counts[runlength:n], rep(0, runlength - 1))
    which(abs(result) >= minrun)
}

nelsonr3 <- function(x, minrun = 6) {
    # Nelson's QC rule 3: detect strict increase or decrease in >= 6 points in a row
    # Between 6 points you have 5 instances of increasing or decreasing. Therefore minrun - 1.
    n <- length(x)
    signs <- sign(c(x[-1], x[n]) - x)
    counts <- signs
    for (rl in 2:(minrun - 1)) {
        counts <- counts + c(signs[rl:n], rep(0, rl - 1))
    }
    which(abs(counts) >= minrun - 1)
}

nelsonr4 <- function(x, m = mean(x), minrun = 14, directing_from_mean = FALSE) {
    # Nelson's QC rule 4: 14 points in a row alternating in direction from the mean,
    # or 14 points in a row alternating in increase and decrease
    n <- length(x)
    if (directing_from_mean == TRUE) {
        signs <- sign(x - m)
    } else {
        signs <- sign(c(x[-1],x[n]) - x)
    }
    counts <- signs
    fac <- -1
    for (rl in 2:minrun) {
        counts <- counts + fac * c(signs[rl:n], rep(0, rl - 1))
        fac <- -fac
    }
    counts <- abs(counts)
    which(counts >= minrun)
}

nelsonr5 <- function(x, m = mean(x), s = sd(x), minrun = 3) {
    # Nelson's QC rule 5: two out of 3 >2 sd from mean in the same direction
    n <- length(x)
    pos <- 1 * ((x - m) / s > 2)
    neg <- 1 * ((x - m) / s < -2)
    poscounts <- pos
    negcounts <- neg
    for (rl in 2:minrun) {
        poscounts <- poscounts + c(pos[rl:n], rep(0, rl - 1))
        negcounts <- negcounts + c(neg[rl:n], rep(0, rl - 1))
    }
    counts <- apply(cbind(poscounts, negcounts), 1, max)
    which(counts >= minrun -1)
}

nelsonr6 <- function(x, m = mean(x), s = sd(x), minrun = 5) {
    # Nelson's QC rule 6: four out of five > 1 sd from mean in the same direction
    n <- length(x)
    pos <- 1 * ((x - m) / s > 1)
    neg <- 1 * ((x - m) / s < -1)
    poscounts <- pos
    negcounts <- neg
    for (rl in 2:minrun) {
        poscounts <- poscounts + c(pos[rl:n], rep(0, rl - 1))
        negcounts <- negcounts + c(neg[rl:n], rep(0, rl - 1))
    }
    counts <- apply(cbind(poscounts, negcounts), 1, max)
    which(counts >= minrun - 1)
}

nelsonr7 <- function(x, m = mean(x), s = sd(x), minrun = 15) {
    # Nelson's QC rule 7: >= 15 points in a row within 1 sd from the mean
    n <- length(x)
    within <- 1 * (abs((x - m) / s) < 1)
    counts <- within
    for (rl in 2:minrun)
        counts <- counts + c(within[rl:n], rep(0, rl - 1))
    which(counts >= minrun)
}

nelsonr8 <- function(x, m = mean(x), s = sd(x), minrun = 8) {
    # Nelson's QC rule 8: >= 8 points in a row all outside the m + -1s range
    n <- length(x)
    outofrange <- 1 * (abs((x - m) / s) > 1)
    counts <- outofrange
    for (rl in 2:minrun)
        counts <- counts + c(outofrange[rl:n], rep(0, rl - 1))
    which(counts >= minrun)
}
