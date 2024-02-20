# Example for section 4.8 Montgomery, Peck & Vining (2006)
x <- c(1.0, 1.0, 2.0, 3.3, 3.3, 4.0, 4.0, 4.0, 4.7, 5.0,
       5.6, 5.6, 5.6, 6.0, 6.0, 6.5, 6.9)

y <- c(10.84, 9.30, 16.35, 22.88, 24.35, 24.56, 25.86,
       29.16, 24.59, 22.25, 25.90, 27.20, 25.61, 25.45,
       26.56, 21.03, 21.46)

dt1 <- data.frame(x, y)

plot(x=x, y=y, pch=20, cex=2, col="red")
mod <- lm(y ~ x, data=dt1)
abline(mod, lty="dashed", col="blue", lwd=2)

lack_fit_test(mod)

# Example 9.9 Montgomery & Runger (1996)
x <- c(1.0, 1.0, 2.0, 3.3, 3.3, 4.0, 4.0, 4.0, 5.0,
       5.6, 5.6, 5.6, 6.0, 6.0, 6.5, 6.9)

y <- c(2.3, 1.8, 2.8, 1.8, 3.7, 2.6, 2.6, 2.2, 2.0, 3.5,
       2.8, 2.1, 3.4, 3.2, 3.4, 5.0)

plot(x=x, y=y, pch=20, cex=2, col="red")

mod <- lm(y ~ x)

abline(mod, lty="dashed", col="blue", lwd=2)

lack_fit_test(mod)
