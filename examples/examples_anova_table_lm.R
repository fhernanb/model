# Ejemplo RLS datos soldadura
file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE)

mod1 <- lm(Resistencia ~ Edad, data=datos)

anova_table_lm(mod1)


# Ejemplo 9.4 de Montgomery (1996)
datos <- structure(list(nivel_hidro = c(0.99, 1.02, 1.15, 1.29, 1.46, 
                                        1.36, 0.87, 1.23, 1.55, 1.4, 
                                        1.19, 1.15, 0.98, 1.01, 1.11, 1.2, 
                                        1.26, 1.32, 1.43, 0.95), 
                        pureza = c(90.01, 89.05, 91.43, 93.74,
                                   96.73, 94.45, 87.59, 91.77, 
                                   99.42, 93.65, 93.54, 92.52, 90.56,
                                   89.54, 89.85, 90.39, 93.25, 93.41, 
                                   94.98, 87.33)), 
                   class = "data.frame", row.names = c(NA, -20L))

mod2 <- lm(pureza ~ nivel_hidro, data=datos)
anova_table_lm(mod2)


# Ejemplo 10-4 de Montgomery (1996)
tiempo <- c(9.95, 24.45, 31.75, 35.00, 25.02, 16.86, 14.38, 
            9.60, 24.35, 27.50, 17.08, 37.00, 41.95, 11.66, 
            21.65, 17.89, 69.00, 10.30, 34.93, 46.59, 44.88, 
            54.12, 56.23, 22.13, 21.15)

cantidad <- c(2, 8, 11, 10, 8, 4, 2, 2, 9, 8, 4, 11, 12, 2, 4, 
              4, 20, 1, 10, 15, 15, 16, 17, 6, 5)

distancia <- c(50, 110, 120, 550, 295, 200, 375, 52, 100, 
               300, 412, 400, 500, 360, 205, 400, 600, 585, 
               540, 250, 290, 510, 590, 100, 400)

datos <- data.frame(tiempo, cantidad, distancia)

mod3 <- lm(tiempo ~ cantidad + distancia, data=datos)

anova_table_lm(mod3)

