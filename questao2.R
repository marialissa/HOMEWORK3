#item 1

x <- penguins_data$body_mass_g
y <- penguins_data$bill_length_mm

plot(x, y, 
     main = "Massa Corporal e Comprimento do Bico",
     xlab = "Massa Corporal (g)", 
     ylab = "Comprimento do Bico (mm)",
     pch = 19, col = "pink")


#item 2

beta1 <- sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
beta0 <- mean(y) - beta1 * mean(x)

modelo <- lm(y ~ x)
coef_lm <- coef(modelo)

cat("Manual: Beta0 =", beta0, " Beta1 =", beta1, "\n")
cat("lm():   Beta0 =", coef_lm[1], "Beta1 =", coef_lm[2], "\n")

abline(modelo, col = "purple", lwd = 2)


#item 3

residuos <- y - (beta0 + beta1 * x)
plot(x, residuos, 
     main = "Gráfico de Resíduos", 
     xlab = "Massa Corporal (g)", ylab = "Resíduos (y - y_pred)",
     pch = 19, col = "pink")
abline(h = 0, lty = 2)

n <- length(y)
rmse <- sqrt(sum(residuos^2) / n)
sst <- sum((y - mean(y))^2)
ssr <- sum(residuos^2)      
r_quadrado <- 1 - (ssr / sst)

cat("RMSE:", rmse, "\n")
cat("R2:", r_quadrado, "\n")


#item 4

x_out <- x
y_out <- y

x_out[1] <- 10000  #massa de 10kg (muito pesado)
y_out[1] <- 20     #bico de 20mm (muito curto)

modelo_outlier <- lm(y_out ~ x_out)
coef_lm_outlier <- coef(modelo_outlier)

rmse_out <- sqrt(mean(resid(modelo_outlier)^2))
r2_out   <- summary(modelo_outlier)$r.squared

cat("Beta0 original:", coef_lm[1], " Beta0 com Outlier:", coef_lm_outlier[1], "\n")
cat("Beta1 original:", coef_lm[2], " Beta1 com Outlier:", coef_lm_outlier[2], "\n")
cat("R2 original:", r_quadrado, " R2 com Outlier:", r2_out, "\n")
cat("RMSE original:", rmse, " RMSE com Outlier:", rmse_out, "\n")

plot(x_out, y_out, main = "Influência do Outlier", 
     xlab = "x (Massa)", ylab = "y (Bico)", col = "pink")
points(x_out[1], y_out[1], col = "deeppink", pch = 19, cex = 2) 
abline(modelo_linear, col = "purple", lty = 2) 
abline(modelo_outlier, col = "deeppink", lwd = 2)   

legend("topright", 
       legend = c("Modelo Original", "Modelo com Outlier", "Ponto Outlier"),
       col = c("purple", "deeppink", "deeppink"), 
       lty = c(2, 1, NA),  
       pch = c(NA, NA, 19), 
       lwd = c(2, 2, NA),
       bty = "n",           
       cex = 0.8)
