# Instalare si incarcare a pachetelor necesare
install.packages("glmnet")
install.packages("caret",type="binary")
install.packages("stats")
install.packages("sandwich")
install.packages("MASS")
install.packages("lmtest")
install.packages("tseries")
install.packages("olsrr")
install.packages("car")
install.packages("AICcmodavg")
install.packages("plm")
install.packages("gplots")
library(gplots)
library(ggplot2)
library(plm)
library(AICcmodavg)
library(lmtest)
library(glmnet)
library(caret)
library(stats)
library(MASS)
library(sandwich)
library(tseries)
library(car)
library(olsrr)
library(dplyr)
# Incarcarea datelor
dataset <- read.csv("Cleaned_Dairy_Dataset.csv", header = TRUE, sep = ",")
str(dataset)
dataset$Date <- as.Date(dataset$Date, format = "%Y-%m-%d")

# Crearea subseturilor
small_farms <- subset(dataset, Farm.Size == "Small")
medium_farms <- subset(dataset, Farm.Size == "Medium")
large_farms <- subset(dataset, Farm.Size == "Large")
str(dataset)
process_farm_data <- function(farm_data,farm_size)
{
  cat("----------------------------------START----------------------------------\n")
  # Selectarea variabilelor numerice
  # Selectarea variabilelor numerice
  numerical_columns <- c("Total.Land.Area..acres.", "Number.of.Cows", "Quantity..liters.kg.", 
                         "Price.per.Unit", "Quantity.Sold..liters.kg.", "Price.per.Unit..sold.", 
                         "Approx..Total.Revenue.INR.", "Quantity.in.Stock..liters.kg.", 
                         "Minimum.Stock.Threshold..liters.kg.", "Reorder.Quantity..liters.kg.")
  
  data <- farm_data[, numerical_columns]
  
  # Eliminarea valorilor lipsa
  data <- na.omit(data)
  
  # Separarea in variabile predictori si tinta
  X <- as.matrix(data[, !colnames(data) %in% "Approx..Total.Revenue.INR."])
  y <- as.vector(data$Approx..Total.Revenue.INR.)
  
  # Standardizarea datelor (scalare)
  X_scaled <- scale(X)  # Scalare setului de antrenament
  
  # Impartirea datelor in seturi de antrenament si test
  set.seed(42)
  train_index <- sample(1:nrow(X_scaled), size = 0.8 * nrow(X_scaled))
  X_train <- X_scaled[train_index, ]
  X_test <- X_scaled[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]
  
  # Lasso Regression
  lasso_model <- cv.glmnet(X_train, y_train, alpha = 1, standardize = FALSE)
  lasso_coefs <- coef(lasso_model, s = "lambda.min")
  print("Lasso Coefficients:")
  print(lasso_coefs)
  
  # Ridge Regression
  ridge_model <- cv.glmnet(X_train, y_train, alpha = 0, standardize = FALSE)
  ridge_coefs <- coef(ridge_model, s = "lambda.min")
  print("Ridge Coefficients:")
  print(ridge_coefs)
  
  # Elastic Net Regression
  elastic_net_model <- cv.glmnet(X_train, y_train, alpha = 0.5, standardize = FALSE)
  elastic_net_coefs <- coef(elastic_net_model, s = "lambda.min")
  print("Elastic Net Coefficients:")
  print(elastic_net_coefs)
  
  # Evaluarea modelelor
  lasso_pred <- predict(lasso_model, X_test, s = "lambda.min")
  ridge_pred <- predict(ridge_model, X_test, s = "lambda.min")
  elastic_net_pred <- predict(elastic_net_model, X_test, s = "lambda.min")
  
  lasso_rmse <- sqrt(mean((lasso_pred - y_test)^2))
  ridge_rmse <- sqrt(mean((ridge_pred - y_test)^2))
  elastic_net_rmse <- sqrt(mean((elastic_net_pred - y_test)^2))
  
  cat("Lasso RMSE:", lasso_rmse, "\n")
  cat("Ridge RMSE:", ridge_rmse, "\n")
  cat("Elastic Net RMSE:", elastic_net_rmse, "\n")
  
  # Alegerea Lasso si eliminarea variabilelor nesemnificative
  significant_vars <- rownames(as.matrix(lasso_coefs))[as.matrix(lasso_coefs) != 0 & rownames(as.matrix(lasso_coefs)) != "(Intercept)"]
  cat("Variabile semnificative selectate de Lasso:\n")
  print(significant_vars)
  
  # Crearea unui nou set de date cu variabilele semnificative
  data_significant <- data[, significant_vars]
  X_significant <- as.matrix(data_significant)
  
  # Reantrenarea modelului Lasso doar pe variabilele semnificative
  set.seed(42)
  train_index <- sample(1:nrow(X_significant), size = 0.8 * nrow(X_significant))
  X_train <- X_significant[train_index, ]
  X_test <- X_significant[-train_index, ]
  y_train <- y[train_index]
  y_test <- y[-train_index]
  
  lasso_model_refined <- cv.glmnet(X_train, y_train, alpha = 1, standardize = FALSE)
  lasso_pred_refined <- predict(lasso_model_refined, X_test, s = "lambda.min")
  lasso_rmse_refined <- sqrt(mean((lasso_pred_refined - y_test)^2))
  
  cat("Lasso RMSE cu variabilele semnificative:", lasso_rmse_refined, "\n")
  
  # Model de regresie multipla folosind variabilele semnificative
  final_data <- data.frame(data_significant, Revenue = y)
  model <- lm(Revenue ~ ., data = final_data)
  summary_model <- summary(model)
  
  cat("\nRezumatul modelului de regresie multipla:\n")
  print(summary_model)
  
  # Identificarea variabilelor semnificative din modelul de regresie multipla (p < 0.05)
  p_values <- summary_model$coefficients[, 4]
  significant_vars_model <- rownames(summary_model$coefficients[p_values < 0.1, ])
  cat("Variabile semnificative în modelul de regresie multipla:\n")
  print(significant_vars_model)
  
  # Salvează numele variabilelor semnificative într-o variabilă separată
  significant_vars_names <- significant_vars_model
  
  # Eliminarea variabilelor nesemnificative din modelul de regresie multipla
  insignificant_vars <- rownames(summary_model$coefficients[p_values > 0.05, ])
  data_refined <- data_significant[, which(colnames(data_significant) %in% significant_vars_model)]
  
  # Model de regresie multipla cu variabile semnificative
  final_data <- data.frame(data_refined, Revenue = y)
  model <- lm(Revenue ~ ., data = final_data)
  summary_model <- summary(model)
  
  cat("\nRezumatul modelului de regresie multipla cu variabile semnificative:\n")
  print(summary_model)
  
  
  
  
#=================================================IPOTEZA 1========================================
  #Verificam liniaritatea functionalei
  #Verificam daca modelul de regresie multipla este liniar
  #H0: Modelul este liniar
  #H1: Modelul nu este liniar
  linear_hypothesis <- ols_test_normality(model)
  cat("\nTestul de liniaritate a functiei:\n")
  print(linear_hypothesis)

  #Testul de liniaritate a functiei nu a respins ipoteza nula, deci putem considera ca modelul este liniar
  
  
  #=================================================IPOTEZA 2========================================
  # Testarea ipotezei 2: Variabilitatea in X este pozitiva
  #H0: Variabilitatea in X este pozitiva
  #H1: Variabilitatea in X nu este pozitiva
  variances <- apply(data, 2, var)
  positive_variance <- all(variances > 0)
  cat("\nTestul de variabilitate a variabilelor explicative:\n")
  if (positive_variance) {
    cat("Ipoteza nulă nu a fost respinsă: Variabilitatea în X este pozitivă.\n")
  } else {
    cat("Ipoteza nulă a fost respinsă: Unele variabile explicative au variabilitate nulă sau negativă.\n")
  }
  #Testul de variabilitate a variabilelor explicative nu a respins ipoteza nula, deci putem considera ca variabilitatea in X este pozitiva
  #=================================================IPOTEZA 3========================================
  # Testarea ipotezei 3: Erorile au media 0
  #H0: Erorile au media 0
  #H1: Erorile nu au media 0
  residuals <- residuals(model)
  mean_residuals <- mean(residuals)
  cat("\nMedia reziduurilor:", mean_residuals, "\n")
  cat("\nTestul de medie a erorilor:\n")
  if (abs(mean_residuals) < 1e-6) {
    cat("Ipoteza nulă nu a fost respinsă: Erorile au medie 0.\n")
  } else {
    cat("Ipoteza nulă a fost respinsă: Erorile nu au medie 0.\n")
    
  }
  #=================================================IPOTEZA 4========================================
  # Testarea ipotezei 4: Homoscedasticitatea erorilor
  # H0: Erorile sunt homoscedastice
  # H1: Erorile nu sunt homoscedastice
  # Testul Breusch-Pagan
  cat("\nTestul de omoscedasticitate a erorilor:\n")
  homoscedasticity_test <- bptest(model)
  print(homoscedasticity_test)
  
  # Testul White
  cat("\nTestul de omoscedasticitate a erorilor (White):\n")
  white_test <- bptest(model, studentize = TRUE)
  print(white_test)
  
  # Vizualizare reziduuri
  cat("\nVizualizare reziduuri:\n")
  residuals_model <- residuals(model)
  
  # Graficul reziduurilor vs valorile ajustate
  plot(fitted(model), residuals_model, main = "Reziduuri vs Valori Ajustate",
       xlab = "Valori Ajustate", ylab = "Reziduuri")
  abline(h = 0, col = "red")
  
  # Histogramă a reziduurilor
  hist(residuals_model, main = "Distribuția Reziduurilor", xlab = "Reziduuri", col = "lightblue", border = "black")
  
  cat("\nReziduuri - Sumariu:\n")
  summary(residuals_model)
  
  # Aplicăm WLS doar dacă ipoteza nulă a fost respinsă
  if (homoscedasticity_test$p.value < 0.05) {
    cat("Ipoteza nulă a fost respinsă: Erorile nu sunt homoscedastice.\n")
    
    # Aplicăm WLS folosind ponderi standard
    cat("\nAplicam WLS folosind ponderi standard:\n")
    
    # Aplicăm WLS folosind ponderi bazate pe o variabilă semnificativă
    # Ponderi standard (de exemplu, folosind variabila "Quantity.Sold..liters.kg.")
    wls_model <- lm(Revenue ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold., data = final_data, weights = 1 / fitted(model)^2)
    
    summary_wls_model <- summary(wls_model)
    print(summary_wls_model)
    
    # Testăm din nou omoscedasticitatea după aplicarea WLS
    cat("\nTestul de omoscedasticitate a erorilor (White) dupa aplicarea WLS:\n")
    white_test_wls <- bptest(wls_model, studentize = TRUE)
    print(white_test_wls)
    
    # Testul Breusch-Pagan
    cat("\nTestul de omoscedasticitate a erorilor (Breusch-Pagan) dupa aplicarea WLS:\n")
    homoscedasticity_test_wls <- bptest(wls_model)
    print(homoscedasticity_test_wls)
    
  } else {
    cat("Ipoteza nulă nu a fost respinsă: Erorile sunt homoscedastice.\n")
  }
  
  
  
  #=================================================IPOTEZA 5========================================
  # Testarea ipotezei 5: Erorile nu sunt autocorelate
  #H0: Erorile nu sunt autocorelate
  #H1: Erorile sunt autocorelate
  #Testul Durbin-Watson
  cat("\nTestul de autocorelare a erorilor (Durbin-Watson):\n")
  durbin_watson_test <- durbinWatsonTest(model)
  print(durbin_watson_test)
  
  cat("\nAcceptam ipoteza nula: Erorile nu sunt autocorelate.\n")
  
  #=================================================IPOTEZA 6========================================
  # Testarea ipotezei 6: Necorelare intre regresor si erorile aleatoare
  #H0: Necorelare intre regresor si erorile aleatoare
  #H1: Corelare intre regresor si erorile aleatoare
  #Testul cor 
  cat("\nTestul de corelatie intre regresor si erorile aleatoare:\n")
  cor_test <- cor.test(data$Quantity.Sold..liters.kg., residuals)
  print(cor_test)
  
  if (cor_test$p.value < 0.05) {
    cat("Ipoteza nulă a fost respinsă: Există corelație între regresor și erorile aleatoare.\n")
    
    #Aplicam GLS
    cat("\nAplicam GLS:\n")
    gls_model <- gls(Revenue ~ ., data = final_data, correlation = corAR1())
    summary_gls_model <- summary(gls_model)
    print(summary_gls_model)
    
  } else {
    cat("Ipoteza nulă nu a fost respinsă: Nu există corelație între regresor și erorile aleatoare.\n")
  }
  
  #=================================================IPOTEZA 7========================================
  # Testarea ipotezei 7: Erorile sunt distribuite normal
  #H0: Erorile sunt distribuite normal
  #H1: Erorile nu sunt distribuite normal
  
  #Testul Jarque-Bera
  cat("\nTestul de normalitate a erorilor (Jarque-Bera):\n")
  jarque_bera_test <- jarque.bera.test(residuals)
  print(jarque_bera_test)
  
  if (jarque_bera_test$p.value < 0.05) {
    cat("Ipoteza nulă a fost respinsă: Erorile nu sunt distribuite normal.\n")
    
    #Eliminam valorile extreme identificate cu distantele Cook
    cat("\nEliminam valorile extreme identificate cu distantele Cook:\n")
    cooks_distance <- cooks.distance(model)
    influential_points <- cooks_distance > 4 / nrow(data)
    data_cleaned <- data[!influential_points, ]
    
    #Reantrenam modelul
    final_data_cleaned <- data.frame(data_cleaned, Revenue = y[!influential_points])
    model_cleaned <- lm(Revenue ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold., data = final_data_cleaned)
    summary_model_cleaned <- summary(model_cleaned)
    print(summary_model_cleaned)
    
    residuals_cleaned <- residuals(model_cleaned)

    
    #Jarque-Bera test
    cat("\nTestul de normalitate a erorilor (Jarque-Bera) dupa eliminarea valorilor extreme:\n")
    jarque_bera_test <- jarque.bera.test(residuals_cleaned)
    print(jarque_bera_test)
    
    if (jarque_bera_test$p.value < 0.05) {
      cat("Ipoteza nulă a fost respinsă: Erorile nu sunt distribuite normal nici după eliminarea valorilor extreme.\n")
      cat("Problema de limitare a datelor. Esantion mare. Teorema limitei centrale ne ajuta in acest sens\n")
      
    } else {
      cat("Ipoteza nulă nu a fost respinsă: Erorile sunt distribuite normal după eliminarea valorilor extreme.\n")
    }
    
    
  } else {
    cat("Ipoteza nulă nu a fost respinsă: Erorile sunt distribuite normal.\n")
  }
  
  #=================================================IPOTEZA 8========================================
  # Testarea ipotezei 8: Multicoliniaritate
  #H0: Nu exista multicoliniaritate
  #H1: Exista multicoliniaritate
  #Testul VIF
  cat("\nTestul de multicoliniaritate (VIF):\n")
  vif_test <- vif(model)
  print(vif_test)
  
  if (max(vif_test) > 10) {
    cat("Ipoteza nulă a fost respinsă: Exista multicoliniaritate.\n")
    
    #Eliminam variabilele cu VIF mare
    cat("\nEliminam variabilele cu VIF mare:\n")
    selected_vars <- c("Quantity.Sold..liters.kg.","Price.per.Unit..sold.","Minimum.Stock.Threshold..liters.kg.","Reorder.Quantity..liters.kg.")
    data_refined <- data[, selected_vars]
    
    # Model de regresie multipla cu variabile semnificative
    final_data <- data.frame(data_refined, Revenue = y)
    model <- lm(Revenue ~ ., data = final_data)
    summary_model <- summary(model)
    
    cat("\nRezumatul modelului de regresie multipla cu variabile semnificative:\n")
    print(summary_model)
    
  } else {
    cat("Ipoteza nulă nu a fost respinsă: Nu exista multicoliniaritate.\n")
  
  }
  return(list(
    model = model,
    data = final_data,
    residuals = residuals_model
  ))
  cat("----------------------------------END----------------------------------\n")
}

result_small <- process_farm_data(small_farms, "Small")
result_medium <- process_farm_data(medium_farms, "Medium")
result_large <- process_farm_data(large_farms, "Large")

model_small <- result_small$model
data_small <- result_small$data
residuals_small <- result_small$residuals

model_medium <- result_medium$model
data_medium <- result_medium$data
residuals_medium <- result_medium$residuals

model_large <- result_large$model
data_large <- result_large$data
residuals_large <- result_large$residuals

enchance_multiple_linear_model <- function(model)
{
  summary_model <- summary(model)
  cat("\nRezumatul modelului de regresie multipla:\n")
  print(summary_model)
}

enchance_multiple_linear_model(model_small)
enchance_multiple_linear_model(model_medium)
enchance_multiple_linear_model(model_large)

compare_farms_models <- function(model_small, model_medium, model_large, data_small, data_medium, data_large) {
  cat("================================= ANALIZA COMPARATIVĂ =================================\n")
  
  # 1. Performanța modelelor
  cat("\n1. Compararea performanței modelelor (RMSE):\n")
  
  # Calcularea RMSE pentru fiecare model
  calc_rmse <- function(model, data) {
    pred <- predict(model, data)
    actual <- data$Revenue
    sqrt(mean((pred - actual)^2))
  }
  
  rmse_small <- calc_rmse(model_small, data_small)
  rmse_medium <- calc_rmse(model_medium, data_medium)
  rmse_large <- calc_rmse(model_large, data_large)
  
  cat("RMSE pentru ferme mici:", rmse_small, "\n")
  cat("RMSE pentru ferme medii:", rmse_medium, "\n")
  cat("RMSE pentru ferme mari:", rmse_large, "\n")
  
  # 2. Compararea coeficienților semnificativi
  cat("\n2. Compararea coeficienților semnificativi din modele:\n")
  print("Ferme mici:")
  print(summary(model_small)$coefficients)
  
  print("Ferme medii:")
  print(summary(model_medium)$coefficients)
  
  print("Ferme mari:")
  print(summary(model_large)$coefficients)
  
  # 3. Testarea diferențelor între modelele reziduurilor folosind ANOVA
  cat("\n3. Testarea diferențelor între modele folosind ANOVA:\n")
  
  # Combinația datelor și reziduurilor pentru ANOVA
  data_small$residuals <- residuals(model_small)
  data_medium$residuals <- residuals(model_medium)
  data_large$residuals <- residuals(model_large)
  
  combined_data <- data.frame(
    Residuals = c(data_small$residuals, data_medium$residuals, data_large$residuals),
    Farm_Type = rep(c("Small", "Medium", "Large"), 
                    times = c(nrow(data_small), nrow(data_medium), nrow(data_large)))
  )
  
  anova_test <- aov(Residuals ~ Farm_Type, data = combined_data)
  print(summary(anova_test))
  
  # 4. Testarea diferențelor dintre medii folosind t-test pentru perechi
  cat("\n4. Testarea diferențelor dintre medii (t-test perechi):\n")
  t_test_small_medium <- t.test(data_small$residuals, data_medium$residuals)
  t_test_medium_large <- t.test(data_medium$residuals, data_large$residuals)
  t_test_small_large <- t.test(data_small$residuals, data_large$residuals)
  
  cat("T-test între ferme mici și medii:\n")
  print(t_test_small_medium)
  
  cat("T-test între ferme medii și mari:\n")
  print(t_test_medium_large)
  
  cat("T-test între ferme mici și mari:\n")
  print(t_test_small_large)
  
  # 5. Vizualizări grafice
  cat("\n5. Vizualizări grafice pentru reziduuri:\n")
  par(mfrow = c(1, 3)) # Setăm layout pentru 3 grafice
  
  hist(data_small$residuals, main = "Ferme Mici", xlab = "Reziduuri", col = "lightblue", border = "black")
  hist(data_medium$residuals, main = "Ferme Medii", xlab = "Reziduuri", col = "lightgreen", border = "black")
  hist(data_large$residuals, main = "Ferme Mari", xlab = "Reziduuri", col = "lightcoral", border = "black")
  
  par(mfrow = c(1, 1)) # Resetăm layout
  
  cat("\n================================= SFÂRȘIT ANALIZĂ =================================\n")
}

compare_farms_models(model_small, model_medium, model_large, data_small, data_medium, data_large)

#Concluzii comparare modele
#1. Modelul pentru fermele mari are cel mai mic RMSE, ceea ce înseamnă că prezice cel mai bine veniturile.
#2. Variabile aditionale: Ferme medii: Total Land Area Acres(seminficatie marginala p=0.056) si Ferme mari : Minimum Stock Threshold liters per kg (marginal semnificativ p=0.063)
# Variabilele Quantity sold si Price per unit sunt predictori puternici si stabili in toate cele 3 modele, dar exista variatii suplimentare pentru fermele medii si mari.
#3. Testul ANOVA arata ca nu exista diferente semnificative intre reziduurile modelelor pentru fermele mici, medii si mari => modelele reprezinta un comportament similar in privinta erorilor.
#4. Testele t-test pentru perechi arata ca nu exista diferente semnificative intre mediile reziduurilor modelelor pentru fermele mici, medii si mari ceea ce confrma stabilitatea modelelor pentru toate tipurile de ferme.
#5. Vizualizarea grafica a reziduurilor arata ca acestea sunt distribuite aproximativ normal pentru toate tipurile de ferme.

choose_best_model <- function(model_small, model_medium, model_large) {
  cat("================================= ALEGEM CEL MAI BUN MODEL =================================\n")
  
  # Calcularea R^2 și R^2 ajustat pentru fiecare model
  calc_r_squared <- function(model) {
    summary_model <- summary(model)
    summary_model$r.squared
  }
  
  r_squared_small <- calc_r_squared(model_small)
  r_squared_medium <- calc_r_squared(model_medium)
  r_squared_large <- calc_r_squared(model_large)
  
  cat("\nR^2 pentru ferme mici:", r_squared_small, "\n")
  cat("R^2 pentru ferme medii:", r_squared_medium, "\n")
  cat("R^2 pentru ferme mari:", r_squared_large, "\n")
  
  # Calcularea R^2 ajustat pentru fiecare model
  calc_adjusted_r_squared <- function(model) {
    summary_model <- summary(model)
    summary_model$adj.r.squared
  }
  
  adjusted_r_squared_small <- calc_adjusted_r_squared(model_small)
  adjusted_r_squared_medium <- calc_adjusted_r_squared(model_medium)
  adjusted_r_squared_large <- calc_adjusted_r_squared(model_large)
  
  cat("\nR^2 ajustat pentru ferme mici:", adjusted_r_squared_small, "\n")
  cat("R^2 ajustat pentru ferme medii:", adjusted_r_squared_medium, "\n")
  cat("R^2 ajustat pentru ferme mari:", adjusted_r_squared_large, "\n")
  
  # Calcularea criteriilor informationale Akaike, Schwarz și Hannan-Quinn
  calc_aic <- function(model) {
    AIC(model)
  }
  
  aic_small <- calc_aic(model_small)
  aic_medium <- calc_aic(model_medium)
  aic_large <- calc_aic(model_large)
  
  cat("\nCriteriul Akaike pentru ferme mici:", aic_small, "\n")
  cat("Criteriul Akaike pentru ferme medii:", aic_medium, "\n")
  cat("Criteriul Akaike pentru ferme mari:", aic_large, "\n")
  
  calc_bic <- function(model) {
    BIC(model)
  }
  
  bic_small <- calc_bic(model_small)
  bic_medium <- calc_bic(model_medium)
  bic_large <- calc_bic(model_large)
  
  cat("\nCriteriul Schwarz pentru ferme mici:", bic_small, "\n")
  cat("Criteriul Schwarz pentru ferme medii:", bic_medium, "\n")
  cat("Criteriul Schwarz pentru ferme mari:", bic_large, "\n")
  
  # Notă: HQIC nu este o funcție standard în R, așa că este necesar să o implementezi sau să verifici dacă pachetul folosit suportă HQIC.
  calc_hq <- function(model) {
    # Înlocuiește cu funcția corectă sau folosește o aproximare
    AIC(model, k = log(length(model$residuals)))
  }
  
  hq_small <- calc_hq(model_small)
  hq_medium <- calc_hq(model_medium)
  hq_large <- calc_hq(model_large)
  
  cat("\nCriteriul Hannan-Quinn pentru ferme mici:", hq_small, "\n")
  cat("Criteriul Hannan-Quinn pentru ferme medii:", hq_medium, "\n")
  cat("Criteriul Hannan-Quinn pentru ferme mari:", hq_large, "\n")
  
  # Alegerea modelului cu cel mai mare R^2 ajustat
  best_model <- model_small
  best_model_label <- "fermele mici"
  best_r_squared <- adjusted_r_squared_small
  
  if (adjusted_r_squared_medium > best_r_squared) {
    best_model <- model_medium
    best_model_label <- "fermele medii"
    best_r_squared <- adjusted_r_squared_medium
  }
  if (adjusted_r_squared_large > best_r_squared) {
    best_model <- model_large
    best_model_label <- "fermele mari"
    best_r_squared <- adjusted_r_squared_large
  }
  
  cat("\nModelul optim este pentru", best_model_label, "cu R^2 ajustat:", best_r_squared, "\n")
  
  return(best_model)
}

best_model <- choose_best_model(model_small, model_medium, model_large)
enchance_multiple_linear_model(best_model)
# Modelul optim este pentru fermele mari conform R^2 ajustat si a criteriilor informationale Akaike, Schwarz și Hannan-Quinn



#Coloanele modelului : $ X                                  : int  0 1 2 3 4 5 6 7 8 9 ...
#$ Location                           : chr  "Telangana" "Uttar Pradesh" "Tamil Nadu" "Telangana" ...
#$ Total.Land.Area..acres.            : num  310.8 19.2 581.7 908 862 ...
#$ Number.of.Cows                     : int  96 44 24 89 21 51 74 77 76 36 ...
#$ Farm.Size                          : chr  "Medium" "Large" "Medium" "Small" ...
#$ Quantity..liters.kg.               : num  222 687 503 823 148 ...
#$ Price.per.Unit                     : num  85.7 42.6 36.5 26.5 83.8 ...
#$ Quantity.Sold..liters.kg.          : int  7 558 256 601 145 74 410 15 860 108 ...
#$ Price.per.Unit..sold.              : num  82.2 39.2 33.8 28.9 83.1 ...
#$ Approx..Total.Revenue.INR.         : num  576 21896 8655 17381 12045 ...
#$ Sales.Channel                      : chr  "Wholesale" "Wholesale" "Online" "Online" ...
#$ Quantity.in.Stock..liters.kg.      : int  215 129 247 222 2 519 347 188 89 277 ...
#$ Minimum.Stock.Threshold..liters.kg.: num  19.6 43.2 15.1 74.5 76 ...
#$ Reorder.Quantity..liters.kg.       : num  64 181.1 140.8 57.7 33.4 ...

#TODO:


#La modelul optim adaugam variabile dummy si termeni de interactiune pentru a analiza comportamentul modelului
#Adaugam variabile dummy
#Adaugam termeni de interactiune

# Model fără termeni de interacțiune

# Model cu termeni de interacțiune
model_with_interaction <- lm(Revenue ~ Price.per.Unit..sold. * Minimum.Stock.Threshold..liters.kg., data = data_large)

# Rezumatul modelului
summary(model_with_interaction)
ggplot(data_large, aes(x = Price.per.Unit..sold., y = Revenue, color = Minimum.Stock.Threshold..liters.kg.)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interacțiunea dintre Preț și Pragul Minim de Stoc",
       x = "Price per Unit Sold",
       y = "Revenue")
#Dupa adaugarea termenului de interactiune, coeficientul pentru termenul de interactiune este semnificativ (p < 0.05),
#Acest lucru sugerează că efectul Price.per.Unit.sold asupra veniturilor variază 
#în funcție de nivelul lui Minimum.Stock.Threshold.liters.kg..
#Totuși, semnificația slabă și coeficientul scăzut (1.006) indică o interacțiune slabă.

#Realizam predictii pentru modelul optim gasit
# Crearea unui nou set de date pentru predicție
new_data <- data.frame(
  Quantity.Sold..liters.kg. = c(500, 600, 700),  # Cantitatea vândută
  Price.per.Unit..sold. = c(50, 55, 60),          # Preț per unitate
  Minimum.Stock.Threshold..liters.kg. = c(70, 80, 90)  # Prag minim de stoc
)

# Utilizarea modelului optim pentru predicții
predicted_revenue <- predict(best_model, newdata = new_data)

# Afișarea predicțiilor
print(data.frame(new_data, Predicted_Revenue = predicted_revenue))

#    Rezultatele Predicției (Predicted Revenue):
#Pentru o cantitate vândută de 500 la un preț de 50, venitul estimat este de 26,677.06.
#Pentru o cantitate vândută de 600 la un preț de 55, venitul estimat este de 33,563.95.
#Pentru o cantitate vândută de 700 la un preț de 60, venitul estimat este de 40,450.83.

#Observații:
  
#  Tendința pozitivă:
#  Veniturile estimate cresc odată cu creșterea cantității vândute și a prețului per unitate, ceea ce este logic și în concordanță cu modelul.
# Influența pragului minim de stoc:
#  Pragul minim de stoc are o influență relativ mai redusă asupra veniturilor (datorită coeficientului mai mic), dar este totuși inclus în estimare.

#Aplicatia 2
dataset2 <- read.csv("aplicatia2_set.csv", header = TRUE, sep = ",")

# Verifică structura dataset-ului
str(dataset2)

# Selectează doar fermele de tip "Large"
large_farms <- subset(dataset2, Farm.Size == "Large")

# Convertirea datei într-un format corespunzător
large_farms$Date <- as.Date(large_farms$Date, format = "%m/%d/%Y")
large_farms$Year <- format(large_farms$Date, "%Y")
# Selectează datele pentru anii doriți (de exemplu, 2019-2021)
years_of_interest <- c("2019", "2020", "2021")
filtered_data <- large_farms %>% filter(format(Date, "%Y") %in% years_of_interest)
# Verifică structura datelor filtrate
str(filtered_data)

# Dacă "Location" este unică, o putem folosi ca ID
if (anyDuplicated(large_farms$Location) == 0) {
  large_farms$ID <- large_farms$Location
} else {
  # Creăm un ID unic pentru fiecare rând
  large_farms$ID <- seq_len(nrow(large_farms))
}

# Confirmăm unicitatea coloanei ID
length(unique(large_farms$ID)) == nrow(large_farms)
# Declarare panel cu ID-ul corect
pdata <- pdata.frame(large_farms, index = c("Location", "Year"))
str(pdata)

dev.off()
coplot(Approx..Total.Revenue.INR. ~ Location|Year, type="l", data=pdata) 
#Heterogenitatea
plotmeans(Approx..Total.Revenue.INR. ~ Location, main = 'Heterogeneitate in randul locatiilor', data = pdata)
plotmeans(Approx..Total.Revenue.INR. ~ Year, main = 'Tendinta veniturilor in timp', data = pdata)
#Model de regresie multipla
ols1_model <- lm(Approx..Total.Revenue.INR. ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold., data = pdata)
summary(ols1_model)
yhat <- ols_model$fitted
ggplot(pdata, aes(x =Approx..Total.Revenue.INR., y =  Quantity.Sold..liters.kg. )) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw()

#Efecte fixe
# Model cu efecte fixe pentru ID-uri
fixed_effects_model <- plm(Approx..Total.Revenue.INR. ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold. + Quantity.in.Stock..liters.kg. + Minimum.Stock.Threshold..liters.kg., data = pdata, index = c("Location", "Year"), model = "within")
summary(fixed_effects_model)
# eliminam variabilele nesemnificative
fixed_effects_model_refined <- plm(Approx..Total.Revenue.INR. ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold. +Minimum.Stock.Threshold..liters.kg. , data = pdata, index = c("Location", "Year"), model = "within")
summary(fixed_effects_model_refined)
pFtest(fixed_effects_model_refined,ols1_model)
#Testul F arata ca modelul cu efecte fixe este semnificativ mai bun decat modelul OLS
#Model cu efecte aleatorii
random_effects_model <- plm(Approx..Total.Revenue.INR. ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold., data = pdata, index = c("Location", "Year"), model = "random")
summary(random_effects_model)

#Test hausman
phtest(fixed_effects_model_refined, random_effects_model)
#Nu avem suficiente dovezi pentru a respinge ipoteza nula, deci putem folosi modelul cu efecte aleatorii


#Testarea efectelor fixe in timp
# Model cu efecte fixe pentru an
fixed_effects_year_model <- plm(Approx..Total.Revenue.INR. ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold. + factor(Year), data = pdata, index = c("Location", "Year"), model = "within")
summary(fixed_effects_year_model)
pFtest(fixed_effects_year_model, fixed_effects_model_refined)
#P-value > 0.05 =>  se recomanda folosirea modelului cu efecte fixe in timp
# Testarea efectelor aleatorii cu Breusch-Pagan Lagrange Multiplier
# Testul ne ajuta sa decidem intre RE si OLS 
pool <- plm(Approx..Total.Revenue.INR. ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold., data = pdata, model = "pooling")
summary(pool)

plmtest(pool, type = c("bp"))
#P-value > 0.05 =>  se recomanda folosirea modelului cu efecte aleatorii

#Testarea dependentei transversale
pcdtest(fixed_effects_model_refined, test = c("lm")) # p-value < 0.05 => exista dependenta transversala
pcdtest(fixed_effects_model_refined, test = c("cd")) # p-value > 0.05 => nu exista dependenta transversala

pbgtest(fixed_effects_model_refined) # p-value > 0.05 => nu avem autocorelare


#Testam heteroschedasticitatea
bptest(Approx..Total.Revenue.INR. ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold. + factor(Location),data=pdata,studentize=FALSE)
#P-value < 0.05 => exista heteroschedasticitate

#Testarea efectelor random
pFtest(random_effects_model,ols1_model)
#P-value < 0.05 => se recomanda folosirea modelului cu efecte random

pbgtest(random_effects_model) # p-value > 0.05 => nu avem autocorelare
bptest(Approx..Total.Revenue.INR. ~ Quantity.Sold..liters.kg. + Price.per.Unit..sold. + factor(Year),data=pdata,studentize=FALSE)
#P-value < 0.05 => exista heteroschedasticitate
