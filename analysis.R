# =============================================================================
# Statistical Analysis of Academic Performance Factors
# Authors: Anna Notaro and Leonardo Vanni
# =============================================================================

#' Detect if the user is on Windows or Linux or Mac:
if (.Platform$OS.type == "windows") {
    # Windows
    x11 <- function() {
        windows()
    }
} else {
    # Linux or macOS
    x11 <- function() {
        X11()
    }
}

#' Setup required packages for analysis
#' @description Installs and loads necessary packages for the analysis
setup_packages <- function() {
    packages <- c(
        "fitdistrplus", "betareg", "statmod", "MASS", "lmtest",
        "car", "dplyr", "ggplot2", "purrr"
    )

    new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
    if (length(new_packages)) install.packages(new_packages)

    invisible(lapply(packages, library, character.only = TRUE))
}

#' Load and preprocess the dataset
#' @param filepath Path to the CSV data file
#' @return Preprocessed dataframe with proper factor conversions
load_data <- function(filepath) {
    data <- read.csv(filepath)

    # Convert relevant columns to factors
    factor_columns <- c(
        "Gender", "Major", "study_location", "study_in_group",
        "enough_sleep", "noisy_environment", "heated_cooled",
        "ventilated", "enough_desk_space", "often_distracted",
        "natural_sun_exposure"
    )

    data[factor_columns] <- lapply(data[factor_columns], as.factor)
    return(data)
}

#' Analyze GPA distribution
#' @param data Dataset containing GPA_ratio column
#' @description Creates distribution plots and performs normality tests
analyze_gpa_distribution <- function(data) {
    summary(data$GPA_ratio)

    x11() # Open a new graphics window for Linux/macOS
    hist(data$GPA_ratio,
        main = "Distribution of GPA Ratio",
        xlab = "GPA Ratio",
        breaks = 20
    )
    readline(prompt = "Press Enter to continue...")

    x11()
    qqnorm(data$GPA_ratio)
    qqline(data$GPA_ratio)
    readline(prompt = "Press Enter to continue...")

    shapiro_test <- shapiro.test(data$GPA_ratio)
    print("Shapiro-Wilk test results:")
    print(shapiro_test)

    fit <- fitdist(data$GPA_ratio, "beta", method = "mle")
    x11()
    plot(fit)
    readline(prompt = "Press Enter to continue...")

    return(fit)
}

#' Create demographic visualizations
#' @param data Dataset containing demographic information
#' @description Generates boxplots for GPA by different demographic factors
create_demographic_plots <- function(data) {
    # Boxplot of GPA ratio by Gender
    x11()
    p1 <- ggplot(data, aes(x = Gender, y = GPA_ratio, fill = Gender)) +
        geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
        labs(title = "GPA Ratio by Gender", x = "Gender", y = "GPA Ratio") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    print(p1)
    readline(prompt = "Press Enter to continue...")

    # Boxplot of GPA ratio by Major
    x11()
    p2 <- ggplot(data, aes(x = Major, y = GPA_ratio, fill = Major)) +
        geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
        labs(title = "GPA Ratio by Major", x = "Major", y = "GPA Ratio") +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
        )
    print(p2)
    readline(prompt = "Press Enter to continue...")

    # Boxplot of GPA ratio by Study Time
    x11()
    p3 <- ggplot(data, aes(x = study_time, y = GPA_ratio, fill = study_time)) +
        geom_boxplot(outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
        labs(title = "GPA Ratio by Study Time", x = "Study Time", y = "GPA Ratio") +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
        )
    print(p3)
    readline(prompt = "Press Enter to continue...")
}

#' Perform environmental factor analysis
#' @param data Dataset containing environmental variables
#' @return List containing test results and correlations
analyze_environmental_factors <- function(data) {
    # Define environmental variables
    env_vars <- c(
        "natural_sun_exposure", "noisy_environment", "ventilated",
        "enough_desk_space", "heated_cooled"
    )

    # Calculate correlations
    correlations <- map_df(env_vars, function(var) {
        cor_test <- cor.test(as.numeric(data[[var]]), data$GPA_ratio,
            method = "spearman", exact = FALSE
        )
        tibble(
            variable = var,
            correlation = cor_test$estimate,
            p_value = cor_test$p.value
        )
    })

    # Create study environment indicator
    data$study_environment <- ifelse(
        data$enough_desk_space == 1 &
            data$natural_sun_exposure == 1 &
            data$noisy_environment == 0 &
            data$ventilated == 1 &
            data$heated_cooled == 1,
        "Good", "Bad"
    )

    # Perform Wilcoxon test
    env_test <- wilcox.test(GPA_ratio ~ study_environment, data = data)

    return(list(
        correlations = correlations,
        environment_test = env_test
    ))
}

#' Perform linear regression analysis and assumptions verification
#' @param data Dataset for modeling
#' @return List containing model results and diagnostics
fit_linear_regression <- function(data) {
    # Fit full linear model
    full_model <- lm(
        GPA_ratio ~ Gender + enough_sleep + study_time +
            study_location + natural_sun_exposure + noisy_environment +
            heated_cooled + ventilated + enough_desk_space +
            often_distracted + study_in_group +
            natural_sun_exposure:heated_cooled +
            natural_sun_exposure:ventilated +
            heated_cooled:ventilated +
            noisy_environment:often_distracted +
            noisy_environment:study_in_group +
            enough_desk_space:study_in_group +
            often_distracted:study_in_group,
        data = data
    )

    # Perform stepwise regression using BIC
    step_model <- stepAIC(full_model, direction = "both", k = log(nrow(data)))

    # Print BIC comparison
    cat("\nBIC Comparison:\n")
    cat("Full model BIC:", BIC(full_model), "\n")
    cat("Step model BIC:", BIC(step_model), "\n\n")

    # Check model assumptions
    # Normality test
    shapiro_test <- shapiro.test(step_model$residuals)
    cat("Shapiro-Wilk normality test:\n")
    print(shapiro_test)

    # Visual diagnostics
    x11()
    par(mfrow = c(2, 2))
    # QQ plot
    qqnorm(step_model$residuals)
    qqline(step_model$residuals)
    # Residuals vs Fitted
    plot(predict(step_model), step_model$residuals,
        xlab = "Fitted values", ylab = "Residuals",
        main = "Residuals vs Fitted"
    )
    abline(h = 0, col = "red")
    # Scale-Location plot
    plot(predict(step_model), sqrt(abs(step_model$residuals)),
        xlab = "Fitted values", ylab = "sqrt(|Residuals|)",
        main = "Scale-Location Plot"
    )
    # Residuals vs Leverage
    plot(hatvalues(step_model), step_model$residuals,
        xlab = "Leverage", ylab = "Residuals",
        main = "Residuals vs Leverage"
    )
    readline(prompt = "Press Enter to continue...")

    # Homoscedasticity test
    bp_test <- bptest(step_model)
    cat("\nBreusch-Pagan test for heteroscedasticity:\n")
    print(bp_test)

    return(list(
        full_model = full_model,
        step_model = step_model,
        normality_test = shapiro_test,
        heteroscedasticity_test = bp_test
    ))
}

#' Fit and evaluate beta regression model
#' @param data Dataset for modeling
#' @return List containing model results and diagnostics
fit_beta_regression <- function(data) {
    # Fit initial full model
    beta_model <- betareg(
        GPA_ratio ~ enough_sleep + often_distracted + natural_sun_exposure +
            heated_cooled + ventilated + noisy_environment +
            enough_desk_space + study_in_group +
            natural_sun_exposure:heated_cooled + often_distracted:ventilated +
            natural_sun_exposure:ventilated,
        data = data,
        link = "logit",
        link.phi = "identity"
    )

    # Fit BIC-selected model
    beta_model_BIC <- betareg(
        GPA_ratio ~ heated_cooled + ventilated +
            enough_desk_space + study_in_group +
            natural_sun_exposure:heated_cooled + often_distracted:ventilated +
            natural_sun_exposure:ventilated,
        data = data,
        link = "logit",
        link.phi = "identity"
    )

    # Compare models using likelihood ratio test
    lr_test <- lrtest(beta_model, beta_model_BIC)

    # Print model comparisons
    cat("\nOriginal model BIC:", BIC(beta_model))
    cat("\nBIC model BIC:", BIC(beta_model_BIC))
    cat("\n\nLikelihood Ratio Test:\n")
    print(lr_test)

    # Residual analysis
    res <- residuals(beta_model_BIC, type = "quantile")
    dev_res <- residuals(beta_model_BIC, type = "deviance")

    # Diagnostic plots
    x11()
    par(mfrow = c(2, 2))

    # Plot 1: Deviance residuals vs fitted
    plot(fitted(beta_model_BIC), dev_res,
        main = "Deviance Residuals vs Fitted",
        xlab = "Fitted values", ylab = "Deviance residuals"
    )
    abline(h = 0, col = "red", lty = 2)

    # Plot 2: Q-Q plot
    qqnorm(dev_res, main = "Q-Q Plot of Deviance Residuals")
    qqline(dev_res, col = "red")

    # Plot 3: Residuals vs index
    plot(dev_res,
        type = "p",
        main = "Residuals vs Index",
        xlab = "Index", ylab = "Deviance residuals"
    )
    abline(h = 0, col = "red", lty = 2)

    # Plot 4: Scale-Location plot
    plot(fitted(beta_model_BIC), sqrt(abs(dev_res)),
        main = "Scale-Location Plot",
        xlab = "Fitted values",
        ylab = "sqrt(|Deviance residuals|)"
    )
    readline(prompt = "Press Enter to continue...")

    # Check normality of residuals
    shapiro_test <- shapiro.test(dev_res)
    cat("\nShapiro-Wilk test for normality of residuals:\n")
    print(shapiro_test)

    # Check for multicollinearity
    vif_results <- vif(beta_model_BIC)
    cat("\nVariance Inflation Factors:\n")
    print(vif_results)

    return(list(
        full_model = beta_model,
        bic_model = beta_model_BIC,
        lr_test = lr_test,
        residuals = res,
        deviance = dev_res,
        shapiro_test = shapiro_test,
        vif = vif_results
    ))
}

# Main execution
main <- function() {
    setup_packages()
    data <- load_data("dataset.csv")

    # Perform analyses
    dist_analysis <- analyze_gpa_distribution(data)
    create_demographic_plots(data)
    env_results <- analyze_environmental_factors(data)
    linear_results <- fit_linear_regression(data)
    beta_results <- fit_beta_regression(data)

    # Print results
    cat("\n=== Environmental Analysis Results ===\n")
    print("Environmental Correlations:")
    print(env_results$correlations)
    print("\nWilcoxon Test for Study Environment:")
    print(env_results$environment_test)

    cat("\n=== Linear Regression Results ===\n")
    print("Linear Regression Model Summary:")
    print(summary(linear_results$step_model))

    cat("\n=== Beta Regression Results ===\n")
    print("Full Beta Regression Model:")
    print(summary(beta_results$full_model))

    print("\nBIC-selected Beta Regression Model:")
    print(summary(beta_results$bic_model))

    print("\nLikelihood Ratio Test (Full vs BIC Model):")
    print(beta_results$lr_test)

    print("\nShapiro-Wilk Test for Residuals:")
    print(beta_results$shapiro_test)

    print("\nVariance Inflation Factors:")
    print(beta_results$vif)
}

main()
