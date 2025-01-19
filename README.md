# Beta Regression Analysis: The Impact of Study Environment on Student GPA Ratios

This repository contains the R code, data, and analysis for the research project investigating the influence of environmental and individual factors on students’ academic performance, as measured by GPA ratios.

The full study is available on [ResearchGate](https://www.researchgate.net/publication/388112517_The_Impact_of_Study_Environment_on_Student_GPA_Ratios_A_Beta_Regression_Analysis).


## Project Overview

This study models GPA ratios (bounded between 0 and 1) using beta regression, which is well-suited for bounded outcomes exhibiting skewness. The analysis reveals the relationship between study environment factors—such as ventilation, lighting, and noise levels—and academic performance.

## Research Highlights

- **Data**: Survey responses from 171 university students based in Milan and Rome, capturing demographic, environmental, and behavioral factors.
- **Key Findings**:
  - Ventilated study environments and natural sunlight in heated/cooled spaces are positively associated with GPA ratios.
  - The interaction between environmental distractions and ventilation highlights complex effects.
  - Unexpected negative correlations, such as those between adequate desk space and GPA ratios, invite further investigation.
- **Methods**:
  - Distribution fitting and exploratory analysis to assess the suitability of beta regression.
  - Model selection using BIC to identify parsimonious relationships.
  - Residual diagnostics to evaluate model validity.

## Dataset Variables

| Variable             | Type        | Description                                   |
|----------------------|-------------|-----------------------------------------------|
| GPA_ratio            | Continuous  | GPA ratio (0-1)                              |
| Gender               | Categorical | Gender of the student                        |
| Major                | Categorical | Field of study                               |
| study_time           | Categorical | Preferred study time (e.g., Morning, Night)  |
| study_location       | Categorical | Study location preference                    |
| natural_sun_exposure | Binary      | Natural sunlight availability (Yes/No)       |
| noisy_environment    | Binary      | Noise level in the study environment         |
| heated_cooled        | Binary      | Adequate temperature control (Yes/No)        |
| ventilated           | Binary      | Ventilation quality (Yes/No)                 |
| enough_desk_space    | Binary      | Adequate desk space (Yes/No)                 |
| often_distracted     | Binary      | Frequency of distractions (Yes/No)           |
| study_in_group       | Binary      | Group study preference (Yes/No)              |

## Analysis Pipeline

1. **Exploratory Data Analysis**:
   - Normality checks for GPA ratios and key variables.
   - Distribution fitting to assess the beta distribution for GPA ratios.

2. **Hypothesis Testing**:
   - Wilcoxon rank-sum tests for non-parametric comparisons.
   - Interaction analysis of environmental and individual factors.

3. **Regression Models**:
   - Initial linear regression attempts and diagnostics.
   - Transition to beta regression with logit link for bounded outcomes.

4. **Model Diagnostics**:
   - Residual analysis using deviance residuals.
   - Investigating heteroscedasticity and model fit.

## Key Results

- **Significant Predictors**:
  - Ventilation positively influences GPA ratios.
  - Natural sunlight in heated/cooled environments has interactive effects.
  - Being often distracted in ventilated environments has negative effects.

- **Counterintuitive Findings**:
  - Adequate desk space shows a negative association with GPA ratios.
  - Group study is associated with lower GPA ratios.

- **Limitations**:
  - Sample size limits generalizability.
  - Observational nature precludes causal inference.

## Usage Instructions

### Running the Analysis
To reproduce the results:
1. Clone the repository:
   ```bash
   git clone https://github.com/VanniLeonardo/Beta-Regression-Environment
   cd Beta-Regression-Study-Environment
   ```

2. Install the required packages (handled by analysis.R).

3. Execute the analysis script:
    ```bash
    source("analysis.R")
    ```

## Scripts

- analysis.R: Main script containing structured, step-by-step analysis.

## License and Citation

If you use this work, please cite: Notaro, A., & Vanni, L. (2024). The Impact of Study Environment on Student GPA Ratios: A Beta Regression Analysis. Bocconi University Mathematical Statistics Project.

The full study is available on [ResearchGate](https://www.researchgate.net/publication/388112517_The_Impact_of_Study_Environment_on_Student_GPA_Ratios_A_Beta_Regression_Analysis).

This project is licensed under the APACHE 2.0 License. See the [LICENSE](LICENSE) file for more details.

## Contact

For any questions, feel free to contact:
- Vanni Leonardo at leonardo.vanni@studbocconi.it
- Notaro Anna at anna.notaro@studbocconi.it