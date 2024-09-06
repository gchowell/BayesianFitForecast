# BayesianFitForecast

**BayesianFitForecast** is an R toolbox designed to streamline Bayesian parameter estimation and forecasting in ordinary differential equation (ODE) models, particularly for infectious disease applications. This toolbox simplifies the complex process of Bayesian inference by automating the generation of Stan files, enabling users to configure models, define priors, and analyze results efficiently, even with minimal programming expertise.

## Features

- **Automated Stan File Generation**: Automatically generate Stan files based on user-defined ODE models, eliminating the need to write code manually.
- **Bayesian Parameter Estimation**: Estimate model parameters using Bayesian inference, incorporating uncertainty and prior knowledge, which is particularly valuable in contexts with limited or noisy data.
- **Model Performance Evaluation**: IIncludes comprehensive tools for evaluating model performance, such as convergence diagnostics, posterior distributions, credible intervals, and advanced performance metrics (e.g., Mean Absolute Error, Mean Squared Error, and Weighted Interval Score).
- **Versatility**: Applicable to a wide range of scientific fields, with demonstrated use cases including the analysis of the 1918 influenza pandemic in San Francisco using different error structures (e.g., Poisson and negative binomial) within the SEIR model.
- **Tutorial Resources**: Includes a detailed tutorial and video guide to help users get started with the toolbox.

## Video tutorial

https://www.youtube.com/watch?v=jnxMjz3V3n8

## Usage

### 1. Configuring Your Model

Start by defining your ODE model, parameters, and priors in the `options_generaluse.R` file. This file allows you to specify the model variables, parameters, error structures, and priors.

### 2. Running the Model

After configuring your model, use the `run_MCMC.R` script to fit the model to your data. This script performs the Bayesian parameter estimation using the Stan files automatically generated by the toolbox.

### 3. Analyzing Results

Once the model fitting is complete, use the `run_analyzeResults.R` script to generate and analyze the results. This script will produce various outputs, including Excel files with the parameter estimates, performance metrics, and convergence diagnostics.

### 4. Example Use Case

For a demonstration of the toolbox’s capabilities, refer to the example provided in the repository, which analyzes the 1918 influenza pandemic in San Francisco using the SEIR model. The example showcases the toolbox's ability to handle real-world epidemic data, offering insights into model calibration and forecasting performance.

## License

This project is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0). See the LICENSE file for details.

## Citation

If you use this toolbox in your research, please cite the following paper:

Karami, H., Bleichrodt, A., Luo, R., & Chowell, G. (2024). BayesianFitForecast: An R toolbox for parameter estimation and forecasting with quantified uncertainty in ordinary differential equation models. Under review.



