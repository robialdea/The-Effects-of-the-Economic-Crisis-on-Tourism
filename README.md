# The Effects of the Economic Crisis on Tourism

This project analyzes the interdependencies between key macroeconomic indicators and the dynamics of tourism in the context of the global economic crisis. The analysis is based on quarterly data (2008–2024) for six variables:

- **Gross Domestic Product (GDP)**
- **Inflation**
- **Unemployment rate**
- **Exchange rate**
- **Tourist arrivals**
- **Tourism share in GDP**

## Methodology
- Time series preprocessing: deseasonalization (STL), standardization, and stationarity testing (ADF, KPSS).
- Long-term equilibrium tested through the **Johansen cointegration test**.
- Estimation of **VAR** and **VECM models** to capture both short-term dynamics and long-term equilibria.
- Diagnostic checks: Portmanteau (autocorrelation), ARCH (heteroscedasticity), Jarque–Bera (normality).
- Causality analysis via **Granger tests**.
- Dynamic interactions explored through **Impulse Response Functions (IRF)** and **Forecast Error Variance Decomposition (FEVD)**.
- Forecasting with confidence intervals illustrated through **fan charts**.

## Key Findings
- **GDP and inflation** act as central drivers, significantly influencing tourism dynamics.  
- **Tourist arrivals** and tourism’s share in GDP adjust towards long-term equilibrium when deviations occur.  
- **Unemployment** and the exchange rate show weaker predictive power, with short-lived effects.  
- Shocks from GDP and inflation have durable and consistent effects on tourism, while shocks from unemployment or exchange rate fade more quickly.  

---

This repository demonstrates how multivariate econometric models (VAR/VECM) can be applied to capture the complex interactions between macroeconomic variables and tourism, offering both analytical insights and short-term forecasts.




# The Effects of the Economic Crisis on Tourism
## Data Source

The multivariate analysis uses a dataset composed of six variables: unemployment rate, fixed-base gross domestic product index (2015=100), number of tourist arrivals relative to the population of the corresponding year, exchange rate, and fixed-base inflation index (2015=100). The time interval for the analysis is organized quarterly, covering the period between the first quarter of 2008 and the last quarter of 2024. In the case of monthly data, conversion operations (sum and average) were applied to transform them into quarterly data. For annual data, such as the share of tourism in GDP, the conversion into quarterly information was performed by weighting with quarterly GDP, in order to achieve a more accurate distribution. Data sources include information extracted from Eurostat and INSEE.

## Preprocessing

The time series corresponding to visitor arrivals shows a strong seasonal influence, which can negatively affect the multivariate analysis. Therefore, to improve accuracy, the “Seasonal-Trend decomposition using Loess” (STL) method was applied. STL efficiently separates the seasonal, trend, and noise components of time series. In the R programming language implementation, the option "periodic" was used for "s.window" to reflect recurring seasonality, after which the seasonal component was removed using the seasadj() function, resulting in the deseasonalized variable sosiri_desez. This transformation reduces the risk that seasonality may distort the relationships among the analyzed variables.

## Multivariate Methodology

As an initial step, the data were preprocessed in quarterly format, followed by conversion into time series. To eliminate seasonality, the Seasonal-Trend decomposition using Loess was applied, along with standardization to ensure compatibility among the analyzed variables. To test the stationarity of each series, the Augmented Dickey-Fuller (ADF) test was used, with differencing applied to integrated series when necessary. The Johansen test was employed to highlight equilibrium relationships between economic and tourism factors over the long term, revealing the existence of cointegration. Consequently, the identified relationship structure, along with a constant, was considered for estimating the VAR(1) and VECM models.

The validity of the models was assessed through diagnostic tests: the Portmanteau test for autocorrelation, the ARCH test for detecting heteroscedasticity of residuals, and the Jarque–Bera test for validating normality. The interaction between the analyzed factors was examined through Impulse Response Functions (IRF) and Forecast Error Variance Decomposition (FEVD), which illustrate the impulse response and forecast error decomposition, applied over a 12-quarter horizon. Furthermore, the influence among variables was tested through Granger causality relationships. Ultimately, the resulting model enabled the development of a short-term forecast of tourism dynamics within the broader economic context.
