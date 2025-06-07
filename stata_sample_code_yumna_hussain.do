/*==============================================================================
                    YUMNA HUSSAIN - STATA CODING SAMPLE
===============================================================================

This coding sample demonstrates advanced econometric and financial data analysis 
through two complementary projects:

1. FINANCIAL DATA ANALYSIS
   - Constructing firm-level performance metrics (ROIC, productivity, leverage)
   - Data cleaning, winsorization, and summary statistics
   - Aggregation to country-year level and benchmarking with quantiles

2. TIME SERIES ANALYSIS
   - Decomposition of monthly U.S. retail trade series from FRED
   - Extraction of trend, seasonal, and irregular components
   - Visualization of time series components

3. ECONOMETRIC MODELING
   - ARMA model identification using ACF and PACF
   - Model estimation and selection using information criteria
   - Diagnostic testing of residuals and stationarity checks

==============================================================================*/


/*==============================================================================
                    SECTION 1: FINANCIAL DATA ANALYSIS
==============================================================================*/

/*------------------------------------------------------------------------------
    PART 1: Load and Prepare Firm-Level Data from GitHub
------------------------------------------------------------------------------*/

version 18
clear all
set more off

* Load firm-level data directly from GitHub
copy "https://raw.githubusercontent.com/yumnahussain444/stata_coding_sample_data/refs/heads/main/Sampledata.csv" ///
     "Sampledata.csv", replace
import delimited "Sampledata.csv", varnames(1) clear

/*------------------------------------------------------------------------------
    PART 2: Construct Financial Performance Metrics
------------------------------------------------------------------------------*/

// Core profitability metrics
gen invested_capital = wsppe + wscurrentassets - wscurrentliabilities - wscash
sort iso year
by iso: gen invested_capital_lagged = invested_capital[_n-1]
gen roic = wsearningsbeforeinttaxesanddepr / invested_capital_lagged

// Growth and age
by iso (year): gen sales_growth = ((wssalesusd - wssalesusd[_n-1]) / wssalesusd[_n-1]) * 100
gen date_incorp = date(wsdateofincorporation, "DMY")
gen incorporation_year = year(date_incorp)
gen firm_age = year - incorporation_year

// Productivity and financial ratios
gen labor_productivity = wssalesusd / employees
gen leverage_ratio = wstotalltdebt / invested_capital
gen current_ratio = wscurrentassets / wscurrentliabilities
gen quick_ratio = (wscurrentassets - wscash) / wscurrentliabilities

// Normalize performance metrics
gen roic_per_worker = roic / employees
gen sales_asset_ratio = wssalesusd / invested_capital

/*------------------------------------------------------------------------------
    PART 3: Summary Statistics and Benchmarking
------------------------------------------------------------------------------*/

// Winsorize to control for outliers
ssc install winsor2, replace
winsor2 roic labor_productivity, cuts(1 99) replace

// Summary statistics
sum roic sales_growth firm_age labor_productivity leverage_ratio current_ratio

// Demeaning (fixed effects-style)
egen mean_lp_by_country = mean(labor_productivity), by(country)
gen lp_demeaned = labor_productivity - mean_lp_by_country

// ROIC quantile-based comparison
xtile roic_qtile = roic, n(5)
tabstat labor_productivity, by(roic_qtile) stat(mean sd)

/*------------------------------------------------------------------------------
    PART 4: Collapse to Country-Year Level
------------------------------------------------------------------------------*/

// Save mapping of country to ISO code
preserve
keep country iso
duplicates drop
tempfile country_iso_map
save `country_iso_map'
restore

// Collapse and aggregate firm-level data
collapse (median) roic (mean) labor_productivity sales_growth leverage_ratio ///
         current_ratio firm_age (count) n_firms = firm_age, by(country year)

// Merge ISO code back in
merge m:1 country using `country_iso_map'
drop if _merge == 2
drop _merge

// Save the country-year panel
save "Final_Country_Year_Financials.dta", replace

// Export summary stats to RTF
estpost summarize roic sales_growth labor_productivity leverage_ratio current_ratio, detail
esttab using "summary_stats.rtf", ///
    cells("mean sd min max") ///
    title("Summary Statistics – Country-Year Financials") ///
    label replace



/*==============================================================================
                    SECTION 2: TIME SERIES ANALYSIS
==============================================================================*/

/*------------------------------------------------------------------------------
    PART 1: Import and Prepare Monthly Retail Trade Data from FRED
------------------------------------------------------------------------------*/

version 18
clear all
set more off

cd "C:\Users\Yumna Hussain\OneDrive\Desktop\ECON 366\Homework 1\Project 1"
log using "time_series_analysis.log", replace

// Load retail trade data
import fred MRTSSM44000USN, clear

// Time series setup
gen dateq = mofd(daten)
format %tm dateq
tsset dateq

// Log-transform
gen ln_retail = ln(MRTSSM44000USN)

// Time trend variables
gen time = _n
gen t = time
gen quad = t^2

// Generate monthly indicators
gen date = dofm(dateq)
format date %d
gen month_num = month(date)

// Plot original log series
tsline ln_retail, title("Original Retail Trade Series (Log)")
graph export "original_series.png", replace

/*------------------------------------------------------------------------------
    PART 2: Classical Time Series Decomposition
------------------------------------------------------------------------------*/

// Fit full model with trend and seasonality
reg ln_retail t quad i.month_num
predict yhat_full

// Trend-only regressions
reg ln_retail t
predict yhat_linear

reg ln_retail t quad
predict yhat_trend

// Extract seasonal and irregular components
gen seasonal = yhat_full - yhat_trend
gen irregular = ln_retail - yhat_full

// Visualize components
tsline yhat_trend, title("Trend Component")
graph export "trend_component.png", replace

tsline seasonal, title("Seasonal Component")
graph export "seasonal_component.png", replace

tsline irregular, title("Irregular Component")
graph export "irregular_component.png", replace

/*==============================================================================
                    SECTION 3: ECONOMETRIC MODELING
==============================================================================*/

/*------------------------------------------------------------------------------
    PART 1: ARMA Model Identification
------------------------------------------------------------------------------*/

// ACF/PACF diagnostics
ac ln_retail, lags(8) title("ACF of Log Retail Sales")
pac ln_retail, lags(8) title("PACF of Log Retail Sales")
corrgram ln_retail

ac irregular, lags(8) title("ACF of Irregular Component")
pac irregular, lags(8) title("PACF of Irregular Component")

// First-difference for stationarity
gen d_irregular = D.irregular
tsline d_irregular, title("First-Differenced Irregular Component")

// ACF/PACF after differencing
ac d_irregular, lags(8) title("ACF of Differenced Irregular Component")
pac d_irregular, lags(8) title("PACF of Differenced Irregular Component")

/*------------------------------------------------------------------------------
    PART 2: Estimate Candidate ARMA Models
------------------------------------------------------------------------------*/

arima d_irregular, ar(1) ma(1)
estimates store arma1_1

arima d_irregular, ar(1) ma(1/2)
estimates store arma1_2

arima d_irregular, ar(1/2) ma(1)
estimates store arma2_1

arima d_irregular, ar(2) ma(2)
estimates store arma2_2

// Model comparison
estimates stats arma*

/*------------------------------------------------------------------------------
    PART 3: Residual Diagnostics for Best Model
------------------------------------------------------------------------------*/

estimates restore arma1_1   // Adjust if another model is preferred
predict residuals, residuals

// ACF/PACF of residuals
ac residuals, lags(8) title("ACF of Model Residuals")
pac residuals, lags(8) title("PACF of Model Residuals")

// White noise tests
wntestq residuals, lags(8)
wntestb residuals, table

// Plot residuals
tsline residuals, title("ARMA Model Residuals")
graph export "residual_analysis.png", replace


