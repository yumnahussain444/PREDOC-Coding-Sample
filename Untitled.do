/*==============================================================================
                    YUMNA HUSSAIN - STATA CODING SAMPLE
===============================================================================

This sample demonstrates proficiency in STATA for economic and financial data 
analysis through three main sections:

1. FINANCIAL DATA ANALYSIS
   - Constructing financial metrics and performance indicators
   - Data manipulation and statistical analysis
   - Cross-country comparative analysis

2. TIME SERIES ANALYSIS
   - Decomposition of retail trade data
   - Trend, seasonal, and irregular component isolation
   - Visualization of time series components

3. ECONOMETRIC MODELING
   - ARMA model specification and selection
   - Diagnostic testing for model adequacy
   - Residual analysis and interpretation

===============================================================================*/


/*==============================================================================
                        SECTION 1: FINANCIAL DATA ANALYSIS
===============================================================================*/

version 18
clear all
set more off

// Configure environment
cd "C:\Users\Yumna Hussain\OneDrive\Desktop\PREDOC Documents\Coding Sample"
log using "financial_analysis.log", replace

// Import financial data
import delimited "Sampledata.csv", varnames(1) clear

/*------------------------------------------------------------------------------
    PART 1: Financial Metric Construction
------------------------------------------------------------------------------*/

// Construct Return on Invested Capital (ROIC)
// ROIC measures how efficiently a company uses its capital to generate profits
gen invested_capital = wsppe + wscurrentassets - wscurrentliabilities - wscash
sort country year
by country: gen invested_capital_lagged = invested_capital[_n-1]
gen roic = wsearningsbeforeinttaxesanddepr / invested_capital_lagged

// Calculate annual sales growth as percentage
// This metric captures year-over-year business expansion
sort country year
by country: gen sales_growth = ((wscommonstock - wscommonstock[_n-1]) / wscommonstock[_n-1]) * 100

// Determine firm age from incorporation date
// Age is often correlated with firm stability and performance patterns
gen date_incorp = date(wsdateofincorp, "DMY")
gen incorporation_year = year(date_incorp)
gen firm_age = year - incorporation_year

// Calculate labor productivity
// Measures efficiency of human capital utilization
gen labor_productivity = wscommonstock / employees

/*------------------------------------------------------------------------------
    PART 2: Outlier Treatment and Statistical Analysis
------------------------------------------------------------------------------*/

// Generate summary statistics for key variables
sum roic sales_growth firm_age labor_productivity invested_capital, detail

// Winsorize variables to mitigate outlier effects (1% and 99%)
// This preserves data structure while reducing extreme value influence
ssc install winsor2, replace
winsor2 roic labor_productivity, cuts(1 99) replace

// Calculate median financial metrics by country-year
// Creates panel dataset for comparative analysis
preserve
collapse (median) median_roic = roic (median) median_productivity = labor_productivity, by(country year)
tempfile median_metrics
save `median_metrics'
restore

/*------------------------------------------------------------------------------
    PART 3: Cross-Country Data Integration
------------------------------------------------------------------------------*/

// Merge with World Economic Outlook data for macroeconomic context
// First prepare the WEO dataset
preserve
import delimited "WEO_Data.csv", clear
destring v7-v9, replace force
bysort country: gen id = _n
reshape long v, i(country id) j(year)
duplicates drop country year, force
rename v gdp_growth
keep country year gdp_growth
tempfile weo_data
save `weo_data'
restore

// Merge company-level data with macroeconomic indicators
merge m:1 country year using `weo_data'
drop if _merge == 2  // Drop unmatched WEO observations
drop _merge

// Create visualizations for selected economies
// Exploring relationship between firm performance and economic growth
preserve
collapse (median) roic gdp_growth, by(country year)

local countries "China Germany Japan Russian Federation South Africa"
foreach c of local countries {
    twoway (line roic year if country == "`c'", yaxis(1)) ///
           (line gdp_growth year if country == "`c'", yaxis(2)), ///
           title("ROIC and GDP Growth: `c'") ///
           ylabel(, axis(1)) ylabel(, axis(2)) ///
           ytitle("Median ROIC", axis(1)) ///
           ytitle("GDP Growth", axis(2)) ///
           legend(order(1 "Median ROIC" 2 "GDP Growth"))
    graph export "`c'_performance.png", replace
}
restore

log close
/*==============================================================================
                    SECTION 2: TIME SERIES ANALYSIS
==============================================================================*/

version 18
clear all
set more off

// Configure environment
cd "C:\Users\Yumna Hussain\OneDrive\Desktop\ECON 366\Homework 1\Project 1"
log using "time_series_analysis.log", replace

/*------------------------------------------------------------------------------
    PART 1: Data Preparation and Exploration
------------------------------------------------------------------------------*/

// Import monthly retail trade data from FRED
import fred MRTSSM44000USN, clear

// Transform date variables for time series analysis
gen dateq = mofd(daten)             
format %tm dateq                    
tsset dateq            

// Apply log transformation to stabilize variance
gen ln_retail = ln(MRTSSM44000USN) 

// Create time trend variables for decomposition
gen time = _n
gen t = time
gen quad = t^2  

// Generate month indicators for seasonal analysis
gen date = dofm(dateq)              
format date %d                       
gen month_num = month(date)         

// Visualize original series
tsline ln_retail, title("Original Retail Trade Series (Log)")  
graph export "original_series.png", replace   

/*------------------------------------------------------------------------------
    PART 2: Time Series Decomposition
------------------------------------------------------------------------------*/

// Estimate full model with trend and seasonal components
reg ln_retail t quad i.month_num   
predict yhat_full      

// Estimate trend-only models for comparison
// Linear trend specification
reg ln_retail t
predict yhat_linear

// Quadratic trend specification
reg ln_retail t quad               
predict yhat_trend                  

// Extract individual components
// Seasonal component = difference between full fit and trend-only fit
gen seasonal = yhat_full - yhat_trend 

// Irregular component = residuals from full model
gen irregular = ln_retail - yhat_full 

// Visualize decomposed components
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

// Examine autocorrelation structure of original series
ac ln_retail, lags(8) title("ACF of Log Retail Sales")
pac ln_retail, lags(8) title("PACF of Log Retail Sales")
corrgram ln_retail

// Analyze irregular component for ARMA modeling
ac irregular, lags(8) title("ACF of Irregular Component")
pac irregular, lags(8) title("PACF of Irregular Component")

// First-differencing for stationarity
gen d_irregular = D.irregular
tsline d_irregular, title("First-Differenced Irregular Component")

// Examine autocorrelation structure of differenced series
ac d_irregular, lags(8) title("ACF of Differenced Irregular Component")
pac d_irregular, lags(8) title("PACF of Differenced Irregular Component")

/*------------------------------------------------------------------------------
    PART 2: Model Estimation and Selection
------------------------------------------------------------------------------*/

// Estimate candidate ARMA models
arima d_irregular, ar(1) ma(1)
estimates store arma1_1

arima d_irregular, ar(1) ma(1/2)
estimates store arma1_2

arima d_irregular, ar(1/2) ma(1)
estimates store arma2_1

arima d_irregular, ar(2) ma(2)
estimates store arma2_2

// Compare models using information criteria
estimates stats arma*

/*------------------------------------------------------------------------------
    PART 3: Diagnostic Testing
------------------------------------------------------------------------------*/

// Restore best model based on information criteria
estimates restore arma1_1  // Adjust based on actual results

// Extract and analyze residuals
predict residuals, residuals

// Check residual autocorrelation
ac residuals, lags(8) title("ACF of Model Residuals")
pac residuals, lags(8) title("PACF of Model Residuals")

// Formal tests for white noise residuals
wntestq residuals, lags(8)
wntestb residuals, table

// Plot residuals for visual inspection
tsline residuals, title("ARMA Model Residuals")
graph export "residual_analysis.png", replace

log close