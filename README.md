*====================================================================
				YUMNA HUSSAIN - STATA CODE SAMPLE 
======================================================================

This file contains three sections:
1. Financial Data Analysis - Variable construction and data manipulation
2. Cross-Country Analysis - Data merging and preparation for inequality analysis

====================================================================*/


/*====================================================================
				SECTION 1: FINANCIAL DATA ANALYSIS
====================================================================*/

version 18
clear all
set more off

// Set working directory and start logging
cd "C:\Users\Yumna Hussain\OneDrive\Desktop\PREDOC Documents\Coding Sample"
log using "Sampledata.log", replace

// Import Sampledata_RA dataset
import delimited "C:\Users\Yumna Hussain\OneDrive\Desktop\PREDOC Documents\Coding Sample\Sampledata.csv", varnames(1) clear

***************************************************************
						* QUESTION 1
***************************************************************

// 1a - Create Return on Invested Capital (ROIC) variable
gen invested_capital = wsppe + wscurrentassets - wscurrentliabilities - wscash

// Use previous year's invested capital for calculation
sort country year
by country: gen invested_capital_lastyear = invested_capital[_n-1]

// Calculate ROIC as a decimal
gen roic = wsearningsbeforeinttaxesanddepr / invested_capital_lastyear

// 1b - Calculate annual sales growth (percentage)
sort country year
by country: gen sales_growth = ((wscommonstock - wscommonstock[_n-1]) / wscommonstock[_n-1]) * 100

// 1c - Calculate firm age from incorporation year
gen date_incorp = date(wsdateofincorp, "DMY")
gen incorporation_year = year(date_incorp)
gen age = year - incorporation_year

// 1d - Calculate labor productivity
gen labor_productivity = wscommonstock / employees

***************************************************************
						* QUESTION 2
***************************************************************

// Summary statistics for all created variables
sum roic sales_growth age labor_productivity invested_capital wsearningsbeforeinttaxesanddepr ///
    wscurrentliabilities wscash wscommonstock employees, detail

***************************************************************
						* QUESTION 3
***************************************************************

// Winsorize ROIC and Labor Productivity at 1% and 99%
ssc install winsor2
winsor2 roic labor_productivity, cuts(1 99) replace

***************************************************************
						* QUESTION 4
***************************************************************

// Calculate median ROIC by country and year
preserve
collapse (median) median_roic = roic, by(country year)

// Save collapsed dataset temporarily
tempfile median_roic
save `median_roic'

restore

***************************************************************
						* QUESTION 5
***************************************************************

// Import WEO dataset
import delimited "C:\Users\Yumna Hussain\OneDrive\Desktop\PREDOC Documents\Coding Sample\WEO_Data.csv", clear

// Check for duplicates in country column
duplicates list country

// Convert string variables to numeric format
destring v7, replace force
destring v8, replace force
destring v9, replace force

// Create a unique identifier for each country observation
bysort country: gen id = _n

// Reshape data to long format with years
reshape long v, i(country id) j(year)

// Ensure unique country-year combinations
duplicates drop country year, force

// Save cleaned WEO dataset
save weo_reshaped.csv, replace

// Reload Sampledata and merge with reshaped WEO dataset
import delimited "C:\Users\Yumna Hussain\OneDrive\Desktop\PREDOC Documents\Coding Sample\Sampledata.csv", clear
duplicates list country year
merge m:1 country year using weo_reshaped.csv

***************************************************************
						* QUESTION 6
***************************************************************

import delimited "C:\Users\Yumna Hussain\OneDrive\Desktop\PREDOC Documents\Coding Sample\Sampledata.csv", clear
merge m:1 country year using weo_reshaped.csv
drop if _merge == 2  // Drop unmatched WEO data
drop _merge

// Ensure ROIC is available
capture confirm variable roic
if _rc != 0 {
    gen invested_capital = wsppe + wscurrentassets - wscurrentliabilities - wscash
    sort country year
    by country: gen invested_capital_lastyear = invested_capital[_n-1]
    gen roic = wsearningsbeforeinttaxesanddepr / invested_capital_lastyear
}

preserve
collapse (median) roic v, by(country year)


// Define list of selected countries
local countries "China Germany Japan Russian Federation South Africa"

// Generate graphs for each country
foreach c of local countries {
    twoway (line roic year if country == "`c'", yaxis(1)) ///
           (line v year if country == "`c'", yaxis(2)), ///
           title("ROIC and GDP Growth: `c'") ///
           ylabel(, axis(1)) ylabel(, axis(2)) ///
           ytitle("Median ROIC", axis(1)) ///
           ytitle("GDP Growth", axis(2)) ///
           legend(order(1 "Median ROIC" 2 "GDP Growth"))
    
    graph export "`c'_graph.png", replace
}

restore

/*====================================================================
		SECTION 2: CROSS-COUNTRY INEQUALITY ANALYSIS
====================================================================*/

version 18
clear all
set more off

// Set working directory
cd "C:\Users\Yumna Hussain\OneDrive\Desktop\PREDOC Documents\Coding Sample"
log using "inequality_analysis.log", replace

// Import and reshape Gini coefficient data from wide to long format
import delimited "C:\Users\Yumna Hussain\OneDrive\Desktop\PREDOC Documents\Coding Sample\gini (4).csv", varnames(1) clear
reshape long yr_, i(country)
gen entity = country
rename _j year
save "C:\Users\Yumna Hussain\OneDrive\Desktop\PREDOC Documents\Coding Sample\gini_long.dta", replace

// Import GDP per capita data 
import delimited "C:\Users\Yumna Hussain\OneDrive\Desktop\PREDOC Documents\Coding Sample\national-poverty-lines-vs-gdp-per-capita (3).csv", clear 

// Keep only observations with non-missing GDP values
keep if gdppercapitapppconstant2017inter != .

// Merge GDP and Gini datasets on country and year
merge 1:1 entity year using "C:\Users\Yumna Hussain\OneDrive\Desktop\PREDOC Documents\Coding Sample\gini_long.dta"

// Keep only complete observations with both GDP and Gini data
keep if _merge == 3
drop _merge

// Calculate log of GDP per capita for analysis
gen log_gdp = ln(gdppercapitapppconstant2017inter)

// Calculate year-over-year growth rates for countries
sort entity year
by entity: gen gdp_growth = (gdppercapitapppconstant2017inter/gdppercapitapppconstant2017inter[_n-1] - 1)*100 if year == year[_n-1] + 1

// Save the merged dataset for further analysis
save "C:\Users\Yumna Hussain\OneDrive\Desktop\PREDOC Documents\Coding Sample\inequality_merged.dta", replace

log close
