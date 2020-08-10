# Documentation for the FOREIGN EXCHANGE RATES CONVERTER
by [JC](https://github.com/corderoai?tab=repositories)

**August 7th, 2020**

The foreign exchange market (Forex) is a global non-centralized market for trading currencies. The dynamics of this market automatically determine the **foreign exchange rates** for all currencies. It is actually the largest market in the world of trading. 
An foreign exchange rate (FER) is the amount of change that one currency will be exchanged for another [[1]](https://en.wikipedia.org/wiki/Exchange_rate). In other words, it is the value of one country's currency in relation to another currency. However, despite of many skeptiscim by many world renamed economists the exchange rates can be changed by each country authority. It is very important to keep track of the changes that different currencies are taking in real time in order to perform a good trading task. Moreover, historical Data about currency exchange rates is the basis for making FOREX analysis and perhaps predictions. 

In this shyny app we show different currency exchange rates accross time for many countries. The Data was obtained using a dataset from the [Federal Reserve Bank of Saint Louis](https://fred.stlouisfed.org) that was stored at [https://datahub.io/core/exchange-rates](https://datahub.io/core/exchange-rates). 

This app can be found at: [https://cordero.shinyapps.io/FOREXAPP/](https://cordero.shinyapps.io/FOREXAPP/)

## Data Processing

As stated in [here](https://datahub.io/core/exchange-rates#readme) the Data is for the Foreign exchange rates from US Federal Reserve in daily, monthly and yearly basis. The following country currencies have USD/currency ratio (UF):

- Australia
- Euro
- Ireland
- New Zealand
- United Kingdom

The rest of countries have currency/USD ratio (FU). Each tupple comprisses the following columns:


 Field Name |	Order | Type (Format) | Description  
 --- | --- | --- | --- 
Date 	     | 1 	    | date (%Y-%m-%d)  |	Date in ISO format. 
Country 	   | 2 	    | string (default) |	Name of a given country. 
Value 	     | 3 	    | number (default) |	Foreign Exchange Rate (FER) to USD. Only AUD, IEP, NZD, GBP and EUR to USD.

Notice that the Data only includes registries for only 22 currencies out of 291 according to the [ISO4217](https://en.wikipedia.org/wiki/ISO_4217) list. We assume that only one currency corresponds to each country in the foreign exchange dataset.
For simplicity the Data is stored in three files (a year value is the mean of the months, a month value is the mean for the days of that month):

- [daily.csv (6MB)](https://datahub.io/core/exchange-rates/r/daily.csv)
- [monthly.csv (408kB)](https://datahub.io/core/exchange-rates/r/monthly.csv)
- [yearly.csv (22kB)](https://datahub.io/core/exchange-rates/r/yearly.csv)

### Data Conversion

Since the Data has two possible conversions (foreign exchange rate to USD and USD to foreign exchange rate) and due to the fact that we would like to provide a two sided coversion, we calculate both cases:

**FER to USD (FU): 1 (USD) / Value(USD/FER).**

**USD to FER (UF): 1 (FER) / Value(FER/USD).**

The following code in [R](https://www.r-project.org/) was used to make and store the conversion rates (FU, UF):

```
calculateFURate <- function(dfRow){
    if(dfRow["Country"] %in% c("Australia","Euro","Ireland","New Zealand","United Kingdom"))
    {
        return(1/as.numeric(dfRow["Value"]))
    }   
    else
    {
        return(as.numeric(dfRow["Value"]))
    }    
}

calculateUFRate <- function(dfRow){
    if(dfRow["Country"] %in% c("Australia","Euro","Ireland","New Zealand","United Kingdom"))
    {
        return(as.numeric(dfRow["Value"]))
    }   
    else
    {
        return(1/as.numeric(dfRow["Value"]))
    }    
}

populateRatesOfCurrencies <- function(dfER){
        
       dfER$FU <- apply(dfER, 1, calculateFURate)
       dfER$UF <- apply(dfER, 1, calculateUFRate)
       
        return(dfER)
    }    

countrieYearlyERFUUF <- populateRatesOfCurrencies(countrieYearlyER)
```
In order to display the proper currency name according to the country it belongs to, we make use of another dataset called ISO 4217 Currency Codes which is available at [https://datahub.io/core/currency-codes](https://datahub.io/core/currency-codes). The structure of the dataset is the following:

| Field Name | Order | Type (Format) | Description | 
| ---------- | ----- | ------------- | ----------- |   
| Entity | 1 | string | Country or region name
| Currency | 2 | string | Name of the currency
| AlphabeticCode | 3 | string | 3 digit alphabetic code for the currency
| NumericCode | 4 | number | 3 digit numeric code
| MinorUnit | 5 | string |  	
| WithdrawalDate | 6 | string | Date currency withdrawn (values can be ranges or months)

Thereafter, we join the currency names and alphabetic currency codes with the country names and its abbreviations (using the `countrycode()` function) in the ui.R file: 

```
countriesER <- read.csv("./data/yearly_csv.csv")
currencyCodes <- read.csv("./data/codes-all_csv.csv")
listOfCountries <- unique(countriesER$Country)
finalCurrencyDf <- data.frame(countryName=character(), 
                                countryAbbreviation=character(),
                                currencyName=character(),	
                                currencyCode=character() 
)
for(country in  listOfCountries)
{
    if(country != "Euro" & country != "South Korea" & country != "Taiwan")
    {	
        coname <- country
        abbrv <- countrycode(country, origin = 'country.name', destination = 'iso3c')
        currN <- fn$sqldf("SELECT Currency AS \"currencyName\"
				        FROM currencyCodes
				        WHERE UPPER(currencyCodes.Entity) = UPPER('$country')
				        LIMIT 1")
        currC <- fn$sqldf("SELECT AlphabeticCode AS \"currencyCode\"

				        FROM currencyCodes
				        WHERE UPPER(currencyCodes.Entity) = UPPER('$country')
				        LIMIT 1")
    }
    else if(country == "Euro")
    { 
        coname <- "European Union"
        abbrv <- "EU"    
        currN <- "Euro"
        currC <- "EUR"	
    }
    else if(country == "South Korea")
    { 
        coname <- "South Korea"
        abbrv <- "KOR"    
        currN <- "Korean Won"
        currC <- "KRW"	
    }
    else if(country == "Taiwan")
    { 
        coname <- "Taiwan"
        abbrv <- "TWN"    
        currN <- "New Taiwan Dollar"
        currC <- "TWD"	
    }
    
    finalCurrencyDf <- rbind(finalCurrencyDf, 
                               data.frame(countryName=coname, 
                                          countryAbbreviation=abbrv,
                                          currencyName=currN,	
                                          currencyCode=currC
                               ))		
}

finalCurrencyDf <- rbind(finalCurrencyDf, 
                           data.frame(countryName="United States", 
                                      countryAbbreviation="USA",
                                      currencyName="US Dollar",	
                                      currencyCode="USD"
                           ))
finalCurrencyDf["displayed"] <- NA
for(row in  1:nrow(finalCurrencyDf))
{
    aux1 <- paste0( finalCurrencyDf[row, "currencyCode"], " - ")
    aux2 <- paste0( aux1, finalCurrencyDf[row, "currencyName"])
    finalCurrencyDf[row, "displayed"] <- aux2
}
```

# Usage of the Shiny APP

This shiny app has 2 major functions:

  1. **Currency Converter**: This introduces some of the functionalities of the interactive application. It is a regular exchange calculator.
  2. **Data Summary**
  
## Currency Converter

In this first pane you can calculate the conversion of a given amount **from** an initial currency type **to** a final currency via the following steps:

- Select the source currency in the **From:** menu.
- Select the destination currency in the **To:** menu.
- Enter the amount to exchange in the **Amount to Convert** field.
- The Amount will be exchanged automatically (there was no need of an action button here).

We set the exchange rate to be the one corresponding to the yearly rate (which is the average exchange rate for that whole year). **The user can actually select her own rate by selecting another year in the sliding bar and clicking again in the action button Convert**. In the case that there is no Data for the exchange rates between the currencies an error message will be displayed.
The results will be printed out in the Currency Converter Panel along with other charts and statistics. The dynamics of these features are self explanatory in the app.

## Data Summary

Presents a Data Summary and a plot of the inverse conversion between rates. The plot is drawn with the default year. 
