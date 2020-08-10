#
#   FOREIGN EXCHANGE RATES CONVERTER - Shiny App
#   Final Project for Developing Data Products - Johns Hopkins University 
#   August / 7th / 2020
#
#   Found Online at:  https://cordero.shinyapps.io/FOREXAPP/
#

# Access datasets
countrieYearlyER <- read.csv("./data/yearly_csv.csv")

# Calculate the list of currencies with country names
getCurrenciesCatalog <- function(){
    currencyCodes <- read.csv("./data/codes-all_csv.csv")
    listOfCountries <- unique(countrieYearlyER$Country)
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
            coname <- "Euro"
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
    
    return(finalCurrencyDf)
}    

# calculate the exchange rates from foreign currency to USD (FU) and from USD to foreign currency (UF),
# then save them in a new dataframe.

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

# Make conversions from one currency X to another one Y
# as in the real world we need to exchange any currencies to USD before we convert them to the destination currency.

makeAtomicExchangeConversion <- function(fromCurrency, toCurrency, amount, givenDate, dfERFUUF){
    print("--------------------------")
    print(paste(amount, paste(paste(fromCurrency, "to"), toCurrency)))
    
    finalAmount <- 1
    if(fromCurrency == "USD"){   # from USD to X Currency   
        logicalQueryFrom <- (dfERFUUF[(dfERFUUF$Date== givenDate & dfERFUUF$Country == toCountryName), ])
        print("To Currency:")
        print(logicalQueryFrom)
        finalAmount <- amount * logicalQueryFrom$FU
        print("Final Converted Amount:")
        print(finalAmount)
    }
    else if(toCurrency == "USD"){ # from X Currency to USD
        logicalQueryTo <- (dfERFUUF[(dfERFUUF$Date== givenDate & dfERFUUF$Country == fromCountryName), ])
        print("To USD:")
        print(logicalQueryTo)
        finalAmount <- amount * logicalQueryTo$UF
        print("Final Converted Amount:")
        print(finalAmount)
    }
    else{   # from X Currency to Y Currency = (X to USD) to Y Currency
        logicalQueryTo <- (dfERFUUF[(dfERFUUF$Date== givenDate & dfERFUUF$Country == fromCountryName), ])
        logicalQueryFrom <- (dfERFUUF[(dfERFUUF$Date== givenDate & dfERFUUF$Country == toCountryName), ])
        print("From Currency:")
        print(logicalQueryTo)
        print("To Currency:")
        print(logicalQueryFrom)
        print(toCountryName)
        print(givenDate)
        amountUSD <- amount * logicalQueryTo$UF
        finalAmount <- amountUSD * logicalQueryFrom$FU
        print("Final Converted Amount:")
        print(finalAmount)
    }
    
    if(length(finalAmount) == 0){ 
        finalAmount <- 0
    }
    
    return(finalAmount)
}

# Global Variables for exchange conversions
currencyCatalog <- getCurrenciesCatalog()
print(currencyCatalog)

fromDisplayed <- "USD - US Dollar"
toDisplayed <- "MXN - Mexican Peso"
fromCountryName <- currencyCatalog[currencyCatalog$displayed == fromDisplayed, ]$countryName
fromCountryAbbreviation <- currencyCatalog[currencyCatalog$displayed == fromDisplayed, ]$countryAbbreviation
fromCurrencyName <- currencyCatalog[currencyCatalog$displayed == fromDisplayed, ]$currencyName
fromCurrencyCode <- currencyCatalog[currencyCatalog$displayed == fromDisplayed, ]$currencyCode
toCountryName <- currencyCatalog[currencyCatalog$displayed == toDisplayed, ]$countryName
toCountryAbbreviation <- currencyCatalog[currencyCatalog$displayed == toDisplayed, ]$countryAbbreviation
toCurrencyName <- currencyCatalog[currencyCatalog$displayed == toDisplayed, ]$currencyName
toCurrencyCode <- currencyCatalog[currencyCatalog$displayed == toDisplayed, ]$currencyCode

setCurrencies <- function(fromDisplayed, toDisplayed){
    fromCountryName <<- currencyCatalog[currencyCatalog$displayed == fromDisplayed, ]$countryName
    fromCountryAbbreviation <<- currencyCatalog[currencyCatalog$displayed == fromDisplayed, ]$countryAbbreviation
    fromCurrencyName <<- currencyCatalog[currencyCatalog$displayed == fromDisplayed, ]$currencyName
    fromCurrencyCode <<- currencyCatalog[currencyCatalog$displayed == fromDisplayed, ]$currencyCode
    toCountryName <<- currencyCatalog[currencyCatalog$displayed == toDisplayed, ]$countryName
    toCountryAbbreviation <<- currencyCatalog[currencyCatalog$displayed == toDisplayed, ]$countryAbbreviation
    toCurrencyName <<- currencyCatalog[currencyCatalog$displayed == toDisplayed, ]$currencyName
    toCurrencyCode <<- currencyCatalog[currencyCatalog$displayed == toDisplayed, ]$currencyCode
}

countrieYearlyERFUUF <- populateRatesOfCurrencies(countrieYearlyER)
print(head(countrieYearlyERFUUF))
print(tail(countrieYearlyERFUUF))

makeAtomicExchangeConversion(fromCurrencyCode, toCurrencyCode, 100, as.Date("2016-01-01"), countrieYearlyERFUUF)
fromDisplayed <- "MXN - Mexican Peso"
toDisplayed <- "USD - US Dollar"
setCurrencies(fromDisplayed, toDisplayed)
makeAtomicExchangeConversion(fromCurrencyCode, toCurrencyCode, 1866.7, as.Date("2016-01-01"), countrieYearlyERFUUF)
makeAtomicExchangeConversion(fromCurrencyCode, toCurrencyCode, 100, as.Date("2016-01-01"), countrieYearlyERFUUF)
fromDisplayed <- "MXN - Mexican Peso"
toDisplayed <- "VEB - Bolivar"
setCurrencies(fromDisplayed, toDisplayed)
makeAtomicExchangeConversion(fromCurrencyCode, toCurrencyCode, 56, as.Date("2016-01-01"), countrieYearlyERFUUF)
fromDisplayed <- "MXN - Mexican Peso"
toDisplayed <- "USD - US Dollar"
setCurrencies(fromDisplayed, toDisplayed)
makeAtomicExchangeConversion(fromCurrencyCode, toCurrencyCode, 56, as.Date("2016-01-01"), countrieYearlyERFUUF)
fromDisplayed <- "USD - US Dollar"
toDisplayed <- "VEB - Bolivar"
setCurrencies(fromDisplayed, toDisplayed)
makeAtomicExchangeConversion(fromCurrencyCode, toCurrencyCode, 2.999946, as.Date("2016-01-01"), countrieYearlyERFUUF)

# Prepare dataframe for table

displayTabular <- function(fromCurrencyDisplayed, toCurrencyDisplayed, year){
    print(fromCurrencyDisplayed)
    print(toCurrencyDisplayed)
    fromDisplayed <<- fromCurrencyDisplayed
    toDisplayed <<- toCurrencyDisplayed
    setCurrencies(fromDisplayed, toDisplayed)
    
    dataTable <- data.frame(
          from =  character(),
          to = character()
    )
    
    amounts <- c(1,5,10,25,50,100,500,1000,5000,10000, 50000, 100000)
    
    for(c in amounts){
        amountConverted <<- makeAtomicExchangeConversion(fromCurrencyCode, toCurrencyCode, c, 
                                                         as.Date(paste(year,1,1,sep="-")), countrieYearlyERFUUF)
        new_row <- c(paste(sprintf("%3.2f", c), fromCurrencyCode),paste(sprintf("%3.2f", amountConverted), toCurrencyCode))
        dataTable <- rbind(dataTable, new_row)    
    }
    
    colnames(dataTable)[1] <- fromCurrencyCode
    colnames(dataTable)[2] <- toCurrencyCode
    
    return(dataTable)
}

displayTabular2 <- function(fromCurrencyDisplayed, toCurrencyDisplayed){
    print(fromCurrencyDisplayed)
    print(toCurrencyDisplayed)
    fromDisplayed <<- fromCurrencyDisplayed
    toDisplayed <<- toCurrencyDisplayed
    setCurrencies(fromDisplayed, toDisplayed)
    
    dataTable <- data.frame(
        Index =  character(),
        Value = numeric()
    )
    
    if(flag == "fromUSD"){
        dataER <- yearlyData$FU 
    }
    else if(flag == "toUSD"){
        dataER <- yearlyData$UF
    }
    else if(flag == "fromXtoY"){
        dataER <- yearlyData$FU
    }
    
    new_row <- c("min", min(dataER))
    dataTable <- rbind(dataTable, new_row)
    new_row <- c("max", max(dataER))
    dataTable <- rbind(dataTable, new_row)
    new_row <- c("mean", mean(dataER))
    dataTable <- rbind(dataTable, new_row)
    new_row <- c("std", sd(dataER))
    dataTable <- rbind(dataTable, new_row)
    
    colnames(dataTable)[1] <- "Index"
    colnames(dataTable)[2] <- "Value"
    
    return(dataTable)
}    

library(shiny)
library(dygraphs)
library(xts)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    
    output$outcc <- renderText({
        fromDisplayed <<- input$fromc
        toDisplayed <<- input$toc
        setCurrencies(fromDisplayed, toDisplayed)
        amountConverted <<- makeAtomicExchangeConversion(fromCurrencyCode, toCurrencyCode, input$amountcc, 
                                     as.Date(paste(input$yearc,1,1,sep="-")), countrieYearlyERFUUF)
        currencyMessage <- paste("The exchange conversion from", input$amountcc)
        currencyMessage <- paste(currencyMessage, currencyCatalog[currencyCatalog$displayed == input$fromc, ]$currencyCode)
        currencyMessage <- paste(currencyMessage, "(")
        currencyMessage <- paste0(currencyMessage, currencyCatalog[currencyCatalog$displayed == input$fromc, ]$countryName)
        currencyMessage <- paste0(currencyMessage, ")")
        currencyMessage <- paste(currencyMessage, "to")
        currencyMessage <- paste(currencyMessage, currencyCatalog[currencyCatalog$displayed == input$toc, ]$currencyCode)
        currencyMessage <- paste(currencyMessage, "(")
        currencyMessage <- paste0(currencyMessage, currencyCatalog[currencyCatalog$displayed == input$toc, ]$countryName)
        currencyMessage <- paste0(currencyMessage, ")")
        currencyMessage <- paste(currencyMessage, "is:")
        currencyMessage <- paste(currencyMessage, sprintf("%3.2f", amountConverted))
        currencyMessage <- paste(currencyMessage, input$toc)
    })  
    output$outFromAmount <- renderText({
        fromDisplayed <<- input$fromc
        toDisplayed <<- input$toc
        setCurrencies(fromDisplayed, toDisplayed)
        currencyMessage <- paste(input$amountcc, fromCurrencyCode, "=", sep = " ")
    })
    output$outToAmount <- renderText({
        fromDisplayed <<- input$fromc
        toDisplayed <<- input$toc
        setCurrencies(fromDisplayed, toDisplayed)
        amountConverted <<- makeAtomicExchangeConversion(fromCurrencyCode, toCurrencyCode, input$amountcc, 
                                                         as.Date(paste(input$yearc,1,1,sep="-")), countrieYearlyERFUUF)
        if( amountConverted > 0){
            currencyMessage <- paste(sprintf("%3.2f", amountConverted), toCurrencyCode, sep = " ")    
        }
        else{
            currencyMessage <- "ERROR: DATA not available for the currencies at the specified year!"
        }
    })
    output$outFromER <- renderText({
        fromDisplayed <<- input$fromc
        toDisplayed <<- input$toc
        setCurrencies(fromDisplayed, toDisplayed)
        amountConverted <<- 1/makeAtomicExchangeConversion(fromCurrencyCode, toCurrencyCode, 1, 
                                                         as.Date(paste(input$yearc,1,1,sep="-")), countrieYearlyERFUUF)
        currencyMessage <- paste("1", toCurrencyCode, "=", amountConverted, fromCurrencyCode, sep = " ")
    })    
    output$outToER <- renderText({
        fromDisplayed <<- input$fromc
        toDisplayed <<- input$toc
        setCurrencies(fromDisplayed, toDisplayed)
        amountConverted <<- makeAtomicExchangeConversion(fromCurrencyCode, toCurrencyCode, 1, 
                                                         as.Date(paste(input$yearc,1,1,sep="-")), countrieYearlyERFUUF)
        currencyMessage <- paste("1", fromCurrencyCode, "=", amountConverted, toCurrencyCode, sep = " ")
    })
    output$outConv <- renderText({
        currencyMessage <- paste(input$fromc, "to", input$toc, sep = " ")
    })    
    output$outUpdated <- renderText({
        currencyMessage <- paste("Last Updated on:", as.Date(paste(input$yearc,1,1,sep="-")), sep = " ")
    })
    output$Plot1 <- renderDygraph({
        if(input$fromc != input$toc){
            print("-------------")
            print(currencyCatalog[currencyCatalog$displayed == input$fromc, ]$countryName)
            print(currencyCatalog[currencyCatalog$displayed == input$fromc, ]$currencyCode)
            print(currencyCatalog[currencyCatalog$displayed == input$toc, ]$countryName)
            print(currencyCatalog[currencyCatalog$displayed == input$toc, ]$currencyCode)
            flag <<- "none"
            
            if( currencyCatalog[currencyCatalog$displayed == input$fromc, ]$countryName == "United States"){
                yearlyData <<- countrieYearlyERFUUF[countrieYearlyERFUUF$Country == currencyCatalog[currencyCatalog$displayed == input$toc, ]$countryName, ]
                flag <<- "fromUSD"
            }
            else if(currencyCatalog[currencyCatalog$displayed == input$toc, ]$countryName == "United States"){
                yearlyData <<- countrieYearlyERFUUF[countrieYearlyERFUUF$Country == currencyCatalog[currencyCatalog$displayed == input$fromc, ]$countryName, ]
                flag <<- "toUSD"
                }    
            else{
                XtoUSDData <- countrieYearlyERFUUF[countrieYearlyERFUUF$Country == currencyCatalog[currencyCatalog$displayed == input$fromc, ]$countryName, ]
                USDtoYData <- countrieYearlyERFUUF[countrieYearlyERFUUF$Country == currencyCatalog[currencyCatalog$displayed == input$toc, ]$countryName, ]
                minYearX <- min(XtoUSDData$Date) #as.numeric(sort(unique(format(as.Date(XtoUSDData$Date, format="%d/%m/%Y"),"%Y"))))
                minYearY <- min(USDtoYData$Date) #as.numeric(sort(unique(format(as.Date(USDtoYData$Date, format="%d/%m/%Y"),"%Y"))))
                #extractOnlyYearsfromDates <- format(as.Date(XtoUSDData$Date, format="%Y-%m-%d"),"%Y")
                startingYear <- max( c(minYearX, minYearY) )
                XtoUSDData <- XtoUSDData[XtoUSDData$Date >= startingYear,]
                USDtoYData <- USDtoYData[USDtoYData$Date >= startingYear,]
                yearlyData <<- XtoUSDData
                
                for(i in 1:nrow(yearlyData)){
                    yearlyData[i,"Country"] <<- paste(currencyCatalog[currencyCatalog$displayed == input$fromc, ]$currencyCode, "-", 
                                                     currencyCatalog[currencyCatalog$displayed == input$toc, ]$currencyCode, sep = " ")
                    yearlyData[i,"FU"] <<-  XtoUSDData[i, "UF"] * USDtoYData[i, "FU"]
                    yearlyData[i,"UF"] <<-  1/(XtoUSDData[i, "UF"] * USDtoYData[i, "FU"])
                }
                flag <<- "fromXtoY"   
            }
                
           print(yearlyData)
           print(str(yearlyData))
           FUPlot1Data <- data.frame(time=yearlyData$Date, 
                                    value=yearlyData$FU
           )
           UFPlot1Data <- data.frame(time=yearlyData$Date, 
                                   value=yearlyData$UF)
           tsFUPlot1Data <- xts(x = FUPlot1Data$value, order.by = as.POSIXct(FUPlot1Data$time))
           tsUFPlot1Data <- xts(x = UFPlot1Data$value, order.by = as.POSIXct(UFPlot1Data$time))
           
           if(flag == "fromUSD"){
               ts <- tsFUPlot1Data
           }
           else if(flag == "toUSD"){
               ts <- tsUFPlot1Data
           }
           else if(flag == "fromXtoY"){
               ts <- tsFUPlot1Data
           }
           
           ylabel <- "Exchange Rate Accross Time"
           serieslabel <- paste(currencyCatalog[currencyCatalog$displayed == input$fromc, ]$currencyCode, "-",
                           currencyCatalog[currencyCatalog$displayed == input$toc, ]$currencyCode)
           mainlabel <- paste(currencyCatalog[currencyCatalog$displayed == input$fromc, ]$displayed, " to ",
                           currencyCatalog[currencyCatalog$displayed == input$toc, ]$displayed,  
                           ". (Interactive Plot)", sep = "")
           
           dygraph(ts, main = mainlabel) %>%
               dyAxis("y", label = ylabel) %>% 
               dySeries("V1", label = serieslabel ) %>%
               dyLegend(show = "follow", hideOnMouseOut = FALSE) %>%
               dyOptions(axisLineWidth = 2.5, fillGraph = TRUE, includeZero = TRUE, 
                         axisLineColor = "brown", fillAlpha = 0.4, drawPoints = TRUE,
                         gridLineColor = "navy")
        }
    })
    output$outTableLabel2 <- renderText({
        paste("Statistics for", input$fromc, "to",input$toc, sep = " ")
    })
    output$tabular2 <- renderDataTable( displayTabular2(input$fromc, input$toc))
    output$outTableLabel <- renderText({
        paste("Convert", input$fromc, "to",input$toc, "(Tabular Form)", sep = " ")
    })
    output$tabular <- renderDataTable( displayTabular(input$fromc, input$toc, input$yearc))    
    output$Plot2 <- renderDygraph({
        if(input$fromc != input$toc){
            
            print(yearlyData)
            print(str(yearlyData))
            FUPlot1Data <- data.frame(time=yearlyData$Date, 
                                      value=yearlyData$FU
            )
            UFPlot1Data <- data.frame(time=yearlyData$Date, 
                                      value=yearlyData$UF)
            tsFUPlot1Data <- xts(x = FUPlot1Data$value, order.by = as.POSIXct(FUPlot1Data$time))
            tsUFPlot1Data <- xts(x = UFPlot1Data$value, order.by = as.POSIXct(UFPlot1Data$time))
            
            if(flag == "fromUSD"){
                ts <- tsUFPlot1Data
            }
            else if(flag == "toUSD"){
                ts <- tsFUPlot1Data
            }
            else if(flag == "fromXtoY"){
                ts <- tsUFPlot1Data
            }
            
            ylabel <- "Exchange Rate Accross Time"
            serieslabel <- paste(currencyCatalog[currencyCatalog$displayed == input$toc, ]$currencyCode, "-",
                                 currencyCatalog[currencyCatalog$displayed == input$fromc, ]$currencyCode)
            mainlabel <- paste(currencyCatalog[currencyCatalog$displayed == input$toc, ]$displayed, " to ",
                               currencyCatalog[currencyCatalog$displayed == input$fromc, ]$displayed,  
                               ". (Interactive Plot)", sep = "")
            
            dygraph(ts, main = mainlabel) %>%
                dyAxis("y", label = ylabel) %>% 
                dySeries("V1", label = serieslabel ) %>%
                dyLegend(show = "follow", hideOnMouseOut = FALSE) %>%
                dyOptions(axisLineWidth = 2.5, stepPlot=TRUE, fillGraph = TRUE, includeZero = TRUE, 
                          axisLineColor = "brown", fillAlpha = 0.4, drawPoints = TRUE,
                          gridLineColor = "navy")
        }
    })
})
