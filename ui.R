#
#   FOREIGN EXCHANGE RATES CONVERTER - Shiny App
#   Final Project for Developing Data Products - Johns Hopkins University 
#   August / 7th / 2020
#
#   Found Online at:  https://cordero.shinyapps.io/FOREXAPP/
#

library(shiny)
library(sqldf)
library(countrycode)
library(dygraphs)

# Processing the list of currencies to be used in this app.

countrieYearlyER <- read.csv("./data/yearly_csv.csv")
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

#Get the list of years for the sliderbar in order to select the exchange rate according to a given year.
countrieYearlyER$Date <- as.Date(countrieYearlyER$Date)
listOfYears <- as.numeric(sort(unique(format(as.Date(countrieYearlyER$Date, format="%d/%m/%Y"),"%Y"))))

# Define UI for application 
shinyUI(pageWithSidebar(

    # Application title
    headerPanel("FOREIGN EXCHANGE RATES CONVERTER"),

    sidebarPanel(
        div(img(src="sample.jpg", height = "50%", width ="50%"), style="text-align: center;"),
        br(),
        selectInput(inputId="fromc", label="From:", choices=sort( finalCurrencyDf$displayed ),
                    multiple = FALSE,selected="USD - US Dollar"),
        selectInput(inputId="toc", label="To:", choices=sort(  finalCurrencyDf$displayed ),
                    multiple = FALSE,selected="MXN - Mexican Peso"),
            conditionalPanel(
                condition = "input.fromc == input.toc",
                br(),
                tags$b("Error!"),
                br(), 
                br(),
                tags$b("Please Do Select a different currency in From: or To:"),
                br(), 
                br()
            ),
        numericInput(inputId="amountcc", label="Amount to Convert:", value= 1,min=1),
        br(),
        #conditionalPanel(
        #    condition = "input.fromc != input.toc",
        #        actionButton("convertAction", "CONVERT")
        #),
        br(),
        sliderInput("yearc",
                    "Select YEAR to obtain the exchange rate for the Currencies:",
                    min = min(listOfYears),
                    max = max(listOfYears),
                    value = "2016"),
        br(),
        br(),
        br(),
        p(strong(em("Documentation:",a("FOREX Converter",href="READMe.html")))),
        p(strong(em("Github Repository:",a("Developing Data Products - Johns Hopkins University",href="https://github.com/corderoai/DDPJHU_Final_Project_ShinyApp"))))
        ),

        mainPanel(
            tabsetPanel(
                tabPanel('Currency Converter', align='center',
                         h3(code(
                         textOutput("outcc") 
                         )),
                         h3(
                             textOutput("outFromAmount") 
                         ),
                         h2(
                             textOutput("outToAmount") 
                         ),
                         h4(
                             textOutput("outFromER") 
                         ),
                         h4(
                             textOutput("outToER") 
                         ),
                         h5(
                             textOutput("outConv") 
                         ),
                         h5(
                             textOutput("outUpdated") 
                         ),
                         dygraphOutput("Plot1"),
                         br(),
                         h2(textOutput("outTableLabel")),
                         dataTableOutput("tabular")
                ),
                tabPanel('Data Summary', align='center',
                         h1("Some Statistics calculated from the dataset in the form of plots"),
                         h2(textOutput("outTableLabel2")),
                         dataTableOutput("tabular2"),
                         br(),
                         dygraphOutput("Plot2")
                )
            )
        )
    
))


# In order to display the ui.R, server.R and README.html into the same page do the following:

# Create a text file with no extension. Name this file: DESCRIPTION. Place the following content and edit as necessary. The display mode is set for Showcase.
#Title: Hello Shiny!
#    Author: RStudio, Inc.
#AuthorUrl: http://www.rstudio.com/
#    License: GPL-3
#DisplayMode: Showcase
#Tags: getting-started
#Type: Shiny
#Save this file in the same directory as your server.R and ui.R files. Deploy as usual. Here is the reference: http://shiny.rstudio.com/articles/display-modes.html. Just be sure that you don't have a file type or extension associated with your DESCRIPTION file.

