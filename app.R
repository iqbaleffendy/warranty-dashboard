library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)
library(tidyverse)
library(readxl)
library(DT)
library(plotly)
library(formattable)


# Load Dataset----
warrantydata <- read_excel("import_data/warrantyclaim.xlsx")
warrantydata <- warrantydata %>% 
  select(DealerClaim, JobOrder) %>% 
  filter(!is.na(JobOrder)) %>% 
  group_by(JobOrder) %>% 
  summarize(DealerClaimNo = paste(DealerClaim, collapse=", "))

failuredata <- read_excel("import_data/techreportsummary.xlsx")
failuredata$OpenDate <- as.Date(failuredata$OpenDate)
failuredata$BranchCode <- as.character(failuredata$BranchCode)

branch <- read_excel("import_data/branch.xlsx")
branch$BranchCode <- as.character(branch$BranchCode)

failuredata <- failuredata %>% 
  left_join(warrantydata, by = c("JobNo" = "JobOrder")) %>% 
  mutate(
    ClaimStatus = case_when(
      is.na(DealerClaimNo) ~ "Not Claimed",
      TRUE ~ "Claimed"
    )
  ) %>% 
  left_join(branch, by = "BranchCode")


ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  useShinydashboard(),
  
  navbarPage(
    title = "Warranty Dashboard",
    position = "fixed-top",
    selected = "Claim Monitoring",
    tags$style(type="text/css", "body {padding-top: 70px;}"),
    tabPanel(
      title = "Claim Monitoring",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(
            inputId = "branchname",
            label = "Select Branch",
            choices = c("All", unique(failuredata$Branch)),
            selected = "All"
          ),
          selectInput(
            inputId = "unitmodel",
            label = "Select Unit Model",
            choices = c("All", unique(failuredata$UnitModel)),
            selected = "All"
          ),
          selectInput(
            inputId = "claimstatus",
            label = "Select Claim Status",
            choices = c("All", unique(failuredata$ClaimStatus)),
            selected = "All"
          ),
          textOutput("cutoffdate")
        ),
        mainPanel(
          width = 9,
          tabsetPanel(
            tabPanel(
              title = "Summary",
              fluidRow(
                column(
                  width = 4,
                  offset = 2,
                  valueBoxOutput("claimsum", width = NULL)
                ),
                column(
                  width = 5,
                  offset = 0,
                  valueBoxOutput("claimpercentage", width = NULL)
                )
              ),
              fluidRow(
                plotlyOutput("claimplot")
              )
            ),
            tabPanel(
              title = "Dataset",
              DTOutput("claimdataset")
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Source Code",
      box(
        title = "Source Code",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        pre(includeText("app.R"))
      )
    )
  )
)


server <- function(input, output) {
  
  # Reactive Expression to Filter Data----
  claim_filtered <- reactive({
    if (input$branchname != "All") {
      failuredata <- failuredata %>% 
        filter(Branch == input$branchname)
    }
    
    if (input$unitmodel != "All") {
      failuredata <- failuredata %>% 
        filter(UnitModel == input$unitmodel)
    }
    
    if (input$claimstatus != "All") {
      failuredata <- failuredata %>% 
        filter(ClaimStatus == input$claimstatus)
    }
    
    failuredata
  })
  
  # Output Service Level Percentage----
  output$claimsum <- renderValueBox({
    warrantyclaimsum <- 
      claim_filtered() %>%
      summarize(Sum = n()) %>% 
      ungroup() %>% 
      select(Sum)
    valueBox(
      warrantyclaimsum,
      subtitle = tags$p("Warranty Claim Amount", style = "font-size: 110%"),
      color = "light-blue",
      icon = icon("list")
    )
  })
  
  # Output Service Level Value----
  output$claimpercentage <- renderValueBox({
    warrantypct <- 
      claim_filtered() %>% 
      group_by(ClaimStatus) %>% 
      summarize(ClaimSum = n()) %>%
      ungroup() %>% 
      mutate(pct = ClaimSum / sum(ClaimSum) * 100) %>% 
      filter(ClaimStatus == "Claimed") %>% 
      select(pct)
    valueBox(
      paste(round(warrantypct, digits = 1), " %"),
      subtitle = tags$p("Warranty Percentage", style = "font-size: 110%"),
      color = "light-blue",
      icon = icon("list")
    )
  })
  
  # Function for Service Level Plot----
  warrantyclaimplot <- function(data, group) {
    data %>% 
      count({{group}}, ClaimStatus) %>%
      group_by({{group}}) %>% 
      mutate(Total = sum(n)) %>% 
      ungroup() %>% 
      mutate(group = fct_reorder({{group}}, Total, .desc = TRUE)) %>% 
      plot_ly(x = ~group, y = ~n, type = "bar", color = ~ClaimStatus) %>% 
      layout(barmode = "stack") %>% 
      layout(legend = list(orientation = "h")) %>% 
      layout(xaxis = list(title = "", tickangle = -45)) %>% 
      layout(yaxis = list(title = "")) %>% 
      layout(legend = list(x = 0.7, y = 0.8, bgcolor = "#E2E2E2"))
  }
  
  # Output Service Level Plot----
  output$claimplot <- renderPlotly({
    if (input$branchname == "All") {
      warrantyclaimplot(claim_filtered(), Branch)
    } else {
      warrantyclaimplot(claim_filtered(), UnitModel)
    }
  })
  
  # Output Cut Off Date----
  output$cutoffdate <- renderText({
    paste(
      "Cut Off Date is ", as.Date(max(failuredata$OpenDate))
    )
  })
  
  # Output Service Level Dataset----
  output$claimdataset <- renderDT({
    datatable(
      claim_filtered() %>% select(1,4,5,8,11,19,20),
      class = 'cell-border stripe'
    )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)