# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(MASS)

# Load data files ----
## South America 
SA <- read.csv(file = "South_America_Life.csv")
SA$Income <- as.numeric(SA$Income)

## G7
countryData <- read.csv(file = "lifeExpect.csv")
countryData$Income <- as.numeric(countryData$Income)

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "black",
    ## Header ----
    dashboardHeader(
      title = "Box-Cox Transformation",
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Box_Cox_Transformation")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("home")
        )
      )
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ## Body ----
    dashboardBody(
      tabItems(
        ### Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Box-Cox Transformation"), 
          p("This app explores the use of the Box-Cox Transformation technique
            in real datasets to make data approximate to normal."),
          h2("Instructions"),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Read the instrutions carefully on each page."),
            tags$li("Explore how Box-Cox transformation improve the normality of
                    regression residuals.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Qiaojuan Tu.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            boastUtils::citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 12/8/2021 by NJH.")
          )
        ),
        ### Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          box(
            title = strong("What is Box Cox Transformation?"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            "The Box-Cox Transformation is a procedure that is designed to find 
            an ideal transformation for \\(Y\\) in a linear regression model when 
            data is not normal. As normality is a crucial assumption for many
            statistical tests, applying a Box-Cox Transformation will be helpful
            with further analysis."
          ),
          box(
            title = strong("\\(\\boldsymbol{\\lambda}\\)-values"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            p("The core of the Box-Cox Transformation is the exponent, \\(\\lambda\\),
            which can vary from -5 to 5. The ideal \\(\\lambda\\) is the one that
            provides the best approximation of a normal distribution curve. The
            original Box-Cox Transformation takes the following form:
            \\[y(\\lambda)=\\begin{cases}
               \\frac{y^{\\lambda}-1}\\lambda, & \\text{if } \\lambda\\neq 0 \\\\
               log(y), & \\text{if } \\lambda = 0
               \\end{cases}\\]"), 
            p(tags$em("Note:"), " the above test only works for positive data.
              If the data contains negative values, a slight modification with
              the formula is needed:
              \\[y(\\lambda)=\\begin{cases}
              \\frac{(y+\\lambda_{2})^{\\lambda_{1}}-1}{\\lambda_{1}}, &
              \\text{if } \\lambda_{1}\\neq 0 \\\\
               log(y+\\lambda_{2}), & \\text{if } \\lambda_{1} = 0
               \\end{cases}\\]
              Here, \\(\\lambda = (\\lambda_{1}, \\lambda_{2})'\\).")
          ), 
          box(
            title = strong("Normal Probability Plot"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%', 
            p("If the data follow a normal distribution with mean \\(\\mu\\) and
              variance \\(\\sigma^2\\), then a plot of the theoretical percentiles
               of the normal distribution versus the observed sample percentiles
              should be approximately linear.")
          ), 
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "explore",
              label = "Explore!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),
        ### Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Finding the Optimal \\(\\lambda\\)-value"),
          p("Instructions:"),
          p("Step 1: Select your interested dataset, then look at the Normal
            QQ-Plot in the middle, try to make a judgement on whether the current
            model is a good fit."),
          p("Step 2: Play with the \\(\\lambda\\) slider, guess which \\(\\lambda\\)
            value for the Box-Cox transformation can make the model a better fit
            based on the Box-Cox Normality Plot."), 
          p("*Hint: If the data is normally distributed, the points in the QQ-Plot
            would roughly lie on a straight diagnal line."),
          br(),
          fluidRow(
            column(
              width = 4, 
              wellPanel(
                selectInput(
                  inputId = "dataSet", 
                  label = "Select your data set", 
                  choices = list("", "South America Country Life Expectancy vs. Income",
                                 "G7 Country Life Expectancy vs. Income"),
                  selected = FALSE, 
                  multiple = FALSE, 
                  width = "100%"
                ), 
                uiOutput("dataDescription"),
                br(),
                sliderInput(
                  inputId = "lambda",
                  label = "\\(\\lambda\\) value",
                  min = -5,
                  max = 5,
                  step = 0.01,
                  value = 0
                )
              )
            ), 
            #Set up the Summary Panel
            column(
              width = 4,
              plotOutput("qqPlot"),
              verbatimTextOutput("modelSummary")
            ),
            column(
              width = 4,
              plotOutput("boxCoxPlot"),
              numericInput(
                inputId = "lambdaGuess",
                label = "Guess the optimal \\(\\lambda\\) value",
                value = "",
                min = -5,
                max = 10,
                step = 0.01
              ),
              fluidRow(
                column(
                  width = 3,
                  bsButton(
                    inputId = "submitGuess",
                    label = "Submit",
                    style = "default",
                    size = "large"
                  )
                ),
                column(
                  width = 9,
                  uiOutput("gradeMark")
                )
              ),
              uiOutput("feedback")
            )
          )
        ),
        ### References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent", 
            "Carey, R. (2019), boastUtils: BOAST Utilities, R Package. Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent", 
            "Chang, W. and Borges Ribeiro, B. (2017). shinydashboard: Create
            Dashboards with 'Shiny'. R package version 0.6.1"
          ),
          p(
            class = "hangingindent", 
            "Chang, W., Cheng J., Allaire, J., Xie, Y., and McPherson, J. (2017). shiny:
            Web Application Framework for R. R package version 1.0.3"
          ),
          p(
            class = "hangingindent", 
            "Life expectancy and income data: https://www.gapminder.org/data/"
          ), 
          p(
            class = "hangingindent", 
            "Perrier, V., Meyer, F. and Granjon, D. (2018). 
            shinyWidgets: Custom Inputs Widgets for Shiny.
            R package version 0.4.3."
          ),
          p(
            class = "hangingindent", 
            "Venables, W. N. and Ripley, B. D. (2002). MASS: Modern Applied Statistics
            with S, Fourth edition. Springer, New York. ISBN 0-387-95457-0"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  ## Buttons Set up ----
  ### Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Use this app to explore Box Cox transformations"
      )
    }
  )
  ### Set up the Go Button ----
  observeEvent(
    eventExpr = input$go1, 
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )
  
  ### Set up the Explore button ----
  observeEvent(
    eventExpr = input$explore, 
    handlerExpr = {
      updateTabItems(
        session = session, 
        inputId = "pages", 
        selected = "explore"
      )
    }
  )
  
  dataSet <- reactiveVal(0)
  scoring <- reactiveValues(
    icon = 0,
    feedback = 0
  )
  
  ## Update dataset selected and description ----
  observeEvent(
    eventExpr = input$dataSet,
    handlerExpr = {
      output$dataDescription <- renderUI({
        if (input$dataSet == "South America Country Life Expectancy vs. Income") {
          p("This data set gives the variables of 13 South America countries
            averged life expectancy in years and income in GDP/capita from 1900 
            to 2020. Countries included Brazil, Argentina, Colombia, Peru, Chile,
            Ecuador, Venezuela, Bolivia, Uruguay, Suriname, Paraguay,
            Trinidad and Tobago. Here, the income is used as the predictor variable,
            and life expectancy is used as the response variable for the linear
            regression model.")
        } else if (input$dataSet == "G7 Country Life Expectancy vs. Income") {
          p("The data set gives the variables of G7 countries (Canada, UK, US,
            Germany, Japan, Italy, and France) averaged life expetancy in years
            and income in GDP/capita from year 1900 to 2020. Here, we use the
            income as predictor variable, and life expectancy as response
            variable for the linear regression model.")
        } else {
          NULL
        }
      })
      
      if (input$dataSet == "South America Country Life Expectancy vs. Income") {
        dataSet(SA)
      } else if (input$dataSet == "G7 Country Life Expectancy vs. Income") {
        dataSet(countryData)
      }
    }
  )
  
  
  ## Create QQ Plot ----
  output$qqPlot <- renderPlot(
    expr = {
      validate(
        need(
          expr = input$dataSet != "",
          message = "Choose a data set"
        )
      )
      if (input$lambda == 0) {
        model <- lm(
          formula = log(Life_expect) ~ Income,
          data = dataSet()
        )
      } else {
        model <- lm(
          formula = (Life_expect^input$lambda - 1) / input$lambda ~ Income,
          data = dataSet()
        )
      }
      qqnorm(
        model$residuals,
        pch = 16,
        main = "QQ-Plot Post Transformation", 
       xlab = "Sample quantile", 
       ylab = "Sample residuals"
      )
      qqline(model$residuals, col = "red")
    },
    alt = "QQ plot for the residuals post the Box-Cox transformation"
  )
  
  ## Print model summary ----
  output$modelSummary <- renderPrint(
    expr = {
      validate(
        need(
          expr = input$dataSet != "",
          message = "Choose a data set"
        )
      )
      if (input$lambda == 0) {
        model <- lm(
          formula = log(Life_expect) ~ Income,
          data = dataSet()
        )
      } else {
        model <- lm(
          formula = (Life_expect^input$lambda - 1) / input$lambda ~ Income,
          data = dataSet()
        )
      }
      summary(model)
    }
  )
  
  ## Create box cox plot ----
  output$boxCoxPlot <- renderPlot(
    expr = {
      validate(
        need(
          expr = input$dataSet != "",
          message = "Choose a data set"
        )
      )
      model <-  lm(
        formula = Life_expect ~ Income,
        data = dataSet()
      )
      boxcox(object = model, lambda = seq(-2, 5, 1/4), data = dataSet())
    },
    alt = "Box Cox plot showing the log-likelihood values for each possible
    lambda value"
  )
  
  ## Answer checking ----
  observeEvent(
    eventExpr = input$submitGuess,
    handlerExpr = {
      if (is.na(input$lambdaGuess)) {
        sendSweetAlert(
          session = session,
          title = "Error",
          text = "Please enter a guess for the lambda value first.",
          type = "error"
        )
      } else {
        model <-  lm(
          formula = Life_expect ~ Income,
          data = dataSet()
        )
        bcResult <- boxcox(
          object = model,
          lambda = seq(-2, 5, 1/4),
          data = dataSet(),
          plotit = FALSE
        )
        correctAnswer <- bcResult$x[which.max(bcResult$y)]
        
        if (input$lambdaGuess == correctAnswer) {
          scoring$icon <- "correct"
          scoring$feedback <- "Great job!"
        } else if (abs(input$lambdaGuess - correctAnswer) <= 0.5) {
          scoring$icon <- "correct"
          scoring$feedback <- "Good guess!"
        } else if (abs(input$lambdaGuess - correctAnswer) <= 1) {
          scoring$icon <- "partial"
          scoring$feedback <- "You're close."
        } else {
          scoring$icon <- "incorrect"
          scoring$feedback <- "Not quite; please try again."
        }
        
        output$gradeMark <- renderIcon(icon = scoring$icon)
        output$feedback <- renderUI({p(scoring$feedback)})
      }
    }
  )
  
  ## Clear marks and feedback ----
  observeEvent(
    eventExpr = c(input$dataSet, input$lambda, input$lambdaGuess),
    handlerExpr = {
      output$gradeMark <- renderIcon()
      output$feedback <- renderUI({NULL})
    }
  )
  
  observeEvent(
    eventExpr = c(input$dataSet, input$lambda),
    handlerExpr = {
      updateNumericInput(
        session = session,
        inputId = "lambdaGuess",
        value = ""
      )
    }
  )
  
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
