# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(rmarkdown)
library(shinyAce)
library(MASS)
library(ggplot2)
library(knitr)
library(shinyjs)

# Load additional dependencies and setup functions
# source("helpers.R")

#South America 
SA <- read.csv(file = "South_America_Life.csv")
SA$Income <- as.numeric(SA$Income)
#G7
countryData <- read.csv(file = "lifeExpect.csv")
countryData$Income <- as.numeric(countryData$Income)


#G7 Model
G7_model1 <- lm(formula = Life_expect ~ Income, data = countryData)
bc_life <- boxcox(object = G7_model1, lambda = seq(-2, 5, 1/4), data = countryData)
bc_life_value <- bc_life$x[which.max(bc_life$y)]


#SA Model 
SA_model1 <- lm(formula = Life_expect ~ Income, data = SA)
bc_SA <- boxcox(object = SA_model1, lambda = seq(-2, 5, 1/4), data = SA)
bc_SA_value <- bc_SA$x[which.max(bc_SA$y)]



# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Box-Cox Transformation", # You may use a shortened form of the title here
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
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Explore", tabName = "explore", icon = icon("wpexplorer")),
        # menuItem("Challenge", tabName = "challenge", icon = icon("gears")), 
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Box-Cox Transformation"), # This should be the full name.
          p("This app explores the use of the Box-Cox Transformation technique
            in real datasets to make data approximate to normal."),
          h2("Instructions"),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Read the instrutions carefully on each page."),
            tags$li("Challenge yourself based on what you learn!"),
          ),
          ##### Go Button--location will depend on your goals ----
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
          ##### Create two lines of space ----
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Qiaojuan Tu.",
            br(),
            br(),
            br(),
            br(),
            div(class = "updated", "Last Update: 5/19/2021 by NJH.")
          )
        ),
        #### Set up the Prerequisites Page ----
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
            "The Box-Cox Transformation is a procedure that is designed to find an
            ideal transformation for \\(Y\\) in a linear regression model when 
            data is not normal. As normality is a crucial assumption for 
            many statistical tests, applying a Box-Cox Transformation will be
            helpful with further analysis."
          ),
          box(
            title = strong("\\(\\lambda\\)-values"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            p("The core of the Box-Cox Transformation is the exponent, \\(\\lambda\\),
            which can vary from -5 to 5. The ideal \\(\\lambda\\) is the one that
            provides the best approximation of a normal distribution curve. 
            The original Box-Cox Transformation takes the following form:"), 
            p("$$y(\\lambda)=\\begin{cases}
               \\frac{y^{\\lambda}-1}\\lambda,  & \\text{if $\\lambda\\neq$ 0} \\\\
               log(y), & \\text{if $\\lambda$ = 0}
               \\end{cases}\\!$$"), 
            p("*Note that the above test only works for positive data. If the data 
              contains negative values, slight modification with the formula is
              needed:"),
            p("$$y(\\lambda)=\\begin{cases}
               \\frac{(y+\\lambda_{2})^{\\lambda_{1}}-1}{\\lambda_{1}}, 
               & \\text{if $\\lambda_{1}\\neq$ 0} \\\\
               log(y+\\lambda_{2}), & \\text{if $\\lambda_{1}$ = 0}
               \\end{cases}\\!$$"), 
            p(
              "$$Here, \\lambda = (\\lambda_{1}, \\lambda_{2})'$$"
            )
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
            style = "text-align: center",
            bsButton(
              inputId = "explore",
              label = "Explore!",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          )
        ),
        
        #### Set up an Explore Page ----
        tabItem(
          tabName = "explore",
          withMathJax(),
          h2("Finding optimal \\(\\lambda\\)-value for the Box-Cox Transformation"),
          p("Instructions:"),
          p("Step 1: Select your interested dataset, then look at the Normal QQ-Plot 
            on the right, try to make a judgement on whether the current model is a good fit."),
          p("Step 2: Play with the slidebar of the \\(\\lambda\\) value input, guess what 
            \\(\\lambda\\) value for the box-cox transformation can make the model a better fit based on the Box-Cox Normality Plot."), 
          p("*Hint: If the data is normally distributed, the points in the QQ-Plot would roughly lie
            on a straight diagnal line."),
          br(),
          fluidRow(
            #Set up the Normal QQ plot
            withMathJax(),
            column(
              width = 4, 
              wellPanel(
                p(tags$strong("Select your dataset:")), 
                selectInput(
                  inputId = "dataset", 
                  label = NULL, 
                  choices = list("", "South America Country Life Expectancy vs. Income",
                                 "G7 Country Life Expectancy vs. Income"),
                  selected = FALSE, 
                  multiple = FALSE, 
                  width = "100%"
                ), 
                conditionalPanel(
                  condition = "input.dataset == 'South America Country Life Expectancy vs. Income'", 
                  p("Dataset description:"),
                  uiOutput("SA_Description")
                ),
                conditionalPanel(
                  condition = "input.dataset == 'G7 Country Life Expectancy vs. Income'", 
                  p("Dataset description:"),
                  uiOutput("G7_Description")
                ),
                br(),
                conditionalPanel(
                  condition = "input.dataset == 'South America Country Life Expectancy vs. Income'", 
                  sliderInput(
                    inputId = "lambda", 
                    label = "\\(\\lambda\\)", 
                    min = -5, 
                    max = 5, 
                    step = 0.01, 
                    value = 0
                  )
                ), 
                conditionalPanel(
                  condition = "input.dataset == 'G7 Country Life Expectancy vs. Income' ", 
                  sliderInput(
                    inputId = "lambda2", 
                    label = "\\(\\lambda\\)", 
                    min = -5, 
                    max = 5, 
                    step = 0.01, 
                    value = 0
                  )
                )
              )
            ), 
            
            
            #Set up the Summary Panel
            column(
              width = 4, 
              conditionalPanel(
                condition = "input.dataset == 'South America Country Life Expectancy vs. Income'", 
                plotOutput("SA_Transformed_Normal"),
                plotOutput("SA_BoxCox") 
              ), 
              conditionalPanel(
                condition = "input.dataset == 'G7 Country Life Expectancy vs. Income'", 
                # plotOutput("G7_Original_Normal"), 
                plotOutput("G7_Transformed_Normal"), 
                plotOutput("G7_BoxCox")
              ),
              #   conditionalPanel(
              #     condition = "input.dataset == 'SA'", 
              #     plotOutput("SA_Transformed_Normal"), 
              #     plotOutput("SA_BoxCox")
              #   )
            ),
            column(
              width = 4, 
              conditionalPanel(
                condition = "input.dataset == 'South America Country Life Expectancy vs. Income'", 
                verbatimTextOutput("SA_Transformed_Summary"), 
                br(), 
                br(),
                br(),
                wellPanel(
                  p(tags$strong("Guess the optimal \\(\\lambda\\)-value according to the graph")),            
                  numericInput(
                    inputId = "lam_input_SA", 
                    label = NULL, 
                    value = "", 
                    min = -5, 
                    step = 0.01, 
                    max = 10
                  ),
                  actionButton(
                    inputId = "lam_SA",
                    label = "Submit",
                    size = "small",
                    icon = icon("bolt"),
                    style = "default"
                  ),
                  uiOutput("submit_lam_SA"),
                  uiOutput("lam_icon_SA")
                )
              ),
              
              conditionalPanel(
                condition  = "input.dataset == 'G7 Country Life Expectancy vs. Income'",
                # verbatimTextOutput("G7_Original_Summary"), 
                verbatimTextOutput("G7_Transformed_Summary"), 
                br(), 
                br(),
                br(),
                wellPanel(
                  p(tags$strong("Guess the optimal \\(\\lambda\\)-value according to the graph")),            
                  numericInput(
                    inputId = "lam_input", 
                    label = NULL, 
                    value = " ", 
                    min = -5, 
                    step = 0.01, 
                    max = 10
                  ),
                  actionButton(
                    inputId = "lam",
                    label = "Submit",
                    size = "small",
                    icon = icon("bolt"),
                    style = "default"
                  ),
                  uiOutput("submit_lam"),
                  uiOutput("lam_icon")
                  # uiOutput("G7_bc_value")
                )
              )
            )
          )
        ),
        #### Set up a Challenge Page ----
        #         tabItem(
        #           tabName = "challenge",
        #           withMathJax(),
        #           h2("Generating the best \\(\\lambda\\)-value and making prediction "),
        #           p("Instructions:"),
        #           p("Step 1: Work with the SA dataset in which was showed in the previous
        #             explore page, and run the following the sample code in the R workspace."),
        #           p("Step 2: Try to learn from the sample code, then try to code for the US Life Expectany
        #             vs. Income dataset."), 
        #           column(
        #             p("R Workspace:"), 
        #             width = 6,
        #             
        #             aceEditor("rmd",
        #                       mode = "markdown", 
        #                       value = '
        # ```{r}
        # #Dataset Structure
        # str(SA)
        # 
        # #Linear Model
        # fullmodel = lm(Petal.Length~Petal.Width, data = SA)
        # 
        # #BoxCox Graph
        # bc = boxcox(fullmodel)
        # 
        # #Generating the best lambda value 
        # best.lambda = bc$Petal.Width[which(bc$Petal.Length == max(bc$Petal.Length))]
        # 
        # ````
        #                
        #   
        #           
        #            ' ), 
        #             withBusyIndicatorUI(
        #               actionButton(
        #                 inputId = "r_code", 
        #                 label = "Run"
        #               )
        #             )
        #           ), 
        #           column(
        #             width = 6,  
        #             p("Knited Output:"),
        #             htmlOutput("knitDoc")
        #           )
        #         ),
        
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent", 
            "Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis. 
            Springer-Verlag New York. ISBN 978-3-319-24277-4"
          ),
          p(
            class = "hangingindent", 
            "Venables WN and Ripley BD (2002). MASS: Modern Applied Statistics
            with S, Fourth edition. Springer, New York. ISBN 0-387-95457-0"
          ),
          p(
            class = "hangingindent", 
            "Chang W, Cheng J, Allaire J, Xie Y and McPherson J (2017). shiny:
            Web Application Framework for R. R package version 1.0.3"
          ), 
          p(
            class = "hangingindent", 
            "Chang W and Borges Ribeiro B (2017). shinydashboard: Create
            Dashboards with 'Shiny'. R package version 0.6.1"
          ),
          p(
            class = "hangingindent", 
            " Attali D (2016). shinyjs: Easily Improve the User 
            Experience of Your Shiny Apps in Seconds. R package version 0.9"
          ),
          p(
            class = "hangingindent", 
            "Victor Perrier, Fanny Meyer and David Granjon (2018). 
            shinyWidgets: Custom Inputs Widgets for Shiny.
            R package version 0.4.3."
          ),
          p(
            class = "hangingindent", 
            "Allaire JJ, Xie Y., rmarkdown: Convert R Markdown documents into a variety
            of formats."
          ),
          p(
            class = "hangingindent", 
            "Xie Y., Sarma A., Vogt A., knitr: Provides a general=purpose tool for
            dynamic report generation in R using Literate Programming techniques."
          ),
          p(
            class = "hangingindent", 
            "Carey, R. (2019), boastUtils: BOAST Utilities, R Package. Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ), 
          p(
            class = "hangingindent", 
            "Nijs V., Fang F., shinyAce: Ave editor bidings to enable a rich text editing
            environment within Shiny. R package version 0.4.1."
          ),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent", 
            "G7_Expectancy data: https://www.gapminder.org/data/"
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
        text = "This App Template will help you get started building your own app"
      )
    }
  )
  ### Set up the Go Button ----
  observeEvent(input$go1, {
    updateTabItems(
      session = session, 
      inputId = "pages",
      selected = "prerequisites"
    )
  }
  )
  
  ### Set up the Explore button ----
  observeEvent(input$explore, {
    updateTabItems(
      session = session, 
      inputId = "pages", 
      selected = "explore"
    )
  })
  
  #### G7 submit button render
  observeEvent(
    eventExpr = input$lam,
    handlerExpr = {
      output$lam_icon <- renderIcon()
      output$submit_lam <- renderUI(NULL)
    }
  )

  ## Set up the lambda submit button for G7 ----
  observeEvent(
    eventExpr = input$lam,
    updateTabItems(
      session = session,
      inputId = "lam",
      output$lam_icon <- renderIcon(
        if (input$lam_input <= 5 & input$lam_input >= 4) {condition = "correct"}
        else if (input$lam_input < 4 | input$lam_input >= 3.5) {condition = "partial"}
        else {condition = "incorrect"}
      )
    )
  )

  #### G7 life lambda value submit
  observeEvent(
    eventExpr = input$lam,
    updateTabItems(
      session = session,
      inputId = "lam",
      output$submit_lam <- renderText(({
        if (input$lam_input >= 4 | input$lam_input >= 5) {print("Congratulations! Your guess for \\(\\lambda\\)-value is correct!")}
        else if (input$lam_input < 4 | input$lam_input >= 3.5) {print("You are close!")}
        else {print("Try again!")}
      }))
    )
  )

  ### Set up the lamda submit button for SA ----
  observeEvent(
    eventExpr = input$lam_SA,
    updateTabItems(
      session = session,
      inputId = "lam_SA",
      output$lam_icon_SA <- renderIcon(
        if (input$lam_input_SA <= 2.5 & input$lam_input_SA >= 1.5) {condition = "correct"}
        else if (input$lam_input_SA < 1.5 | input$lam_input_SA >= 1) {condition = "partial"}
        else if (input$lam_input_SA < 3 | input$lam_input_SA > 2.5) {condition = "partial"}
        else {condition = "incorrect"}
      )
    )
  )
  
  
  #### SA life lambda value submit
  observeEvent(
    eventExpr =  input$lam_SA,
    updateTabItems(
      session = session,
      inputId = "lam_SA",
      output$submit_lam_SA <- renderText(({
        if (input$lam_input_SA <= 2.5 & input$lam_input_SA >= 1.5) {print("Congratulations! Your guess for \\(\\lambda\\) value is correct!")}
        else if (input$lam_input_SA < 1.5 | input$lam_input_SA >= 1 ) {print("You are close!")}
        else if (input$lam_input_SA < 3 | input$lam_input_SA > 2.5) {print("You are close!")}
        else {print("Try again!")}
      }))
    )
  )
  # 
  # observeEvent(
  #   eventExpr = input$lam_SA, 
  #   handlerExpr = {
  #     output$lam_icon_SA <- renderIcon(
  #             if (input$lam_input_SA <= 2.5 & input$lam_input_SA >= 1.5) {condition = "correct"}
  #             else if (input$lam_input_SA < 1.5 | input$lam_input_SA >= 1) {condition = "partial"}
  #             else if (input$lam_input_SA < 3 | input$lam_input_SA > 2.5) {condition = "partial"}
  #             else {condition = "incorrect"}
  #           )
  #     output$submit_lam_SA <- renderText(({
  #             if (input$lam_input_SA <= 2.5 & input$lam_input_SA >= 1.5) {print("Congratulations! Your guess for \\(\\lambda\\) value is correct!")}
  #             else if (input$lam_input_SA < 1.5 | input$lam_input_SA >= 1 ) {print("You are close!")}
  #             else if (input$lam_input_SA < 3 | input$lam_input_SA > 2.5) {print("You are close!")}
  #             else {print("Try again!")}
  #           }))
  #   }
  # )  
  # 
  # 
  
  

  ## Set up the Explore Page ----
  
  ### The Data Description ----
  #### SA Data Set
  output$SA_Description <- renderUI({
    p("The data set gives the variables of 13 South America countries averged life expectancy in years
      and income in GDP/capita from year 1900 to 2020, includes Brazil, Argentina, Colombia, Peru, Chile, Ecuador, 
      Venezuela, Bolivia, Uruguay, Suriname, Paraguay, Trinidad and Tobago. Here, the income is used as the predictor variable,
      and life expectancy is used as the response variable for the linear regression model.")
  })
  
  #### Life expect vs Income Data Set  
  output$G7_Description <- renderUI({
    p("The data set gives the variables of G7 countries (Canada, UK, US, Germany, Japan,
    Italy, and France) averaged life expetancy in years 
      and income in GDP/capita from year 1900 to 2020. Here, we use the income as
      predictor variable, and life expectancy as response variable for the linear
      regression model.")
  })
  
  
  
  
  ### The Original Normal Plot ----
  ####SA Normal Plot
  output$SA_Original_Normal <- renderPlot({
    #fit linear regression model, Petal.Length as responding variable
    SA_model1 <- lm(formula = Life_expect ~ Income, data = SA)
    qqnorm(SA_model1$residuals, pch = 16, main = "Normal QQ-Plot for South 
           America Countries Life Expectancy vs. Income",
           xlab = "Sample quantile", 
           ylab = "Sample residuals")
    qqline(SA_model1$residuals, col = "red")
  })
  
  
  #### G7_Expectancy Normal Plot
  output$G7_Original_Normal <- renderPlot({
    G7_model1 <- lm(countryData$Life_expect~countryData$Income)
    qqnorm(G7_model1$residuals, pch = 16, main = "Normal QQ-Plot for U.S. Life Expectancy vs. Income", 
           xlab = "Sample quantile", 
           ylab = "Sample residuals")
    qqline(G7_model1$residuals, col = "red")
  })
  
  ### Transformed Normal Plot ----
  ####SA Transformed Normal Plot
  output$SA_Transformed_Normal <- renderPlot({
    #fit new model using box cox
    #using log when lambda = 0
    if (input$lambda == 0) {
      SA_new_model1 <- lm(log(SA$Life_expect)~SA$Income)
      qqnorm(SA_new_model1$residuals, pch = 16, main = 
               "Normal QQ-Plot for South America
      Countries Life Expectancy vs. 
      Income after Transformation", 
             xlab = "Sample quantile", 
             ylab = "Sample residuals")
      qqline(SA_new_model1$residuals, col = "red")
    }
    else {
      SA_new_model2 <- lm(((SA$Life_expect^input$lambda-1)/input$lambda) ~ SA$Income)
      qqnorm(SA_new_model2$residuals, pch = 16, main =
               "Normal QQ-Plot for South America
       Countries Life Expectancy vs. 
       Income after Transformation", 
             xlab = "Sample quantile", 
             ylab = "Sample residuals")
      qqline(SA_new_model2$residuals, col = "red")
    }
  })
  
  #### G7_Expectancy Transformed Normal Plot
  output$G7_Transformed_Normal <- renderPlot({
    if (input$lambda2 == 0) {
      G7_new_model1 <- lm(log(countryData$Life_expect)~countryData$Income)
      qqnorm(G7_new_model1$residuals, pch = 16, main = "Normal QQ-Plot for U.S. Life Expectancy
             v.s. Income after Transformation", 
             xlab = "Sample quantile", 
             ylab = "Sample residuals")
      qqline(G7_new_model1$residuals, col = "red")
    }
    else {
      G7_new_model2 <- lm(((countryData$Life_expect^input$lambda2-1)/input$lambda2) ~ countryData$Income)
      qqnorm(G7_new_model2$residuals, pch = 16, main = "Normal QQ-Plot for U.S. Life Expectanct
      v.s. Income after Transformation", 
             xlab = "Sample quantile", 
             ylab = "Sample residuals")
      qqline(G7_new_model2$residuals, col = "red")
    }
  })
  
  ### Summary Output ---- 
  ### SA original summary
  output$SA_Original_Summary <- renderPrint({
    SA_model1 <- lm(SA$Life_expect~SA$Income)
    summary(SA_model1)
  })
  
  ### SA Transformed summary 
  output$SA_Transformed_Summary <- renderPrint({
    if (input$lambda == 0) {
      SA_new_model1 <- lm(log(SA$Life_expect)~SA$Income)
      summary(SA_new_model1)
    }
    else{
      SA_new_model2 <- lm(((SA$Life_expect^input$lambda-1)/input$lambda) ~ SA$Income)
      summary(SA_new_model2)
    }
  })
  
  ### G7 Original summary
  output$G7_Original_Summary <- renderPrint({
    G7_model1 <- lm(countryData$Life_expect~countryData$Income)
    summary(G7_model1)
  })
  
  ### G7 Transformed summary
  output$G7_Transformed_Summary <- renderPrint({
    if (input$lambda2 == 0) {
      G7_new_model1 <- lm(log(countryData$Life_expect)~countryData$Income)
      summary(G7_new_model1)
    }
    else {
      G7_new_model2 <- lm(((countryData$Life_expect^input$lambda2-1)/input$lambda2) ~ countryData$Income) 
      summary(G7_new_model2)
    }
  })
  
  ### G7 Box Cox Plot----
  ###Plot
  output$G7_BoxCox <- renderPlot({
    # G7_model1 <- lm(countryData$Life_expect~countryData$Income)
    bc_life <- boxcox(G7_model1, lambda = seq(-2,5, 1/4), data = countryData)
  })
  
  ### G7 lambda value output
  output$G7_bc_value <- renderPrint({
    bc_life_value <- bc_life$x[which.max(bc_life$y)]
  })
  
  ### SA Box Cox Plot ---- 
  ### Plot
  output$SA_BoxCox <- renderPlot({
    bc_SA <- boxcox(object = SA_model1, lambda = seq(-2, 5, 1/4), data = SA)
  })
  
  ### SA lambda value output 
  output$SA_bc_value <- renderPrint({
    bc_SA_value <- bc_SA$x[which.max(bc_SA$y)]
  })
  
  
  
  ## Set up the Challenge Page ----
  ### Rmarkdown knit output 
  observeEvent(input$r_code, {
    withBusyIndicatorServer(
      "r_code", 
      {
        output$knitDoc <- renderUI({
          return(
            isolate(
              HTML(knit2html(
                text = input$rmd, 
                fragment.only = TRUE, 
                quiet = FALSE
              )
              )
            )
          )
        })
      }
    )
  })
  
  output$output <- renderPrint({
    return(isolate(
      r_code(
        parse(
          text = input$code
        )
      )
    ))
  })
}


### Reactive Input

inputs = reactive(
  {
    value = c(input$lambda)
  }
)
# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
