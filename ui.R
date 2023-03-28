# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Bayesian updating: results of a simulation study for two-group comparisons"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      h3("Select scenario"),
      br(),

      radioButtons("HypSet", label = "Hypothesis Set",inline=FALSE,
                   choices = list("Set 1     H0: mu1 = mu2 versus H1: mu1 < mu2" = 1, "Set 2     H0: mu1 = mu2 versus H1: mu1 != mu2" = 2, "Set 3     H0: mu1 > mu2 versus H1: mu1 < mu2" = 3), selected = 1),
      br(),
      
      conditionalPanel(
        condition = "input.HypSet < 3",
      radioButtons("ES", label = "Effect size",inline=FALSE,
                   choices = list("ES=0" = 1, "ES = 0.2" = 2, "ES = 0.5" = 3, "ES = 0.8" = 4), selected = 1),
      br()),
      
      conditionalPanel(
        condition = "input.HypSet ==3",
        radioButtons("ES3", label = "Effect size",inline=FALSE,
                     choices = list( "ES = 0.2" = 2, "ES = 0.5" = 3, "ES = 0.8" = 4), selected = 2),
        br()),
      
      radioButtons("BFtarget", label = "Target BF",inline=FALSE,
                   choices = list("Target BF = 3" = 1, "Target BF = 5" = 2, "Target BF = 10" = 3, "Target BF = 20" = 4), selected = 1),
      br(),

      radioButtons("fraction", label = "Fraction",inline=FALSE,
                   choices = list("Fraction = 1" = 1, "Fraction = 2" = 2, "Fraction = 3" = 3), selected = 1),
      br(),
      
      radioButtons("type", label = "Type of test",inline=FALSE,
                   choices = list("equal variances t-test" = 1, "unequal variances t-test (i.e. Welch's test)" = 2), selected = 1),
      br(),
      
      radioButtons("Nmin", label = "Minimum group size",inline=FALSE,
                   choices = list("Nmin = 5" = 1, "Nmin = 10" = 2, "Nmin = 20" = 3), selected = 1),
      br(),
      
      radioButtons("Nmax", label = "Maximum group size",inline=FALSE,
                   choices = list("Nmax = 50" = 1, "Nmax = 100" = 2, "Nmax = 200" = 3, "Nmax = 50000" = 4), selected = 1),
      br()
      
      
            
  #    submitButton("Submit")
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(   h3("Distribution of Group Size"),
                 plotOutput(outputId = "histN"),
                 textOutput("meanN"),
                 textOutput("medianN"),
                 textOutput("maxN"),
                 br(),
                 textOutput("percnonerror"),
                 textOutput("percerror"),
                 textOutput("percinconclusive"),
                 h3("Distribution of Bayes Factor"),
                 plotOutput(outputId = "histBF",width="50%")
              
    )
  )
)