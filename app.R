# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(dplyr)
library(exactci)
library(plotly)

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "purple",
    ### Create the app header ----
    dashboardHeader(
      title = "Bayesian Inference", 
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Bayesian Inference")
      ),
      tags$li(
        class = "dropdown",
        tags$a(href = 'https://shinyapps.science.psu.edu/',
               icon("house")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Example",tabName = "example",icon = icon("book-open-reader")),
        menuItem("Hypothesis Testing", tabName = "frequentist", icon = icon("wpexplorer")),
        menuItem("Bayesian Inference", tabName = "bayesian", icon = icon("wpexplorer")),
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
          h1("Bayesian Inference and Hypothesis Testing"),
          p("This app provides a comparison between traditional (frequentist)
            inference methods (e.g., ", tags$em("p"),"-values and confidence
            intervals) and Bayesian inference methods (e.g., Bayes Factors and
            credible regions)."),
          h2("Instructions"),
          p("In order to use this app more effectively, it is recommended to 
            explore in the following order."),
          tags$ol(
            tags$li("Review prerequistes using the Prerequistes tab."),
            tags$li("When you're ready to start, use the left-hand menu to select 
                    which activity you wish to exlore.")
          ),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "goPre",
              label = "Prerequisites!",
              size = "large",
              icon = icon("book"),
              style = "default"
            )
          ),
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Jing Fu.",
            br(),
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 11/29/2022 by NJH.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the following
            explanations of various terms by clicking on the plus signs to expand
            the boxes."),
          box(
            title = strong(tags$em("P"),"-value"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "The probability that the test statistic takes on a value at least as
            extreme as we observed when the hull hypothesis is true."
          ),
          box(
            title = strong("\\((1-\\alpha)100\\)% Level Confidence Interval"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "An interval of tested parameter values that give", tags$em("p"), 
            "-values greater than \\(\\alpha\\)."
          ),
          box(
            title = strong("Likelihood"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p("The distribution of the data given the parameter expressed as a function 
            of the parameter. \\[L\\left(\\mu; x\\right)=f\\left(x\\big|\\mu\\right)\\]")
          ),
          box(
            title = strong("Maximum Likelihood Estimate"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "The parameter estimate that maximizes the likelihood function for 
            the observed data."
          ),
          box(
            title = strong("Prior"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p("The distribution of the parameter prior to observing any new data;
              often denoted as \\(P\\left(\\mu\\right)\\).")
          ),
          box(
            title = strong("Posterior"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p("The distribution of the parameter after observing new data. We
              express this as \\[P\\left(\\mu\\big|x\\right)=\\frac{P(\\mu)
              P\\left(x\\big|\\mu\\right)}{P(x)}\\]")
          ),
          box(
            title = strong("\\(1-\\alpha\\) Level Credible Region"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "An interval of parameter values that has a total posterior
            probability of \\(1-\\alpha\\)."
          ),
          box(
            title = strong("Bayes Factor"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p("The ratio of the likelihood of one model (\\(M_1\\)) with respect
              to the likelihood of another model (\\(M_0\\)): 
              \\[\\frac{L\\left(M_1\\right)}{L\\left(M_0\\right)}\\]")
          ),
          box(
            title = strong("Conjugate Prior"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p("A prior distributional model for the parameter of a likelihood such 
              that the posterior distribution follows the same distributional model 
              (but with new parameters reflecting the information provided by the data). 
              Examples used in this app:"),
            tags$ul(
              tags$li("The Beta distribution is a conjugate prior to the Binomial
                      likelihood."),
              tags$li("The Gamma distribution is a conjugate prior to the Poisson
                      likelihood."),
              tags$li("The Normal distribution is a conjugate prior to the Normal
                      likelihood (known variance case).")
            )
          ),
          box(
            title = strong("Noninformative Prior"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            'A noninformative prior is an attempt to provide a distribution that 
            expresses "ignorance" about the parameter at hand. Researchers have 
            made different proposals for how to operationalize that idea. 
            In this app we illustrate the concept by using the uniform distribution 
            as a noninformative prior for the parameter p in a Binomial experiment.'
          )
        ),
        #### Set up Example Page----
        tabItem(
          tabName = "example",
          withMathJax(),
          h2("Example Comparing the Two Approaches"),
          p("Researchers at the University of California at San Diego studied the
            degree to which people have a resemblance to their purebred pet dogs.
            They theorized that people tend to acquire breeds that have some
            similar traits to themselves. Photographs were taken at dog parks
            separately of people (waist up) and their pets; making sure to have
            different backgrounds for the different pictures. Judges then looked
            at pictures of each person and asked which of two dogs looked more 
            like them, their own pet or a randomly chosen purebred dog photographed
            at the same dog park. Out of 25 human-dog pairs, it turned out that
            the judges correctly matched the person and dog 16 times and chose
            the wrong dog in 9 cases. Let", tags$em("p"), "represent the true
            proportion of times the judges can correctly guess which of two dogs
            belongs to a particular person from the population of purebred dog
            owners."),
          h3("(Frequentist) Hypothesis Testing Approach"),
          p("After observing the data, we might seek a confidence interval for",
            tags$em("p"), "and/or test the null hypothesis that \\(p=0.5\\)."),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              wellPanel(
                sliderInput(
                  inputId = "clE1",
                  label = "Confidence level",
                  min = 0.6,
                  max = 0.99,
                  step = 0.01,
                  value = 0.95
                ),
                checkboxInput(
                  inputId = "add1",
                  label = "Show Prior and Posterior plot",
                  value = FALSE
                ),
                withMathJax(
                  p("Null Hypothesis: \\(p_0=0.5\\)"),
                  p("Results from Sample Data:"),
                  p("\\(n = 25\\) judgements", br(), "\\(x = 16\\) correct pairings",
                    br(), "\\(\\widehat{p}=0.64\\)")
                )
              )
            ),
            column(
              width = 8,
              offset = 0,
              plotOutput("pvaluefunctionE1"),
              conditionalPanel(
                condition = "input.add1 == 1",
                plotOutput("plotsgroup1E1")
              ),
              checkboxInput("results1E1",
                            "Results table", FALSE),
              conditionalPanel(
                condition = "input.results1E1==1",
                tableOutput("httableE1")
              )
            )
          ),
          h3("Bayesian Inference Approach"),
          p("Our initial uncertainty about", tags$em("p"), "might be quantified 
            by a prior distribution using the Beta distribution. After observing
            the data, we would look at the posterior distribution of", tags$em("p"),
            "and seek a credible region. To compare different models for what",
            tags$em("p"), "might be with the specific model that p = 0.5,
            we can use the Bayes Factor."),
          fluidRow(
            column(
              width = 4,
              offset = 0,
              wellPanel(
                tags$strong("Beta Prior"),
                sliderInput(
                  inputId = "para1E1",
                  label = "Mean, \\(\\mu\\)",
                  value = 0.5,
                  min = 0,
                  max = 1,
                  step = 0.1
                ),
                sliderInput(
                  inputId = "para2E1",
                  label = "SD, \\(\\sigma\\)",
                  value = 0.289,
                  min = 0,
                  max = 0.5,
                  step = 0.001
                ),
                bsButton(
                  inputId = "nonpriorE1",
                  label = "Use a noninformative prior",
                  size = "large",
                  style = "default"
                ),
                br(),
                br(),
                sliderInput(
                  inputId = "creE1",
                  label = "Credible level",
                  value = 0.95,
                  min = 0.6,
                  max = 0.99,
                  step = 0.01
                ),
                checkboxInput(
                  inputId = "add2",
                  label = "Show P-value Function plot",
                  value = FALSE
                ),
                withMathJax(
                  p("Model for comparison: \\(p_c=0.5\\)"),
                  p("Results from Sample Data:"),
                  p("\\(n = 25\\) judgements", br(), "\\(x = 16\\) correct pairings",
                    br(), "\\(\\widehat{p}=0.64\\)")
                )
              )
            ),
            column(
              width = 8,
              offset = 0,
              plotOutput("credibleE1"),
              conditionalPanel(
                condition = "input.add2 == 1",
                plotOutput("plotsgroup2E1")
              ),
              checkboxInput("results2E1",
                            "Results table", FALSE),
              conditionalPanel(
                condition = "input.results2E1==1",
                tableOutput("bftableE1")),
              br(),
              plotlyOutput("bfE1")
            )
          )
        ),
        #### Set up Frequentist Hypothesis Testing Page----
        tabItem(
          tabName = "frequentist",
          withMathJax(),
          h2("(Frequentist) Hypothesis Testing"),
          p("The following tabs each present a different scenario where we can 
            use (frequentist) hypothesis testing methods to learn about the
            situation presented. Use the sliders to set up a simulation and conduct
            a hypothesis test."),
          br(),
          tabsetPanel(
            id = "scenarioF",
            type = "tabs",
            ##### Binomial context----
            tabPanel(
              title = "Binomial",
              value = "bioF",
              br(),
              p("A food truck sells hot sandwiches with a choice of a chicken, 
                fish, or vegetable patty. The owner of the truck is thinking of 
                moving its location closer to a downtown museum and wonders if 
                that might change the chance,", tags$em("p"), ", that a customer
                will order the vegetable patty (vegetable patties make up a quarter
                of sales at the current location). The owner wonders if the value
                of", tags$em("p"), "at the current location is compatible with
                the data about to be taken at the new location. Use the sliders
                to explore how the owner's decision about the sample size affects
                the result."),
              br(),
              column(
                width = 4,
                offset = 0,
                wellPanel(
                  ###### input parts----
                  sliderInput(
                    inputId = "clF1",
                    label = "Confidence level, \\(1-\\alpha\\)",
                    value = 0.95,
                    min = 0.6,
                    max = 0.99,
                    step = 0.01
                  ),
                  sliderInput(
                    inputId = "nF1",
                    label = "Sample size",
                    value = 25,
                    min = 1,
                    max = 90,
                    step = 1
                  ),
                  sliderInput(
                    inputId = "nullF1",
                    label = "Null hypothesis, \\(p_0\\)",
                    value = 0.01,
                    min = 0,
                    max = 1,
                    step = 0.01
                  ),
                  sliderInput(
                    inputId = "trueF1",
                    label = "True value of \\(p\\)",
                    value = 0.01,
                    min = 0,
                    max = 1,
                    step = 0.01
                  ),
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "simf1",
                      label = "Simulate!",
                      size = "large",
                      style = "default"
                    )
                  )
                )
              ),
              column(
                width = 8,
                offset = 0,
                ###### output----
                plotOutput("pvalueFunction1"),
                checkboxInput("resultsF1",
                              "Results table", FALSE),
                conditionalPanel(
                  condition = "input.resultsF1==1",
                  tableOutput("pvalueF1")
                )
              )
            ),
            tabPanel(
              ##### Poisson context----
              title = "Poisson",
              value = "poiF",
              br(),
              p("You like to eat at your favorite sushi restaurant on Friday 
                evenings so, when you move into a new apartment, you wonder about
                the availability of ride service vehicles to take you across town
                to the restaurant. The previous tenant claimed that there was an
                Uber available on Friday evenings within 1 mile of the apartment
                90% of the time. Knowing that the number of available vehicles
                within a 1-mile radius should be well-modeled by a Poisson 
                distribution, this implies that the mean, \\(\\lambda\\), would 
                be about 2.3 (since \\(e^{-2.3}\\approx 0.1\\) ). You wonder if 
                the value of \\(\\lambda = 2.3\\) will be compatible with the
                data you are about to collect on Friday evenings going forward.
                Use the sliders to explore how your decision about the sample size
                and the true value of \\(\\lambda\\) will affect the result of
                an hypothesis test."),
              br(),
              column(
                width = 4,
                offset = 0,
                wellPanel(
                  ###### input parts----
                  sliderInput(
                    inputId = "clF2",
                    label = "Confidence level, \\(1-\\alpha\\)",
                    value = 0.95,
                    min = 0.6,
                    max = 0.99,
                    step = 0.01
                  ),
                  sliderInput(
                    inputId = "nF2",
                    label = "Sample size",
                    value = 25,
                    min = 1,
                    max = 90,
                    step = 1
                  ),
                  sliderInput(
                    inputId = "nullF2",
                    label = "Null hypothesis, \\(\\lambda_0\\)",
                    value = 0.1,
                    min = 0,
                    max = 10,
                    step = 0.1
                  ),
                  sliderInput(
                    inputId = "trueF2",
                    label = "True value of \\(\\lambda\\)",
                    value = 0.1,
                    min = 0,
                    max = 10,
                    step = 0.1
                  ),
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "simf2",
                      label = "Simulate!",
                      size = "large",
                      style = "default"
                    )
                  )
                )
              ),
              column(
                width = 8,
                offset = 0,
                ###### output----
                plotOutput("pvalueFunction2"),
                checkboxInput("resultsF2",
                              "Results table", FALSE),
                conditionalPanel(
                  condition = "input.resultsF2==1",
                  tableOutput("pvalueF2")
                )
              )
            ),
            tabPanel(
              title = "Normal",
              value = "norF",
              ##### Normal context----
              br(),
              p("Marine biologists are able to track the movements of whales in
                the wild using GPS devices attached by suction cups. Knowing the
                location of a whale allows them to measure the size of the whale
                at any time using a swarm of drones sent to the location that take
                multiple images from different angles giving a three-dimensional
                model of the whale’s size. Finally, those volume measurements are
                converted to measures of body mass based on estimates for the
                average density of different tissue types for the particular
                species at hand. This system has been calibrated for accuracy with
                whales that had beached themselves where direct measurements were
                taken and it was found to be unbiased on the log scale with a
                standard deviation of about 6% of the whale’s weight (regardless
                of life stage or size of the whale). Researchers now want to test
                the system in wild whales that are captured and released to see 
                if they are still unbiased on the log scale. The difference 
                between the log of their weights and the estimated log weight 
                based on the volume measurements can be modeled as a normal
                distribution with an unknown mean, \\(\\mu\\) and a standard
                deviation of about 0.058 (\\(\\ln(1.06)\\)). Use the sliders 
                to explore how a hypothesis test and confidence interval for
                \\(\\mu\\) might  depend on the sample size, the confidence level,
                and the true value of \\(\\mu\\)."),
              br(),
              column(
                width = 4,
                offset = 0,
                wellPanel(
                  ###### input parts----
                  sliderInput(
                    inputId = "clF3",
                    label = "Confidence level, \\(1-\\alpha\\)",
                    value = 0.95,
                    min = 0.6,
                    max = 0.99,
                    step = 0.01
                  ),
                  sliderInput(
                    inputId = "nF3",
                    label = "Sample size",
                    value = 25,
                    min = 1,
                    max = 90,
                    step = 1
                  ),
                  sliderInput(
                    inputId = "nullF3",
                    label = "Null hypothesis, \\(\\mu_0\\)",
                    value = 0,
                    min = -0.5,
                    max = 0.5,
                    step = 0.01
                  ),
                  sliderInput(
                    inputId = "trueF3",
                    label = "True value of \\(\\mu\\)",
                    value = 0,
                    min = -0.5,
                    max = 0.5,
                    step = 0.01
                  ),
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "simf3",
                      label = "Simulate!",
                      size = "large",
                      style = "default"
                    )
                  )
                )
              ),
              column(
                width = 8,
                offset = 0,
                ###### output----
                plotOutput("pvalueFunction3"),
                checkboxInput("resultsF3",
                              "Results table", FALSE),
                conditionalPanel(
                  condition = "input.resultsF3==1",
                  tableOutput("pvalueF3")
                )
              )
            )
          ),
          fluidRow(
            # This extra fluid row helps to ensure that tabset panel is fully
            # contained in the page
          )
        ),
        #### Set up Bayesian Inference Page----
        tabItem(
          tabName = "bayesian",
          withMathJax(),
          h2("Bayesian Hypothesis Testing"),
          p("The following tabs each present a different scenario where we can 
            use Bayesian inference methods to learn about the situation presented.
            Use the sliders to set up a simulation and analyze the results."),
          br(),
          tabsetPanel(
            id = "scenarioB",
            type = "tabs",
            ##### Binomial context----
            tabPanel(
              title = "Binomial",
              value = "bioB",
              br(),
              p("A food truck sells hot sandwiches  with a choice of a chicken, 
                fish, or vegetable patty. The owner of the truck is thinking 
                of moving its location closer to a downtown museum and wonders 
                if that might change the chance,", tags$em("p"), ", that a customer
                will order the vegetable patty. To model the uncertainty about 
                the value of", tags$em("p"), "at the new location, the owner will
                use the Beta distribution. Since the vegetable patty made up 
                about a quarter of sales at the old location, the best prior
                information would have \\(\\beta = 3\\alpha\\) so that the expected
                value of", tags$em("p"), "would be 0.25 (that is, \\(E(p)=\\alpha/
                \\left(\\alpha+\\beta\\right)\\)). Knowing that this may be off 
                by a good deal, the owner wants to have a standard deviation that 
                is also about 0.25. The owner is about to take data at the new 
                location.Use the slider to explore how the researcher's decisions 
                about the sample size and prior knowledge affect the result."),
              br(),
              column(
                width = 4,
                offset = 0,
                wellPanel(
                  ###### bayes input parts----
                  tags$strong("Beta prior"),
                  sliderInput(
                    inputId = "para1B1",
                    label = "Mean, \\(\\mu\\)",
                    value = 0.5,
                    min = 0,
                    max = 1,
                    step = 0.1
                  ),
                  sliderInput(
                    inputId = "para2B1",
                    label = "SD, \\(\\sigma\\)",
                    value = 0.289,
                    min = 0,
                    max = 0.5,
                    step = 0.001
                  ),
                  bsButton(
                    inputId = "nonpriorB1",
                    label = "Use a noninformative prior",
                    size = "large",
                    style = "default"
                  ),
                  br(),
                  br(),
                  sliderInput(
                    inputId = "clB1",
                    label = "Credible level, \\(1-\\alpha\\)",
                    value = 0.95,
                    min = 0.6,
                    max = 0.99,
                    step = 0.01
                  ),
                  sliderInput(
                    inputId = "nB1",
                    label = "Sample size",
                    value = 5,
                    min = 1,
                    max = 90,
                    step = 1
                  ),
                  sliderInput(
                    inputId = "nullB1",
                    label = "Model for comparison, \\(p_c\\)",
                    value = 0.01,
                    min = 0,
                    max = 1,
                    step = 0.01
                  ),
                  sliderInput(
                    inputId = "trueB1",
                    label = "True value of \\(p\\)",
                    value = 0.01,
                    min = 0,
                    max = 1,
                    step = 0.01
                  ),
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "simb1",
                      label = "Simulate!",
                      size = "large",
                      style = "default"
                    )
                  )
                )
              ),
              column(
                width = 8,
                offset = 0,
                ###### bayes outputs----
                plotOutput("credibleB1"),
                checkboxInput("resultsB1",
                              "Results table", FALSE),
                conditionalPanel(
                  condition = "input.resultsB1==1",
                  tableOutput("tableB1")
                ),
                br(),
                plotlyOutput("bfB1")
              )
            ),
            tabPanel(
              title = "Poisson",
              value = "poiB",
              ##### Poisson context----
              br(),
              p("You like to eat at your favorite sushi restaurant on Friday 
                evenings so, when you move into a new apartment, you wonder about 
                the availability of ride service vehicles to take you across town 
                to the restaurant. The previous tenant claimed that there was an 
                Uber available on Friday evenings within 1 mile of the apartment 
                90% of the time. Knowing that the number of available vehicles 
                within a 1-mile radius should be well modeled by a Poisson distribution, 
                this implies that the mean, \\(\\lambda\\) would be about 2.3 
                (since \\(e^{-2.3}\\approx 0.1\\))e. Of course, there is still a
                great deal of uncertainty about the value of \\(\\lambda\\) that
                you model by assuming \\(\\lambda\\) follows a Gamma distribution.
                Before taking any data, you need to decide on the parameters of
                this prior distribution and how much data to collect. Use the 
                sliders to see how your decisions might affect the posterior
                distribution of \\(\\lambda\\) and how much you might favor
                one value of \\(\\lambda\\) over another."),
              br(),
              column(
                width = 4,
                offset = 0,
                wellPanel(
                  ###### bayes input parts----
                  tags$strong("Gamma prior"),
                  sliderInput(
                    inputId = "para1B2",
                    label = "Shape, \\(\\alpha\\)",
                    value = 3,
                    min = 1,
                    max = 10,
                    step = 1
                  ),
                  sliderInput(
                    inputId = "para2B2",
                    label = "Rate, \\(\\beta\\)",
                    value = 2,
                    min = 1,
                    max = 10,
                    step = 1
                  ),
                  sliderInput(
                    inputId = "clB2",
                    label = "Credible level, \\(1-\\alpha\\)",
                    value = 0.95,
                    min = 0.6,
                    max = 0.99,
                    step = 0.01
                  ),
                  sliderInput(
                    inputId = "nB2",
                    label = "Sample size",
                    value = 5,
                    min = 1,
                    max = 90,
                    step = 1
                  ),
                  sliderInput(
                    inputId = "nullB2",
                    label = "Model for comparison, \\(\\lambda_c\\)",
                    value = 0.1,
                    min = 0,
                    max = 10,
                    step = 0.1
                  ),
                  sliderInput(
                    inputId = "trueB2",
                    label = "True value for \\(\\lambda\\)",
                    value = 0.1,
                    min = 0,
                    max = 10,
                    step = 0.1
                  ),
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "simb2",
                      label = "Simulate!",
                      size = "large",
                      style = "default"
                    )
                  )
                )
              ),
              column(
                width = 8,
                offset = 0,
                ###### bayes outputs----
                plotOutput("credibleB2"),
                checkboxInput("resultsB2",
                              "Results table", FALSE),
                conditionalPanel(
                  condition = "input.resultsB2==1",
                  tableOutput("tableB2")
                ),
                br(),
                plotlyOutput("bfB2")
              )
            ),
            tabPanel(
              title = "Normal",
              value = "norB",
              ##### Normal context----
              br(),
              p("Marine biologists are able to track the movements of whales in 
                the wild using GPS devices attached by suction cups. Knowing the
                location of a whale allows them to measure the size of the whale
                at any time using a swarm of drones sent to the location that take
                multiple images from different angles giving a three-dimensional 
                model of the whale’s size.Finally, those volume measurements are 
                converted to measures of body mass based on estimates for the average 
                density of different tissue types for the particular species at 
                hand. This system has been calibrated for accuracy with whales 
                that had beached themselves where direct measurements were taken
                and it was found to be unbiased on the log scale with a standard
                deviation of about 6% of the whale’s weight (regardless of life 
                stage or size of the whale). Researchers now want to test the
                system in wild whales that are captured and released to see if 
                they are still unbiased on the log scale. The difference between
                the log of their weights and the estimated log weight based on the 
                volume measurements can be modeled as a normal distribution with 
                an unknown mean, \\(\\mu\\), and a standard deviation of about
                0.058 (\\(\\ln(1.06)\\)). Prior to collecting data, the uncertainty
                in the value of \\(\\mu\\) is modeled as a Normal distribution 
                with a mean of \\(\\tau\\) and a standard deviation of \\(\\sigma\\).
                Use the sliders to see how your decisions about the prior 
                distribution of \\(\\mu\\) might affect its posterior distribution 
                and how much you might favor one value over another."),
              br(),
              column(
                width = 4,
                offset = 0,
                wellPanel(
                  ###### bayes input parts----
                  tags$strong("Normal prior"),
                  sliderInput(
                    inputId = "para1B3",
                    label = "Mean, \\(\\tau\\)",
                    value = 0,
                    min = -1,
                    max = 1,
                    step = 0.1
                  ),
                  sliderInput(
                    inputId = "para2B3",
                    label = "SD, \\(\\sigma\\)",
                    value = 0.01,
                    min = 0,
                    max = 1,
                    step = 0.001
                  ),
                  sliderInput(
                    inputId = "clB3",
                    label = "Credible level, \\(1-\\alpha\\)",
                    value = 0.95,
                    min = 0.6,
                    max = 0.99,
                    step = 0.01
                  ),
                  sliderInput(
                    inputId = "nB3",
                    label = "Sample size",
                    value = 5,
                    min = 1,
                    max = 90,
                    step = 1
                  ),
                  sliderInput(
                    inputId = "nullB3",
                    label = "Model for comparison, \\(\\mu_c\\)",
                    value = 0,
                    min = -0.5,
                    max = 0.5,
                    step = 0.01
                  ),
                  sliderInput(
                    inputId = "trueB3",
                    label = "True value for \\(\\mu\\)",
                    value = 0,
                    min = -0.5,
                    max = 0.5,
                    step = 0.01
                  ),
                  div(
                    style = "text-align: center;",
                    bsButton(
                      inputId = "simb3",
                      label = "Simulate!",
                      size = "large",
                      style = "default"
                    )
                  )
                )
              ),
              column(
                width = 8,
                offset = 0,
                ###### bayes outputs----
                plotOutput("credibleB3"),
                checkboxInput("resultsB3",
                              "Results table", FALSE),
                conditionalPanel(
                  condition = "input.resultsB3==1",
                  tableOutput("tableB3")
                ),
                br(),
                plotlyOutput("bfB3")
              )
            )
          ),
          fluidRow(
            # This extra fluid row helps to ensure that tabset panel is fully
            # contained in the page
          )
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from https://CRAN.R-project.org/package=shinyBS"
          ),
          p(
            class = "hangingindent",
            "Carey, R. and Hatfield, N. J. (2022). boastUtils: BOAST utlities.
            (v 0.1.12.3). [R package]. Available from https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = "hangingindent",
            "Change, W., and Borges Ribeiro, B. (2021). shinydashboard: Create 
            dashboards with 'shiny'. (v 0.7.2) [R package]. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = "hangingindent",
            "Chang, W., Cheng J., Allaire, J., Sievert, C., Schloerke, B., Xie, Y.,
            Allen, J., McPherson, J., Dipert, A., and Borges, B. (2021). shiny:
            Web application framework for R. (v 1.7.1). [R package]. Available
            from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = "hangingindent",
            "Fay, M. (2010). exactci: Exact P-values and Matching Confidence 
            Intervals for simple Discrete Parametric Cases. (v 1.4-2).[R package].
            Available from 
            https://CRAN.R-project.org/package=exactci
            "
          ),
          p(
            class = "hangingindent",
            "Perrier, V., Meyer, F., and Granjon, D. (2022). shinyWidgets: Custom
            inputs widgets for shiny. (v 0.7.0). [R package]. Available from
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = "hangingindent",
            "Sievert, C. (2020). plotly: Interactive Web-Based Data Visualization 
            with R, plotly, and shiny. Chapman and Hall/CRC Florida, 2020."
          ),
          p(
            class = "hangingindent",
            "Wickham, H., François, R., Henry, L., Müller, K. (2022). dplyr: A 
            Grammar of Data Manipulation. (v 1.0.9).[R package]. Available from 
            https://dplyr.tidyverse.org"
          ),
          p(
            class = "hangingindent",
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Springer-Verlag:New York. (v 3.3.6) [R package]. Available from
            https://ggplot2.tidyverse.org"
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
  boastUtils::typesetMath(session = session)
  ## Set up Info button ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "Use this software to explore hypothesis testing and Bayesian
        hypothesis testing."
      )
    }
  )
  ## Set buttons----
  ### prerequisites button
  observeEvent(
    eventExpr = input$goPre,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
      )
    }
  )
  
  # Example Page----
  ## parameters
  ## parameters range
  ## sd < sqrt(mu(1-mu))
  observeEvent(
    eventExpr = input$para1E1,
    handlerExpr = {
      updateSliderInput(
        session = session,
        inputId = "para2E1",
        max = round(sqrt(input$para1E1*(1 - input$para1E1)), digits = 2)
      )
    }
  )
  ## update noninformative prior
  observeEvent(
    eventExpr = input$nonpriorE1,
    handlerExpr = {
        updateSliderInput(
          session = session,
          inputId = "para1E1",
          value = 1/2
        )
        updateSliderInput(
          session = session,
          inputId = "para2E1",
          value = sqrt(1/12)
        )
    }
  )

  ## n =25, success=16
  playplots <- reactiveValues(
    pvaluefunction = NULL,
    credible = NULL
  )
  output$pvaluefunctionE1 <- renderPlot({
    validate(
      need(
        expr = input$clE1 != 1,
        message =  "Confidence Levels must be positive values less than 1"
      )
    )
    alphaP <- 1 - input$clE1
    ### calculate pvalue
    pValue <- binom.exact(
      x = 16,
      n = 25,
      p = 0.5,
      alternative = "two.side",
      tsmethod = "central"
    )$p.value
    ### ci
    ciP <- binom.exact(
      x = 16,
      n = 25,
      p = 0.5,
      alternative = "two.side",
      tsmethod = "central",
      conf.level = input$clE1
    )$conf.int

    ### get p-value list
    changeP <- (1 - 0)/1000
    thetaP <- 0
    pvaluelistP <- c()
    thetalistP <- c()
    genepvaluesP <- function(thetaP){
      binom.exact(
        x = 16,
        n = 25,
        p = thetaP,
        alternative = "two.side",
        tsmethod = "central"
      )$p.value
    }
    while (thetaP <= 1) {
      pvaluesP <- genepvaluesP(thetaP)
      pvaluelistP <- c(pvaluelistP, pvaluesP)
      thetalistP <- c(thetalistP, thetaP)
      thetaP <- thetaP + changeP
    }
    
    ### plot
    data <- as.data.frame(cbind(thetalistP, pvaluelistP))
    data <- rename(data, theta = thetalistP)
    data <- rename(data,pValue = pvaluelistP)
    gP <- ggplot() +
      geom_line(
        data = data,
        mapping = aes(x = theta,y = pValue),
        color = "blue",
        linewidth = 1,
        alpha = 0.5
      ) +
      scale_x_continuous(
        limits = c(0, 1),
        expand = expansion(mult = 0, add = 0),
        breaks = seq.int(from = 0, to = 1, by = 0.2),
        labels = c("0","0.2","0.4","0.6", "0.8", "1")
      ) +
      scale_y_continuous(
        expand = expansion(mult = 0.05),
        breaks = seq.int(from = 0, to = 1, by = 0.2),
        labels = c("0","0.2","0.4","0.6", "0.8", "1")
      ) +
      labs(
        title = "P-value Function",
        x = "Null hypothesis proportion p", 
        y = "P-value",
        alt = "A plot of a set of p values versus different proportions "
      ) +
      geom_segment(
        mapping = aes(
          x = ciP[1],
          xend = ciP[2],
          y = 0,
          yend = 0,
          colour = "Confidence interval"
        ),
        linewidth = 1,
        na.rm = TRUE
      ) +
      geom_point(
        mapping = aes(x = c(ciP[1], ciP[2]), y = c(0,0)),
        alpha = 0
      ) +
      geom_errorbarh(
        mapping = aes(
          xmin = ciP[1],
          xmax = ciP[2],
          y = 0,
          colour = "Confidence interval"
        ),
        height = 0.05*1,
        linewidth = 1
      ) +
      geom_segment(
        mapping = aes(
          x = 0,
          xend = 16/25,
          y = 1,
          yend = 1,
          colour = "Observed estimate"
        ),
        na.rm = TRUE
      ) +
      geom_segment(
        mapping = aes(
          x = 16/25,
          xend = 16/25,
          y = 0,
          yend = 1,
          colour = "Observed estimate"
        ),
        na.rm = TRUE
      ) +
      geom_segment(
        mapping = aes(
          x = 0,
          xend = 0.5,
          y = pValue,
          yend = pValue,
          colour = "Null value"
        ),
        na.rm = TRUE
      ) +
      geom_segment(
        mapping = aes(
          x = 0.5,
          xend = 0.5,
          y = 0,  
          yend = pValue,
          colour = "Null value"
        ),
        na.rm = TRUE
      ) +
      scale_color_manual(
        name = NULL,
        values = c(
          "Confidence interval" = psuPalette[1],
          "Observed estimate" = psuPalette[4],
          "Null value" = "black"
        )
      ) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom",
        plot.margin = margin(t = 0, b = 0, r = 1, l = 1, unit = "cm")
      )
    
    playplots$pvaluefunction <- gP
    
    return(gP)
  })
  
  output$httableE1 <- renderTable(
    expr = {
      validate(
        need(expr = input$clE1 != 1, message = "")
      )
      pValue <- binom.exact(
        x = 16,
        n = 25,
        p = 0.5,
        alternative = "two.side",
        tsmethod = "central"
      )$p.value
      ciP <- binom.exact(
        x = 16,
        n = 25,
        p = 0.5,
        alternative = "two.side",
        tsmethod = "central",
        conf.level = input$clE1
      )$conf.int
      ctable <- matrix(
        data = c(round(pValue, 3), round(ciP[1], 3), round(ciP[2], 3)),
        nrow = 1
      )
      c2 <- paste("Confidence interval", " lower bound", sep = "<br>")
      c3 <- paste("Confidence interval", " upper bound", sep = "<br>")
      colnames(ctable) <- c("p-value", c2, c3)
      ctable
    },
    bordered = TRUE,
    sanitize.text.function = identity
  )
  
  ### Bayes----
  s1E <- reactive(((1 - input$para1E1)/input$para2E1^2 - 1/input$para1E1)*input$para1E1^2)
  s2E <- reactive((((1 - input$para1E1)/input$para2E1^2 - 1/input$para1E1)*input$para1E1^2)*(1/input$para1E1 - 1))
  
  output$credibleE1 <- renderPlot({
    validate(
      need(
        expr = input$creE1 != 1,
        message = "Credible Levels must be positive values less than 1"
      ),
      need(
        expr = input$para1E1 > 0 && input$para1E1 < 1,
        message = "Provide a valid prior"
      ),
      need(
        expr  = input$para2E1 > 0 &&
          input$para2E1 < sqrt(input$para1E1*(1 - input$para1E1)),
        message = "Provide a valid prior"
      )
    )
    ###prior
    pRange <- seq(0, 1, length = 1000)
    # density
    priory <- dbeta(x = pRange, shape1 = s1E(), shape2 = s2E())
    priordata <- data.frame("x" = pRange, "priory" = priory)
    ###posterior
    posteriory <- dbeta(x = pRange, shape1 = s1E() + 16, shape2 = s2E() + 9)
    posteriordata <- data.frame("x" = pRange, "posteriory" = posteriory)
    ### Credible region
    alphaB <- 1 - input$creE1
    ci <- qbeta(c(alphaB/2, 1 - alphaB/2), shape1 = s1E() + 16, shape2 = s2E() + 9)
    mllE1 <- posteriordata[which.max(posteriordata$posteriory), ]
    mle <- if (input$para1E1 == 1/2 && input$para2E1 == 0.289) {
      geom_segment(
        aes(
          x = mllE1$x,
          xend = mllE1$x,
          y = 0,
          yend = mllE1$posteriory,
          colour = "Max Log-Likelihood",
          linetype = "Max Log-Likelihood"
        ),
        linewidth = 1,
        na.rm = TRUE
      )
    }
    ### combined plot
    combined1 <- ggplot() +
      geom_line(
        data = priordata,
        mapping = aes(x = x, y = priory, colour = "Prior", linetype = "Prior")
      ) +
      geom_line(
        data = posteriordata,
        mapping = aes(x = x, y = posteriory, colour = "Posterior", linetype = "Posterior")
      )
    combined <- combined1 +
    geom_segment(
      mapping = aes(
        x = ci[1],
        xend = ci[2],
        y = 0,
        yend = 0,
        colour = "Credible region",
        linetype = "Credible region"
      ),
      linewidth = 1,
      na.rm = TRUE
    ) +
    geom_point(
      mapping = aes(x = c(ci[1], ci[2]), y = c(0, 0)),
      alpha = 0
    ) +
    geom_errorbarh(
      mapping = aes(
        xmin = ci[1],
        xmax = ci[2],
        y = 0,
        colour = "Credible region",
        linetype = "Credible region"
      ),
      height = 0.05*layer_scales(combined1)$y$get_limits()[2],
      linewidth = 2
    ) +
    mle +
    labs(
      title = "Prior and Posterior",
      x = "Proportion p",
      y = "Density",
      alt = "The plot combined the prior distribution and the posterior plot"
    ) +
    scale_x_continuous(
      limits = c(0, 1),
      expand = expansion(mult = 0, add = 0),
      breaks = seq.int(from = 0, to = 1, by = 0.2),
      labels = c("0","0.2","0.4","0.6", "0.8", "1")
    ) +
    scale_y_continuous(expand = expansion(mult = .05)) +
    scale_color_manual(
      name = NULL,
      values = c(
        "Credible region" = psuPalette[1],
        "Prior" = psuPalette[3],
        "Posterior" = psuPalette[2],
        "Max Log-Likelihood" = "blue"
      )
    ) +
    scale_linetype_manual(
      name = NULL,
      values = c(
        "Credible region" = "solid",
        "Prior" = "solid",
        "Posterior" = "solid",
        "Max Log-Likelihood" = "dashed"
      )
    ) +
    theme_bw() +
    theme(
      plot.caption = element_text(size = 18),
      text = element_text(size = 18),
      axis.title = element_text(size = 16),
      legend.position = "bottom",
      plot.margin = margin(t = 0, b = 0, r = 1, l = 1.5, unit = "cm")
    )
    playplots$credible <- combined
    return(combined)
  })
  
  output$bfE1 <- renderPlotly({
    validate(
      need(
        expr = input$creE1 != 1,
        message = "Credible Levels must be positive values less than 1"
      ),
      need(
        expr = input$para1E1 > 0 && input$para1E1 < 1,
        message = "Provide a valid prior"
      ),
      need(
        expr = input$para2E1 > 0 &&
          input$para2E1 < sqrt(input$para1E1*(1 - input$para1E1)),
        message = "Provide a valid prior"
      )
    )

    pRange <- seq(0, 1, length = 1000)
    likelihoody <- dbinom(x = 16, size = 25, prob = pRange)
    # denominator
    denominator <- dbinom(x = 16, size = 25, prob = 0.5)
    # log(10)
    bf <- log10(likelihoody/denominator)
    bfdata <- data.frame(x = pRange, bf = bf)
    # label symbol
    pc <- expression(p[c]~v)
    # max ll
    maxll <- bfdata[which.max(bfdata$bf), ]
    # return finite min
    minbf <- min(ifelse(is.infinite(bfdata$bf), NA, bfdata$bf), na.rm = TRUE)
    g <- plot_ly(
      data = bfdata,
      x = ~x,
      y = ~bf,
      type = "scatter",
      mode = "lines",
      opacity = 0.4,
      line = list(
        width = 3,
        dash = "solid",
        color = "blue"
      ),
      text = paste("p:", round(bfdata$x,3), "<br>Log(BF):", round(bfdata$bf, 3)),
      showlegend = F,
      hoverinfo = "text"
    ) %>%
      add_trace(
        x = c(seq(0, 0.5, len = 1000), rep(0.5, 1000)),
        y = c(rep(0, 1000), seq(0, minbf, len = 1000)),
        mode = "lines",
        hoverinfo = "text",
        name = "p<sub>c</sub> vs. p<sub>c</sub>",
        opacity = 1,
        showlegend = T,
        text = paste("p:", 0.5, "<br>Log(BF):", 0),
        line = list(
          width = 1,
          dash = "dot",
          color = psuPalette[4]
        )
      ) %>%
      add_trace(
        x = c(seq(0, maxll$x, len = 1000), rep(maxll$x, 1000)),
        y = c(rep(maxll$bf, 1000), seq(minbf, maxll$bf, len = 1000)),
        mode = "lines",
        hoverinfo = "text",
        name = "Max Log-Likelihood",
        opacity = 1,
        showlegend = T,
        text = paste("p:", round(maxll$x, 3), "<br>Log(BF):", round(maxll$bf, 3)),
        line = list(
          width = 1,
          dash = "dash",
          color = "blue"
        )
      ) %>%
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "pan2d", "autoScale2d",
                                   "hoverCompareCartesian", "hoverClosestCartesian",
                                   "toImage")
      ) %>%
      layout(
        title = list(
          text = "Log<sub>10</sub> Bayes Factor Compared to p<sub>c</sub>",
          font = list(size = 18),
          xanchor = "right"
        ),
        xaxis = list(title = "Proportion p", titlefont = list(size = 16)),
        yaxis = list(title = "Log Bayes Factor", titlefont = list(size = 16)),
        showlegend = T,
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.3))
    return(g)
  })
  
  output$bftableE1 <- renderTable(
    expr = {
      validate(
        need(expr = input$creE1 != 1, message = ""),
        need(expr = input$para1E1 > 0 && input$para1E1 < 1, message = ""),
        need(
          expr = input$para2E1 > 0 &&
            input$para2E1 < sqrt(input$para1E1*(1 - input$para1E1)),
          message = ""
        )
      )
      # ci
      alphaB <- 1 - input$creE1
      ci <- qbeta(c(alphaB/2, 1 - alphaB/2), shape1 = s1E() + 16, shape2 = s2E() + 9)
      c1 <- paste("Credible region", " lower bound", sep = "<br>")
      c2 <- paste("Credible region", " upper bound", sep = "<br>")
      ctable <- matrix(c(round(ci[1], 3), round(ci[2], 3)), nrow = 1)
      colnames(ctable) <- c(c1, c2)
      ctable
    },
    bordered = TRUE,
    sanitize.text.function = identity
  )
  
  output$plotsgroup1E1 <- renderPlot({
    playplots$credible
  })
  
  output$plotsgroup2E1 <- renderPlot({
    playplots$pvaluefunction
  })
  
  ## HT & BI Pages----
  ### Binomial Situation ----
  #### Update buttons and sliders ----
  observeEvent(
    eventExpr = input$simf1,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "simf1",
        label = "Re-simulate!",
        style = "default",
        disabled = FALSE 
      )
    }
  )
  observeEvent(
    eventExpr = input$simb1,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "simb1",
        label = "Re-simulate!",
        style = "default",
        disabled = FALSE 
      )
    }
  )
  
  ## parameters range
  ## sd < sqrt(mu(1-mu))
  observeEvent(
    eventExpr = input$para1B1,
    handlerExpr = {
      updateSliderInput(
        session = session,
        inputId = "para2B1",
        max = round(sqrt(input$para1B1*(1 - input$para1B1)), 2)
        )
    }
  )
  observeEvent(
    eventExpr = input$nonpriorB1,
    handlerExpr = {
        updateSliderInput(
          session = session,
          inputId = "para1B1",
          value = 1/2
        )
        updateSliderInput(
          session = session,
          inputId = "para2B1",
          value = sqrt(1/12)
        )
    }
  )
  
  ### Update inputs---
  ### Frequentist side
  
  # NO!!!!!!!
  # trueF1 <- eventReactive(
  #   eventExpr = input$simf1,
  #   valueExpr = {
  #     input$trueF1
  #   }
  # )
  # 
  # nullF1<-eventReactive({
  #   input$simf1
  # },
  # valueExpr = {
  #   input$nullF1
  # }
  # )
  # 
  # nF1<-eventReactive({
  #   input$simf1
  # },
  # valueExpr = {
  #   input$nF1
  # }
  # )
  # 
  # clF1<-eventReactive({
  #   input$simf1
  # },
  # valueExpr = {
  #   input$clF1
  # }
  # )
  # ### Bayes side
  # trueB1<-eventReactive({
  #   input$simb1
  # },
  # valueExpr = {
  #   input$trueB1
  # }
  # )
  # ## mu -> shape1
  # para1B1<-eventReactive({
  #   input$simb1
  # },
  # valueExpr = {
  #   ((1-input$para1B1)/input$para2B1^2-1/input$para1B1)*input$para1B1^2
  # }
  # )
  # ## sd ->shape2
  # para2B1<-eventReactive({
  #   input$simb1
  # },
  # valueExpr = {
  #   (((1-input$para1B1)/input$para2B1^2-1/input$para1B1)*input$para1B1^2)*(1/input$para1B1-1)
  # }
  # )
  # 
  # clB1<-eventReactive({
  #   input$simb1
  # },
  # valueExpr = {
  #   input$clB1
  # }
  # )
  # 
  # nB1<-eventReactive({
  #   input$simb1
  # },
  # valueExpr = {
  #   input$nB1
  # }
  # )
  # 
  # nullB1<-eventReactive({
  #   input$simb1
  # },
  # valueExpr = {
  #   input$nullB1
  # }
  # )
  
  #### Outputs----
  ##### Frequentist Side----
  freqBinCheck <- reactiveVal(0)
  simDataBF <- reactiveVal(NULL)
  
  observeEvent(
    eventExpr = c(input$clF1, input$nF1, input$nullF1, input$trueF1),
    handlerExpr = {freqBinCheck(0)}
  )
  
  observeEvent(
    eventExpr = input$simf1,
    handlerExpr = {
      simDataBF(sample(
        x = c(0, 1),
        size = input$nF1,
        prob = c(1 - input$trueF1, input$trueF1),
        replace = TRUE
      ))
      freqBinCheck(1)
    }
  )
  
  ###### Make Bin. Plot ----
  output$pvalueFunction1 <- renderPlot({
    validate(
      need(
        expr = input$nullF1 != 0 && input$nullF1 != 1,
        message = "Provide a null hypothesis p between (0,1)"
      ),
      need(
        expr = input$trueF1 != 0 && input$trueF1 != 1,
        message = "Provide a true p between (0,1)"
      ),
      need(
        expr = input$simf1 >= 1 & freqBinCheck() == 1,
        message = "Click the Simulate button to generate new data"
      ),
    )

    alphaP <- 1 - input$clF1
    ### calculate pvalue
    pValue <- binom.exact(
      x = sum(simDataBF()),
      n = input$nF1,
      p = input$nullF1,
      alternative = "two.side",
      tsmethod = "central"
    )$p.value
    ### calculate true value's pvalue
    truePvalue <- binom.exact(
      x = sum(simDataBF()),
      n = input$nF1,
      p = input$trueF1,
      alternative = "two.side",
      tsmethod = "central"
    )$p.value
    ### ci
    ciP <- binom.exact(
      x = sum(simDataBF()),
      n = input$nF1,
      p = input$nullF1,
      alternative = "two.side",
      tsmethod = "central",
      conf.level = input$clF1
    )$conf.int
    ### set xlim
    cimaxP <- binom.exact(
      x = sum(simDataBF()),
      n = input$nF1,
      p = input$nullF1,
      alternative = "two.side",
      tsmethod = "central",
      conf.level = 0.999
    )$conf.int
    
    xlimP <- c(max(0, cimaxP[1]), cimaxP[2])
    
    ### get p-value list
    changeP <- (xlimP[2] - xlimP[1])/1500
    thetaP <- xlimP[1]
    pvaluelistP <- c()
    thetalistP <- c()
    genepvaluesP <- function(thetaP){
      binom.exact(
        x = sum(simDataBF()),
        n = input$nF1,
        p = thetaP,
        alternative = "two.side",
        tsmethod = "central"
      )$p.value
    }
    while (thetaP <= xlimP[2]) {
      pvaluesP <- genepvaluesP(thetaP)
      pvaluelistP <- c(pvaluelistP, pvaluesP)
      thetalistP <- c(thetalistP, thetaP)
      thetaP <- thetaP + changeP
    }
    
    ### plot
    data <- as.data.frame(cbind(thetalistP, pvaluelistP))
    data <- rename(data, theta = thetalistP)
    data <- rename(data, pValue = pvaluelistP)
    gP <- ggplot() +
      geom_line(
        data = data,
        mapping = aes(x = theta, y = pValue),
        color = "blue",
        linewidth = 1,
        alpha = 0.5
      ) +
      scale_x_continuous(
        limits = xlimP,
        expand = expansion(mult = 0, add = 0)
      ) +
      scale_y_continuous(expand = expansion(mult = .05)) +
      labs(
        title = "P-value Function",
        x = "Null hypothesis proportion p", 
        y = "P-value",
        alt = "A plot of a set of p values versus different proportions "
      ) +
      geom_segment(
        mapping = aes(
          x = ciP[1],
          xend = ciP[2],
          y = 0,
          yend = 0,
          color = "Confidence interval"
        ),
        linewidth = 1
      ) +
      geom_point(
        mapping = aes(x = c(ciP[1], ciP[2]), y = c(0, 0)),
        alpha = 0
      ) +
      geom_errorbarh(
        mapping = aes(
          xmin = ciP[1],
          xmax = ciP[2],
          y = 0,
          color = "Confidence interval"
        ),
        height = 0.05*1,
        linewidth = 1
      ) +
      geom_segment(
        mapping = aes(
          x = xlimP[1],
          xend = mean(simDataBF()),
          y = 1,
          yend = 1,
          color = "Observed estimate"
        )
      ) +
      geom_segment(
        mapping = aes(
          x = mean(simDataBF()),
          xend = mean(simDataBF()),
          y = 0,
          yend = 1,
          color = "Observed estimate"
        )
      ) +
      geom_segment(
        mapping = aes(
          x = xlimP[1],
          xend = input$nullF1,
          y = pValue,
          yend = pValue,
          color = "Null value"
        )
      ) +
      geom_segment(
        mapping = aes(
          x = input$nullF1,
          xend = input$nullF1,
          y = 0,
          yend = pValue,
          color = "Null value"
        )
      ) +
      geom_segment(
        mapping = aes(
          x = xlimP[1],
          xend = input$trueF1,
          y = truePvalue,
          yend = truePvalue,
          color = "True value"
        )
      ) +
      geom_segment(
        mapping = aes(
          x = input$trueF1,
          xend = input$trueF1,
          y = 0,
          yend = truePvalue,
          color = "True value"
        )
      ) +
      scale_color_manual(
        name = NULL,
        values = c(
          "Confidence interval" = psuPalette[1],
          "Observed estimate" = psuPalette[4],
          "Null value" = "black",
          "True value" = boastPalette[3]
        )
      ) +
      theme_bw() +
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      )
    
    return(gP)
  })
  
  ##### Make Bin. Table ----
  output$pvalueF1 <- renderTable(
    expr = {
      validate(
        need(expr = freqBinCheck() == 1, message = ""),
        need(expr = input$nullF1 != 0 && input$nullF1 != 1, message = ""),
        need(expr = input$trueF1 != 0 && input$trueF1 != 1, message = ""),
        need(expr = input$clF1 != 1, message = "")
      )
      pValue <- binom.exact(
        x = sum(simDataBF()),
        n = input$nF1,
        p = input$nullF1,
        alternative = "two.side",
        tsmethod = "central"
      )$p.value
      ciP <- binom.exact(
        x = sum(simDataBF()),
        n = input$nF1,
        p = input$nullF1,
        alternative = "two.side",
        tsmethod = "central",
        conf.level = input$clF1
      )$conf.int
      ctable <- matrix(
        data = c(round(pValue, 3), round(ciP[1], 3), round(ciP[2], 3)),
        nrow = 1
      )
      c2 <- paste("Confidence interval", " lower bound", sep = "<br>")
      c3 <- paste("Confidence interval", " upper bound", sep = "<br>")
      colnames(ctable) <- c("p-value", c2, c3)
      ctable
    },
    bordered = TRUE,
    sanitize.text.function = identity
  )
  
  ## NEIL PICK UP HERE ----

  #### Bayes Side----
  ## parameters
  s1<-reactive(((1-input$para1B1)/input$para2B1^2-1/input$para1B1)*input$para1B1^2)
  s2<-reactive((((1-input$para1B1)/input$para2B1^2-1/input$para1B1)*input$para1B1^2)*(1/input$para1B1-1))
  ## sample
  simulationB1<-eventReactive(
    eventExpr = input$simb1,
    valueExpr = {
      sample(c(0,1),size=nB1(),prob=c(1-trueB1(),trueB1()),replace = TRUE)
    }
  )
  output$credibleB1<-renderPlot({
    validate(
      need(
        expr = input$simb1,
        message = "Set parameters and press the Simulate button."
      ),
      need(
        expr = input$nullB1 != 0 && input$nullB1 != 1,
        message = "Provide a null hypothesis p between (0,1)"
      ),
      need(
        expr = input$trueB1 != 0 && input$trueB1 != 1, 
        message = "Provide a true p between (0,1)"
      ),
      need(
        input$para1B1>0 && input$para1B1<1,
        "Provide a valid prior"
      ),
      need(
        input$para2B1>0 && input$para2B1<sqrt(input$para1B1*(1-input$para1B1)),
        "Provide a valid prior"
      ),
      need(
        input$clB1!=1,
        "Credible Levels must be positive values less than 1"
      )
    )
    validate(
      validate(
        need(
          expr = input$nB1 == nB1() && input$trueB1 == trueB1() && input$nullB1 == nullB1(),
          message = "Use the simulate button to generate new data"
        )
      )
    )
    ###prior
    pRange<-seq(0,1,length=1000)
    # density
    priory<-dbeta(
      x=pRange, 
      shape1 = s1(), 
      shape2 = s2()
    )
    x<-pRange
    priordata<-as.data.frame(cbind(x,priory))
    ###posterior
    posteriory<-dbeta(
      x=pRange, 
      shape1 =s1() + sum(simulationB1()),
      shape2 =s2() + nB1() - sum(simulationB1())
    )
    posteriordata<-as.data.frame(cbind(x,posteriory))
    ### Credible region
    alphaB<-1-input$clB1
    ci<-qbeta(
      c(alphaB/2,1-alphaB/2),
      shape1 =s1() + sum(simulationB1()),
      shape2 =s2() + nB1() - sum(simulationB1()))
    # MLE
    mllB1<-posteriordata[which.max(posteriordata$posteriory),]
    mleB1<-
      if(input$para1B1==0.5 && input$para2B1==0.289){
        geom_segment(
          aes(
            x=mllB1$x,
            y=0,
            xend=mllB1$x,
            yend=mllB1$posteriory,
            colour = "Max Log-Likelihood",
            linetype ="Max Log-Likelihood"
            ),
          size = 1
        )
      }
    ### combined plot
    combined1<-
      ggplot()+
      geom_line(
        data=priordata,
        mapping = aes(x=x,y=priory,colour = "Prior", linetype="Prior")
      )+
      geom_line(
        data=posteriordata,
        mapping = aes(x=x,y=posteriory,colour = "Posterior", linetype="Posterior")
      )
    combined<-
      combined1+
      geom_segment(
        aes(x=ci[1],y=0,xend=ci[2],yend=0,colour = "Credible region", linetype ="Credible region"),
        size = 1
        )+
      mleB1+
      geom_point(
        mapping=aes(x=c(ci[1],ci[2]),y=c(0,0)),
        alpha=0
      )+
      geom_errorbarh(
        aes(xmin=ci[1],xmax=ci[2],y=0,colour = "Credible region", linetype = "Credible region"),
        height=0.05*layer_scales(combined1)$y$get_limits()[2],
        size=1
      )+
      labs(
        title = "Prior and Posterior",
        x = "Proportion p", 
        y = "Density",
        alt = "The plot combined the prior distribution and the posterior plot"
      )+
      scale_x_continuous(expand = expansion(mult = 0)) +
      scale_y_continuous(expand = expansion(mult = .05))+
      scale_color_manual(
        name = NULL,
        values = c(
          "Credible region" = psuPalette[1],
          "Prior" = psuPalette[3],
          "Posterior" = psuPalette[2],
          "Max Log-Likelihood" = "blue"
        )
      )+
      scale_linetype_manual(
        name = NULL,
        values = c(
          "Credible region" = "solid",
          "Prior" = "solid",
          "Posterior" = "solid",
          "Max Log-Likelihood" = "dashed"
        )
      )+
      theme_bw()+
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      )
    return(combined)
  })

  output$bfB1<-renderPlotly({
    validate(
      need(
        expr = input$simb1,
        message = "Select the area you want to zoom in and double-clike to zoom out"
      ),
      need(
        expr = input$nullB1 != 0 && input$nullB1 != 1,
        message = "Provide a null hypothesis p between (0,1)"
      ),
      need(
        expr = input$trueB1 != 0 && input$trueB1 != 1, 
        message = "Provide a true p between (0,1)"
      ),
      need(
        input$para1B1>0 && input$para1B1<1,
        "Provide a valid prior"
      ),
      need(
        input$para2B1>0 && input$para2B1<sqrt(input$para1B1*(1-input$para1B1)),
        "Provide a valid prior"
      ),
      need(
        input$clB1!=1,
        "Confidence Levels must be positive values less than 1"
      )
    )
    validate(
      need(
        expr = input$nB1 == nB1() && input$trueB1 == trueB1() && input$nullB1 == nullB1(),
        message = "Use the simulate button to generate new data"
      )
    )
    pRange<-seq(0,1,length=1000)
    likelihoody<-dbinom(
      x = sum(simulationB1()),
      size= nB1(),
      p=pRange
    )
    x<-pRange
    # denominator 
    denominator<-dbinom(
      x = sum(simulationB1()),
      size= nB1(),
      p=nullB1()
    )
    # log(10)
    bf<-log10(likelihoody/denominator)
    bfdata<-as.data.frame(cbind(x,bf))
    # p/pc
    pll<-dbinom(
      x = sum(simulationB1()),
      size= nB1(),
      p=trueB1()
    )
    pvpc<-log10(pll/denominator)
    # max bf
    maxll<-bfdata[which.max(bfdata$bf),]
    minbf<-min(ifelse(is.infinite(bfdata$bf),NA,bfdata$bf),na.rm = TRUE)
    g<-plot_ly(
      data = bfdata,
      x=~x,
      y=~bf,
      type = "scatter",
      mode="lines",
      opacity = 0.4,
      line = list(
        width=3,
        dash="solid",
        color = "blue"
      ),
      text=paste("p:",round(bfdata$x,3),
                 "<br>Log(BF):",round(bfdata$bf,3)),
      showlegend=F,
      hoverinfo="text"
    )%>%
      add_trace(
        x=c(seq(0,trueB1(),len=1000),rep(trueB1(),1000)),
        y=c(rep(pvpc,1000),seq(minbf,pvpc,len=1000)),
        mode="lines",
        hoverinfo="text",
        name = "p vs. p<sub>c</sub>",
        opacity = 1,
        showlegend=T,
        text=paste("p:",trueB1(),"<br>Log(BF):",round(pvpc,3)),
        line = list(
          width=1,
          dash="long",
          color = boastPalette[3])
      )%>%
      add_trace(
        x=c(seq(0,nullB1(),len=1000),rep(nullB1(),1000)),
        y=c(rep(0,1000),seq(0,minbf,len=1000)),
        mode="lines",
        hoverinfo="text",
        name = "p<sub>c</sub> to p<sub>c</sub>",
        opacity = 1,
        showlegend=T,
        text=paste("p:",nullB1(),"<br>Log(BF):",0),
        line = list(
          width=1,
          dash="dot",
          color = psuPalette[4])
      )%>%
      add_trace(
        x=c(seq(0,maxll$x,len=1000),rep(maxll$x,1000)),
        y=c(rep(maxll$bf,1000),seq(minbf,maxll$bf,len=1000)),
        mode = "lines",
        hoverinfo="text",
        name = "Max Log-Likelihood",
        opacity = 1,
        showlegend=T,
        text=paste("p:",round(maxll$x,3),"<br>Log(BF):",round(maxll$bf,3)),
        line = list(
          width=1,
          dash="dash",
          color = "blue")
      )%>%
      config(displaylogo=FALSE)%>%
      config(
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d",
        "autoScale2d","hoverCompareCartesian","hoverClosestCartesian","toImage"))%>%
      layout(
        title=list(text="Log<sub>10</sub> Bayes Factor Compared to p<sub>c</sub>",
                   font=list(size=18),xanchor="right"),
        xaxis=list(title="Proportion p",titlefont=list(size=16)),
        yaxis=list(title="Log Bayes Factor",titlefont=list(size=16)),
        showlegend=T,
        legend = list(orientation = "h",xanchor = "center",x = 0.5,y=-0.3))
    return(g)
  })
  
  output$tableB1<-renderTable({
    validate(
      need(
        expr = input$simb1,
        message = ""
      ),
      need(
        expr = input$nullB1 != 0,
        message = ""
      ),
      need(
        expr = input$trueB1 != 0,
        message = ""
      ),
      need(
        expr = input$nullB1 != 1,
        message = ""
      ),
      need(
        expr = input$trueB1 != 1,
        message = ""
      ),
      need(
        input$para1B1>0 && input$para1B1<1,
        ""
      ),
      need(
        input$para2B1>0 && input$para2B1<sqrt(input$para1B1*(1-input$para1B1)),
        ""
      ),
      need(
        input$clB1!=1,
        ""
      )
    )
    validate(
      need(
        expr = input$nB1 == nB1() && input$trueB1 == trueB1() && input$nullB1 == nullB1(),
        message = ""
      )
    )
    # p/pc
    denominator<-dbinom(
      x = sum(simulationB1()),
      size= nB1(),
      p=nullB1()
    )
    pll<-dbinom(
      x = sum(simulationB1()),
      size= nB1(),
      p=trueB1()
    )
    pvpll<-log10(pll/denominator)
    # ci
    alphaB<-1-input$clB1
    ci<-qbeta(
      c(alphaB/2,1-alphaB/2),
      shape1 =s1() + sum(simulationB1()),
      shape2 =s2() + nB1() - sum(simulationB1()))
    ctable<-matrix(c(round(pvpll,3),round(ci[1],3),round(ci[2],3)),nrow=1)
    c2<-paste("Credible region"," lower bound",sep = "<br>")
    c3<-paste("Credible region"," upper bound",sep = "<br>")
    colnames(ctable)<-c("Log(BF) comparing p to p<sub>c</sub>",c2,c3)
    ctable
  },bordered = TRUE,sanitize.text.function=identity)
  ## Poisson----
  ### Update buttons----
  observeEvent(
    eventExpr = input$simf2,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "simf2",
        label = "Re-simulate!",
        style = "default",
        disabled = FALSE 
      )
    }
  )
  observeEvent(
    eventExpr = input$simb2,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "simb2",
        label = "Re-simulate!",
        style = "default",
        disabled = FALSE 
      )
    }
  )
  
  ### Update inputs----
  ### Frequentist side
  trueF2<-eventReactive({
    input$simf2
  },
  valueExpr = {
    input$trueF2
  }
  )
  
  nullF2<-eventReactive({
    input$simf2
  },
  valueExpr = {
    input$nullF2
  }
  )
  
  nF2<-eventReactive({
    input$simf2
  },
  valueExpr = {
    input$nF2
  }
  )
  
  clF2<-eventReactive({
    input$simf2
  },
  valueExpr = {
    input$clF2
  }
  )
  ### Bayes side
  trueB2<-eventReactive({
    input$simb2
  },
  valueExpr = {
    input$trueB2
  }
  )
  
  para1B2<-eventReactive({
    input$simb2
  },
  valueExpr = {
    input$para1B2
  }
  )
  
  para2B2<-eventReactive({
    input$simb2
  },
  valueExpr = {
    input$para2B2
  }
  )
  
  clB2<-eventReactive({
    input$simb2
  },
  valueExpr = {
    input$clB2
  }
  )
  
  nB2<-eventReactive({
    input$simb2
  },
  valueExpr = {
    input$nB2
  }
  )
  
  nullB2<-eventReactive({
    input$simb2
  },
  valueExpr = {
    input$nullB2
  }
  )
  
  ###Outputs----
  #### Frequentist Side----
  # sample
  simulationF2<-eventReactive(
    eventExpr = input$simf2,
    valueExpr = {
      rpois(n=nF2(),lambda = trueF2())
    }
  )
  
  output$pvalueFunction2<-renderPlot({
    validate(
      need(
        expr = input$simf2,
        message = "Set parameters and press the Simulate button."
      ),
      need(
        expr = input$nullF2 != 0 ,
        message = "Provide a null hypothesis \u03BB larger than 0"
      ),
      need(
        expr = input$trueF2 != 0 , 
        message = "Provide a true \u03BB larger than 0"
      ),
      need(
        input$clF2!=1,
        "Confidence Levels must be positive values less than 1"
      )
    )
    validate(
      need(
        expr = input$nF2 == nF2() && input$trueF2 == trueF2() && input$nullF2 == nullF2(),
        message = "Use the simulation button to generate new data"
      )
    )
    alpha<-1-input$clF2
    ### calculate p-value 
    pValue<-poisson.exact(
      x = sum(simulationF2()),
      T = nF2(),
      r = nullF2(),
      alternative = "two.side",
      tsmethod = "central"
    )$p.value
    ### ci
    ci<-poisson.exact(
      x = sum(simulationF2()),
      T = nF2(),
      r = nullF2(),
      alternative = "two.side",
      tsmethod = "central",
      conf.level = input$clF2
    )$conf.int
    ### calculate true value's pvalue
    truePvalue<-poisson.exact(
      x = sum(simulationF2()),
      T = nF2(),
      r = trueF2(),
      alternative = "two.side",
      tsmethod = "central",
      conf.level = input$clF2
    )$p.value
    ### set xlim
    cimax<-poisson.exact(
      x = sum(simulationF2()),
      T = nF2(),
      r = nullF2(),
      alternative = "two.side",
      tsmethod = "central",
      conf.level = 0.999
    )$conf.int
    
    xlim<-c(max(0,cimax[1]),cimax[2])
    
    ### get p-values list
    change<-(xlim[2]-xlim[1])/1500
    theta<-xlim[1]
    pvaluelist<-c()
    thetalist<-c()
    genepvalues<-function(theta){
      poisson.exact(
        x = sum(simulationF2()),
        T = nF2(),
        r = theta,
        alternative = "two.side",
        tsmethod = "central",
        conf.level = 0.99
      )$p.value
    }
    while(theta<=xlim[2]){
      pvalues<-genepvalues(theta)
      pvaluelist<-c(pvaluelist,pvalues)
      thetalist<-c(thetalist,theta)
      theta<-theta+change
    }
    ### plot
    data<-as.data.frame(cbind(thetalist,pvaluelist))
    data<-rename(data,theta=thetalist)
    data<-rename(data,pValue=pvaluelist)
    g<-
      ggplot()+
      geom_line(
        data=data,
        mapping = aes(x=theta,y=pValue),
        color = "blue",
        size = 1,
        alpha = 0.5
      )+
      scale_x_continuous(
        limits = xlim,
        expand = expansion(mult =0, add = 0)
      )+
      scale_y_continuous(expand = expansion(mult = .05))+
      labs(
        title = "P-value Function",
        x = "Null hypothesis mean", 
        y = "P-value",
        alt = "A plot of a set of p values versus different means "
      )+
      geom_segment(
        aes(x=ci[1],y=0,xend=ci[2],yend=0,colour = "Confidence interval"),
        size = 1
      )+
      geom_point(
        mapping=aes(x=c(ci[1],ci[2]),y=c(0,0)),
        alpha=0
      )+
      geom_errorbarh(
        aes(xmin=ci[1],xmax=ci[2],y=0,colour = "Confidence interval"),
        height=0.05*1,
        size=1
      )+
      geom_segment(
        aes(x = xlim[1], y = 1, xend = mean(simulationF2()), yend = 1, colour = "Observed estimate")
      )+
      geom_segment(
        aes(x = mean(simulationF2()), y = 0, xend = mean(simulationF2()), yend = 1, colour = "Observed estimate")
      )+
      geom_segment(
        aes(x = xlim[1], y = pValue, xend = nullF2(), yend = pValue, colour = "Null value")
      )+
      geom_segment(
        aes(x = nullF2(), y = 0, xend = nullF2(), yend = pValue, colour = "Null value")
      )+
      geom_segment(
        aes(x = xlim[1], y = truePvalue, xend = trueF2(), yend = truePvalue, colour = "True value")
      )+
      geom_segment(
        aes(x = trueF2(), y = 0, xend = trueF2(), yend = truePvalue, colour = "True value")
      )+
      scale_color_manual(
        name = NULL,
        values = c(
          "Confidence interval" = psuPalette[1],
          "Observed estimate" = psuPalette[4],
          "Null value" = "black",
          "True value" = boastPalette[3]
        )
      )+
      theme_bw()+
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      )
    
    return(g)
  })
  ##table
  output$pvalueF2<-renderTable({
    validate(
      need(
        expr = input$nF2 == nF2() && input$trueF2 == trueF2() && input$nullF2 == nullF2(),
        message = ""
      ),
      need(
        expr = input$nullF2 != 0 ,
        message = ""
      ),
      need(
        expr = input$trueF2 != 0 , 
        message = ""
      ),
      need(
        input$clF2!=1,
        ""
      )
    )
    ### calculate p-value 
    pValue<-poisson.exact(
      x = sum(simulationF2()),
      T = nF2(),
      r = nullF2(),
      alternative = "two.side",
      tsmethod = "central"
    )$p.value
    ### ci
    ci<-poisson.exact(
      x = sum(simulationF2()),
      T = nF2(),
      r = nullF2(),
      alternative = "two.side",
      tsmethod = "central",
      conf.level = input$clF2
    )$conf.int
    ctable<-matrix(c(round(pValue,3),round(ci[1],3),round(ci[2],3)),nrow=1)
    c2<-paste("Confidence interval"," lower bound",sep = "<br>")
    c3<-paste("Confidence interval"," upper bound",sep = "<br>")
    colnames(ctable)<-c("p-value",c2,c3)
    ctable
  },bordered = TRUE,sanitize.text.function=identity)
  
  #### Bayes Side----
  ## sample
  simulationB2<-eventReactive(
    eventExpr = input$simb2,
    valueExpr = {
      rpois(n=nB2(),lambda = trueB2())
    }
  )
  
  output$credibleB2<-renderPlot({
    validate(
      need(
        expr = input$simb2,
        message = "Set parameters and press the Simulate button."
      ),
      need(
        expr = input$nullB2 != 0 ,
        message = "Provide a null hypothesis \u03BB larger than 0"
      ),
      need(
        expr = input$trueB2 != 0 , 
        message = "Provide a true \u03BB larger than 0"
      ),
      need(
        input$clB2!=1,
        "Credible Levels must be positive values less than 1"
      )
    )
    validate(
      validate(
        need(
          expr = input$nB2 == nB2() && input$trueB2 == trueB2() && input$nullB2 == nullB2(),
          message = "Use the simulate button to generate new data"
        )
      )
    )
    ###prior
    pRange<-seq(0,10,by=0.01)
    # density
    priory<-dgamma(
      x=pRange, 
      shape = input$para1B2, 
      rate = input$para2B2
    )
    x<-pRange
    priordata<-as.data.frame(cbind(x,priory))
    ###posterior
    posteriory<-dgamma(
      x=pRange, 
      shape = input$para1B2 + sum(simulationB2()),
      rate = input$para2B2 + nB2()
    )
    posteriordata<-as.data.frame(cbind(x,posteriory))
    ### Credible region
    alphaB<-1-input$clB2
    ci<-qgamma(
      c(alphaB/2,1-alphaB/2),
      shape = input$para1B2 + sum(simulationB2()),
      rate = input$para2B2 + nB2()
    )
    ### combined plot
    combined1<-
      ggplot()+
      geom_line(
        data=priordata,
        mapping = aes(x=x,y=priory,colour = "Prior", linetype = "Prior")
      )+
      geom_line(
        data=posteriordata,
        mapping = aes(x=x,y=posteriory,colour = "Posterior", linetype="Posterior")
      )
    combined<-
      combined1+
      geom_segment(
        aes(x=ci[1],y=0,xend=ci[2],yend=0,colour = "Credible region", linetype = "Credible region"),
        size = 1
      )+
      geom_point(
        mapping=aes(x=c(ci[1],ci[2]),y=c(0,0)),
        alpha=0
      )+
      geom_errorbarh(
        aes(xmin=ci[1],xmax=ci[2],y=0,colour = "Credible region",linetype = "Credible region"),
        height=0.05*layer_scales(combined1)$y$get_limits()[2],
        size=1
      )+
      labs(
        title = "Prior and Posterior",
        x = "Mean \u3BB", 
        y = "Density",
        alt = "The plot combined the prior distribution and the posterior plot"
      )+
      scale_x_continuous(expand = expansion(mult = 0)) +
      scale_y_continuous(expand = expansion(mult = .05))+
      scale_color_manual(
        name = NULL,
        values = c(
          "Credible region" = psuPalette[1],
          "Prior" = psuPalette[3],
          "Posterior" = psuPalette[2]
        )
      )+
      scale_linetype_manual(
        name = NULL,
        values = c(
          "Credible region" = "solid",
          "Prior" = "solid",
          "Posterior" = "solid"
        )
      )+
      theme_bw()+
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      )
    return(combined)
  })
  
  output$bfB2<-renderPlotly({
    validate(
      need(
        expr = input$simb2,
        message = "Select the area you want to zoom in and double-clike to zoom out"
      ),
      need(
        expr = input$nullB2 != 0 ,
        message = "Provide a null hypothesis \u03BB larger than 0"
      ),
      need(
        expr = input$trueB2 != 0 , 
        message = "Provide a true \u03BB larger than 0"
      ),
      need(
        input$clB2!=1,
        "Confidence Levels must be positive values less than 1"
      )
    )
    validate(
      need(
        expr = input$nB2 == nB2() && input$trueB2 == trueB2() && input$nullB2 == nullB2(),
        message = "Use the simulate button to generate new data"
      )
    )
    pRange<-seq(0,10,length=1000)
    ## likelihood for total counts
    likelihoody<-dpois(
      x = sum(simulationB2()),
      lambda = pRange*nB2()
    )
    x<-pRange
    # denominator 
    denominator<-dpois(
      x = sum(simulationB2()),
      lambda = nullB2()*nB2()
    )
    # log(10)
    bf<-log10(likelihoody/denominator)
    bfdata<-as.data.frame(cbind(x,bf))
    # p/pc
    pll<-dpois(
      x = sum(simulationB2()),
      lambda=trueB2()*nB2()
    )
    pvpc<-log10(pll/denominator)
    # max bf
    maxll<-bfdata[which.max(bfdata$bf),]
    minbf<-min(ifelse(is.infinite(bfdata$bf),NA,bfdata$bf),na.rm = TRUE)
    g<-plot_ly(
      data = bfdata,
      x=~x,
      y=~bf,
      type = "scatter",
      mode="lines",
      opacity = 0.4,
      line = list(
        width=3,
        dash="solid",
        color = "blue"
      ),
      text=paste("\u3BB:",round(bfdata$x,3),
                 "<br>Log(BF):",round(bfdata$bf,3)),
      showlegend=F,
      hoverinfo="text"
    )%>%
      add_trace(
        x=c(seq(0,trueB2(),len=1000),rep(trueB2(),1000)),
        y=c(rep(pvpc,1000),seq(minbf,pvpc,len=1000)),
        mode="lines",
        hoverinfo="text",
        name = "\u3BB vs. \u03BB<sub>c</sub>",
        opacity = 1,
        showlegend=T,
        text=paste("\u3BB:",trueB2(),"<br>Log(BF):",round(pvpc,3)),
        line = list(
          width=1,
          dash="long",
          color = boastPalette[3])
      )%>%
      add_trace(
        x=c(seq(0,nullB2(),len=1000),rep(nullB2(),1000)),
        y=c(rep(0,1000),seq(0,minbf,len=1000)),
        mode="lines",
        hoverinfo="text",
        name = "\u03BB<sub>c</sub> to \u03BB<sub>c</sub>",
        opacity = 1,
        showlegend=T,
        text=paste("\u3BB:",nullB2(),"<br>Log(BF):",0),
        line = list(
          width=1,
          dash="dot",
          color = psuPalette[4])
      )%>%
      add_trace(
        x=c(seq(0,maxll$x,len=1000),rep(maxll$x,1000)),
        y=c(rep(maxll$bf,1000),seq(minbf,maxll$bf,len=1000)),
        mode = "lines",
        hoverinfo="text",
        name = "Max Log-Likelihood",
        opacity = 1,
        showlegend=T,
        text=paste("\u3BB:",round(maxll$x,3),"<br>Log(BF):",round(maxll$bf,3)),
        line = list(
          width=1,
          dash="dash",
          color = "blue")
      )%>%
      config(displaylogo=FALSE)%>%
      config(
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d",
        "autoScale2d","hoverCompareCartesian","hoverClosestCartesian","toImage"))%>%
      layout(
        title=list(text="Log<sub>10</sub> Bayes Factor Compared to \u3BB<sub>c</sub>",
                   font=list(size=18),xanchor="right"),
        xaxis=list(title="Mean \u3BB",titlefont=list(size=16)),
        yaxis=list(title="Log Bayes Factor",titlefont=list(size=16)),
        showlegend=T,
        legend = list(orientation = "h",xanchor = "center",x = 0.5,y=-0.3))
    return(g)
  })
  
  output$tableB2<-renderTable({
    validate(
      need(
        expr = input$simb2,
        message = ""
      ),
      need(
        expr = input$nullB2 != 0 ,
        message = ""
      ),
      need(
        expr = input$trueB2 != 0 , 
        message = ""
      ),
      need(
        input$clB2!=1,
        ""
      )
    )
    validate(
      need(
        expr = input$nB2 == nB2() && input$trueB2 == trueB2() && input$nullB2 == nullB2(),
        message = ""
      )
    )
    # p/pc
    # denominator 
    denominator<-dpois(
      x = sum(simulationB2()),
      lambda = nullB2()*nB2()
    )
    # p/pc
    pll<-dpois(
      x = sum(simulationB2()),
      lambda=trueB2()*nB2()
    )
    pvpll<-log10(pll/denominator)
    # ci
    alphaB<-1-input$clB2
    ci<-qgamma(
      c(alphaB/2,1-alphaB/2),
      shape = input$para1B2 + sum(simulationB2()),
      rate = input$para2B2 + nB2()
    )
    ctable<-matrix(c(round(pvpll,3),round(ci[1],3),round(ci[2],3)),nrow=1)
    c2<-paste("Credible region"," lower bound",sep = "<br>")
    c3<-paste("Credible region"," upper bound",sep = "<br>")
    colnames(ctable)<-c("Log(BF) comparing \u03BB to \u03BB<sub>c</sub>",c2,c3)
    ctable
  },bordered = TRUE,sanitize.text.function=identity)
  
  ## Normal----
  ### Update buttons----
  observeEvent(
    eventExpr = input$simf3,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "simf3",
        label = "Re-simulate!",
        style = "default",
        disabled = FALSE 
      )
    }
  )
  observeEvent(
    eventExpr = input$simb3,
    handlerExpr = {
      updateButton(
        session = session,
        inputId = "simb3",
        label = "Re-simulate!",
        style = "default",
        disabled = FALSE 
      )
    }
  )

  ### Update inputs----
  ### Frequentist side
  trueF3<-eventReactive({
    input$simf3
  },
  valueExpr = {
    input$trueF3
  }
  )
  
  nullF3<-eventReactive({
    input$simf3
  },
  valueExpr = {
    input$nullF3
  }
  )
  
  nF3<-eventReactive({
    input$simf3
  },
  valueExpr = {
    input$nF3
  }
  )
  
  clF3<-eventReactive({
    input$simf3
  },
  valueExpr = {
    input$clF3
  }
  )
  ### Bayes side
  trueB3<-eventReactive({
    input$simb3
  },
  valueExpr = {
    input$trueB3
  }
  )
  
  para1B3<-eventReactive({
    input$simb3
  },
  valueExpr = {
    input$para1B3
  }
  )
  
  para2B3<-eventReactive({
    input$simb3
  },
  valueExpr = {
    input$para2B3
  }
  )
  
  clB3<-eventReactive({
    input$simb3
  },
  valueExpr = {
    input$clB3
  }
  )
  
  nB3<-eventReactive({
    input$simb3
  },
  valueExpr = {
    input$nB3
  }
  )
  
  nullB3<-eventReactive({
    input$simb3
  },
  valueExpr = {
    input$nullB3
  }
  )
  
  ###Outputs----
  #### Frequentist Side----
  # sample
  simulationF3<-eventReactive(
    eventExpr = input$simf3,
    valueExpr = {
      rnorm(nF3(),mean = trueF3(),sd=0.058)
    }
  )
  
  output$pvalueFunction3<-renderPlot({
    validate(
      need(
        expr = input$simf3,
        message = "Set parameters and press the Simulate button."
      ),
      need(
        input$clF3!=1,
        "Confidence Levels must be positive values less than 1"
      )
    )
    validate(
      need(
        expr = input$nF3 == nF3() && input$trueF3 == trueF3() && input$nullF3 == nullF3(),
        message = "Use the simulation button to generate new data"
      )
    )
    alpha<-1-input$clF3
    ### calculate p-value 
    zscore<-(mean(simulationF3())-nullF3())/(0.058/sqrt(nF3()))
    pvalue<-2*pnorm(-abs(zscore))
    ### ci
    getci<-function(alpha){
      lowerbound<-mean(simulationF3())-qnorm(1-alpha/2)*(0.058/sqrt(nF3()))
      upperbound<-mean(simulationF3())+qnorm(1-alpha/2)*(0.058/sqrt(nF3()))
      bound<-c(lowerbound,upperbound)
      return(bound)
    }
    lowerbound<-getci(alpha)[1]
    upperbound<-getci(alpha)[2]
    ci<-c(lowerbound,upperbound)
    ### calculate true value's pvalue
    zscoret<-(mean(simulationF3())-trueF3())/(0.058/sqrt(nF3()))
    truePvalue<-2*pnorm(-abs(zscoret))
    ### set xlim
    genelimit<-function(){
      lowerboundmax<-mean(simulationF3())+qnorm(0.001/2)*(0.058/sqrt(nF3()))
      upperboundmax<-mean(simulationF3())+qnorm(1-0.001/2)*(0.058/sqrt(nF3()))
      limit<-c(lowerboundmax,upperboundmax)
      return(limit)
    }
    lowerboundmax<-genelimit()[1]
    upperboundmax<-genelimit()[2]
    ### get p-values list
    genepvalues<-function(theta){
      z_score<-(mean(simulationF3())-theta)/(0.058/sqrt(nF3()))
      pValue<-2*pnorm(-abs(z_score))
      return(pValue)
    }
    thetarange<-c(lowerboundmax,upperboundmax)
    changetheta<-diff(thetarange)/1500
    theta<-lowerboundmax
    pvaluelist<-c()
    thetalist<-c()
    while(theta<=upperboundmax){
      pvalues<-genepvalues(theta)
      pvaluelist<-c(pvaluelist,pvalues)
      thetalist<-c(thetalist,theta)
      theta=theta+changetheta}
    ### xlim
    xlim<-c(lowerboundmax,upperboundmax)
    ###plot
    data<-as.data.frame(cbind(thetalist,pvaluelist))
    data<-rename(data,theta=thetalist)
    data<-rename(data,pValue=pvaluelist)
    g<-
      ggplot()+
      geom_line(
        data=data,
        mapping = aes(x=theta,y=pValue),
        color = "blue",
        size = 1,
        alpha = 0.5
      )+
      scale_x_continuous(
        limits = xlim,
        expand = expansion(mult =0, add = 0)
      )+
      scale_y_continuous(expand = expansion(mult = .05))+
      labs(
        title = "P-value Function",
        x = "Null hypothesis mean", 
        y = "P-value",
        alt = "A plot of a set of p values versus different means "
      )+
      geom_segment(
        aes(x=ci[1],y=0,xend=ci[2],yend=0,colour = "Confidence interval"),
        size = 1
      )+
      geom_point(
        mapping=aes(x=c(ci[1],ci[2]),y=c(0,0)),
        alpha=0
      )+
      geom_errorbarh(
        aes(xmin=ci[1],xmax=ci[2],y=0,colour = "Confidence interval"),
        height=0.05*1,
        size=1
      )+
      geom_segment(
        aes(x = xlim[1], y = 1, xend = mean(simulationF3()), yend = 1, colour = "Observed estimate")
      )+
      geom_segment(
        aes(x = mean(simulationF3()), y = 0, xend = mean(simulationF3()), yend = 1, colour = "Observed estimate")
      )+
      geom_segment(
        aes(x = xlim[1], y = pvalue, xend = nullF3(), yend = pvalue, colour = "Null value")
      )+
      geom_segment(
        aes(x = nullF3(), y = 0, xend = nullF3(), yend = pvalue, colour = "Null value")
      )+
      geom_segment(
        aes(x = xlim[1], y = truePvalue, xend = trueF3(), yend = truePvalue, colour = "True value")
      )+
      geom_segment(
        aes(x = trueF3(), y = 0, xend = trueF3(), yend = truePvalue, colour = "True value")
      )+
      scale_color_manual(
        name = NULL,
        values = c(
          "Confidence interval" = psuPalette[1],
          "Observed estimate" = psuPalette[4],
          "Null value" = "black",
          "True value" = boastPalette[3]
        )
      )+
      theme_bw()+
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      )
    
    return(g)
  })
  ##table
  output$pvalueF3<-renderTable({
    validate(
      need(
        expr = input$nF3 == nF3() && input$trueF3 == trueF3() && input$nullF3 == nullF3(),
        message = ""
      ),
      need(
        input$clF3!=1,
        ""
      )
    )
    ### calculate p-value 
    alpha<-1-input$clF3
    zscore<-(mean(simulationF3())-nullF3())/(0.058/sqrt(nF3()))
    pvalue<-2*pnorm(-abs(zscore))
    getci<-function(x){
      lowerbound<-mean(simulationF3())-qnorm(1-x/2)*(0.058/sqrt(nF3()))
      upperbound<-mean(simulationF3())+qnorm(1-x/2)*(0.058/sqrt(nF3()))
      bound<-c(lowerbound,upperbound)
      return(bound)
    }
    # lower bound should larger than 0 
    lowerbound<-getci(alpha)[1]
    upperbound<-getci(alpha)[2]
    ci<-c(lowerbound,upperbound)
    ctable<-matrix(c(pvalue,ci[1],ci[2]),nrow=1)
    c2<-paste("Confidence interval"," lower bound",sep = "<br>")
    c3<-paste("Confidence interval"," upper bound",sep = "<br>")
    colnames(ctable)<-c("p-value",c2,c3)
    return(ctable)
  },bordered = TRUE,sanitize.text.function=identity)
  
  #### Bayes Side----
  ## sample
  simulationB3<-eventReactive(
    eventExpr = input$simb3,
    valueExpr = {
      rnorm(nB3(),mean = trueB3(),sd=0.058)
    }
  )
  
  output$credibleB3<-renderPlot({
    validate(
      need(
        expr = input$simb3,
        message = "Set parameters and press the Simulate button."
      ),
      need(
        input$para2B3>0,
        "Provide a valid standard deviation"
      ),
      need(
        input$clB3!=1,
        "Credible Levels must be positive values less than 1"
      )
    )
    validate(
      validate(
        need(
          expr = input$nB3 == nB3() && input$trueB3 == trueB3() && input$nullB3 == nullB3(),
          message = "Use the simulate button to generate new data"
        )
      )
    )
    ###range
    pRange<-seq(-1,1,length=1000)
    # density
    priory<-dnorm(
      x=pRange, 
      mean = input$para1B3, 
      sd = input$para2B3
    )
    x<-pRange
    priordata<-as.data.frame(cbind(x,priory))
    ###posterior
    a<-1/(input$para2B3^2)
    b<-nB3()/(0.058^2)
    mupost<-(a*input$para1B3+b*mean(simulationB3()))/(a+b)
    sd2post<-1/(a+b)
    posteriory<-dnorm(
      x=pRange, 
      mean = mupost,
      sd = sqrt(sd2post)
    )
    posteriordata<-as.data.frame(cbind(x,posteriory))
    ### Credible region
    alphaB<-1-input$clB3
    ci<-qnorm(
      c(alphaB/2,1-alphaB/2),
      mean = mupost,
      sd = sqrt(sd2post)
    )
    ### combined plot
    combined1<-
      ggplot()+
      geom_line(
        data=priordata,
        mapping = aes(x=x,y=priory,colour = "Prior",linetype="Prior")
      )+
      geom_line(
        data=posteriordata,
        mapping = aes(x=x,y=posteriory,colour = "Posterior", linetype="Posterior")
      )
    combined<-
      combined1+
      geom_segment(
        aes(x=ci[1],y=0,xend=ci[2],yend=0,colour = "Credible region",linetype = "Credible region"),
        size = 1
      )+
      geom_point(
        mapping=aes(x=c(ci[1],ci[2]),y=c(0,0)),
        alpha=0
      )+
      geom_errorbarh(
        aes(xmin=ci[1],xmax=ci[2],y=0,colour = "Credible region", linetype ="Credible region"),
        height=0.05*layer_scales(combined1)$y$get_limits()[2],
        size=1
      )+
      labs(
        title = "Prior and Posterior",
        x = "Mean \u03BC", 
        y = "Density",
        alt = "The plot combined the prior distribution and the posterior plot"
      )+
      scale_x_continuous(expand = expansion(mult = 0)) +
      scale_y_continuous(expand = expansion(mult = .05))+
      scale_color_manual(
        name = NULL,
        values = c(
          "Credible region" = psuPalette[1],
          "Prior" = psuPalette[3],
          "Posterior" = psuPalette[2]
        )
      )+
      scale_linetype_manual(
        name = NULL,
        values = c(
          "Credible region" = "solid",
          "Prior" = "solid",
          "Posterior" = "solid"
        )
      )+
      theme_bw()+
      theme(
        plot.caption = element_text(size = 18),
        text = element_text(size = 18),
        axis.title = element_text(size = 16),
        legend.position = "bottom"
      )
    return(combined)
  })
  
  output$bfB3<-renderPlotly({
    validate(
      need(
        expr = input$simb3,
        message = "Select the area you want to zoom in and double-clike to zoom out"
      ),
      need(
        input$para2B3>0,
        "Provide a valid standard deviation"
      ),
      need(
        input$clB3!=1,
        "Confidence Levels must be positive values less than 1"
      )
    )
    validate(
      need(
        expr = input$nB3 == nB3() && input$trueB3 == trueB3() && input$nullB3 == nullB3(),
        message = "Use the simulate button to generate new data"
      )
    )
    pRange<-seq(-1,1,length=1000)
    likelihoody<-dnorm(
      x = pRange,
      mean = mean(simulationB3()),
      sd = 0.058
    )
    x<-pRange
    # denominator 
    denominator<-dnorm(
      x = nullB3(),
      mean = mean(simulationB3()),
      sd = 0.058
    )
    # log(10)
    bf<-log10(likelihoody/denominator)
    bfdata<-as.data.frame(cbind(x,bf))
    # p/pc
    pll<-dnorm(
      x = trueB3(),
      mean = mean(simulationB3()),
      sd = 0.058
    )
    pvpc<-log10(pll/denominator)
    # max bf
    maxll<-bfdata[which.max(bfdata$bf),]
    minbf<-min(ifelse(is.infinite(bfdata$bf),NA,bfdata$bf),na.rm = TRUE)
    g<-plot_ly(
      data = bfdata,
      x=~x,
      y=~bf,
      type = "scatter",
      mode="lines",
      opacity = 0.4,
      line = list(
        width=3,
        dash="solid",
        color = "blue"
      ),
      text=paste("\u03BC:",round(bfdata$x,3),
                 "<br>Log(BF):",round(bfdata$bf,3)),
      showlegend=F,
      hoverinfo="text"
    )%>%
      add_trace(
        x=c(seq(0,trueB3(),len=1000),rep(trueB3(),1000)),
        y=c(rep(pvpc,1000),seq(minbf,pvpc,len=1000)),
        mode="lines",
        hoverinfo="text",
        name = "\u03BC vs. \u03BC<sub>c</sub>",
        opacity = 1,
        showlegend=T,
        text=paste("\u03BC:",trueB3(),"<br>Log(BF):",round(pvpc,3)),
        line = list(
          width=1,
          dash="long",
          color = boastPalette[3])
      )%>%
      add_trace(
        x=c(seq(0,nullB3(),len=1000),rep(nullB3(),1000)),
        y=c(rep(0,1000),seq(0,minbf,len=1000)),
        mode="lines",
        hoverinfo="text",
        name = "\u03BC<sub>c</sub> vs. \u03BC<sub>c</sub>",
        opacity = 1,
        showlegend=T,
        text=paste("\u03BC:",nullB3(),"<br>Log(BF):",0),
        line = list(
          width=1,
          dash="dot",
          color = psuPalette[4])
      )%>%
      add_trace(
        x=c(seq(0,maxll$x,len=1000),rep(maxll$x,1000)),
        y=c(rep(maxll$bf,1000),seq(minbf,maxll$bf,len=1000)),
        mode = "lines",
        hoverinfo="text",
        name = "Max Log-Likelihood",
        opacity = 1,
        showlegend=T,
        text=paste("\u03BC:",round(maxll$x,3),"<br>Log(BF):",round(maxll$bf,3)),
        line = list(
          width=1,
          dash="dash",
          color = "blue")
      )%>%
      config(displaylogo=FALSE)%>%
      config(
        modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","pan2d",
        "autoScale2d","hoverCompareCartesian","hoverClosestCartesian","toImage"))%>%
      layout(
        title=list(text="Log<sub>10</sub> Bayes Factor Compared to \u03BC<sub>c</sub>",
                   font=list(size=18),xanchor="right"),
        xaxis=list(title="Mean \u03BC",titlefont=list(size=16)),
        yaxis=list(title="Log Bayes Factor",titlefont=list(size=16)),
        showlegend=T,
        legend = list(orientation = "h",xanchor = "center",x = 0.5,y=-0.3))
    return(g)
  })
  
  output$tableB3<-renderTable({
    validate(
      need(
        expr = input$simb3,
        message = ""
      ),
      need(
        input$para2B3>0,
        ""
      ),
      need(
        input$clB3!=1,
        ""
      )
    )
    validate(
      need(
        expr = input$nB3 == nB3() && input$trueB3 == trueB3() && input$nullB3 == nullB3(),
        message = ""
      )
    )
    # denominator 
    denominator<-dnorm(
      x = nullB3(),
      mean = mean(simulationB3()),
      sd = 0.058
    )
    # p/pc
    pll<-dnorm(
      x = trueB3(),
      mean = mean(simulationB3()),
      sd = 0.058
    )
    pvpll<-log10(pll/denominator)
    # ci
    alphaB<-1-input$clB3
    a<-1/(input$para2B3^2)
    b<-nB3()/(0.058^2)
    mupost<-(a*input$para1B3+b*mean(simulationB3()))/(a+b)
    sd2post<-1/(a+b)
    ci<-qnorm(
      c(alphaB/2,1-alphaB/2),
      mean = mupost,
      sd = sqrt(sd2post)
    )
    ctable<-matrix(c(round(pvpll,3),round(ci[1],3),round(ci[2],3)),nrow=1)
    c2<-paste("Credible region"," lower bound",sep = "<br>")
    c3<-paste("Credible region"," upper bound",sep = "<br>")
    colnames(ctable)<-c("Log(BF) comparing \u03BC to \u03BC<sub>c</sub>",c2,c3)
    ctable
  },bordered = TRUE,sanitize.text.function=identity)
}
  
# Boast App Call --- 
boastApp(ui = ui, server = server)
