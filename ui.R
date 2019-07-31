#load libraries
library(shiny)
library(shinydashboard)
library(plotly)

# Define UI for application that displays an about page and the app itself
dashboardPage(skin="blue",
              #add title
              dashboardHeader(title="Mortgage Loans in Washington State in 2016",titleWidth=700),
              #define sidebar items
              dashboardSidebar(sidebarMenu(
                menuItem("About", tabName = "about", icon = icon("archive")),
                menuItem("Data Summaries", tabName = "graphics", icon = icon("chart-bar")),
                menuItem("Interactive Plot", tabName = "interactive", icon = icon("chart-bar")),
                menuItem("Clustering", tabName = "cluster", icon = icon("project-diagram")),
                menuItem("Predictive Modeling", tabName = "model", icon = icon("chart-line")),
                menuItem("View Data", tabName = "data", icon = icon("table"))
              )),
              #define the body of the app
              dashboardBody(
                tabItems(
                  # First tab content
                  tabItem(tabName = "about",
                          fluidRow(
                            #Show a plot of the prior    
                            column(12,
                                   tabsetPanel(
                                     tabPanel("About the Data",           
                                              #box to contain description
                                              box(background="black",width=12,
                                                  h4("The Home Mortgage Disclosure Act (HDMA) requires many financial institutions to maintain, report, and publicly disclose loan-level information about mortgages. These public data are important because they help show whether lenders are serving the housing needs of their communities; they give public officials information that helps them make decisions and policies; and they shed light on lending patterns that could be discriminatory."),
                                                  h4("The data used in this app captures all mortgage applicaions in the state of Washington that resulted in a loan being issued or a loan being denied by the bank in the year of 2016. The data will be reviewed to see if signs of discrimination are evident."),
                                                  h4(uiOutput("datalink"))
                                                )#end box
                                          ), #end tab panel
                                     tabPanel("Data Field Reference",
                                              DT::dataTableOutput("fields")
                                     ), #end tab panel
                                     tabPanel("About the App", 
                                              #box to contain description
                                              box(background="black",width=12,
                                                  h4("The Data Summaries tab has controls on the left that allow the user to update the content displayed on the right."),
                                                  h4("The Numeric Summaries tab shows loan approval rates and contingency tables. The Numeric Summaries tab also shows six number summaries for applicant's income and loan amounts."),
                                                  h4("The Graphical Summaries tab shows stacked bar charts for loan types and loan purposes. The Graphical Summaries tab also shows the income and loan amount distributions."),
                                                  hr(),
                                                  h4("The Interactive Plot tab includes a side-by-side box plot that shows loan amount by race. There are quite a few outliers in the data. These outliers can be removed from the plot by clicking the point on the graph."),
                                                  hr(),
                                                  h4("The Clustering tab uses Hierarchical Clustering to generate the dendrogram. The user can update the dendrogram by changing the linkage method. The user also has the option to save the dendrogram."),
                                                  hr(),
                                                  h4("The Predictive Modeling tab shows Logistic Regression and Random Forest Classification results."),
                                                  h4("The left side shows Logistic Regression results. The Logisitc Regression model is used to predict the probabilty that the loan is approved. The model includes income and loan amount as predictors along with any other predictors selected by the user. The user must enter values for all predictors to obtain the predicted probabilty of the loan approved. The Logistic Regression models success probability using the logistic function "),
                                                  withMathJax(),
                                                  h4(helpText(' $$P(Loan Approved| Predictors) = \\frac{e^{\\beta_0+\\beta_income (x_income)+\\beta_loan (x_loan)+...+\\beta_p (x_p)}}{1+e^{\\beta_0+\\beta_income (x_income)+\\beta_loan (x_loan)+...+\\beta_p (x_p)}}$$')),
                                                  h4("The right side shows the Random Forest Classification results. The user specifies the Number of Predcitors and the Number of Trees to use in the Random Forest Model. These selections will update the accuracy and misclassification rates for the testing data set. The variable importance plot ranks the importance of the predictors within the model. The Random Forest results show that the applicant's income and loan amount are the most important variables to predict if the loan application is approved."),
                                                  hr(),
                                                  h4("The View Data tab allows the user to explore the raw data. The user can select the variables of interest and all observations (n=327,888) will be displayed. The user can then export the selected data for further review."),
                                                  hr(),
                                                  h4("Conclusion: This app was built to investigate whether or not lending patterns in Washington seem to be discriminatory. This app focuses on comparing by gender and by race to see if a particular group have lower lower loan approval rates. After invetigating this data set, I have no reason to believe that the lenders are discrimating against loan applicants.")
                                              ) #end box
                                     ) #end tab panel
                                   ) #end tab set
                            ) #end column
                          ) #end fluidrow
                  ),#end about tab
                  #Second tab content     
                  tabItem(tabName = "graphics",
                          fluidRow(
                            column(3,
                                   selectizeInput("sex", "Applicant Sex", selected = "Female", choices = levels(as.factor(modelData$sexRevised))),
                                   br(),
                                   checkboxInput("getIncLoan", h4("Get Income and Loan Summaries", style = "color:blue;")),
                                   conditionalPanel(condition = "input.getIncLoan == true",
                                                    sliderInput("maxInc", "Select Max in (000s) shown on Income Dist on Graphical Summaries tab",min = 500, max = 4000, value = 4000, step = 500)),
                                   conditionalPanel(condition = "input.getIncLoan == true",
                                                    sliderInput("maxLoan", "Select Max in (000s) shown on Loan Dist on Graphical Summaries tab", min = 500, max = 4000, value = 4000, step = 500))
                            ), #end column
                            column(9,
                                   tabsetPanel(
                                     tabPanel("Numeric Summaries",
                                              column(12,
                                                     h3(withSpinner(textOutput("approveRate"))),
                                                     withSpinner(DT::dataTableOutput("actiontable"))
                                              ),
                                              column(6,
                                                     withSpinner(uiOutput("title1")),
                                                     withSpinner(DT::dataTableOutput("numSumm1"))
                                                     ),
                                              column(6,
                                                     withSpinner(uiOutput("title2")),
                                                     withSpinner(DT::dataTableOutput("numSumm2"))
                                                     )
                                     ), #end tab panel
                                     tabPanel("Graphical Summaries",
                                              column(6,
                                                     withSpinner(plotOutput("bar1"))
                                              ),
                                              column(6,
                                                     withSpinner(plotOutput("bar2"))
                                              ),
                                              column(6,
                                                     hr(),
                                                    withSpinner(plotlyOutput("scatter1"))
                                                    ),
                                              column(6,
                                                     hr(),
                                                     withSpinner(plotlyOutput("scatter2"))
                                                     )
                                     ) #end tab panel
                                   ) #end tab set
                            )#end column 
                          ) #end fluidrow
                  ), #end tabItem
                  #Third tab content
                  tabItem(tabName = "interactive",
                          fluidRow(
                            column(12, 
                                   em(h3("Click outliers to remove from the box plot. Click the Reset button to include all data in the plot.")),
                                   withSpinner(plotOutput("box1",height = 350,
                                              click = "plot1_click",
                                              brush = brushOpts(
                                                id = "plot1_brush"))),
                                   actionButton("exclude_reset", "Reset")
                            ) #end column
                          ) #end fluidrow
                  ), #end tabItem
                  #Fourth tab content
                  tabItem(tabName = "cluster",
                          fluidRow(
                            column(3,
                                   selectizeInput("linkage", "Select Linkage Method", selected = "average", choices = list("average","centroid","complete","single")),
                                   downloadButton("downloadPlot", "Save Dendrogram")
                                   ),
                            column(12,
                                  hr()
                            ),
                            column(12,
                                   withSpinner(plotOutput("clus1"))
                            ) #end column
                          ) #end fluidrow
                  ), #end tabItem
                  #Fifth tab content
                  tabItem(tabName = "model",
                          fluidRow(
                            column(6,
                                   h2("Logisitic Regression Results"),
                                   h3(withSpinner(textOutput("logReg"))),
                                   hr(),
                                   h4("Logisitic Regression Prediction Controls"),
                                   column(4,
                                          h4("Select Predictors"),
                                          em(h6("Note: Loan Amount and Income are also included in the model.")),
                                          checkboxInput("agencyBox", "Agency Name", value = TRUE),
                                          checkboxInput("sexBox", "Applicant Sex", value = TRUE),
                                          checkboxInput("raceBox", "Applicant Race", value = TRUE),
                                          checkboxInput("loanPBox", "Loan Purpose", value = TRUE),
                                          checkboxInput("loanTBox", "Loan Type", value = TRUE)
                                   ),
                                   column(8,
                                          h4("Enter Values of Predictors"),
                                          numericInput("incomeInput", "Annual Income in 000s", value=50,min=0,max=99999, step = 5),
                                          numericInput("loanInput", "Loan Amount in 000s", value=50,min=0,max=99999, step = 5),
                                          conditionalPanel(condition = "input.agencyBox == true",
                                                           selectizeInput("agencyInput", "Agency Name", choices = levels(as.factor(predData$agency_name)))),
                                          conditionalPanel(condition = "input.sexBox == true",
                                                           selectizeInput("sexInput", "Applicant Sex", choices = levels(as.factor(predData$sexRevised)))),
                                          conditionalPanel(condition = "input.raceBox == true", 
                                                           selectizeInput("raceInput", "Applicant Race", choices = levels(as.factor(predData$raceRevised)))),
                                          conditionalPanel(condition = "input.loanPBox == true", 
                                                           selectizeInput("loanPInput", "Loan Purpose", choices = levels(as.factor(predData$loan_purpose_name)))),
                                          conditionalPanel(condition = "input.loanTBox == true",
                                                           selectizeInput("laonTInput", "Loan Type", choices = levels(as.factor(predData$loan_type_name))))
                                   ) #end column
                                  ), #end column
                            column(6,
                                   h2("Random Forest Classification Results"),
                                   h3(withSpinner(textOutput("rf"))),
                                   hr(),
                                   h4("Random Forest Classification Controls"),
                                   column(6,
                                          numericInput("numPred", "Number of Predictors (up to 6)", value=2,min=1,max=6, step = 1)
                                          ),
                                   column(6,
                                          numericInput("numTree", "Number of Trees (10-100)", value=50,min=10,max=100, step = 5)
                                          ),
                                   h4("The variable importance graphs show that income and loan amount are the most important predictors."),
                                   withSpinner(plotOutput("rfPlot"))
                            ) #end column
                          ) #end fluidrow
                  ), #end tabItem
                  #Sixth tab content
                  tabItem(tabName = "data",
                          fluidRow(
                            column(3,
                                   checkboxGroupInput("checkbox", "Select Variables", selected = "sequence_number", choices = list("action_taken_name", "agency_name", "applicant_ethnicity_name", "applicant_income_000s", "raceRevised", "sexRevised", "census_tract_number", "county_name", "denial_reason_name_1", "hud_median_family_income", "lien_status_name", "loan_amount_000s", "loan_purpose_name", "loan_type_name", "minority_population", "msamd_name", "owner_occupancy_name", "population", "property_type_name", "purchaser_type_name", "sequence_number", "tract_to_msamd_income")),
                                   downloadButton("downloadData", "Save Data")
                            ), #end column
                            column(9,
                                   h3("Select Variables and Export Data for Exploration"),
                                   h5(em("Reference the About tab for variable descriptions.")),
                                   withSpinner(DT::dataTableOutput("table"))
                            ) #end column
                          ) #end fluidrow
                  ) #end tabItem
                ) #end tabItems
            ) # end dashboard body
)#end dashboard page 
