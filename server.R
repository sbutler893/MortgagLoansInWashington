shinyServer(function(input, output, session) {
##### BEGIN ABOUT TAB #####
  #saving HMDA url
  url <- a("HMDA Loan Application Register (LAR)", href="https://cfpb.github.io/api/hmda/queries.html")
  #rendering the text with the URL
  output$datalink <- renderUI({
    tagList("The data is sourced from the ", url, ".")
  })
  
  #create table with fields and field definitions    
  output$fields <- DT::renderDataTable({
    datatable(fieldDef)
  })
##### END ABOUT TAB #####
  
  
##### BEGIN DATA SUMMARIES TAB #####
  #get reactive data set and calculate approval rate. 
  getData <- reactive({
    newData <- modelData%>%filter(sexRevised == input$sex)
    newDataAppr<-newData%>%filter(action_taken_name == "Loan originated")
    approvalRate<- round((nrow(newDataAppr)/nrow(newData))*100,2)
  })
  
  #render text for approval rate 
  output$approveRate <- renderText({
    #get filtered data
    newData <- getData()
    newDataAppr <- getData()
    approvalRate <- getData()
    paste("The approval rate for", input$sex, "applicants in Washington in 2016 was",approvalRate, "%.", sep = " ")
  })
  
  # create contingency table for sex and action taken 
  output$actiontable<-DT::renderDataTable({
    as.data.frame.matrix(table(modelData$sexRevised,modelData$action_taken_name))
  }, rownames = TRUE)
  
  #updates titles for numeric summaries - Loan Approved
  output$title1 <- renderUI({
    if (input$sex =='Male' & input$getIncLoan==TRUE) {
      h3("Loan Originated for Male Applicants")
    } else if (input$sex=='Female' & input$getIncLoan==TRUE){
      h3("Loan Originated for Female Applicants")
    } else if (input$sex=='Not applicable' & input$getIncLoan==TRUE){
      h3("Loan Originated where Applicant's Sex is Unknown")
    } 
  })
  
  #updates titles for numeric summaries - Loan Denied
  output$title2 <- renderUI({
    if (input$sex =='Male' & input$getIncLoan==TRUE) {
      h3("Loan Denied for Male Applicants")
    } else if (input$sex=='Female' & input$getIncLoan==TRUE){
      h3("Loan Denied for Female Applicants")
    } else if (input$sex=='Not applicable' & input$getIncLoan==TRUE){
      h3("Loan Denied where Applicant's Sex is Unknown")
    } 
  })
  
  #six number summary table for Loan Approved
  output$numSumm1 <- DT::renderDataTable({
    if (input$getIncLoan==TRUE) {
      round(as.data.frame(getNumericSummaries(input$sex,"Loan originated")),0)
    }
  })
  
  #six number summary table for Loan Denied
  output$numSumm2 <- DT::renderDataTable({
    if (input$getIncLoan==TRUE) {
      round(as.data.frame(getNumericSummaries(input$sex,"Application denied by financial institution")),0)
    }
  })
  
  #stacked bar chart for loan type
  output$bar1<-renderPlot({
    bar1Data<-modelData%>%filter(sexRevised == input$sex)
    g<-ggplot(data = bar1Data, aes(x = loan_type_name))
    g + geom_bar(aes(fill = action_taken_name))+ theme(legend.position="bottom")+ggtitle(paste(input$sex,"Loan Type",sep=" "))
  })
  
  #stacked bar chart for loan purpose
  output$bar2<-renderPlot({
    bar2Data<-modelData%>%filter(sexRevised == input$sex)
    g<-ggplot(data = bar2Data, aes(x = loan_purpose_name))
    g + geom_bar(aes(fill = action_taken_name))+ theme(legend.position="bottom")+ggtitle(paste(input$sex,"Loan Purpose",sep=" "))
  })
  
  #histogram of income. Removed blanks and outliers
  output$scatter1<-renderPlotly({
    if (input$getIncLoan==TRUE){
      graphData<-modelData%>%filter(sexRevised == input$sex & action_taken_name == "Loan originated" & applicant_income_000s != "" & applicant_income_000s <input$maxInc)
      plot_ly(data = graphData, x = ~applicant_income_000s, type ="histogram", nbinsx = 200)%>%layout(title=paste("Income Distribution for", input$sex, "Applicants",sep=" "))
    }
  })
  
  #histogram of loan amounts Removed blanks and outliers
  output$scatter2<-renderPlotly({
    if (input$getIncLoan==TRUE){
      graphData<-modelData%>%filter(sexRevised == input$sex & action_taken_name == "Loan originated" & loan_amount_000s != "" & loan_amount_000s <input$maxLoan)
      plot_ly(data = graphData, x = ~loan_amount_000s, type ="histogram", nbinsx = 200)%>%layout(title=paste("Loan Amount Distribution for", input$sex, "Applicants",sep=" "))
    }
  })
##### END DATA SUMMARIES TAB #####
  
  
##### BEGIN INTERACTIVE PLOT TAB #####
  # For storing which rows have been excluded
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(boxData))
  )
  
  #graph box plot of loan amount by race
  output$box1 <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- boxData[ vals$keeprows, , drop = FALSE]
    exclude <- boxData[!vals$keeprows, , drop = FALSE]
    ggplot(keep, aes(x = raceRevised, y = loan_amount_000s, color = raceRevised)) + geom_boxplot() + theme(legend.position="bottom")+ggtitle("Loan Amount in (000)s by Race")
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    res <- nearPoints(boxData, input$plot1_click, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Toggle points that are brushed, when button is clicked
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(boxData, input$plot1_brush, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(boxData))
  })
##### END INTERACTIVE PLOT TAB #####
  
  
##### BEGIN CLUSTERING TAB #####
  #create dendrogram based in linkage input from user
  plotInput<- function(){
    hc1<-hclust(d, method = input$linkage )
    plot(hc1,xlab = "")
  }
  
  #render dendrogram for output
  output$clus1 <- renderPlot({
    plotInput()
  })
  
  #download handler to save plot as png
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("Dendrogram-", Sys.Date(), ".png", sep="")
    },
    content = function(file){
      ggsave(file,plot=plotInput())
    }
  )
##### END CLUSTERING TAB #####
  
  
##### BEGIN PREDICTIVE MODELING TAB #####
  #get data for logisitc regression and fit logistic regression
  getpredData <- reactive({
    #for predictive modeling. Each if checks if variable is selected and if so that variable is added to the select statment.
    syntax<-"dplyr::select(predData, application_result,applicant_income_000s,loan_amount_000s"
    if (input$agencyBox==TRUE){
      syntax<-paste(syntax, ",agency_name", sep="")
      if(input$sexBox==TRUE){
        syntax<-paste(syntax, ",sexRevised", sep="")
        if (input$raceBox==TRUE) {
          syntax<-paste(syntax, ",raceRevised", sep="")
          if (input$loanPBox==TRUE) {
            syntax<-paste(syntax, ",loan_purpose_name", sep="")
            if (input$loanTBox==TRUE) {
              syntax<-paste(syntax, ",loan_type_name", sep="")
            }
          }
        }
      }
    }#end nested if statements
    #adds ) to end select statement 
    syntax<-paste(syntax,")",sep="")
    #converts so select statement is not read as a string
    syntax<-eval(parse(text=syntax))
    #drops NAs
    predData <- syntax%>%drop_na()
    #creates train and test data sets 
    train <- sample(1:nrow(predData), size = nrow(predData)*0.8) 
    test <- dplyr::setdiff(1:nrow(predData), train) 
    predDataTrain <- predData[train, ] 
    predDataTest <- predData[test, ]
    #fits logistic regression with train data 
    glmFit<-glm(application_result~., data=predDataTrain, family="binomial")
  })
  
  #calculate P(Loan Approved) based on inputs provided by the user. 
  getpredResult <- reactive({
    predDataTrain <- getpredData()
    glmFit <- getpredData()
    newpredData = data.frame(applicant_income_000s=c(input$incomeInput),loan_amount_000s=c(input$loanInput),agency_name=c(input$agencyInput),sexRevised=c(input$sexInput),raceRevised=c(input$raceInput),loan_purpose_name=c(input$loanPInput),loan_type_name=c(input$laonTInput))
    predictionResult<-predict(glmFit, newdata = newpredData, type="response")
    predictionResult<-round(predictionResult*100,2)
  })
  
  #render text to shows calculated P(Loan Approved) based on inputs provided by the user.
  output$logReg <- renderText({
    #get filtered data
    predictionResult <- getpredResult()
    paste("The estimated probability that the loan is approved given the selected criteria is",predictionResult, "%.", sep = " ")
  })
  
  #get data, run random forest model, and see how well model performs on test data set. Renders accuracy and misclassification rate results. 
  output$rf <- renderText({
    #get data for random forest. A sample of the data is taken due to computer limitations. 
    ranForData<-modelData%>%mutate_if(is.character, as.factor)%>%select(application_result,applicant_income_000s,loan_amount_000s,agency_name,sexRevised,raceRevised,loan_purpose_name,loan_type_name)%>%drop_na()%>%sample_n(10000, replace = FALSE)
    #create train and test data set
    train <- sample(1:nrow(ranForData), size = nrow(ranForData)*0.8) 
    test <- dplyr::setdiff(1:nrow(ranForData), train) 
    rfDataTrain <- ranForData[train, ] 
    rfDataTest <- ranForData[test, ]
    #fit random forest model with user specified number of variables and number of trees
    rfFit <- randomForest(application_result ~ ., data = rfDataTrain, mtry = input$numPred, ntree = input$numTree, importance = TRUE)
    #use test data set to set how well model performs 
    rfPred <- predict(rfFit, newdata = dplyr::select(rfDataTest, -application_result))
    randomForestRes<-confusionMatrix(rfPred, rfDataTest$application_result)
    #compute accuracy
    accuracy<-sum(diag(randomForestRes$table))/sum(randomForestRes$table)
    accuracy<-round(accuracy*100,2)
    #compute misclassification rate
    misclass<-1-(sum(diag(randomForestRes$table))/sum(randomForestRes$table))
    misclass<-round(misclass*100,2)
    #text to be rendered 
    paste("The accuracy rate on the Testing dataset for the random forest method is",accuracy, "%. The misclassification rate on the Testing dataset for the random forest method is",misclass, "%.", sep = " ")
  })
  
  #renders variable importance plot 
  output$rfPlot <- renderPlot({
    varImpPlot(rfFit)
  })
##### END PREDICTIVE MODELING TAB #####
  
  
##### BEGIN VIEW DATA TAB #####
  #create output of observations, only displaying columns selected by the user.
  output$table <- DT::renderDataTable({
    datatable(modelData%>%select(input$checkbox),options=list(scrollX=TRUE))
  })
  
  #download handler to save plot as png
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("DataExport-", Sys.Date(), ".csv", sep="")
    },
    content = function(file){
      write.csv(modelData%>%select(input$checkbox), file)
    }
  )
##### END VIEW DATA TAB #####
  
  
})#end shiny server
