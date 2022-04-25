
riskcalc <- expression({

 linearpredictor <- (-2.7144998 +  0.028597071 * (data$Age_Index) - 6.0631548e-06 * pmax(data$Age_Index -48, 0)^3 +
                                   0.00012444097 * pmax(data$Age_Index - 71, 0)^3 - 0.00022281038 * pmax(data$Age_Index - 81, 0)^3 +
                                   0.00010443256 * pmax(data$Age_Index - 91 , 0)^3 - 0.016915066 * data$eGFR.computed + 4.2808103e-06 * pmax(data$eGFR.computed - 31, 0)^3 - 8.9758926e-06 * pmax(data$eGFR.computed - 65, 0)^3 + 4.6950823e-06 * pmax(data$eGFR.computed - 96, 0)^3 +
                                   0.11030707 * data$logtroponin +
                                   0.41765085 * (data$Anaemia == "Yes") +
                                   0.16789179 * (data$ECG == "Yes") +
                                   0.42463213 * (data$PrevHFHospitalisation == "Yes") +
                                   0.30616934 * (data$PrevDiabetes == "Yes") +
                                   0.083418182 * (data$PrevIHD == "Yes") +
                                   0.0062885784 * data$HeartRate - 2.057656e-06 * pmax(data$HeartRate - 65, 0)^3 + 3.4697729e-06 * pmax(data$HeartRate - 100, 0)^3 - 1.4121169e-06 * pmax(data$HeartRate - 151, 0)^3)
  
  s0365 = 0.7760240
  prob.primout = 1-s0365^exp(linearpredictor)
  prob.primout*100
  
})



Testtable <- data.frame("Risk group" = c("Low-risk", "Intermediate-risk", "High-risk"), 
                        "Event probability" = c("<= 13%", " > 13% and <= 34% ", "> 34%"), check.names = FALSE)


check_rangeHR <- function(input) {
  if (input <20 | input >240 ) {
    "Please enter a valid number for Heart rate. Value must be between 20 (b.p.m) and 240 (b.p.m)"
  } else if (input == "") {
    FALSE
  } else {
    NULL
  }
}


check_rangeEGFR <- function(egfr) {
  if (egfr <5 | egfr >200 ) {
    "Please enter a valid number for eGFR. Value must be between 5 (ml/min) and 200 (ml/min)"
  } else if (egfr == "") {
    FALSE
  } else {
    NULL
  }
}

shinyServer(function(input, output){
  observe_helpers(withMathJax = TRUE)
  
  
  output$Test_Table = DT::renderDataTable(
 
    Testtable, options = list( ordering=FALSE, searching = FALSE, paging=FALSE, info=FALSE,
                                columnDefs = list(list(className = 'dt-center', targets = "_all")),
                                initComplete = htmlwidgets::JS("function(settings, json) {",paste0("$(this.api().table().container()).css({'font-size': '", "9pt", "'});"),"}"),
                                               rowCallback = JS('function(row, data, index, rowId) {',
                                                             'console.log(rowId)','if(rowId >= 0 && rowId <= 3) {',
                                                             'row.style.backgroundColor = "white";','}','}')), 
                                selection = 'none',
                                rownames= FALSE) 
                                        
  
 
  inputdata <- reactive({
    

    req(input$Age)
    req(input$HeartRate)
    validate(check_rangeHR(as.numeric(input$HeartRate)))
    req(input$PeakTroponin,input$eGFR)
    validate(check_rangeEGFR(as.numeric(input$eGFR)))
    req(input$MyocardialIschaemia,input$DiabetesMellitus,
        input$Anaemia, input$PrevHFH,input$PrevIHD)
     
    data <- data.frame(
      Age_Index = as.numeric(input$Age),
      HeartRate = as.numeric(input$HeartRate),
      logtroponin <- ifelse(input$TypeTrop=="Troponin I", log(as.numeric(input$PeakTroponin)), 
                                           (0.04585+1.23005*log(as.numeric(input$PeakTroponin)))),
      
      eGFR.computed = as.numeric(input$eGFR),
      ECG = input$MyocardialIschaemia,
      PrevDiabetes = input$DiabetesMellitus,
      Anaemia = input$Anaemia,
      PrevHFHospitalisation = input$PrevHFH,
      PrevIHD = input$PrevIHD
    )
    data
  })

 
  output$result <- renderPrint({
    data=inputdata()
    riskCalculation = round(eval(riskcalc, data),1)
    cat("The probability of myocardial infarction or all-cause death at one year is:\n", riskCalculation, "%")
  })

    
  output$category <- renderText({ 
    
    data=inputdata()
    riskCalculation = eval(riskcalc, data)
    riskgroup <- ifelse(riskCalculation <= 13, return(paste("<span style=\"color:green\"> Patient is considered: Low-risk</span>")), 
                                           ifelse(riskCalculation > 13 & riskCalculation <= 34, 
                                          return(paste("<span style=\"color:orange\"> Patient is considered: Intermediate-risk</span>")), 
                                          ifelse (riskCalculation > 34 , return(paste("<span style=\"color:red\"> Patient is considered: High-risk</span>")), "0" )))
    
  })
  
  output$legend <- renderText({"IMPORTANT: This web portal is for research purposes only. The algorithm has not been validated for clinical use."})
  output$legend2 <- renderText({"IMPORTANT: This web portal is for research purposes only. The algorithm has not been validated for clinical use."})
  output$legend3 <- renderText({"IMPORTANT: This web portal is for research purposes only. The algorithm has not been validated for clinical use."})
  output$funding <- renderText({"FUNDING: This work has been supported by a Starter Grant for Clinical Lecturers from the Academy of Medical Sciences, a BHF-Turing Cardiovascular Data Science award and a 
                                Chancellor's Fellowship granted by the University of Edinburgh."})
  output$space <- renderText({" "})
  })




