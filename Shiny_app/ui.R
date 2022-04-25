
library(magrittr)
library(shinyhelper)
library(DT)


fluidPage(
  HTML('<meta name="viewport" content="width=1024">'),
  theme = bslib::bs_theme(bootswatch = "cerulean"),
  

  titlePanel(
    
    
    fluidRow(
      column(10, offset =.7, "T2 risk stratification tool for patients with type 2 myocardial infarction", 
             style = "color: midnightblue;"),
      column(1, img(src="logoEdinburgh.png", with=90, height=90)))),

  
    sidebarLayout(
 
      sidebarPanel( 
   
          helpText("Fill-in the following fields with patient data:"),
          br(),
  
          textInput("Age", "Age (years)"),
          textInput("HeartRate", "Heart rate (b.p.m)"),
          textInput("eGFR", "eGFR (ml/min)")%>% 
          helper(type = "inline",
             title = "eGFR",
             colour = "gray",
             content =  c("Glomerular filtration rate was estimated employing the 
                          <a href=https://scholar.google.com/scholar?hl=en&as_sdt=0%2C5&q=A+simplified+equation+to+predict+glomerular+filtration+rate+from+serum+creatinine&btnG= ;>MDRD Study equation.</a>")),
 

          div(selectInput("TypeTrop", "Troponin assay:", c("Troponin I","Troponin T")), style="display:inline-block"),
          div(style="display: inline-block; width: 50px;",HTML("<br>")),
          div(textInput("PeakTroponin", "hs-cTn (ng/L)"), style="display:inline-block; width: 150px;"),

 
          selectInput("MyocardialIschaemia", "Myocardial Ischaemia on ECG", choices = c("","No", "Yes")),
          selectInput("DiabetesMellitus", "Diabetes Mellitus", choices = c("","No", "Yes")),
          selectInput("Anaemia", "Anaemia", choices = c("","No", "Yes")) %>% 
                      helper(type = "inline",
                             title = "Anaemia",
                             colour = "gray",
                            content = c("Defined as an haemoglobin level <130 g/L in men, and <120 g/L in women according to the 
                           <a href= https://www.who.int/vmnis/indicators/haemoglobin.pdf ;>World Health Organization criteria.</a>")),
 

          selectInput("PrevHFH", "Previous heart failure hospitalisation", choices = c("","No", "Yes")),
          selectInput("PrevIHD", "Previous ischaemic heart disease", choices = c("", "No", "Yes"))
    ),
    

    mainPanel(

      tags$style(HTML(".tabbable > .nav > li > a{
                        color: midnightblue;}")),
      
      
      tabsetPanel(
        
        tabPanel( "Result",
                
                  tags$style("#legend {font-size:12px;
                              color:midnightblue;
                              font-style: italic;
                              display:block;
                              bottom: 12px; 
                              position:absolute;
                              width: 100%;
                              left:0px;}"),
      
                  tags$style("#result {font-size:19px;
                              color:black;
                              top: 56px; 
                              position:absolute;
                              font-style: bold;}"),
      
                  tags$style("#category {font-size:19px;
                              color:black;
                              top: 146px; 
                              position:absolute;
                              font-style: bold;}"),
      
      p(" "),
      verbatimTextOutput("result"),
      htmlOutput("category"),
      textOutput("legend")
    ),
    
    
    tabPanel("About this",
             tags$style("#legend2 {font-size:12px;
                        color:midnightblue;
                        font-style: italic;
                        display:block;
                        bottom: 1px; 
                        position:absolute;
                        width: 100%;
                        left:0px;}"),
             
             tags$style("#funding {font-size:13px;
                        color:gray;
                        font-weight: bold;
                        display:block;}"),
             
             p(" "),
             
             p("T2-risk is a novel tool that predicts risk of myocardial infarction or all-cause death at one year 
                in patients with type 2 myocardial infarction.
                It uses routinely collected patient data including age, heart rate, maximal cardiac troponin
                concentration, estimated glomerular filtration rate, myocardial ischemia on electrocardiogram,
                diabetes mellitus, anaemia, known ischemic heart disease, and previous heart failure."),

            p("The tool was constructed employing a Cox proportional hazards model. Performance was evaluated in a derivation cohort 
                (",a("High-STEACS", href="https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(18)31923-8/fulltext"),") and 
                two external validation cohorts (", a("Karolinska Hospital", href="https://www.ajconline.org/article/S0002-9149(18)31771-5/pdf"),
             "and",a("APACE", href="https://www.ahajournals.org/doi/10.1161/circulationaha.111.023937#:~:text=Conclusions%E2%80%94,other%20causes%20of%20cTn%20elevations."), "international cohort study)."), 
           
            p(""),
            
            p("Risk groups were identified based on quantiles of the estimated event probabilities:"),
            DT::dataTableOutput("Test_Table",  width = "42%"),
            
            p(""),
            p(" "),
           br(),
           
            p("The analysis code is available at", a("GitHub", href="https://github.com/KarlaMonterrubioG/T2-risk-score.git"), 
              "and the manuscript published", a("here.", href="https://github.com/KarlaMonterrubioG/T2-risk-score.git"), 
              style = "font-size:11px;"),
            br(),
            br(),
            br(),
           
            textOutput("funding"),
            p(""),
            p(" "),
           
            textOutput("space"),
            br(),
            br(),
           
            textOutput("legend2")
      
    ),
    
    
    tabPanel("Reference",
             tags$style("#legend3 {font-size:12px;
                        color:midnightblue;
                        font-style: italic;
                        display:block;
                        bottom: 12px; 
                        position:absolute;
                        width: 100%;
                        left:0px;}"),
             p(" "),
          
            p("Taggart C, Monterrubio-Gómez K, Roos A, Boeddinghaus J, Kimenai D, Kadesjo E, Bularga A, Wereski R,
               Ferry A, Lowry M, Anand A, Lee K, Duodesis D, Manolopoulou I, Nestelberger T, Koechlin L, Lopez Ayala P, 
              Mueller C, Mills NL, Vallejos CA, and Chapman AR.
              Improving risk stratification for patients with type 2 myocardial infarction. 2022"), 
            
             textOutput("legend3"))
    
  )
)
)
)



