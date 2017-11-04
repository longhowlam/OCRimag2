library(shiny)
library(miniUI)
library(leaflet)
library(ggplot2)

library(jpeg)
library(tesseract)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(quanteda)
library(DT)

ui <- miniPage(
  gadgetTitleBar(left = NULL, right = NULL,"OCR an image"),
  miniTabstripPanel(
    
    miniTabPanel("introduction", icon = icon("area-chart"),
                 miniContentPanel(
                   htmlOutput("intro")
                 )
    ),
    
    miniTabPanel( "Parameters", icon = icon("sliders"),
                 miniContentPanel(
                   fileInput('file1', 'Choose an image (max 5MB)'),
                   numericInput("maxwords", "Max number words in cloud",value=100),
                   numericInput("minfreq", "Minimum word frequency in cloud", value=2),
                   checkboxInput("stopwords", "Remove (English) stopwords", value = FALSE)
                 )
    ),
 
    miniTabPanel("image", icon = icon("file-image-o"),
                 miniContentPanel(
                   padding = 0,
                   imageOutput("plaatje")
                 )
    ),
    miniTabPanel("OCR text", icon = icon("table"),
                 miniContentPanel(
                   verbatimTextOutput("OCRtext")
                 )
    ),
    miniTabPanel("Word cloud", icon = icon("cloud"),
                 miniContentPanel(
                       plotOutput("cloud", height = "800px")
                 )
    )
  )
)

server <- function(input, output, session) {
  
  extractedText <- reactive({
    
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(
      message = 'OCR in progress', 
      detail = 'This may take 5-10 sec...'
    )
    
    inFile = input$file1
    
    if (!is.null(inFile))
    {
      Extext <- ocr(inFile$datapath)
    }
    else
    {
      Extext <- ocr("www/ocr-test.jpg")
    }
    Extext
  })
  
  output$intro <- renderUI({
    list(
      h4("This shiny app is the mobile version of my previous ", a("shiny app", href="http://5.100.228.219:3838/sample-apps/OCRimage/")), 
      h4("It worked on mobile devices but the display was not optimal. This app is a slight modification using the miniUI package."),
      h4("Use a photo for OCR, the extracted text is then used to form a wordcloud image, (English) stopwords can be removed"),
      h4("If no image is selected a default ocr test image is used. The R source can be found on my ", a("github", href="https://github.com/longhowlam/OCRinShiny")),
      h4("Cheers, Longhow")
    )
    
  })
  
  
  output$plaatje <- renderImage({
    
    inFile = input$file1
    print(inFile)
    if (!is.null(inFile))
    {
      
      width  <- session$clientData$output_plaatje_width
      height <- session$clientData$output_plaatje_height
      list(
        src = inFile$datapath,
        width=width,
        height=height
      )
    }
    else
    {
      list(src="www/ocr-test.jpg")
    }
  },
  deleteFile = FALSE
  )
  
  
  output$OCRtext = renderPrint({
    
    cat(extractedText())
  })
  
  
  output$cloud = renderPlot({
    
    text = extractedText()
    cp = Corpus(VectorSource(text))
    cp = tm_map(cp, content_transformer(tolower))
    cp = tm_map(cp, removePunctuation)
    if(input$stopwords){
      cp = tm_map(cp, removeWords, stopwords('english'))
    }
    
    pal <- brewer.pal(9,"BuGn")
    pal <- pal[-(1:4)]
    wordcloud(
      cp, 
      max.words = input$maxwords,
      min.freq = input$minfreq,
      random.order = FALSE,
      colors = pal
    )
    
  })
  
  
  observeEvent(input$done, {
    stopApp(TRUE)
  })
}



shinyApp(ui, server)
