library(shiny)
library(ggplot2)
library(readr)
library(dplyr)
library(formattable)
library(tm)
library(DT)
library(tidyverse)
library(corrplot)
library(fmsb)
library(reshape)
library(SnowballC)
library(RColorBrewer)


daily_spotify <- read_csv('./data.csv')
spotify_data <- read.csv('featuresdf.csv')

spotify_data$duration_ms <- round(spotify_data$duration_ms / 1000)
colnames(spotify_data)[15] <- "duration"

top_artists <- spotify_data %>%
  group_by(artists)  %>%
  summarise(n_apperance = n()) %>%
  filter(n_apperance > 1) %>%
  arrange(desc(n_apperance))

top_artists$artists <- factor(top_artists$artists, levels = top_artists$artists[order(top_artists$n_apperance)]) 

ed_sheeran_daily <- daily_spotify %>%
  filter(Region == "us", Artist == "Ed Sheeran", Position <= 100)

formatted_ed <- ed_sheeran_daily %>%
  group_by(`Track Name`) %>%
  summarise(n_daily = n()) %>%
  arrange(desc(n_daily))

choice.type <-
  c('loudness', 'valence', 'speechiness')
choice.value <-
  c(
    'energy',
    'tempo',
    'liveness',
    'danceability',
    'duration_ms'
  )


# UI
ui <- navbarPage("Top Songs in 2017",
  tabPanel("Introduction",
    tags$h1("Top Songs Analysis"),
    tags$h2("We want to find out why top music is popular.")),
  
  tabPanel("Data",
    tags$h1("Data"),
    DT::dataTableOutput("data")),
  
  tabPanel(
    "Single Variable",
    tags$h1("Summrizing time!"),
    br(),
    sidebarLayout(
      sidebarPanel(
        selectInput('SV.input', 'type', c(choice.type, choice.value), selectize = TRUE)
      ),
      mainPanel(plotOutput("SV.plot"))
    ),
    
    tags$h1("Summary"),
    verbatimTextOutput("summary")
    
  ),  
  tabPanel(
    "PART1",
    navlistPanel(
      "Data Analysis",
      tabPanel("Top Artists",
               mainPanel(
                 h3("Most songs enter the top 100 "),
                 plotOutput("plot1")
      )),
      tabPanel("Song Play Count",
               h3("The song's daily views"),
               mainPanel(
                 tableOutput('plot2')
      )),
      tabPanel("Correlation of Variable",
               mainPanel(
                 plotOutput('plot3')      
      )),
      "Feature",
      tabPanel("Key",
               mainPanel(
                 plotOutput("plot4")
             )),
      tabPanel("Density Plots of Correlated Variables",
               mainPanel(
                 plotOutput('plot5')
               ))
    )
    ),
  tabPanel(
    "PART2.",
    tags$h1("Box Plot"),
    sidebarLayout(
      sidebarPanel(
        selectInput('PA.type', 'type', choice.type, selectize = TRUE),
        selectInput('PA.value', 'Value', choice.value, selectize =
                      TRUE)
      ),
      mainPanel(plotOutput("PA.plot"))
    )
  ),

  tabPanel(
    "PART3",
    navlistPanel(
      "Data Analysis",
      tabPanel("Most Popular Songs",
               mainPanel(
                 h3("Top 5 Songs"),
                 plotOutput('plot6')
               )),
      tabPanel("Most Popular Singers",
               h3("Artists has more than 2 songs."),
               mainPanel(
                 plotOutput("plot7")
               ))
    )),
  tabPanel(
    "Singers Analysis",
      fluidRow(
        column(4, wellPanel(
          radioButtons("picture2", "Singer:",
                       c("Ed sheeran", "The Chainsmokers", "Martin Garrix"))
        )),
        column(4,
               imageOutput("image2")
        )
      )),
  tabPanel(
    "Key Analysis",
    fluidRow(
      column(4, wellPanel(
        radioButtons("picture3", "Key Analysis:",
                     c("Top100", "Major v.s. Minor", "Top100 Songs Major"))
      )),
      column(4,
             imageOutput("image3")
      )
    )
  ),
  tabPanel(
    "Tempo Analysis",
    fluidRow(
      column(4, wellPanel(
        radioButtons("picture4", "頻率分析:",
                     c("Type", "Interval", "Q-Q Plot"))
      )),
      column(4,
             imageOutput("image4")
      )
    )
  ),
  tabPanel(
    "Heatmap",
      fluidRow(
        column(4, wellPanel(
          radioButtons("picture5", "Picture:",
                       c("Valence", "Danceability", "Energy", "Speechiness"))
        )),
        column(4,
               imageOutput("image5")
        )
      )
    ))

# server
server <- function(input, output, session) {
 
  output$data <- renderDataTable({
    DT::datatable(spotify_data, options = list(pageLength = 25))
  })
  
  output$SV.plot <- renderPlot({
    if( is.element(input$SV.input, choice.type) ){
      ggplot(data = spotify_data, aes_string(x = input$SV.input)) +
        geom_bar() +
        labs(y = "count", x = input$SV.input)
    }
    else{
      ggplot(data = spotify_data, aes_string(x = input$SV.input)) +
        geom_histogram() +
        labs(y = "count", x = input$SV.input)
    }
  })
  
  output$PA.plot <- renderPlot({
    ggplot(data = spotify_data, aes_string(x = input$PA.type, y = input$PA.value)) +
      geom_boxplot() + coord_flip() +
      labs(y = input$PA.value, x = input$PA.type)
    
  })
  
  tabPanel("Scatter Plot",                   
           plotOutput("scatter"),
           textInput("text_scatter", label = "Interpretation", value = "Enter text..."))
  
  output$summary <- renderPrint({
    summary(spotify_data)
  })
  
  output$data.raw <- DT::renderDataTable({
    DT::datatable(spotify_data)
  })
  
  output$data.summary <- DT::renderDataTable({
    DT::datatable(summary(spotify_data))
  })

  
  output$plot1<-renderPlot({
    ggplot(top_artists, aes(x = artists, y = n_apperance)) +
      geom_bar(stat = "identity",  fill = "tomato2", width = 0.6 ) + 
      labs(title = " Top Artists of 2017", x = "Artists", y = "Number of Apperance on the Top 100") +
      theme(plot.title = element_text(size=15,hjust=-.3,face = "bold"), axis.title = element_text(size=12)) +
      geom_text(aes(label=n_apperance), hjust = 2, size = 3, color = 'white') +
      coord_flip()})

  output$plot2<-renderTable({
   formattable(formatted_ed)})

  output$plot3<-renderPlot({
    spotify_data_num <- spotify_data[,-(1:3)]
    mtCor <- cor(spotify_data_num)
    corrplot(mtCor, method = "ellipse", type = "upper", tl.srt = 45) })

  output$plot4<-renderPlot({
    spotify_data$key <- as.character(spotify_data$key)

    spotify_data$key <- revalue(spotify_data$key, c("0" = "C", "1" = "C<U+266F>,D<U+266D>", "2" = "D", "3" = "D<U+266F>,E<U+266D>", "4" = "E", "5" =  "F", "6" = "F<U+266F>,G<U+266D>",
                                                    "7" = "G","8" = "G<U+266F>,A<U+266D>","9" = "A","10" = "A<U+266F>,B<U+266D>","11" = "B"))
    
    song_keys <- spotify_data %>%
      group_by(key) %>%
      summarise(n_key = n()) %>%
      arrange(desc(n_key))
    
    song_keys$key <- factor(song_keys$key, levels = song_keys$key[order(song_keys$n_key)]) 
    ggplot(song_keys, aes(x = reorder(key,-n_key), y = n_key, fill = reorder(key,-n_key))) +
      geom_bar(stat = "identity") +
      labs(title = "Distribution of the Keys of Top Songs", x = "Keys", y = "Count of Keys on the Top 100") +
      geom_text(aes(label=n_key), position = position_stack(vjust = 0.8)) +
      theme_bw() +
      theme(plot.title = element_text(size=15,face = "bold"), axis.title = element_text(size=12)) +
      theme(legend.position="none")
    })
  
  output$plot5<-renderPlot({
    correlated_density <- ggplot(spotify_data) +
      geom_density(aes(energy, fill ="energy", alpha = 0.1)) + 
      geom_density(aes(valence, fill ="valence", alpha = 0.1)) + 
      geom_density(aes(danceability, fill ="danceability", alpha = 0.1)) + 
      scale_x_continuous(name = "Energy, Valence and Danceability") +
      scale_y_continuous(name = "Density") +
      ggtitle("Density plot of Energy, Valence and Danceability") +
      theme_bw() +
      theme(plot.title = element_text(size = 14, face = "bold"),
            text = element_text(size = 12)) +
      theme(legend.title=element_blank()) +
      scale_fill_brewer(palette="Accent")
    correlated_density})
  
  output$plot6<-renderPlot({

    
    m5 <- spotify_data[c(1:5),]
    m5<- m5[, c(1,3,4,8,9,11,12)] 
    m5<- as.data.frame(m5)
    m5.long <- melt(m5, id.vars="name")
    
    mp1<- ggplot(data=m5.long, aes(x=variable, y=value))+geom_bar(aes(y=value, fill=name),stat="identity", alpha=0.8 , position="dodge")+ ylab("Value")+ xlab("Variables to a song")+coord_flip()+ggtitle("Top 5 songs in Spotify 2017 ")
    mp1})
  
  output$plot7<-renderPlot({    
    spotify_data <- read.csv('featuresdf.csv')
    a1 <- group_by(spotify_data, artists )
    a2 <- dplyr::summarise(a1,  count=n())
    a2 <- arrange(a2, desc(count))
    a3 <- filter(a2, count>1)
    

    ap1 <- ggplot(a3, aes(x=reorder(artists,count),y=count))+
      geom_bar(aes(y=count,fill=artists), stat="identity")+
      labs(x="Artists", y="Number of Songs",
           title="Artists Has more than 2 Songs")+ theme(legend.position="none", axis.text.x = element_text(angle = 60, hjust = 1)) 
    ap1})
  
  output$image2 <- renderImage({
    if (is.null(input$picture2))
      return(NULL)
    
    if (input$picture2 == "Ed sheeran") {
      return(list(
        src = "2.png",
        contentType = "image/png",
        alt = "Ed sheeran"
      ))
    } else if (input$picture2 == "The Chainsmokers") {
      return(list(
        src = "3.png",
        contentType = "image/png",
        alt = "The Chainsmokers"
      ))
    } else if (input$picture2 == "Martin Garrix") {
      return(list(
        src = "4.png",
        contentType = "image/png",
        alt = "Martin Garrix"
      ))
    }
  }, deleteFile = FALSE)
  
  output$image3 <- renderImage({
    if (is.null(input$picture3))
      return(NULL)
    
    if (input$picture3 == "Top100") {
      return(list(
        src = "5.png",
        contentType = "image/png",
        alt = "Top100"
      ))
    } else if (input$picture3 == "Major v.s. Minor") {
      return(list(
        src = "6.png",
        contentType = "image/png",
        alt = "Major v.s. Minor"
      ))
    } else if (input$picture3 == "Top100 Songs Major") {
      return(list(
        src = "7.png",
        contentType = "image/png",
        alt = "Top100 Songs Major"
      ))
    }
  }, deleteFile = FALSE)
  
  output$image4 <- renderImage({
    if (is.null(input$picture4))
      return(NULL)
    
    if (input$picture4 == "Type") {
      return(list(
        src = "10.png",
        contentType = "image/png",
        alt = "Type"
      ))
    } else if (input$picture4 == "Interval") {
      return(list(
        src = "11.png",
        contentType = "image/png",
        alt = "Interval"
      ))
    } else if (input$picture4 == "Q-Q Plot") {
      return(list(
        src = "12.png",
        contentType = "image/png",
        alt = "Q-Q Plot"
      ))
    }
  }, deleteFile = FALSE)
  
  output$image5 <- renderImage({
    if (is.null(input$picture5))
      return(NULL)
    
    if (input$picture5 == "Valence") {
      return(list(
        src = "13.png",
        contentType = "image/png",
        alt = "Valence"
      ))
    } else if (input$picture5 == "Danceability") {
      return(list(
        src = "14.png",
        contentType = "image/png",
        alt = "Danceability"
      ))
    } else if (input$picture5 == "Energy") {
      return(list(
        src = "15.png",
        contentType = "image/png",
        alt = "Energy"
      ))
    } else if (input$picture5 == "Speechiness") {
      return(list(
        src = "16.png",
        contentType = "image/png",
        alt = "Speechiness"
      ))
    }
    
  }, deleteFile = FALSE)
  
  error = function(err){
  # edit the error message
  err$message <- paste("While training model", err, sep = " ")
  # and re-raise
  stop(err)
}
  
} 


# Run the application 
shinyApp(ui = ui, server = server)