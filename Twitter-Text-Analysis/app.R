library(twitteR)
library(tidyverse)
library(wordcloud)
library(tm)
library(SnowballC)
library(syuzhet)
library(tidytext)
library(highcharter)
library(shiny)

# -----------Twitter API-----------
# set up api keys and tokens
consumer_key <- "Z8iHE1dFs4jJq87fcc4hn3IDw"
consumer_secret <- "bNV4CU3d39g6au4tgeFHNcJkLOvvmaDA0fPaCpxWcFoOO6uOKz"
access_token <- "784188931478724608-c6YOOlOb043e46nnmKpl2Erz6kNNqbD"
access_secret <- "ctXgsyavMLN72oOsYbjXPdo8iQ0DuJAonYludTI9FTaIN"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

# -----------Functions-----------
tweet.corpus <- function(tweets){
  # turn into dataframe and keep only 'text' + 'created date'
  search.df <- twListToDF(tweets) %>%
    select(text, created)
  tweet_corpus <- Corpus(VectorSource(search.df$text))
  trim <- function(x) gsub("http[s]?://[A-Za-z\\d/\\.]+|&amp;|&lt;|&gt;", "", x)
  tweet_corpus <- tm_map(tweet_corpus, content_transformer(trim)) %>%
    tm_map(removeWords, stop_words$word) %>%
    tm_map(content_transformer(tolower))
  return(tweet_corpus)
}

plot_wordcloud <- function(corpus){
  m <- TermDocumentMatrix(corpus) %>%
    as.matrix()
  freq <- sort(rowSums(m), decreasing = T)
  freq.df <- data.frame("word" = names(freq), freq)
  wordcloud(words = freq.df$word, freq = freq.df$freq, 
            min.freq = 3, max.words = 100, random.order = F,
            rot.per = 0.15, colors = brewer.pal(12, "Paired"))
}

plot_emotion <- function(corpus){
  senti <- get_nrc_sentiment(as.character(corpus))
  senti.df <- data.frame("number" = colSums(senti[,]))
  senti.df <- cbind("sentiment" = rownames(senti.df), senti.df)
  rownames(senti.df) <- NULL
  highchart() %>%
    hc_title(text = "Emotion Indicator") %>%
    hc_chart(polar = T) %>% 
    hc_xAxis(categories = senti.df$sentiment, 
             labels = list(style = list(fontSize= '14px')), title =NULL, tickmarkPlacement = "on", lineWidth = 0) %>% 
    hc_plotOptions(series = list(marker = list(enabled = F))) %>% 
    hc_yAxis(gridLineInterpolation = "polygon", lineWidth = 0, min = 0) %>% 
    hc_add_series(name = "Emotions Score", senti.df$number, type ="area", color = "#56A5EC", pointPlacement = "on")
}
# -----------UI-----------
ui <- fluidPage(
   
   titlePanel("Twitter Text Analysis"),
   
   sidebarLayout(
      sidebarPanel(
        numericInput("num", h4("Number of Tweets"), 
                     value = 500),
         textInput("search", h4("Search"), 
                    value = "Enter search topic..."),
        actionButton("goButton", "Search", icon("twitter"),
                     style="color: #fff; background-color: #337ab7")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Sentiment Indicator",
                   highchartOutput("plot1")),
          tabPanel("Word Cloud",
                   plotOutput("plot2"))
          )
        )
          
      )
   )

# -----------Server-----------
server <- function(input, output) {
  
  tweets <- eventReactive(input$goButton,{
    tweet.corpus(searchTwitter(input$search, n = input$num, lang = "en"))
  }) 
  
  output$plot1 <- renderHighchart({
    plot_emotion(tweets())
  })
  
  output$plot2 <- renderPlot({
    plot_wordcloud(tweets())
  })
}

shinyApp(ui = ui, server = server)

