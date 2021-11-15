#install.packages(shinythemes)
library(shiny)
library(tidyverse)
library(wordcloud)
library(ggplot2)
library(RColorBrewer)
library(tidytext)
library(shinythemes)
getwd()
books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

#text <-  tibble(text = readLines(sprintf("C:/Users/spa94/Documents/UNCC/DSBA5122/5122_ProbSet_3_SamAvis/data/%s.txt", book), encoding="UTF-8"))

# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  text <-  tibble(text = readLines(sprintf("./data/%s.txt", book), encoding="UTF-8"))
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE)
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}

# task6: add in shinythemes function

ui <- fluidPage(
  theme = shinytheme('cerulean'),
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  sidebarLayout(
    sidebarPanel(
      selectInput('select', label = 'Choose a book:',
                  choices = books),

      checkboxInput('stopwords', label = 'Stop Words',
                    value = TRUE),
      actionButton('rerunbutton', label = 'Rerun'),
      hr(),

      h3('Word Cloud Settings'),

      sliderInput('maxwords', label = 'Max # of Words:',
                  min = 10, max = 200, step = 10, value = 100),

      sliderInput('largestwords','Size of largest words:',
                  min = 1, max=8, value = 4),

      sliderInput('smallestwords', label = 'Size of smallest words:',
                  min = 0.1, max = 4, value = 0.5),

      hr(),

      h3('Word Count Settings'),

      sliderInput('minwordcount', label = 'Minimum words for Counts Chart:',
                  min = 10, max = 100, value = 25),
      sliderInput('wordsizecount', 'Word size for Counts Chart:',
                  min = 8, max = 30, value = 14)

    ),
  mainPanel(
    tabsetPanel(type = 'tabs',
                tabPanel('Word Cloud', plotOutput('cloud', height = '600px')),
                tabPanel('Word Counts', plotOutput('freq', height = '600px')))
          )
      )

  # task1: add in the sidebarLayout with sidebarPanel and mainPanel

  # task2: add in the inputs in the sidebarPanel

  # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)

  # task3: add in the outputs in the sidebarPanel

  # task6: and modify your figure heights
)

server <- function(input, output) {
    freq <- eventReactive(input$rerunbutton,{
      withProgress({
        setProgress(message = "Processing corpus...")
        getFreq(input$select,
                input$stopwords)
      })
                           })


    output$cloud <- renderPlot({
      v <- freq()
      pal <- brewer.pal(8,"Dark2")

      v %>%
        with(
          wordcloud(
            word,
            n,
            scale = c(input$largestwords, input$smallestwords),
            random.order = FALSE,
            max.words = input$maxwords,
            colors=pal))

    })

    output$freq <- renderPlot({
      b <- freq()
      b %>%
        filter(n>input$minwordcount) %>%
        ggplot(aes(reorder(x=word, n), y = n)) +
        geom_col() +
        theme(text = element_text(size = input$wordsizecount),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()) +
        coord_flip()

    })

  # task5: add in reactivity for getFreq function based on inputs

}

shinyApp(ui = ui, server = server)

# Task 7 my published link is https://cltdsba.shinyapps.io/5122_ProbSet_3_SamAvis/
