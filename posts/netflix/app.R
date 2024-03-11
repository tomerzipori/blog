library(tidyverse)
library(extrafont)
library(lubridate)
library(ggwordcloud)
library(quanteda)
library(quanteda.textplots)
library(quanteda.textstats)
library(shiny)

data <- read_csv("C:/Users/tomer/OneDrive/GitHub/blog/posts/netflix/netflix_titles.csv", show_col_types = F)

months_names <- data.frame(name = month.name,
                           month = seq(1,12,1))

data_clean <- data |>
  mutate(month_added = word(date_added, 1),
         day_added = str_remove_all(word(date_added, 2), ","),
         year_added = word(date_added, 3)) |>
  left_join(months_names, by = join_by(month_added == name)) |>
  mutate(date_added = paste(day_added, month, year_added, sep = ".")) |>
  select(-month_added, -day_added, -month) |>
  mutate(date_added = dmy(date_added)) |>
  mutate(date_added_ym = zoo::as.yearmon(date_added))

data_clean_dfm <- data_clean |>
  mutate(description = str_remove_all(description, pattern = "[[:punct:]]")) |>
  corpus(text_field = "description") |>
  tokens(remove_separators = T) |>
  tokens_remove(stopwords()) |>
  dfm()

netflix_tf_idf <- data_clean_dfm |>
  dfm_tolower() |>
  dfm_wordstem() |>
  dfm_tfidf()

# Personal wordcloud
personal_wordcloud <- function(shows){
  data |>
    mutate(month_added = word(date_added, 1),
           day_added = str_remove_all(word(date_added, 2), ","),
           year_added = word(date_added, 3)) |>
    filter(title %in% shows) |>
    left_join(months_names, by = join_by(month_added == name)) |>
    mutate(date_added = paste(day_added, month, year_added, sep = ".")) |>
    select(-month_added, -day_added, -month) |>
    mutate(date_added = dmy(date_added)) |>
    mutate(date_added_ym = zoo::as.yearmon(date_added)) |>
    
    
    mutate(description = str_remove_all(description, pattern = "[[:punct:]]")) |>
    corpus(text_field = "description") |>
    tokens(remove_separators = T) |>
    tokens_remove(stopwords()) |>
    dfm() |>
    
    
    textstat_frequency() |>
    mutate(r_frequency = 100*frequency/sum(frequency)) |>
    
    
    ggplot(aes(label = feature, size = r_frequency, fill = r_frequency, color = r_frequency)) +
    scale_size_area(max_size = 7) +
    geom_text_wordcloud(seed = 14) +
    scale_fill_gradient(low = "#000000", high = "#990011FF") +
    theme_classic() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 11, face = "bold"),
          legend.position = "none")
}

ui <- fluidPage(
  titlePanel("Test app"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "watched_wordcloud",
        label = "I watched...",
        choices = data_clean$title,
        multiple = T,
        selected = data_clean$title[14]
      )
    ),
    mainPanel(
      plotOutput("cloud1", height = "350px")
    )
  )
)

server <- function(input, output) {
  shows_watched <- reactive({input$watched_wordcloud})
  
  output$cloud1 <- renderPlot({
    personal_wordcloud(shows_watched())
  })
}

shinyApp(ui, server)
