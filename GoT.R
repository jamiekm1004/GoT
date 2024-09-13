
# Title: Game of Thrones Script Analysis
# Description: The app provides insights into the "Game of Thrones" script,
#             focus on sentiment and word analysis.
# Details: Focusing primarily on sentiment analysis and word frequency analysis,
#          including bigrams. The user can filter the results by specific seasons,
#          episode, and characters.
# Author: In Kim  

library(shiny)
library(bslib)     # for creating nice shiny dashboards
library(tidyverse) # for data manipulation and graphics
library(tidytext)  # for text mining
library(plotly)    # for web-interactive graphics
library(DT)        # to work with HTML table widgets
library(shinyWidgets)


# =======================================================
# Sentiment Lexicons
# =======================================================
bing = read_csv("bing.csv", col_types = "cc")
afinn = read_csv("afinn.csv", col_types = "cc")
nrc = read_csv("nrc.csv", col_types = "cc")
loughran = read_csv("loughran.csv", col_types = "cc")


# ===============================================
# Import data
# ===============================================
 dat = read_csv(
  file = "Game_of_Thrones_Script.csv", 
   col_names = c("Date", "Season", "Episode", "Title", "Name", "Sentence"),
   skip = 1,
  col_types = cols(
     Date = col_character(),
     Season = col_character(),
     Episode = col_character(),
     Title = col_character(),
     Name = col_character(),
     Sentence = col_character()
   ))
character_names = unique(dat$Name)
character_names = sort(character_names)
character_names = c("", character_names)


# =======================================================
# Define UI for application
# =======================================================
ui <- page_fluid(
  title = "GoT",
  header = "Header",
  h1("The Language of Ice and Fire: Game of Thrones",
     style = "text-align: center;"),
  
  # -------------------------------------------------------
  # Bootstrap theme
  # -------------------------------------------------------
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",
    base_font = font_google("Inter")
  ),
  
  # -------------------------------------------------------
  # Input widgets: distributed across 4 columns
  # -------------------------------------------------------
  layout_columns(
    #season
    card(
      card_header("Select a Season"),
      sliderInput(inputId = "season", 
                  label = "Season (1-8)", 
                  min = 1, 
                  max = 8, 
                  value = 1,
                  step = 1, 
                  ticks = TRUE, 
                  animate = TRUE),
      checkboxInput(inputId = "all_seasons",
                    label = "All Seasons",
                    value = FALSE) #not checked
    ), # closes card 1
    
    #episode
    card(
      card_header("Select an Episode"),
      selectInput(inputId = "episode", 
                   label = "Episode (1-10)", 
                   choices = c("None",
                              "Episode 1" ,
                              "Episode 2" ,
                              "Episode 3" ,
                              "Episode 4" ,
                              "Episode 5" ,
                              "Episode 6" ,
                              "Episode 7" ,
                              "Episode 8" ,
                              "Episode 9" ,
                              "Episode 10"),
                   selected = "None")
    ), # closes card 2
    
    #lexicon(bing, afinn, nrc, loughran)
    card(
      card_header("Sentiment Analysis Method"),
      radioButtons(inputId = "lexicon", 
                   label = "Sentiment Lexicon", 
                   choices = c("BING" = 1,
                              "AFINN" = 2,
                              "NRC" = 3,
                              "LOUGHRAN" = 4),
                   selected = 1)
    ), # closes card 3
    
    #character name
    card(
      card_header("Your Favorite Character"),
      pickerInput(inputId = "character_name",
                label = "Character Name",
                choices = character_names,
                options = list(`live-search` = TRUE,
                               `deselect-all-text` = "Remove Selection"),
                multiple = FALSE),
      actionButton(inputId = "reset_button",
                   label = "Reset",
                   class = "btn-outline-secondary")
    ), # closes card 4
    
  ), # closes layout_columns
  
  # -------------------------------------------------------
  # Tabset Panel of outputs
  # -------------------------------------------------------
  navset_card_tab(
    nav_panel(
      title = tagList(icon("snowflake"), " ICE: Sentiment"),
      p("Providing a comprehensice sentiment overview, 
      can be filtered by seasons, episodes, and characters"),
      card(
        card_header("Sentiment Anaylsis"),
        height = 500,
        plotlyOutput(outputId = "plot1"),
      ),
      card(
        card_header("Sentiment Table"),
        height = 500,
        style = "resize:vertical;",
        card_body(
          div(dataTableOutput(outputId = "table1"))
        )
      )
    ), # closes nav_panel 1
    
    nav_panel(
      title = tagList(icon("fire"), " FIRE: Bigram"),
      p("Highlighting most frequently occurring word pairs,
        can be filtered by seasons, episodes, characters"),
      card(
        card_header("Bigram Analysis"),
        plotlyOutput(outputId = "plot2")
      ),
      card(
        card_header("Bigram Table"),
        height = 500,
        style = "resize:vertical;",
        card_body(
          div(dataTableOutput(outputId = "table2"))
        )
      )
    ), # closes nav_panel 2
    
  ) # closes navset_card_tab
) # closes page_sidebar (UI)


# ======================================================
# Define server logic
# ======================================================
server <- function(input, output, session) {
  
  #sentiment
  game_sentiment = reactive({
    
    game_dat = data.frame(text = dat$Sentence, 
                          Season = dat$Season, 
                          Episode = dat$Episode,
                          Name = dat$Name)
    
   #tokenization, stopwords
   tidy_game = unnest_tokens(tbl = game_dat, 
                              output = word, 
                              input = text,
                              token = "words") %>%
     anti_join(stop_words, by = "word")
   
   
   season_string = paste("Season", input$season)
   
   if (input$character_name != "") {
     #name selected
     character_lower = tolower(input$character_name)
     tidy_game = tidy_game %>%
       filter(grepl(character_lower, tolower(Name), fixed = TRUE))
     
     #season and episode
     if (!input$all_seasons && input$episode != "None") {
       tidy_game = tidy_game %>%
         filter(Season == season_string, Episode == input$episode) %>%
         count(Season, Episode, Name, word, sort = TRUE) %>%
         ungroup()
     } else if (!input$all_seasons) {
       #season 
       tidy_game = tidy_game %>%
         filter(Season == season_string) %>%
         count(Season, Name, word, sort = TRUE) %>%
         ungroup()
     } else {
       #all season
       tidy_game = tidy_game %>%
         count(Name, word, sort = TRUE) %>%
         ungroup()
     }
   } else {
     #not selected
     if (!input$all_seasons && input$episode != "None") {
       tidy_game = tidy_game %>%
         filter(Season == season_string, Episode == input$episode) %>%
         count(Season, Episode, word, sort = TRUE) %>%
         ungroup()
     } else if (!input$all_seasons) {
       tidy_game = tidy_game %>%
         filter(Season == season_string) %>%
         count(Season, word, sort = TRUE) %>%
         ungroup()
     } else {
       tidy_game = tidy_game %>%
         count(word, sort = TRUE) %>%
         ungroup()
     }
   }
   
    #sentiment based on selected lexicon
    sentiment_data = NULL
    if(input$lexicon == 1) {
      sentiment_data = tidy_game %>% 
        inner_join(bing, by = "word")
    } else if(input$lexicon == 2) {
      sentiment_data = tidy_game %>% 
        inner_join(afinn, by = "word")
    } else if(input$lexicon == 3) {
      sentiment_data = tidy_game %>% 
        inner_join(nrc, by = "word")
    } else {
      sentiment_data = tidy_game %>% 
        inner_join(loughran, by = "word")
    }
    sentiment_data
  })
  
  #bigram
  game_word_trend = reactive({
    
    game_dat = data.frame(text = dat$Sentence, 
                          Season = dat$Season, 
                          Episode = dat$Episode,
                          Name = dat$Name)
    
    #token then bigram
    game_bigrams_token = game_dat %>%
      unnest_tokens(output = bigram, input = text, token = "ngrams", n = 2) %>%
      filter(!is.na(bigram))
    
    #seperate then stopwords
    game_bigrams = game_bigrams_token %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word)
    
    #unite
    game_bigrams = game_bigrams %>%
      unite(bigram, word1, word2, sep = " ")
    
    
    season_string = paste("Season", input$season)
    
    if (input$character_name != "") {
      #name selected
      character_lower = tolower(input$character_name)
      game_bigrams = game_bigrams %>%
        filter(grepl(character_lower, tolower(Name), fixed = TRUE))
      
      #season and episode
      if (!input$all_seasons && input$episode != "None") {
        game_bigrams = game_bigrams %>%
          filter(Season == season_string, Episode == input$episode) %>%
          count(Season, Episode, Name, bigram, sort = TRUE) %>%
          ungroup()
      } else if (!input$all_seasons) {
        #season 
        game_bigrams = game_bigrams %>%
          filter(Season == season_string) %>%
          count(Season, Name, bigram, sort = TRUE) %>%
          ungroup()
      } else {
        #all season
        game_bigrams = game_bigrams %>%
          count(Name, bigram, sort = TRUE) %>%
          ungroup()
      }
    } else {
      #not selected
      if (!input$all_seasons && input$episode != "None") {
        game_bigrams = game_bigrams %>%
          filter(Season == season_string, Episode == input$episode) %>%
          count(Season, Episode, bigram, sort = TRUE) %>%
          ungroup()
      } else if (!input$all_seasons) {
        game_bigrams = game_bigrams %>%
          filter(Season == season_string) %>%
          count(Season, bigram, sort = TRUE) %>%
          ungroup()
      } else {
        game_bigrams = game_bigrams %>%
          count(bigram, sort = TRUE) %>%
          ungroup()
      }
    }
      game_bigrams
    
  }) #close bigrams
  
  observeEvent(input$reset_button, {
    updatePickerInput(session, "character_name", selected = "")
  })#reset name search
  

  
  # ============================================================
  # Outputs for the first TAB
  # ============================================================
  # ------------------------------------------------------------
  # Plot for 1st analysis
  # ------------------------------------------------------------
  output$plot1 <- renderPlotly({
    
    sentiment_data = game_sentiment() %>%
      slice_head(n=20)
    
    #plot title
    plot_title = if (input$character_name != "") {
      paste("Game of Thrones: Top 20 Sentiment Analysis for", input$character_name)
    } else {
      "Game of Thrones: Top 20 Most Common Words with Associated Sentiment"
    }
  
    
    if (input$lexicon == 2) {
      ggplot(sentiment_data, aes(x = reorder(word, n), y = n, fill = value)) +
        geom_col() +
        coord_flip() +
        labs(title = plot_title,
             x = "Word", 
             y = "Count")
    }else if(input$lexicon == 3){
      
      sentiment_freq = game_sentiment() %>%
        group_by(word) %>%
        summarize(total_count = sum(n), .groups = 'drop')
      
      #top 20 words
      top_words = sentiment_freq %>%
        arrange(desc(total_count)) %>%
        slice_head(n = 20) %>%
        pull(word)
      
      #most frequent sentiment for each word
      sentiment_top = game_sentiment() %>%
        filter(word %in% top_words) %>%
        group_by(word) %>%
        count(sentiment) %>%
        top_n(1, n) %>%
        ungroup()
      
      #join
      plot_data = sentiment_top %>%
        inner_join(sentiment_freq, by = "word")
      
      ggplot(plot_data, aes(x = reorder(word, total_count), 
                            y = total_count, fill = sentiment)) +
        geom_col() +
        coord_flip() +
        labs(title = plot_title,
             x = "Word", y = "Total Count")
      }else{
      ggplot(sentiment_data, aes(x = reorder(word, n), 
                                 y = n, fill = sentiment)) +
        geom_col() +
        coord_flip() +
        labs(title = plot_title,
             x = "Word", 
             y = "Count")
    }
  }) #plot 1
  
  # ------------------------------------------------------------
  # Table for 1st analysis
  # ------------------------------------------------------------
  output$table1 <- renderDataTable({
    game_sentiment = game_sentiment()
    
    if (input$character_name != "") {
      if (!input$all_seasons && input$episode != "None") {
        colnames(game_sentiment) = c("Season", "Episode", "Name","Word", "Count", "Sentiment")
      } else if (!input$all_seasons) {
        colnames(game_sentiment) = c("Season", "Name","Word", "Count", "Sentiment")
      } else {
        colnames(game_sentiment) = c("Name","Word", "Count", "Sentiment")
      }
    } else {
      if (!input$all_seasons && input$episode != "None") {
        colnames(game_sentiment) = c("Season", "Episode", "Word", "Count", "Sentiment")
      } else if (!input$all_seasons) {
        colnames(game_sentiment) = c("Season", "Word", "Count", "Sentiment")
      } else {
        colnames(game_sentiment) = c("Word", "Count", "Sentiment")
      }
    }
    datatable(game_sentiment, options = list(pageLength = 20))
  })
  
  
  # ============================================================
  # Outputs for the second TAB
  # ============================================================
  # ------------------------------------------------------------
  # Plot for 2nd analysis
  # ------------------------------------------------------------
  output$plot2 <- renderPlotly({
    game_bigrams = slice(game_word_trend(), 1:20)
    
    plot_title = if (input$character_name != "") {
      paste("Game of Thrones: Most Frequent Bigrams for", input$character_name)
    }else{
      "Game of Thrones: Most Frequent Bigrams"
    }
    ggplot(data = game_bigrams) +
      geom_col(aes(x = reorder(bigram, n), y = n)) +
      coord_flip() + 
      labs(title = plot_title,
           x = "Bigram",
           y = "Count") +
      theme_minimal()
  })
  
  # ------------------------------------------------------------
  # Table for 2nd analysis
  # ------------------------------------------------------------
  output$table2 <- renderDataTable({
    game_bigrams = game_word_trend()
    
    if (input$character_name != "") {
      if (!input$all_seasons && input$episode != "None") {
        colnames(game_bigrams) = c("Season", "Episode", "Name", "Bigram", "Count")
      } else if (!input$all_seasons) {
        colnames(game_bigrams) = c("Season", "Name", "Bigram", "Count")
      } else {
        colnames(game_bigrams) = c("Name", "Bigram", "Count")
      }
    } else {
      if (!input$all_seasons && input$episode != "None") {
        colnames(game_bigrams) = c("Season", "Episode", "Bigram", "Count")
      } else if (!input$all_seasons) {
        colnames(game_bigrams) = c("Season", "Bigram", "Count")
      } else {
        colnames(game_bigrams) = c("Bigram", "Count")
      }
    }
    datatable(game_bigrams, options = list(pageLength = 20))
  })

} # closes server


# Run the application 
shinyApp(ui = ui, server = server)
