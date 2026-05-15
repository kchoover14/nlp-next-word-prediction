# load libraries
library(stringr)         # string manipulation
library(dplyr)           # data wrangling
library(data.table)      # fast file reading
library(ggplot2)         # plotting
library(kableExtra)      # table formatting
library(viridis)         # color palettes
library(shiny)           # web app framework
library(bslib)           # Bootstrap theming
library(shinycssloaders) # loading spinners
library(udpipe)          # POS tagging for predicted word selection
library(quanteda)        # stop words list

# load model data -- files live in same folder as app.R
unigram_df  <- fread("tidytext-unigrams-train.csv")
bigram_df   <- fread("tidytextbigrams-train.csv")
trigram_df  <- fread("tidytexttrigrams-train.csv")
quadgram_df <- fread("tidytextquadgrams-train.csv")

# load udpipe model for POS-based predicted word selection
ud_model_path <- "english-ewt-ud-2.5-191206.udpipe"
if (!file.exists(ud_model_path)) {
  dl       <- udpipe_download_model(language = "english-ewt")
  ud_model <- udpipe_load_model(dl$file_model)
} else {
  ud_model <- udpipe_load_model(ud_model_path)
}

# get POS tag for a single word
get_pos <- function(word) {
  if (is.na(word) || nchar(word) == 0) return(NA_character_)
  result <- as.data.frame(udpipe_annotate(ud_model, x = word))
  if (nrow(result) == 0) return(NA_character_)
  result$upos[1]
}

# return first content word (noun, verb, adj, adv, pronoun) from predictions
get_predicted_word <- function(predictions_df) {
  stops <- stopwords()
  for (w in predictions_df$predicted) {
    if (!w %in% stops) {
      pos <- get_pos(w)
      if (!is.na(pos) && pos %in% c("NOUN", "PROPN", "VERB", "ADJ", "ADV", "PRON")) {
        return(w)
      }
    }
  }
  return(predictions_df$predicted[1])  # fallback to top prediction
}

# function to clean user input: matches similar text cleansing done to the corpus to ensure a match can be found
userInput_cleaner <- function(input) {
  input_cleaning <- str_to_lower(input)
  input_cleaning <- gsub("'s\\b", "", input_cleaning)  # possessives: Smith's -> Smith
  input_cleaning <- gsub("'", "", input_cleaning)       # contractions: can't -> cant
  input_clean    <- str_trim(input_cleaning)
  return(input_clean)
}

# determine which ngram df to predict against based on word count of input
set_df_and_ngram <- function(userInput) {
  userInput_words        <- unlist(str_split(userInput, " "))
  userInput_words_length <- length(userInput_words)

  if (userInput_words_length >= 3) {
    ngram_input <- paste(userInput_words[userInput_words_length-2],
                         userInput_words[userInput_words_length-1],
                         userInput_words[userInput_words_length])
    target_df <- quadgram_df

  } else if (userInput_words_length == 2) {
    ngram_input <- paste(userInput_words[userInput_words_length-1],
                         userInput_words[userInput_words_length])
    target_df <- trigram_df

  } else {
    ngram_input <- paste(userInput_words[userInput_words_length])
    target_df   <- bigram_df
  }

  assign("ngram_input", ngram_input, envir = .GlobalEnv)
  assign("target_df",   target_df,   envir = .GlobalEnv)
}

# backoff: if no match in target_df, back off to smaller ngram level
decide_df <- function(ngram_input) {
  ngram_matches <- sum(target_df$first == ngram_input)

  if (ngram_matches == 0) {
    if (length(target_df$ngram) == length(bigram_df$ngram)) {
      ngram_input   <- ngram_input
      target_df     <- unigram_df
      ngram_matches <- 0

    } else {
      if (length(target_df$ngram) == length(trigram_df$ngram)) {
        ngram_input   <- word(ngram_input, -1)
        target_df     <- bigram_df
        ngram_matches <- sum(target_df$first == ngram_input)

      } else {
        ngram_input   <- word(ngram_input, -2, -1)
        target_df     <- trigram_df
        ngram_matches <- sum(target_df$first == ngram_input)

        if (ngram_matches == 0) {
          ngram_input   <- word(ngram_input, -1)
          target_df     <- bigram_df
          ngram_matches <- sum(target_df$first == ngram_input)
        }
      }
    }
  }

  assign("ngram_matches", ngram_matches, envir = .GlobalEnv)
  assign("ngram_input",   ngram_input,   envir = .GlobalEnv)
  assign("target_df",     target_df,     envir = .GlobalEnv)
  return(target_df)
}

# gather top 10 predictions from matched ngram level
set_top_predictions <- function(ngram_input, target_df) {
  if (length(target_df$ngram) == length(unigram_df$ngram)) {
    predictions <- unigram_df %>%
      mutate(Probability = round(n / sum(n), 4)) %>%
      arrange(desc(Probability)) %>%
      top_n(n = 10, wt = Probability) %>%
      arrange(desc(Probability)) %>%
      slice(row_number(1:10))
    predictions <- as.data.frame(predictions)

  } else {
    predictions <- target_df %>%
      filter(grepl(paste0("^", ngram_input, "$"), first)) %>%
      mutate(Probability = round(n / sum(n), 4)) %>%
      arrange(desc(Probability)) %>%
      top_n(n = 10, wt = Probability) %>%
      arrange(desc(Probability)) %>%
      slice(row_number(1:10))
    predictions <- as.data.frame(predictions)

    if (nrow(predictions) == 0) {
      predictions <- unigram_df %>%
        mutate(Probability = n / sum(n)) %>%
        top_n(n = 10, wt = n) %>%
        slice(row_number(1:25))
      predictions <- predictions[sample(nrow(predictions), 10, replace = TRUE), ]
    }
  }

  assign("predictions_df", predictions, envir = .GlobalEnv)
}

# ── UI ──────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  theme = bs_theme(
    version      = 5,
    bootswatch   = "flatly",
    primary      = "#3B4A6B",
    base_font    = font_google("Source Sans Pro"),
    heading_font = font_google("Source Serif 4")
  ),

  tags$head(tags$style(HTML("
    body { background-color: #d6e4ee; }
    .top-panel {
      background: #ffffff;
      border-radius: 8px;
      padding: 24px 28px 20px;
      margin-bottom: 20px;
      border: 3px solid #2d6e85;
    }
    .result-word {
      font-size: 2rem;
      font-weight: 700;
      color: #1a1a2e;
      letter-spacing: -0.5px;
    }
    .result-label {
      font-size: 0.85rem;
      color: #1a1a2e;
      text-transform: uppercase;
      letter-spacing: 0.08em;
      margin-bottom: 2px;
    }
    .bottom-panel {
      background: #ffffff;
      border-radius: 8px;
      padding: 20px 24px;
      border: 3px solid #2d6e85;
      height: 100%;
    }
    .panel-subtitle {
      font-size: 0.8rem;
      color: #1a1a2e;
      margin-top: -4px;
      margin-bottom: 12px;
    }
    .go-btn { margin-top: 4px; }
    h4 { color: #1a1a2e; }
  "))),

  # enter key trigger
  tags$head(tags$script(HTML("
    $(document).on('keypress', '#user_input', function(e) {
      if (e.which == 13) {
        $('#go').click();
      }
    });
  "))),

  # ── top panel ──
  div(class = "top-panel",
      fluidRow(
        column(8,
               p("Enter words below and click Go to predict the next word.",
                 style = "color: #1a1a2e; margin-bottom: 12px;"),
               fluidRow(
                 column(8,
                        textInput("user_input", label = NULL,
                                  placeholder = "Enter text here",
                                  width = "100%")
                 ),
                 column(2,
                        actionButton("go", "Go", class = "btn btn-primary go-btn",
                                     width = "100%")
                 )
               )
        ),
        column(4,
               div(class = "result-label", "Predicted next word"),
               div(class = "result-word", textOutput("predicted_next_word"))
        )
      )
  ),

  # ── bottom panels ──
  fluidRow(
    column(7,
           div(class = "bottom-panel",
               h4("Top 10 Predictions"),
               p("Ranked by probability from matched n-gram level.",
                 class = "panel-subtitle"),
               withSpinner(plotOutput("top10_hist", height = "380px"), color = "#3B4A6B")
           )
    ),
    column(5,
           div(class = "bottom-panel",
               h4("Meta Data"),
               p("Summary information about the prediction.",
                 class = "panel-subtitle"),
               withSpinner(tableOutput("general_info_table"), color = "#3B4A6B"),
               HTML("<ul style='font-size:0.78rem; color:#1a1a2e; margin-top:10px; padding-left:18px;'>
                 <li><strong>N-gram match found in:</strong> the ngram level where a match was found (unigrams, bigrams, trigrams, or quadgrams)</li>
                 <li><strong>Total Possibilities:</strong> number of ngrams in that particular ngram dataset</li>
                 <li><strong>Total Matches:</strong> number of matches found in that ngram dataset that matched your text</li>
               </ul>")
           )
    )
  )
)

# ── SERVER ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  user_input_reactive <- eventReactive(input$go, {
    input$user_input
  })

  output$predicted_next_word <- renderText({
    if (input$go == 0) return("\u2014")
    validate(need(user_input_reactive() != "", "Please enter some text."))
    processing_text <- userInput_cleaner(user_input_reactive())
    set_df_and_ngram(processing_text)
    target_df <- decide_df(ngram_input)
    set_top_predictions(ngram_input, target_df)
    return(get_predicted_word(predictions_df))
  })

  output$top10_hist <- renderPlot({
    validate(need(user_input_reactive() != "", "Please enter some text."))
    if (input$go == 0) return(NULL)
    processing_text <- userInput_cleaner(user_input_reactive())
    set_df_and_ngram(processing_text)
    target_df <- decide_df(ngram_input)
    set_top_predictions(ngram_input, target_df)
    df     <- predictions_df %>% arrange(Probability)
    n_bars <- nrow(df)
    pal    <- viridis::mako(n_bars, begin = 0.2, end = 0.8, direction = -1)
    df$color <- pal
    ggplot(df, aes(x = reorder(predicted, Probability), y = Probability)) +
      geom_bar(stat = "identity", fill = df$color, alpha = 0.92) +
      geom_text(aes(label = sprintf("%.3f", Probability)),
                hjust = -0.1, size = 3.5, color = "#333333") +
      coord_flip() +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "Probability") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        axis.text.y        = element_text(color = "#333"),
        axis.text.x        = element_text(color = "#888"),
        axis.title.x       = element_text(color = "#888", size = 11)
      )
  })

  output$general_info_table <- function() {
    if (input$go == 0) return(NULL)
    validate(need(user_input_reactive() != "", "Please enter some text."))
    processing_text <- userInput_cleaner(user_input_reactive())
    set_df_and_ngram(processing_text)
    target_df <- decide_df(ngram_input)
    set_top_predictions(ngram_input, target_df)
    Ngram_DF_Evaluated <- if (str_count(ngram_input, "\\S+") == 3) {
      "Quadgrams"
    } else if (str_count(ngram_input, "\\S+") == 2) {
      "Trigrams"
    } else if (str_count(ngram_input, "\\S+") == 1) {
      "Bigrams"
    } else {
      "Unigrams"
    }
    general_info_df <- data.frame(
      Data   = c("Your Text", "N-gram match found in", "Total Possibilities", "Total Matches"),
      Result = c(user_input_reactive(), Ngram_DF_Evaluated,
                 length(target_df$ngram), ngram_matches)
    )
    general_info_df %>%
      kable("html", align = "l", escape = FALSE, row.names = FALSE) %>%
      kable_styling("bordered", full_width = TRUE) %>%
      row_spec(0, background = "#2c3e6b", color = "white", bold = TRUE)
  }
}

shinyApp(ui = ui, server = server)