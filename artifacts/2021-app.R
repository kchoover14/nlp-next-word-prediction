#load libraries
library(stringr); library(dplyr); library(data.table)
library(ggplot2); library(ggpubr); library(kableExtra)
library(shiny); library(shinythemes); library(shinycssloaders)

#load model data
unigram_df <- fread("unigrams.csv")
bigram_df <- fread("bigrams.csv")
trigram_df <- fread("trigrams.csv")
quadgram_df <- fread("quadgrams.csv")

#function to clean user input: matches similar text cleansing done to the corpus to ensure a match can be found
userInput_cleaner <- function(input){
    input_cleaning <- str_to_lower(input)
    input_cleaning <- gsub("'", '', input_cleaning)
    input_clean <- str_trim(input_cleaning)
    return(input_clean)
}

set_df_and_ngram <- function(userInput) {
    userInput_words <- unlist(str_split(userInput, " "))
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
        target_df <- bigram_df

    }
    assign("ngram_input", ngram_input, envir = .GlobalEnv)
    assign("target_df", target_df, envir = .GlobalEnv)
}

decide_df <- function(ngram_input) {
    ngram_matches <- sum(target_df$first == ngram_input)

    if(ngram_matches == 0) {
        if(length(target_df$ngram) == length(bigram_df$ngram)) {
            ngram_input <- ngram_input
            target_df <- unigram_df
            ngram_matches <- 0

        } else {
            if(length(target_df$ngram) == length(trigram_df$ngram)) {
                ngram_input <- word(ngram_input, -1)
                target_df <- bigram_df
                ngram_matches <- sum(target_df$first == ngram_input)

            } else {
                ngram_input <- word(ngram_input, -2, -1)
                target_df <- trigram_df
                ngram_matches <- sum(target_df$first == ngram_input)

                if(ngram_matches == 0) {
                    ngram_input <- word(ngram_input, -1)
                    target_df <- bigram_df
                    ngram_matches <- sum(target_df$first == ngram_input)
                }
            }
        }
    }
    assign("ngram_matches", ngram_matches, envir = .GlobalEnv)
    assign("ngram_input", ngram_input, envir = .GlobalEnv)
    assign("target_df", target_df, envir = .GlobalEnv)
    return(target_df)
}

set_top_predictions <- function(ngram_input, target_df) {
    if (length(target_df$ngram) == length(unigram_df$ngram)) {
        predictions <- unigram_df %>%
            mutate(Probability = round(n / sum(n), 4)) %>%
            arrange(desc(Probability))  %>%
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
                slice(row_number(1:25)) %>%
            predictions <- predictions[sample(nrow(predictions), 10, replace = TRUE),]

        }
    }
    predictions_df <- predictions
    assign("predictions_df", predictions, envir = .GlobalEnv)
}

ui <- fluidPage(
    theme=shinytheme("readable"),
    titlePanel("Predictive Next Word Text Model"),
    sidebarLayout(
        sidebarPanel(
            helpText("User Instructions: Enter some words in the box and click the 'Go' button.
            In rare instances, using a proper names with an apostrophe (John Smith's) will throw an error.
            If so, please remove the name from the phrase.
            The tabs provide summary information about the prediction."),
            textInput("user_input", label = h5("Input Text"), value = ""),
            actionButton("go", "Go"),
            h5("Predicted Next Word (NA for no matches): "),
            h5(textOutput("predicted_next_word"))),
        mainPanel(
            h4("Top 10 Predictions"),
            plotOutput("top10_hist"),
            htmlOutput("top10_hist_text"),
            h4("Meta Data"),
            tableOutput("general_info_table"),
            htmlOutput("general_info_table_text")
        )))



server <- function(input, output, session) {
    user_input_reactive <- eventReactive(input$go, {
        input$user_input
    })
    output$predicted_next_word <- renderText({
        if (input$go == 0) {
            return()
        }  else {
            validate(
                need(user_input_reactive() != '', 'Please enter some text.')
            )
            processing_text <- userInput_cleaner(user_input_reactive())
            set_df_and_ngram(processing_text)
            target_df <- decide_df(ngram_input)
            set_top_predictions(ngram_input, target_df)
            return(predictions_df$predicted[1])
        }
    })

    output$top10_hist <- renderPlot({
        validate(
            need(user_input_reactive() != '', 'Please enter some text')
        )
        if (input$go == 0) {
            return()
        } else {
            processing_text <- userInput_cleaner(user_input_reactive())
            set_df_and_ngram(processing_text)
            target_df <- decide_df(ngram_input)
            set_top_predictions(ngram_input, target_df)
            ggplot(predictions_df, aes(x = reorder(predicted, n), y = Probability, fill = Probability == max(Probability), color = Probability == max(Probability))) +
                geom_text(aes(label = round(Probability, 2)), color = "black", hjust = -0.07, size = 4) +
                geom_bar(stat = "identity", alpha = .95, show.legend = FALSE) +
                scale_fill_manual(values = c("purple", "green")) +
                labs(y = "", x = "") +
                coord_flip() +
                theme_classic2(base_size = 16)
        }
    })

    output$top10_hist_text <- renderUI({
        validate(
            need(user_input_reactive() != '', '')
        )
        if (input$go == 0) {
            return()
        }  else {
            HTML("<ul><li>
                If there is no match, the graph will be empty.
                </li></ul>")
        }
    })
    output$general_info_table <- function() {
        validate(
            need(user_input_reactive() != '', 'Please enter some text')
        )
        if (input$go == 0) {
            return()
        }  else {
            req(user_input_reactive())
            processing_text <- userInput_cleaner(user_input_reactive())
            set_df_and_ngram(processing_text)
            target_df <- decide_df(ngram_input)
            set_top_predictions(ngram_input, target_df)
            User_Input <- user_input_reactive()
            Ngram_DF_Evaluated <- if(str_count(ngram_input, "\\S+") == 3) {
                "Quadgrams"
            }       else if(str_count(ngram_input, "\\S+") == 2) {
                "Trigrams"
            }       else if(str_count(ngram_input, "\\S+") == 1) {
                "Bigrams"
            }       else
                "Unigrams"
            Total_Ngram_Possibilities <- length(target_df$ngram)
            Matches_Identified <- ngram_matches
            general_info_df <- data.frame(
                Data = c("Your Text", "N-gram match found in ",  "Total Possibilities", "Total Matches"),
                Result = c(User_Input, Ngram_DF_Evaluated, Total_Ngram_Possibilities, Matches_Identified)
            )
            general_info_df %>%
                kable("html", align = "l", escape = F) %>%
                kable_styling("bordered", full_width = T)
        }}

    output$general_info_table_text <- renderUI({
        validate(
            need(user_input_reactive() != '', '')
        )
        if (input$go == 0) {
            return()
        }  else {
            HTML("<ul><li>N-gram matches can be unigrams, bigrams, trigrams, or quadgrams.
            </li><li>
            Total Possibilities: number of ngrams in that particular ngram dataset
            </li><li>
            Total Matches: Number of matches found in that ngrams dataset that matched your text.
            </li></ul>")
        }
    })
}

shinyApp(ui = ui, server = server)
