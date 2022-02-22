library(shiny)
library(purrr)
library(dplyr)
library(readr)
library(stringr)

# Load dictionary
word_dict <- read_tsv('word_dict.tsv')

# Template for a keyboard keys
keyboard_button <- function(id){
    actionButton(id, id, width = '50px')
}

# Check if string contains all letters from the letter_list with vectorization
str_detect_all <- function(string, letter_list){
    len_str = length(c(string))
    len_let = length(letter_list)
    
    # if pattern is an empty vector return TRUE for all strings
    if(!len_let){
        return(rep(T, len_str))
    }
    
    check_matrix = matrix(F, nrow = len_str, ncol = len_let)
    for(let_index in seq_along(letter_list)){
            check_matrix[, let_index] = str_detect(string, letter_list[let_index])
    }
    
    return(apply(check_matrix, 1, all))
}


ui <- fluidPage(
    tags$style(".glyphicon-ok-sign {color:#4bb013}"),
    tags$style(".glyphicon-remove-sign {color:#d81f1f}"),
    # Application title
    titlePanel("Helper to solve Wordly"),
    fluidRow(
        column(12, align="center",
               selectInput('num_letters', 'Number of letters in the word', c(4, 5, 6)),
               uiOutput('add_letters'),
               br(),
               # Keyboard keys
               keyboard_button('Q'),
               keyboard_button('W'),
               keyboard_button('E'),
               keyboard_button('R'),
               keyboard_button('T'),
               keyboard_button('Y'),
               keyboard_button('U'),
               keyboard_button('I'),
               keyboard_button('O'),
               keyboard_button('P'),
               br(),
               keyboard_button('A'),
               keyboard_button('S'),
               keyboard_button('D'),
               keyboard_button('F'),
               keyboard_button('G'),
               keyboard_button('H'),
               keyboard_button('J'),
               keyboard_button('K'),
               keyboard_button('L'),
               br(),
               keyboard_button('Z'),
               keyboard_button('X'),
               keyboard_button('C'),
               keyboard_button('V'),
               keyboard_button('B'),
               keyboard_button('N'),
               keyboard_button('M'),
               div(
                   style = 'margin-top: 20px; margin-bottom: 20px;',
                   actionButton('search', 'Search Words', style="color: #fff; background-color: #337ab7; border-color: #2e6da4", width = "150px"),
                   actionButton('reset', 'Reset', style="color: #fff; background-color: #cc5047; border-color: #963a33", width = "150px")
                   ),
               DT::DTOutput("words"),
               # Debug output
               # verbatimTextOutput("buttonValue"),
               # verbatimTextOutput("LS")
               )
    )
)

# Define server logic
server <- function(input, output, session) {
    # Status of the letter button:
    # 0 - probably present
    # 1 - must be absent
    # 2 - must be present
    letters_status = rep(0, 26)
    names(letters_status) = LETTERS
    ls = reactiveValues(letters_status = letters_status)
    
    # Check if any letter is marked absent in the word
    set_key_status <- function(key_id){
        # output$buttonValue <- renderPrint({ls$letters_status[key_id]})
        ls$letters_status[key_id] <- (ls$letters_status[key_id] + 1) %% 3
        if(ls$letters_status[key_id] == 1){
            updateActionButton(session, key_id, icon = icon("glyphicon glyphicon-remove-sign", lib = "glyphicon"))
        }else if(ls$letters_status[key_id] == 2){
            updateActionButton(session, key_id, icon = icon("glyphicon glyphicon-ok-sign", lib = "glyphicon"))
        }else{
            updateActionButton(session, key_id, icon = icon(NULL))
            # output$buttonValue = renderPrint({'Clear'})
        }
    }
    
    observeEvent(input$Q, set_key_status('Q'))
    observeEvent(input$W, set_key_status('W'))
    observeEvent(input$E, set_key_status('E'))
    observeEvent(input$R, set_key_status('R'))
    observeEvent(input$T, set_key_status('T'))
    observeEvent(input$Y, set_key_status('Y'))
    observeEvent(input$U, set_key_status('U'))
    observeEvent(input$I, set_key_status('I'))
    observeEvent(input$O, set_key_status('O'))
    observeEvent(input$P, set_key_status('P'))
    observeEvent(input$A, set_key_status('A'))
    observeEvent(input$S, set_key_status('S'))
    observeEvent(input$D, set_key_status('D'))
    observeEvent(input$F, set_key_status('F'))
    observeEvent(input$G, set_key_status('G'))
    observeEvent(input$H, set_key_status('H'))
    observeEvent(input$J, set_key_status('J'))
    observeEvent(input$K, set_key_status('K'))
    observeEvent(input$L, set_key_status('L'))
    observeEvent(input$Z, set_key_status('Z'))
    observeEvent(input$X, set_key_status('X'))
    observeEvent(input$C, set_key_status('C'))
    observeEvent(input$V, set_key_status('V'))
    observeEvent(input$B, set_key_status('B'))
    observeEvent(input$N, set_key_status('N'))
    observeEvent(input$M, set_key_status('M'))
    
    # Add letters
    letters_ids = reactive(paste0('l', seq_len(input$num_letters)))
    
    output$add_letters <- renderUI({
        map(letters_ids(), ~ div(style="display:inline-block", selectInput(.x, '', c('?', LETTERS), selected = '?', width = '70px'))) #  textInput(.x, '', value = "", width = "40px")
    })
    
    # Search in dict
    observeEvent(
        input$search,
        {
            # Make work copy of dictionary with required length of words
            word_dict_wc <- word_dict %>% 
                                filter(Length == input$num_letters) %>%
                                select(-Length)
            
            # Search pattern from the letter boxes
            selected_letters <- rep('.', input$num_letters)
            
            for(l_index in seq_along(letters_ids())){
                if(input[[letters_ids()[l_index]]] != '?'){
                    selected_letters[l_index] <- input[[letters_ids()[l_index]]]
                }
            }
            
            if(!all(selected_letters == '.')){
                pattern <- paste(selected_letters, collapse = '')
                word_dict_wc <- word_dict_wc %>%
                    mutate(Keep = str_detect(Word, pattern)) %>%
                    filter(Keep) %>%
                    select(-Keep)
            }
            
            # Remove words with letters that should not be in the searched word
            drop_letters = names(ls$letters_status[ls$letters_status == 1])
            
            if(length(drop_letters)){
                word_dict_wc <- word_dict_wc %>%
                    mutate(Keep = !str_detect(Word, paste(drop_letters, collapse = '|'))) %>%
                    filter(Keep) %>%
                    select(-Keep)
            }
            
            # Remove words that DO NOT contain letters that must be present
            keep_letters = names(ls$letters_status[ls$letters_status == 2])
            
            if(length(keep_letters)){
                word_dict_wc <- word_dict_wc %>%
                    mutate(Keep = str_detect_all(Word, keep_letters)) %>%
                    filter(Keep) %>%
                    select(-Keep)
            }
            
            output$words <- DT::renderDT(word_dict_wc %>% 
                                            filter(row_number() <= 25),
                                         rownames = FALSE
                                        )
        }
    )
    
    # Reset
    observeEvent(
        input$reset,
        {
            ls$letters_status = letters_status
            for(letter in LETTERS){
                updateActionButton(session, letter, icon = icon(NULL))
            }
            
            for(field in letters_ids()){
                updateSelectInput(session, field, selected = '?')
            }
        }
    )
}
    
# Run the application 
shinyApp(ui = ui, server = server)
