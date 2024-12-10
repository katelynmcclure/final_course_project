#
#

library(shiny)
library(tidyverse)
library(rvest)
library(tidymodels)
library(janitor)

# Access data from Mac Athletics website
team_vballdata <- function(year) {
  year <- as.character(year)
  url <- paste0("https://athletics.macalester.edu/sports/womens-volleyball/stats/", year)
  table <- read_html(url) %>%
    html_table() %>%
    .[[6]] %>%
    mutate(Opponent = str_extract(Opponent, "[A-Z].*")) %>% # remove "vs" and "at" from opponent name
    mutate(`W/L` = as.factor(`W/L`)) %>%
    mutate(PCT = PCT * 100) %>% # convert hitting percentage to percentage
    filter(Date != "") %>% # remove year-end stats rows
    mutate(across(c(SA, SE, K, E, TA, AST, RE, DIG, BS, BA, BE, TB, BHE), ~ .x / SP)) # calculate per set stats (rather than raw totals)
  return(table)
}

team_training <- map(c(2021, 2022, 2023, 2024), team_vballdata) %>% # 2024 in the model because not predicting 2024 outcomes anymore, predicting theoretical games
  list_rbind()

team_2024 <- team_vballdata(2024)

vars_to_select <- function(df){
  df %>%
    select(where(is.numeric))
}

# function for logistic regression model (on training data)
team_vballmodel <- function(df, vars) {
  df <- df %>%
    mutate(`W/L` = as.factor(`W/L`)) %>% 
    mutate(`W/L` = relevel(`W/L`, ref = "L")) # reference is the 0
  
  logistic_spec <- logistic_reg() %>%
    set_mode("classification") %>% 
    set_engine("glm")
  
  # create formula based on variables passed in (VECTOR OF STRINGS)
  formula <- as.formula(paste("`W/L` ~", paste(vars, collapse = " + ")))
  
  variable_recipe <- recipe(formula, data = df)
  
  logistic_workflow <- workflow() %>% 
    add_recipe(variable_recipe) %>%
    add_model(logistic_spec)
  
  logistic_model <- logistic_workflow %>% 
    fit(data = df)
  return(logistic_model) # returns model coefficients
}

# function for interpreting model coefficients
interpretmodel <- function(tidy_model) {
  interpretations <- tidy_model %>%
    filter(term != "(Intercept)") %>%
    mutate(
      odds_change = exp(estimate),
      pval = ifelse(p.value < 0.05, "SIGNIFICANT", "NOT SIGNIFICANT"),
      interpretation = ifelse(
        estimate > 0,
        paste0(
          "When controlling all other variables, for each one-unit increase in ", term,
          ", the probability of winning increases by approximately ",
          round((odds_change / (odds_change + 1)) * 100, 2), "%. ", term, " has a p-value of ", round(p.value, 6), ", which is ", pval, "."
        ),
        paste0(
          "When controlling all other variables, for each one-unit increase in ", term,
          ", the probability of winning decreases by approximately ",
          abs(round((odds_change / (odds_change + 1)) * 100, 2)), "%. ", term, " has a p-value of ", round(p.value, 6), ", which is ", pval, "."
        )
      )
    )
  
  
  # Return only the interpretations
  return(interpretations$interpretation)
}

# in-sample stats function
insampleconf <- function(model, df) {
  in_sample_classifications <- model %>%
    augment(new_data = df) %>%
    mutate(.pred_class = ifelse(is.na(.pred_class), "W", .pred_class)) %>%
    mutate(.pred_class = if_else(.pred_class == "1", "L", "W")) %>%
    mutate(.pred_class = as.factor(.pred_class)) %>%
    mutate(.pred_class = droplevels(.pred_class)) %>%
    mutate(`W/L` = factor(`W/L`, levels = c("L", "W"))) 
  
  confmat <- in_sample_classifications %>% 
    conf_mat(truth = `W/L`, estimate = .pred_class) 
  
  
  return(confmat) # returns in-sample confusion matrix
}

insampleconfmatrix <- function(class){
  confmat <- class %>% 
    conf_mat(truth = `W/L`, estimate = .pred_class)
  return(confmat)
}

modelaccuracy <- function(model, df) {
  in_sample_classifications <- model %>%
    augment(new_data = df) %>%
    mutate(.pred_class = ifelse(is.na(.pred_class), "W", .pred_class)) %>%
    mutate(.pred_class = if_else(.pred_class == "1", "L", "W")) %>%
    mutate(.pred_class = as.factor(.pred_class)) %>%
    mutate(.pred_class = droplevels(.pred_class)) %>%
    mutate(`W/L` = factor(`W/L`, levels = c("L", "W")))
  accuracy <- in_sample_classifications %>%
    metrics(truth = `W/L`, estimate = .pred_class) %>%
    filter(.metric == "accuracy") %>%
    pull(.estimate)
  return(accuracy)
}

predictWL <- function(tidymodel, inputs) {
  # Extract the intercept and coefficients
  intercept <- as.numeric(tidymodel[tidymodel$term == "(Intercept)", "estimate"])
  coefficients <- tidymodel %>%
    filter(term != "(Intercept)") %>%
    select(term, estimate) %>%
    mutate(estimate = as.numeric(estimate))
  
  # Ensure inputs are named and match the coefficients
  inputs <- as.list(inputs)
  stopifnot(all(coefficients$term %in% names(inputs))) # Verify matching inputs
  
  # Calculate the linear predictor
  linear_predictor <- intercept + sum(unlist(inputs) * coefficients$estimate)
  
  # Convert to probability
  prob <- exp(linear_predictor) / (1 + exp(linear_predictor))
  
  # Classify as "Win" or "Loss"
  winloss <- ifelse(prob > 0.5, "WIN", "LOSS")
  
  # Final statement
  statement <- paste0(winloss, " (", round(prob * 100, 2), "% probability of winning the match)")
  return(statement)
}


#### Number Below

# function to predict new data
predictnewdata <- function(model, newdata) {
  predictions <- model %>% 
    augment(new_data = newdata) %>%
    mutate(.pred_class = ifelse(is.na(.pred_class), "W", .pred_class)) %>%
    mutate(.pred_class = if_else(.pred_class == "1", "L", "W")) %>%
    mutate(Match = if_else(.pred_class == `W/L`, TRUE, FALSE))
  return(predictions) # returns predictions
}

# new model accuracy function (returns single value)
newmodelaccuracy <- function(predictions) {
  accuracy <- predictions %>%
    group_by(Match) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    summarize(
      total_true = sum(count[Match == TRUE]),
      total = sum(count)
    ) %>%
    mutate(accuracy = total_true / total) 
  
  return(accuracy$accuracy) 
}


#
#
#
#### Static page functions
year_links <- str_c("https://athletics.macalester.edu/sports/womens-volleyball/stats/", c(2021, 2022, 2023, 2024))

seniors <- c("Wooten, Gwen", "Preston, Adisa", "Geber, Stephanie", "Norton, Nicole", "Galer, Grace", "Williams, Torrance", "MacInnis, Jill")

year_to_df <- function(link){
  all_tables <- read_html(link) %>%
    html_table()
  
  tables <- c(all_tables[2], all_tables[3])
  cleaner_tables <- map(tables, ~ as.data.frame(.x) %>% row_to_names(1) %>% select(-c(`#`, `Bio Link`)))
  
  return_tables <- cleaner_tables[[1]] %>%
    rename(off_attempts = TA) %>%
    left_join(cleaner_tables[[2]] %>% rename(def_attempts = TA), by = join_by("Player", "SP")) %>%
    mutate(
      year = str_sub(link, nchar(link) - 3),
      Player = str_extract(Player, ".*")) %>%
    filter(Player %in% seniors)
  
  return(return_tables)
}

full_senior_stats <- map(year_links, year_to_df) %>%
  list_rbind()

clean_senior_stats <- full_senior_stats %>%
  mutate(across(!Player, as.numeric)) %>%
  mutate(anon_player_id = as.factor(match(Player, seniors)))

####

## App

ui <- fluidPage(
  navbarPage(
    "Macalester Women's Volleyball Statistical Analysis",
    tabPanel("Intro",
             mainPanel(h4("We should put a few paragraphs of introductory text here maybe :)"))),
    tabPanel(
      "Model", 
      sidebarLayout(
        sidebarPanel(
          h4("Choose stats to include in the model:"),
          checkboxGroupInput(
            "stats",
            NULL,
            team_training %>% select(where(is.numeric)) %>% names(),
            selected = c("PCT", "SA", "SE")
          ),
          h4("Predict Match Outcome:"),
          uiOutput("dynamicInputs"), # Placeholder for dynamic inputs
          actionButton("predict", "Predict"), # Button to trigger prediction
          htmlOutput("outcome") 
        ),
        mainPanel(
          htmlOutput("code_book"),
          plotOutput("model_plot"),
          div(uiOutput("accuracy"), style = "text-align: center;"),
          uiOutput("predictions")
        )
      )
    ),
    tabPanel("Static",
             mainPanel(
               plotOutput("static_sp"),
               plotOutput("static_hitting"),
               plotOutput("static_reception")
             ))
  )
)

server <- function(input, output, session) {
  
  # Codebook for volleyball stats
  output$code_book <- renderUI({
    HTML("
    <b>Volleyball Stat Abbreviations</b><br>
    <table>
      <tr>
        <td>SP = sets played </td>
        <td>TB = total blocks </td>
      </tr>
      <tr>
        <td>K = kills</td>
        <td>BHE = ball handling errors </td>
      </tr>
      <tr>
        <td>E = errors </td>
        <td>BS = block solos </td>
      </tr>
      <tr>
        <td>TA = total attempts </td>
        <td>BA = block assists </td>
      </tr>
      <tr>
        <td>PCT = hitting percentage &nbsp; &nbsp;</td>
        <td>RE = receiving errors </td>
      </tr>
      <tr>
        <td>AST = assists </td>
        <td>DIG = digs </td>
      </tr>
      <tr>
        <td>SA = service aces </td>
        <td>SE = service errors </td>
      </tr>
      <td> &nbsp; </td>
        <td> &nbsp; </td>
    </table>
  ")
  })
  
  
  # Dynamic input boxes for user input based on selected stats
  output$dynamicInputs <- renderUI({
    req(input$stats) # Ensure stats are selected
    lapply(input$stats, function(stat) {
      numericInput(
        inputId = paste0("input_", stat),
        label = paste("Enter per-set value for", stat),
        value = 0
      )
    })
  })
  
  # Render mosaic for model fit
  output$model_plot <- renderPlot({
    team_model <- team_training %>%
      team_vballmodel(vars = input$stats) # create model with selected variables
    
    insampleconf(team_model, team_training) %>% 
      autoplot() +
      labs(subtitle = "Mosaic Plot of Model Accuracy") +
      theme(plot.subtitle = element_text(size = 20, face = "bold"))
  })
  
  output$accuracy <- renderUI({
    roundedaccuracy <- round(modelaccuracy(team_training %>% team_vballmodel(vars = input$stats), team_training), 4)
    percentaccuracy <- roundedaccuracy * 100
    HTML(paste0("<b>Model Accuracy: ", percentaccuracy, "%</b>"))
  })
  
  # Render model interpretation
  output$predictions <- renderUI({
    tidy_team_model <- team_training %>%
      team_vballmodel(vars = input$stats) %>%
      tidy()
    
    interpretations <- interpretmodel(tidy_team_model)
    
    # Wrap each interpretation in a styled HTML tag
    HTML(paste0(
      "<ul>",
      paste0("<li>", interpretations, "</li>", collapse = ""),
      "</ul>"
    ))
  })

  
  # Predict the outcome based on user inputs
  observeEvent(input$predict, {
    req(input$stats) # Ensure stats are selected
    
    # Collect user inputs dynamically
    user_inputs <- sapply(input$stats, function(stat) {
      input[[paste0("input_", stat)]]
    })
    names(user_inputs) <- input$stats # Ensure names match stats
    
    # Fit the model
    tidy_team_model <- team_training %>%
      team_vballmodel(vars = input$stats) %>%
      tidy()
    
    # Predict using the generalized function
    outcome <- predictWL(tidy_team_model, user_inputs)
    
    # Display prediction
    output$outcome <- renderText({
      paste("Predicted model outcome:", outcome)
    })
  })
  
  output$static_hitting <- renderPlot({
    clean_senior_stats %>%
      filter(off_attempts > 50) %>%
      ggplot(aes(x = year, y = PCT, color = anon_player_id)) +
      geom_point(aes(size = off_attempts)) +
      geom_line() +
      labs(
        title = "Senior Hitting Percentage Over Time",
        x = "Year",
        y = "Hitting %",
        color = "Player ID",
        size = "Offensive Attempts") +
      theme_bw()
  })
  
  output$static_reception <- renderPlot({
    clean_senior_stats %>%
      filter(def_attempts > 50) %>%
      ggplot(aes(x = year, y = `Rec%`, color = anon_player_id)) +
      geom_point(aes(size = def_attempts)) +
      geom_line() +
      labs(
        title = "Senior Reception Percentage Over Time",
        x = "Year",
        y = "Reception %",
        color = "Player ID",
        size = "Defensive Attempts") +
      theme_bw()
  })
  
  output$static_sp <- renderPlot({
    clean_senior_stats %>%
      ggplot(aes(x = year, y = SP, color = anon_player_id)) +
      geom_point() +
      geom_line() +
      labs(
        title = "Senior Sets Played Over Time",
        x = "Year",
        y = "Sets Played",
        color = "Player ID",
        size = "Defensive Attempts") +
      theme_bw()
  })
}

# Run app
shinyApp(ui = ui, server = server)

