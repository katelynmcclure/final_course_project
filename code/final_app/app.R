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
#
#
#

# Pages to scrape
year_links <- str_c("https://athletics.macalester.edu/sports/womens-volleyball/stats/", c(2021, 2022, 2023, 2024))

seniors <- c("Wooten, Gwen", "Preston, Adisa", "Geber, Stephanie", "Norton, Nicole", "Galer, Grace", "Williams, Torrance", "MacInnis, Jill")

# Scrape Macalester seniors statistics from the Athletics webpage
year_to_df <- function(link){
  # Read in the tables from the webpage
  all_tables <- read_html(link) %>%
    html_table()
  
  # Choose the tables with the individual stats
  tables <- c(all_tables[2], all_tables[3])
  cleaner_tables <- map(tables, ~ as.data.frame(.x) %>% row_to_names(1) %>% select(-c(`#`, `Bio Link`)))
  
  # join the offensive and defensive stats into one data frame
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

# clean the data and make players names anonymous in the shiny app
clean_senior_stats <- full_senior_stats %>%
  mutate(across(!Player, as.numeric)) %>%
  mutate(anon_player_id = as.factor(match(Player, seniors)))

# Make a plot of an individual statistic over time, filtering out players with low attempts
plot_with_attempts <- function(stat, is_off){
  if(is_off){
    return(
      clean_senior_stats %>%
        filter(off_attempts > 50) %>%
        ggplot(aes(x = year, y = {{stat}}, color = anon_player_id)) +
        geom_point(aes(size = off_attempts)) +
        geom_line() +
        theme_bw()
    )
  }
  else{
    return(
      clean_senior_stats %>%
        filter(def_attempts > 50) %>%
        ggplot(aes(x = year, y = {{stat}}, color = anon_player_id)) +
        geom_point(aes(size = def_attempts)) +
        geom_line() +
        theme_bw()
    )
  }
}

# Make a plot of an individual statistic over time
plot_no_attempts <- function(stat){
  clean_senior_stats %>%
    ggplot(aes(x = year, y = {{stat}}, color = anon_player_id)) +
    geom_point() +
    geom_line() +
    theme_bw()
}

####

## App

ui <- fluidPage(
  navbarPage(
    "Macalester Women's Volleyball Statistical Analysis",
    tabPanel("Intro",
             h2("Welcome to our COMP/STAT 212 final project: Macalester Women's Volleyball Statistical Analysis."),
             HTML("The Macalester Women's Volleyball team has student employees at their matches collecting game statistics. However, much of the time, these game statistics sit around in a database or are used for a few basic calculations by the coaching staff to evaluate performance. <b>This project aims to use these statistics to create insights into the team's performance and development over the past 4 seasons (2021-2024).</b> This is important for a team because it <b>can help the coaches and players understand their strengths and weaknesses</b> and help them improve their performance by <b>alerting them what aspects of play that could be paid more attention to in practice</b>. While sports analytics is a well-established field in professional and D1 sports, it is less common in the context of small, liberal arts college sports teams, especially women's sports teams. This project aims to bridge that gap by providing a detailed analysis of Macalester Women's Volleyball team performance that aims to answer data questions from the coaching staff themselves. <br>"),
             HTML("<br>We aim to investigate two main questions influenced by our collaboration with the Macalester Volleyball’s coaching staff, Mary Johnston and Keelin Severtson. First, we investigate a logistic regression model’s ability to predict the outcome (win/loss) of games based on selected game statistics. Second, we focus on the volleyball seniors (graduating class of 2025) and examine trends over their varsity careers at Macalester (2021-2024). <br>"),
             HTML("<br>The <b>“Model”</b> tab in our Shiny app allows for an interactive exploration of our model. The model is trained on 2021-2024 team data from the <a href='https://athletics.macalester.edu/sports/2008/1/29/VB_0129081140.aspx'>Macalester Athletics</a> website. The user can select which variables (<b>calculated per-set</b>, ex. game statistics divided by the number of sets played) to include in the model. The variables that the Macalester coaching staff were interested in are pre-selected with the ability to select and deselect variables. <br>"), 
             HTML("<ul><li>A <b>mosaic plot</b> allows for a visualization of the accuracy of the model. The size of the boxes in the upper left and lower right corners compared to the upper right and lower left shows how often the model correctly predicted a win as a win or a loss as a loss when run on the 2021-2024 data.</li><li> The <b>percent accuracy</b> is also printed below the plot, along with the <b>interpretations of the model coefficients</b> and whether or not they are statistically significant (p-value < 0.05).</li><li> Finally, below the model inputs is a tool that, when given per-set values for the chosen model statistics, will <b>predict the outcome of a game</b> using the model. This could be useful for coaches to use in between sets of a match to estimate their win percentage.</ul><br>"),
             HTML('The <b>“Static”</b> portion of our model focuses on analyzing trends in the performance of <b>current seniors</b> over their time at Mac. It will also draw some insight from the <b>“Model”</b> tab to help better understand how <b>individual performance</b> has contributed to wins in the past and in the present. Because we aim to analyze trends over time, our analysis is limited to data from the  <a href=https://athletics.macalester.edu/sports/2008/1/29/VB_0129081140.aspx>Macalester Athletics</a>  website for now, but there is a possibility that this project can be expanded on once more years of in-depth data are collected. </b>'),
             HTML('<h4>This project was created by Kynan Desouza-Chen ‘26 and <a href="https://katelynmcclure.github.io/">Katelyn McClure ‘26</a> in the fall of 2024.</h4>')
             ),
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
               HTML("<br>Our first instinct when we got the data was to look at how much seniors have played over their time at Mac. Getting more playing time would definitely be an indication that you are improving after all. We were hoping to notice an upward trend of some sort for every player. Now’s probably a good time to explain that each player is their own line in one of these graphs, but we were asked not to reveal any of their identities in these graphs. Each player has been given a random number from 1 to 7 to keep them anonymous. This will be the case for every graph going forward.<br>"),
               plotOutput("static_sp"),
               HTML("<br>Sure enough, all but one player had a generally upward trend in their playing time. There were some fluctuations, partly due to the fact that the team plays a different number of sets every year since matches don’t necessarily have a fixed number of sets (this happens because some matches have tiebreaker games). Also, as with any varsity sport, some of the fluctuations in playing time were due to players being injured for parts of the year. A few sets can even get tacked on if the team does well enough in a tournament to get to the championship round of that tournament. The point is that despite these possible confounding factors, we still see a general upward trend in most of the seniors’ playing time, indicating that they really are improving over their time at Mac.<br>"),
               HTML("<br>Obviously, the natural follow-up to learning about playing time is to learn about how well someone played during that time. The main hurdle here is deciding what stats to pick to find meaningful changes in a player's performance. We played around with the model for a bit, and we found 4 stats that do a pretty good job modeling wins without sacrificing accuracy: hitting percentage, service aces, total blocks, and receiving errors. However, this is where we ran into our next problem. Take a look at this plot of reception percentage (which is just a different way of accounting for receiving errors).<br>"),
               plotOutput("static_reception_no_sample_size"),
               HTML("<br>So, you might ask what’s going on with the big crater in the middle of the graph. Well in the process of making these graphs, we realized another issue with examining raw volleyball statistics: positions. Volleyball is a game with relatively set positions. Different positions accrue different stats, to the point where only 1 or 2 players a game consistently get reception opportunities, while others on the team just have different roles. So our graph of reception percentage looks like that because the player with a crater probably only had one reception attempt all year in 2023 (which they were unfortunately not successful on). To remove this issue going forward, we ensured that every player had 50 defensive attempts if they were going to be on a graph for a defensive stat, and 50 offensive attempts if they were going to be on an offensive graph. By doing this we can see what’s really happening.<br>"),
               plotOutput("static_reception"),
               HTML("<br>This paints a much more clear picture of what’s going on. For reception percentage at least, it looks like the team did some major work in year one with the main two people getting receptions, and they improved the reception percentage drastically the next year. You can’t really fault them for plateauing at above 0.9 either (trust me, I looked it up and that’s pretty good). Notice here that the size of the dots represents how many plays like this the player was involved in, so we can get a sense of how often they need to use this skill as well. Now, here’s a look at the 3 other big stats we identified from the model.<br>"),
               plotOutput("static_hitting"),
               plotOutput("static_sa"),
               plotOutput("static_blk"),
               HTML("<br>To me, the graph of service aces looks like the most convincing sign of improvement. The most obvious jump is the amount of service aces player 2 got in 2022 compared to 2023, but most of the players were better in their final year than in their first year. It looks like there’s only one player that consistently blocks in this year’s senior class, but they also seem to have made their biggest improvements going into 2023. While these 2 jumps in efficiency look promising, the graph of hitting percentage tells another story. It’s hard to say why hitting percentages look unchanged for most of the players. We can’t even begin to speculate, since this project is our first experience with volleyball statistics. If this doesn’t look good to the coaches, then I might suggest this as an area to focus on, however, once again, my recommendation is based purely on speculation and should be taken with more than just a grain of salt.<br><br>")
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
  
  output$static_reception_no_sample_size <- renderPlot({
    plot_no_attempts(`Rec%`) +
      labs(
        title = "Senior Reception Percentage Over Time",
        x = "Year",
        y = "Reception %",
        color = "Player ID")
  })
  
  output$static_hitting <- renderPlot({
    plot_with_attempts(PCT, TRUE) +
    labs(
      title = "Senior Hitting Percentage Over Time",
      x = "Year",
      y = "Hitting %",
      color = "Player ID",
      size = "Offensive Attempts")
  })
  
  output$static_reception <- renderPlot({
    plot_with_attempts(`Rec%`, FALSE) +
      labs(
        title = "Senior Reception Percentage Over Time",
        x = "Year",
        y = "Reception %",
        color = "Player ID",
        size = "Defensive Attempts")
  })
  
  output$static_sa <- renderPlot({
    plot_with_attempts(`SA/S`, TRUE) +
      labs(
        title = "Senior Service Aces per Set Over Time",
        x = "Year",
        y = "Service Aces per Set",
        color = "Player ID",
        size = "Offensive Attempts")
  })
  
  output$static_blk <- renderPlot({
    plot_with_attempts(`BLK/S`, FALSE) +
      labs(
        title = "Senior Blocks per Set Over Time",
        x = "Year",
        y = "Blocks per Set",
        color = "Player ID",
        size = "Defensive Attempts")
  })
  
  output$static_sp <- renderPlot({
    plot_no_attempts(SP) +
      labs(
        title = "Senior Sets Played Over Time",
        x = "Year",
        y = "Sets Played",
        color = "Player ID",
        size = "Defensive Attempts")
  })
}

# Run app
shinyApp(ui = ui, server = server)

