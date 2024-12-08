library(shiny)
library(DBI)
library(RSQLite)
library(digest)
library(dplyr)
library(DT)
library(odbc)

ui <- fluidPage(
  div(style = "margin-bottom: 10px; text-align: center; font-size: small; color: gray;",
      "© 2024 Stefano Parravano. All rights reserved. For inquiries, contact: stefano.parravano@example.com"
  ),
  titlePanel("Pickleball App"),
  
  tabsetPanel(
    id = "tabs",
    
    # Match Generation Tab
    tabPanel(
      "Match Generation",
      sidebarLayout(
        sidebarPanel(
          h4("Enter Player Names (one per line)"),
          textAreaInput("player_names", "Player Names", rows = 8, placeholder = "Enter one name per line"),
          br(),
          h4("Enter Court Names (one per line)"),
          textAreaInput("court_names", "Court Names", rows = 4, placeholder = "Enter one court name per line"),
          br(),
          div(style = "text-align: center;",
              actionButton("generate", "Generate Matchups", class = "btn-primary", style = "width: 200px;")
          ),
          br(),
          uiOutput("warning")
        ),
        mainPanel(
          div(style = "margin-bottom: 20px;",
              h3("Matchups", style = "text-align: center;"),
              uiOutput("matchups")
          ),
          div(style = "text-align: center; margin-bottom: 20px;",
              actionButton("submit_scores", "Submit Scores", class = "btn-primary", style = "width: 200px;")
          ),
          uiOutput("leftover_header"),
          uiOutput("leftover_table"),
          br()
        )
      )
    ),
    
    # Player Analytics Tab
    tabPanel(
      "Player Analytics",
      fluidRow(
        column(
          width = 12,
          h3("Player Statistics", style = "text-align: center; margin-top: 30px;"),
          div(
            style = "border: 2px solid #ddd; padding: 10px; border-radius: 10px; background-color: #f9f9f9;",
            DT::dataTableOutput("player_ratings")
          ),
          div(
            style = "margin-top: 10px; text-align: right;",
            uiOutput("analytics_summary")
          )
        )
      )
    ),
    
    # Score Differential Analysis Tab
    tabPanel(
      "Score Differential Analysis",
      fluidRow(
        column(
          width = 12,
          h3("Distribution of Score Differentials Across All Matches", style = "text-align: center; margin-top: 20px;"),
          div(
            style = "text-align: center; margin-bottom: 20px;",
            plotOutput("score_diff_histogram", height = "400px", width = "80%")
          ),
          div(
            style = "text-align: center; margin-top: -10px;",
            sliderInput(
              "hist_bins",
              "Adjust Number of Bins",
              min = 5,
              max = 50,
              value = 20,
              step = 1,
              width = "80%" # Matches histogram width for alignment
            )
          ),
          div(
            style = "text-align: center; margin-top: 10px;",
            uiOutput("hist_note")
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  rv <- reactiveValues(matchups = NULL, leftover_players = NULL, active_tab = "Match Generation", aggregated_results = NULL)
  
  # Function to compute player summaries
  compute_player_summary <- function(conn) {
    player_results <- dbReadTable(conn, "player_results")
    aggregated_results <- player_results %>%
      group_by(Player) %>%
      summarise(
        Matches_Played = n(),
        Matches_Won = sum(Result == "Win"),
        Win_Percentage = round((Matches_Won / Matches_Played) * 100, 2),
        Points_For = sum(Points_Scored),
        Points_Against = sum(Points_Against),
        Point_Differential = Points_For - Points_Against,
        .groups = "drop"
      ) %>%
      arrange(desc(Win_Percentage), desc(Point_Differential), desc(Matches_Won)) %>%
      mutate(Rank = row_number())
    return(aggregated_results)
  }
  
  observeEvent(input$generate, {
    player_list <- unlist(strsplit(input$player_names, "\n"))
    player_list <- trimws(player_list)
    player_list <- tolower(player_list)
    player_list <- player_list[player_list != ""]
    
    court_list <- unlist(strsplit(input$court_names, "\n"))
    court_list <- trimws(court_list)
    court_list <- court_list[court_list != ""]
    
    if (length(player_list) == 0) {
      output$warning <- renderUI({
        HTML("<span style='color: red;'>Please enter at least one player.</span>")
      })
      return()
    }
    
    if (length(court_list) == 0) {
      output$warning <- renderUI({
        HTML("<span style='color: red;'>Please enter at least one court.</span>")
      })
      return()
    }
    
    output$warning <- renderUI({})
    shuffled_players <- sample(player_list)
    matchups <- data.frame(
      Court = character(),
      Team1_Player1 = character(),
      Team1_Player2 = character(),
      Team2_Player1 = character(),
      Team2_Player2 = character(),
      Team1_Score = integer(),
      Team2_Score = integer(),
      stringsAsFactors = FALSE
    )
    
    leftover_players <- shuffled_players
    num_courts <- min(length(court_list), floor(length(shuffled_players) / 4))
    
    for (i in seq_len(num_courts)) {
      if (length(leftover_players) >= 4) {
        court <- court_list[i]
        team1_player1 <- leftover_players[1]
        team1_player2 <- leftover_players[2]
        team2_player1 <- leftover_players[3]
        team2_player2 <- leftover_players[4]
        
        matchups <- rbind(
          matchups,
          data.frame(
            Court = court,
            Team1_Player1 = team1_player1,
            Team1_Player2 = team1_player2,
            Team2_Player1 = team2_player1,
            Team2_Player2 = team2_player2,
            Team1_Score = NA,
            Team2_Score = NA
          )
        )
        
        leftover_players <- leftover_players[-(1:4)]
      }
    }
    
    rv$matchups <- matchups
    rv$leftover_players <- leftover_players
    
    output$matchups <- renderUI({
      if (nrow(rv$matchups) > 0) {
        tagList(
          div(style = "border: 1px solid #ddd; padding: 10px; border-radius: 5px; margin-bottom: 20px;",
              fluidRow(
                column(2, strong("Court")),
                column(2, strong("Team One")),
                column(2, strong("Team One Points")),
                column(2, strong("Team Two")),
                column(2, strong("Team Two Points"))
              ),
              lapply(seq_len(nrow(rv$matchups)), function(i) {
                row_style <- if (i %% 2 == 0) "background-color: #e0e0e0;" else "background-color: #cfcfcf;"
                fluidRow(
                  style = paste("padding: 5px;", row_style),
                  column(2, rv$matchups[i, "Court"]),
                  column(2, paste(rv$matchups[i, "Team1_Player1"], "/", rv$matchups[i, "Team1_Player2"])),
                  column(2, numericInput(
                    inputId = paste0("team1_score_", i),
                    label = NULL,
                    value = 0,
                    min = 0,
                    step = 1
                  )),
                  column(2, paste(rv$matchups[i, "Team2_Player1"], "/", rv$matchups[i, "Team2_Player2"])),
                  column(2, numericInput(
                    inputId = paste0("team2_score_", i),
                    label = NULL,
                    value = 0,
                    min = 0,
                    step = 1
                  ))
                )
              })
          )
        )
      } else {
        HTML("No matchups available.")
      }
    })
    
    output$leftover_header <- renderUI({
      if (length(rv$leftover_players) > 0) {
        h4("Players Skipping Next Round", style = "margin-top: 30px;")
      }
    })
    
    output$leftover_table <- renderUI({
      if (length(rv$leftover_players) > 0) {
        div(style = "border: 1px solid #333; padding: 10px; border-radius: 5px;",
            tableOutput("leftover_players")
        )
      }
    })
    
    output$leftover_players <- renderTable({
      data.frame(rv$leftover_players)
    }, colnames = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$submit_scores, {
    req(rv$matchups)
    
    for (i in seq_len(nrow(rv$matchups))) {
      rv$matchups[i, "Team1_Score"] <- input[[paste0("team1_score_", i)]]
      rv$matchups[i, "Team2_Score"] <- input[[paste0("team2_score_", i)]]
    }
    
    rv$matchups$SysTime <- as.character(Sys.time())
    rv$matchups$MatchID <- sapply(1:nrow(rv$matchups), function(i) {
      row <- rv$matchups[i, ]
      digest::digest(paste(row, collapse = ""), algo = "sha256")
    })
    
    rv$matchups[, c("Team1_Player1", "Team1_Player2", "Team2_Player1", "Team2_Player2")] <- 
      lapply(rv$matchups[, c("Team1_Player1", "Team1_Player2", "Team2_Player1", "Team2_Player2")], function(x) {
        trimws(tolower(x))
      })
    
    player_results <- do.call(rbind, lapply(1:nrow(rv$matchups), function(i) {
      row <- rv$matchups[i, ]
      players <- c(row$Team1_Player1, row$Team1_Player2, row$Team2_Player1, row$Team2_Player2)
      scores <- c(row$Team1_Score, row$Team1_Score, row$Team2_Score, row$Team2_Score)
      against_scores <- c(row$Team2_Score, row$Team2_Score, row$Team1_Score, row$Team1_Score)
      results <- c(rep(ifelse(row$Team1_Score > row$Team2_Score, "Win", "Loss"), 2),
                   rep(ifelse(row$Team2_Score > row$Team1_Score, "Win", "Loss"), 2))
      data.frame(
        Player = players,
        MatchID = row$MatchID,
        Result = results,
        Points_Scored = scores,
        Points_Against = against_scores,
        Point_Difference = scores - against_scores,
        stringsAsFactors = FALSE
      )
    }))
    
    conn <- dbConnect(odbc(),
                      Driver = "SQL Server",  # or "ODBC Driver 17 for SQL Server"
                      Server = "DESKTOP-K6N2M3K\\SQLEXPRESS",  # SQL Server instance
                      Database = "pickleball_app",  # Replace with your database name
                      IntegratedSecurity = "True")  # Use Windows Authentication
    dbWriteTable(conn, "matchups", rv$matchups, append = TRUE, row.names = FALSE)
    dbWriteTable(conn, "player_results", player_results, append = TRUE, row.names = FALSE)
    
    # Compute and write player summary
    player_summary <- compute_player_summary(conn)
    dbWriteTable(conn, "player_summary", player_summary, overwrite = TRUE, row.names = FALSE)
    
    dbDisconnect(conn)
    
    showNotification("Scores successfully submitted and player summary updated!", type = "message")
  })
  
  # Automatically update Player Analytics when tab is opened
  observeEvent(input$tabs, {
    if (input$tabs == "Player Analytics") {
      conn <- dbConnect(odbc(),
                        Driver = "SQL Server",  # or "ODBC Driver 17 for SQL Server"
                        Server = "DESKTOP-K6N2M3K\\SQLEXPRESS",  # SQL Server instance
                        Database = "pickleball_app",  # Replace with your database name
                        IntegratedSecurity = "True")  # Use Windows Authentication
      aggregated_results <- dbReadTable(conn, "player_summary")
      matches <- dbReadTable(conn, "matchups")
      dbDisconnect(conn)
      
      output$player_ratings <- DT::renderDataTable({
        DT::datatable(
          aggregated_results,
          options = list(scrollX = TRUE, pageLength = 10),
          rownames = FALSE,
          filter = "top"
        )
      })
      
      output$analytics_summary <- renderUI({
        HTML(paste0("<strong>Total Matches Analyzed:</strong> ", length(unique(matches$MatchID)),
                    " <strong>Total Players Analyzed:</strong> ", nrow(aggregated_results)))
      })
    }
  })
  
  # Automatically update Histogram when tab is opened
  observeEvent(input$tabs, {
    if (input$tabs == "Score Differential Analysis") {
      conn <- dbConnect(odbc(),
                        Driver = "SQL Server",  # or "ODBC Driver 17 for SQL Server"
                        Server = "DESKTOP-K6N2M3K\\SQLEXPRESS",  # SQL Server instance
                        Database = "pickleball_app",  # Replace with your database name
                        IntegratedSecurity = "True")  # Use Windows Authentication
      matches <- dbReadTable(conn, "matchups")
      dbDisconnect(conn)
      output$score_diff_histogram <- renderPlot({
        score_differentials <- abs(matches$Team1_Score - matches$Team2_Score)
        hist(score_differentials,
             breaks = input$hist_bins,
             main = "",
             xlab = "Score Differential",
             col = "skyblue",
             border = "white",
             prob = TRUE)
        abline(v = mean(score_differentials, na.rm = TRUE), col = "red", lwd = 2, lty = 2)
        title(main = "Distribution of Score Differentials", cex.main = 1.5, font.main = 2)
      })
      
      output$hist_note <- renderUI({
        score_differentials <- abs(matches$Team1_Score - matches$Team2_Score)
        n_matches <- length(score_differentials)
        mean_diff <- round(mean(score_differentials, na.rm = TRUE), 2)
        HTML(paste0("Note: The histogram includes ", n_matches, " matches. ",
                    "The mean score differential is ", mean_diff, "."))
      })
    }
  })
}

shinyApp(ui, server)
