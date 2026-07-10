library(shiny)

c3s_professors <- c(
  "Christoph Burchard", "Tiago de Paula Peixoto", "Ulrich Meyer",
  "Paula Helm", "Dan Verständig", "Juliane Engel", "Natalie Welfens",
  "Nico Wunderling", "Azadeh Akbari", "Oskar Hagen", "Manuel Linsenmeier",
  "Sabine Müller-Mall", "Lisa Oswald", "Tobias Rüttenauer"
)

parse_elements <- function(x) {
  values <- trimws(unlist(strsplit(x, "[,\n;]+")))
  unique(values[nzchar(values)])
}

pair_keys <- function(group) {
  if (length(group) < 2) return(character())
  apply(combn(sort(group), 2), 2, paste, collapse = "\r")
}

make_round <- function(people, group_size, pair_history, attempts = 600L) {
  group_count <- ceiling(length(people) / group_size)
  sizes <- rep(length(people) %/% group_count, group_count)
  if (length(people) %% group_count) {
    sizes[seq_len(length(people) %% group_count)] <- sizes[seq_len(length(people) %% group_count)] + 1L
  }

  best <- NULL
  best_score <- Inf
  for (i in seq_len(attempts)) {
    shuffled <- sample(people)
    groups <- split(shuffled, rep(seq_len(group_count), sizes))
    keys <- unlist(lapply(groups, pair_keys), use.names = FALSE)
    score <- sum(pair_history[keys], na.rm = TRUE)
    if (score < best_score) {
      best <- groups
      best_score <- score
      if (score == 0) break
    }
  }
  list(groups = unname(best), repeated_pairs = best_score)
}

generate_rounds <- function(people, group_size, rounds, history, avoid_repeats) {
  pair_history <- if (avoid_repeats) history else integer()
  output <- vector("list", rounds)
  for (r in seq_len(rounds)) {
    result <- make_round(people, group_size, pair_history)
    output[[r]] <- result
    if (avoid_repeats) {
      keys <- unlist(lapply(result$groups, pair_keys), use.names = FALSE)
      for (key in keys) pair_history[key] <- ifelse(is.na(pair_history[key]), 1L, pair_history[key] + 1L)
    }
  }
  output
}

rounds_to_df <- function(rounds) {
  if (!length(rounds)) return(data.frame())
  do.call(rbind, lapply(seq_along(rounds), function(r) {
    do.call(rbind, lapply(seq_along(rounds[[r]]$groups), function(g) {
      data.frame(Round = r, Group = g, Member = rounds[[r]]$groups[[g]], check.names = FALSE)
    }))
  }))
}

ui <- fluidPage(
  tags$head(tags$style(HTML("\n+    body { background: #f6f7f9; } .container-fluid { max-width: 1180px; }\n+    .well { background: white; border: 0; box-shadow: 0 2px 12px #00000012; }\n+    .round-card { background: white; padding: 18px 22px; margin-bottom: 16px; border-radius: 8px; box-shadow: 0 2px 12px #00000012; }\n+    .group-pill { display: inline-block; vertical-align: top; width: 280px; min-height: 120px; margin: 8px; padding: 12px 16px; background: #eef4f3; border-left: 4px solid #176b60; border-radius: 5px; }\n+    .muted { color: #65706f; }\n+  "))),
  titlePanel("Random meeting groups"),
  fluidRow(
    column(4, wellPanel(
      textAreaInput("elements", "People or elements", value = paste(c3s_professors, collapse = ", "), rows = 10,
                    placeholder = "Ada, Grace, Linus, Margaret"),
      helpText("Separate entries with commas, semicolons, or new lines. Duplicate entries are removed."),
      numericInput("group_size", "Preferred group size", value = 3, min = 2, step = 1),
      numericInput("round_count", "Number of groupings (rounds)", value = 3, min = 1, max = 50, step = 1),
      checkboxInput("avoid_repeats", "Minimize people meeting again", TRUE),
      actionButton("generate", "Generate groups", class = "btn-primary btn-lg"),
      tags$hr(),
      actionButton("save_history", "Add result to meeting history"),
      actionButton("clear_history", "Clear history"),
      br(), br(),
      textOutput("history_status")
    )),
    column(8,
      uiOutput("validation"),
      uiOutput("results"),
      conditionalPanel("output.hasResults", wellPanel(
        h4("Report"),
        textInput("report_title", "Report title", "Meeting group report"),
        downloadButton("download_html", "Download HTML report"),
        downloadButton("download_csv", "Download CSV")
      ))
    )
  )
)

server <- function(input, output, session) {
  current <- reactiveVal(NULL)
  history <- reactiveVal(integer())
  saved_signatures <- reactiveVal(character())

  observeEvent(input$generate, {
    people <- parse_elements(input$elements)
    if (length(people) < 2 || input$group_size < 2 || input$group_size > length(people)) {
      current(NULL)
      return()
    }
    current(generate_rounds(people, as.integer(input$group_size), as.integer(input$round_count),
                            history(), isTRUE(input$avoid_repeats)))
  })

  output$validation <- renderUI({
    people <- parse_elements(input$elements)
    if (length(people) < 2) div(class = "alert alert-warning", "Enter at least two unique elements.")
    else if (input$group_size > length(people)) div(class = "alert alert-warning", "Group size cannot exceed the number of elements.")
    else div(class = "muted", sprintf("%d unique elements · groups will differ in size by at most one", length(people)))
  })

  output$results <- renderUI({
    rounds <- current()
    if (is.null(rounds)) return(div(class = "round-card muted", "Configure the options and generate groups."))
    tagList(lapply(seq_along(rounds), function(r) {
      div(class = "round-card",
        h3(sprintf("Round %d", r)),
        if (isTRUE(input$avoid_repeats)) p(class = "muted", sprintf("Repeated-pair score: %d", rounds[[r]]$repeated_pairs)),
        lapply(seq_along(rounds[[r]]$groups), function(g) {
          div(class = "group-pill", strong(sprintf("Group %d", g)), tags$ul(lapply(rounds[[r]]$groups[[g]], tags$li)))
        })
      )
    }))
  })

  output$hasResults <- reactive(!is.null(current()))
  outputOptions(output, "hasResults", suspendWhenHidden = FALSE)

  observeEvent(input$save_history, {
    req(current())
    signature <- paste(capture.output(dput(rounds_to_df(current()))), collapse = "")
    if (signature %in% saved_signatures()) {
      showNotification("These rounds are already in the history.", type = "warning")
      return()
    }
    keys <- unlist(lapply(current(), function(round) unlist(lapply(round$groups, pair_keys))), use.names = FALSE)
    counts <- history()
    for (key in keys) counts[key] <- ifelse(is.na(counts[key]), 1L, counts[key] + 1L)
    history(counts)
    saved_signatures(c(saved_signatures(), signature))
    showNotification("Meeting history updated.", type = "message")
  })

  observeEvent(input$clear_history, {
    history(integer())
    saved_signatures(character())
    showNotification("Meeting history cleared.")
  })

  output$history_status <- renderText({
    counts <- history()
    sprintf("History: %d distinct pairs across %d meetings", length(counts), sum(counts))
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("meeting-groups-", Sys.Date(), ".csv"),
    content = function(file) write.csv(rounds_to_df(current()), file, row.names = FALSE, fileEncoding = "UTF-8")
  )

  output$download_html <- downloadHandler(
    filename = function() paste0("meeting-groups-", Sys.Date(), ".html"),
    content = function(file) {
      req(current())
      title <- htmltools::htmlEscape(input$report_title)
      cards <- lapply(seq_along(current()), function(r) {
        groups <- lapply(seq_along(current()[[r]]$groups), function(g) {
          tags$section(tags$h3(sprintf("Group %d", g)), tags$ul(lapply(current()[[r]]$groups[[g]], tags$li)))
        })
        tags$article(tags$h2(sprintf("Round %d", r)), groups)
      })
      page <- tags$html(tags$head(tags$meta(charset = "utf-8"), tags$title(title), tags$style("body{font-family:system-ui;max-width:900px;margin:40px auto;color:#19332f} article{border-top:2px solid #176b60;margin-top:28px} section{display:inline-block;vertical-align:top;width:250px;margin-right:24px}")),
                        tags$body(tags$h1(title), tags$p(sprintf("Generated %s · %d rounds", Sys.Date(), length(current()))), cards))
      htmltools::save_html(page, file)
    }
  )
}

shinyApp(ui, server)
