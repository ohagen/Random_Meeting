library(shiny)

# Default example data shown when the app starts. Users can replace this entire
# list in the text box; the grouping logic itself is not specific to C3S.
c3s_professors <- c(
  "Christoph Burchard", "Tiago de Paula Peixoto", "Ulrich Meyer",
  "Paula Helm", "Dan Verständig", "Juliane Engel", "Natalie Welfens",
  "Nico Wunderling", "Azadeh Akbari", "Oskar Hagen", "Manuel Linsenmeier",
  "Sabine Müller-Mall", "Lisa Oswald", "Tobias Rüttenauer"
)

parse_elements <- function(x) {
  # Accept commas, semicolons, and new lines, then remove blanks and duplicates.
  values <- trimws(unlist(strsplit(x, "[,\n;]+")))
  unique(values[nzchar(values)])
}

pair_keys <- function(group) {
  # Store a meeting between two people as one order-independent text key.
  if (length(group) < 2) return(character())
  apply(combn(sort(group), 2), 2, paste, collapse = "\r")
}

make_round <- function(people, group_size, pair_history, attempts = 600L) {
  # Balance the groups so their sizes differ by no more than one person.
  group_count <- ceiling(length(people) / group_size)
  sizes <- rep(length(people) %/% group_count, group_count)
  if (length(people) %% group_count) {
    sizes[seq_len(length(people) %% group_count)] <- sizes[seq_len(length(people) %% group_count)] + 1L
  }

  # Try several random arrangements and retain the one with the fewest pairings
  # found in the supplied meeting history. A score of zero cannot be improved.
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
  # Newly created rounds become temporary history for later rounds in the same
  # draw, preventing repeated pairs both across meetings and within this result.
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
  # Convert the nested round/group structure into a tidy export table.
  if (!length(rounds)) return(data.frame())
  do.call(rbind, lapply(seq_along(rounds), function(r) {
    do.call(rbind, lapply(seq_along(rounds[[r]]$groups), function(g) {
      data.frame(Round = r, Group = g, Member = rounds[[r]]$groups[[g]], check.names = FALSE)
    }))
  }))
}

# If you are reading this, you have gained my respect! :)
# The interface keeps configuration on the left and generated results on the right.
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
      checkboxInput("fixed_seed", "Use a fixed seed", FALSE),
      conditionalPanel(
        "input.fixed_seed",
        numericInput("seed", "Seed", value = 12345, min = 1, max = 2147483647, step = 1)
      ),
      helpText("Without a fixed seed, the app creates and records one so the draw can still be reproduced."),
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
  # These reactive values snapshot both the result and the exact settings that
  # produced it. This ensures reports remain accurate if controls later change.
  current <- reactiveVal(NULL)
  current_seed <- reactiveVal(NULL)
  current_seed_mode <- reactiveVal(NULL)
  current_params <- reactiveVal(NULL)
  history <- reactiveVal(integer())
  saved_signatures <- reactiveVal(character())

  observeEvent(input$generate, {
    people <- parse_elements(input$elements)
    if (length(people) < 2 || input$group_size < 2 || input$group_size > length(people)) {
      current(NULL)
      return()
    }
    # Even an automatic draw receives a recorded seed, making every result
    # reproducible when the same inputs, options, and history are used.
    seed <- if (isTRUE(input$fixed_seed)) as.integer(input$seed) else sample.int(.Machine$integer.max, 1L)
    if (is.na(seed) || seed < 1) {
      current(NULL)
      showNotification("Enter a seed from 1 to 2,147,483,647.", type = "error")
      return()
    }
    set.seed(seed)
    current_seed(seed)
    current_seed_mode(if (isTRUE(input$fixed_seed)) "Fixed seed supplied by user" else "Seed generated by the app")
    current_params(list(
      elements = people,
      group_size = as.integer(input$group_size),
      round_count = as.integer(input$round_count),
      avoid_repeats = isTRUE(input$avoid_repeats),
      history_distinct_pairs = length(history()),
      history_pair_meetings = sum(history())
    ))
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
    tagList(
      div(class = "alert alert-info",
          strong(current_seed_mode()),
          sprintf(" — seed %s. Use this seed with the same inputs and meeting history to reproduce the draw.", current_seed())),
      lapply(seq_along(rounds), function(r) {
      div(class = "round-card",
        h3(sprintf("Round %d", r)),
        if (isTRUE(input$avoid_repeats)) p(class = "muted", sprintf("Repeated-pair score: %d", rounds[[r]]$repeated_pairs)),
        lapply(seq_along(rounds[[r]]$groups), function(g) {
          div(class = "group-pill", strong(sprintf("Group %d", g)), tags$ul(lapply(rounds[[r]]$groups[[g]], tags$li)))
        })
      )
      })
    )
  })

  output$hasResults <- reactive(!is.null(current()))
  outputOptions(output, "hasResults", suspendWhenHidden = FALSE)

  observeEvent(input$save_history, {
    # Only accepted results are committed to history. A signature prevents the
    # same displayed result from accidentally being counted more than once.
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
    # Repeat draw-level parameters on each row so the CSV remains self-contained.
    filename = function() paste0("meeting-groups-", Sys.Date(), ".csv"),
    content = function(file) {
      result <- rounds_to_df(current())
      params <- current_params()
      result$Elements <- paste(params$elements, collapse = " | ")
      result$Preferred_group_size <- params$group_size
      result$Requested_rounds <- params$round_count
      result$Minimize_repeat_meetings <- params$avoid_repeats
      result$History_distinct_pairs <- params$history_distinct_pairs
      result$History_pair_meetings <- params$history_pair_meetings
      result$Seed <- current_seed()
      result$Seed_source <- current_seed_mode()
      write.csv(result, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  output$download_html <- downloadHandler(
    # Build a dependency-light standalone HTML report with htmltools/Shiny tags.
    filename = function() paste0("meeting-groups-", Sys.Date(), ".html"),
    content = function(file) {
      req(current())
      title <- htmltools::htmlEscape(input$report_title)
      params <- current_params()
      cards <- lapply(seq_along(current()), function(r) {
        groups <- lapply(seq_along(current()[[r]]$groups), function(g) {
          tags$section(tags$h3(sprintf("Group %d", g)), tags$ul(lapply(current()[[r]]$groups[[g]], tags$li)))
        })
        tags$article(tags$h2(sprintf("Round %d", r)), groups)
      })
      seed_note <- sprintf("%s: %s. Reproduce this draw using the same seed, inputs, options, and meeting history.",
                           current_seed_mode(), current_seed())
      parameter_summary <- tags$section(
        tags$h2("Drawing parameters"),
        tags$dl(
          tags$dt("Elements"), tags$dd(paste(params$elements, collapse = ", ")),
          tags$dt("Preferred group size"), tags$dd(params$group_size),
          tags$dt("Requested groupings (rounds)"), tags$dd(params$round_count),
          tags$dt("Minimize people meeting again"), tags$dd(if (params$avoid_repeats) "Yes" else "No"),
          tags$dt("Meeting history used"),
          tags$dd(sprintf("%d distinct pairs across %d pair meetings",
                          params$history_distinct_pairs, params$history_pair_meetings)),
          tags$dt("Seed"), tags$dd(current_seed()),
          tags$dt("Seed source"), tags$dd(current_seed_mode())
        )
      )
      page <- tags$html(tags$head(tags$meta(charset = "utf-8"), tags$title(title), tags$style("body{font-family:system-ui;max-width:900px;margin:40px auto;color:#19332f} article{border-top:2px solid #176b60;margin-top:28px} article section{display:inline-block;vertical-align:top;width:250px;margin-right:24px} dt{font-weight:700;margin-top:8px} dd{margin-left:0}")),
                        tags$body(tags$h1(title), tags$p(sprintf("Generated %s · %d rounds", Sys.Date(), length(current()))),
                                  tags$p(tags$strong(seed_note)), parameter_summary, cards))
      htmltools::save_html(page, file)
    }
  )
}

shinyApp(ui, server)
