library(shiny)
library(bslib)
library(DBI)
library(dplyr)
library(ggplot2)

source("R/db_connect.R")

pool <- db_connect()
onStop(function() pool::poolClose(pool))

dbExecute(
  pool,
  "
  CREATE TABLE IF NOT EXISTS chores_log (
    id         bigserial PRIMARY KEY,
    work_date  date        NOT NULL,
    person     text        NOT NULL CHECK (person IN ('Debora', 'Ivan')),
    minutes    integer     NOT NULL CHECK (minutes > 0),
    note       text,
    created_at timestamptz NOT NULL DEFAULT now()
  )
"
)

# Migrazione: se esiste la vecchia colonna `activity`, la fonde nella nota e la elimina
dbExecute(
  pool,
  "
  DO $$
  BEGIN
    IF EXISTS (
      SELECT 1 FROM information_schema.columns
      WHERE table_name = 'chores_log' AND column_name = 'activity'
    ) THEN
      UPDATE chores_log
         SET note = activity || COALESCE(' - ' || note, '');
      ALTER TABLE chores_log DROP COLUMN activity;
    END IF;
  END $$;
"
)

PEOPLE <- c("Debora", "Ivan")
PEOPLE_COLORS <- c(Debora = "#d6336c", Ivan = "#1c7ed6")
MONTHS_IT <- c(
  "Gennaio",
  "Febbraio",
  "Marzo",
  "Aprile",
  "Maggio",
  "Giugno",
  "Luglio",
  "Agosto",
  "Settembre",
  "Ottobre",
  "Novembre",
  "Dicembre"
)
WEEKDAYS_IT <- c("Lun", "Mar", "Mer", "Gio", "Ven", "Sab", "Dom")

today_it <- function() as.Date(format(Sys.time(), tz = "Europe/Rome"))

fmt_min <- function(m) {
  m <- as.integer(m)
  sprintf("%d:%02d", m %/% 60, m %% 60)
}

# "h:mm" -> minuti interi; NA se il formato non e' valido
parse_hmm <- function(x) {
  x <- trimws(x)
  if (is.null(x) || !grepl("^[0-9]{1,2}:[0-5][0-9]$", x)) {
    return(NA_integer_)
  }
  p <- as.integer(strsplit(x, ":", fixed = TRUE)[[1]])
  p[1] * 60L + p[2]
}

# Media mobile trailing su `k` giorni; i giorni senza attivita' contano 0
daily_moving_avg <- function(log_df, k = 5) {
  if (nrow(log_df) == 0) {
    return(tibble(
      work_date = as.Date(character()),
      person = character(),
      avg = numeric()
    ))
  }
  days <- seq(
    min(log_df$work_date),
    max(max(log_df$work_date), today_it()),
    by = "day"
  )
  tidyr::expand_grid(work_date = days, person = PEOPLE) |>
    left_join(
      log_df |> summarise(minutes = sum(minutes), .by = c(work_date, person)),
      by = c("work_date", "person")
    ) |>
    mutate(minutes = coalesce(minutes, 0L)) |>
    arrange(person, work_date) |>
    mutate(
      cs = cumsum(minutes),
      avg = (cs - lag(cs, k, default = 0)) / k,
      avg = if_else(row_number() < k, NA_real_, avg),
      .by = person
    ) |>
    filter(!is.na(avg))
}

ui <- page_sidebar(
  title = "Faccende di casa",
  theme = bs_theme(version = 5),
  fillable = FALSE,
  tags$head(tags$style(HTML(
    "
    .cal-grid { display: grid; grid-template-columns: repeat(7, 1fr); gap: 4px; }
    .cal-head { text-align: center; font-weight: 600; font-size: .85rem; color: #6c757d; }
    .cal-cell { min-height: 78px; border: 1px solid #dee2e6; border-radius: 6px;
                padding: 3px 5px; cursor: pointer; font-size: .8rem; }
    .cal-cell:hover { background: #f1f3f5; }
    .cal-empty { border: none; cursor: default; }
    .cal-empty:hover { background: none; }
    .cal-today { border-color: #212529; }
    .cal-selected { background: #fff3bf !important; }
    .cal-num { font-weight: 600; }
    .cal-badge { display: block; color: #fff; border-radius: 4px; padding: 0 4px;
                 margin-top: 2px; font-size: .72rem; }
  "
  ))),
  sidebar = sidebar(
    width = 340,
    h5(textOutput("sel_title", inline = TRUE)),
    selectInput("person", "Chi", choices = PEOPLE),
    textInput("time", "Tempo (h:mm)", value = "0:30", placeholder = "es. 1:15"),
    textAreaInput(
      "note",
      "Nota (facoltativa)",
      rows = 2,
      placeholder = "Cosa hai fatto..."
    ),
    actionButton("add", "Aggiungi", class = "btn-primary w-100"),
    hr(),
    h6("Attivita' del giorno"),
    uiOutput("day_entries")
  ),
  layout_columns(
    col_widths = c(12),
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        actionButton("prev_month", "<", class = "btn-sm btn-outline-secondary"),
        textOutput("month_title", inline = TRUE),
        actionButton("next_month", ">", class = "btn-sm btn-outline-secondary")
      ),
      uiOutput("calendar")
    ),
    card(
      full_screen = TRUE,
      card_header("Media mobile a 5 giorni del tempo lavorato (minuti/giorno)"),
      plotOutput("ma_plot", height = "350px")
    )
  )
)

server <- function(input, output, session) {
  refresh <- reactiveVal(0)
  sel_date <- reactiveVal(today_it())
  month_start <- reactiveVal(as.Date(format(today_it(), "%Y-%m-01")))

  log_df <- reactive({
    refresh()
    dbGetQuery(
      pool,
      "SELECT id, work_date, person, minutes, note FROM chores_log ORDER BY created_at"
    ) |>
      mutate(work_date = as.Date(work_date))
  })

  observeEvent(input$cal_day, sel_date(as.Date(input$cal_day)))
  observeEvent(input$prev_month, {
    month_start(seq(month_start(), by = "-1 month", length.out = 2)[2])
  })
  observeEvent(input$next_month, {
    month_start(seq(month_start(), by = "1 month", length.out = 2)[2])
  })

  output$sel_title <- renderText(format(sel_date(), "Giorno: %d/%m/%Y"))
  output$month_title <- renderText({
    m <- month_start()
    paste(MONTHS_IT[as.integer(format(m, "%m"))], format(m, "%Y"))
  })

  observeEvent(input$add, {
    minutes <- parse_hmm(input$time)
    if (is.na(minutes) || minutes < 1) {
      showNotification(
        "Inserisci il tempo nel formato h:mm (es. 1:15), almeno 0:01.",
        type = "warning"
      )
      return()
    }
    note <- trimws(input$note)
    dbExecute(
      pool,
      "INSERT INTO chores_log (work_date, person, minutes, note)
       VALUES ($1, $2, $3, $4)",
      params = list(
        as.character(sel_date()),
        input$person,
        minutes,
        if (nzchar(note)) note else NA_character_
      )
    )
    updateTextAreaInput(session, "note", value = "")
    refresh(refresh() + 1)
    showNotification("Attivita' aggiunta.", type = "message", duration = 2)
  })

  observeEvent(input$del_id, {
    dbExecute(
      pool,
      "DELETE FROM chores_log WHERE id = $1",
      params = list(as.numeric(input$del_id))
    )
    refresh(refresh() + 1)
  })

  output$day_entries <- renderUI({
    d <- log_df() |> filter(work_date == sel_date())
    if (nrow(d) == 0) {
      return(p(class = "text-muted", "Nessuna attivita'."))
    }
    tagList(lapply(seq_len(nrow(d)), function(i) {
      r <- d[i, ]
      div(
        class = "d-flex justify-content-between align-items-start mb-2",
        div(
          tags$span(
            class = "badge me-1",
            style = paste0("background:", PEOPLE_COLORS[[r$person]]),
            r$person
          ),
          strong(fmt_min(r$minutes)),
          if (!is.na(r$note)) div(class = "small text-muted", r$note)
        ),
        tags$button(
          type = "button",
          class = "btn btn-sm btn-outline-danger ms-2",
          title = "Elimina",
          onclick = sprintf(
            "Shiny.setInputValue('del_id', %s, {priority: 'event'})",
            as.character(r$id)
          ),
          "x"
        )
      )
    }))
  })

  output$calendar <- renderUI({
    first <- month_start()
    n_days <- as.integer(format(
      seq(first, by = "1 month", length.out = 2)[2] - 1,
      "%d"
    ))
    offset <- (as.integer(format(first, "%u")) - 1) # lunedi' = 0
    totals <- log_df() |>
      filter(work_date >= first, work_date < first + n_days) |>
      summarise(minutes = sum(minutes), .by = c(work_date, person))

    cells <- c(
      lapply(WEEKDAYS_IT, \(w) div(class = "cal-head", w)),
      rep(list(div(class = "cal-cell cal-empty")), offset),
      lapply(seq_len(n_days), function(i) {
        d <- first + i - 1
        t <- totals |> filter(work_date == d)
        classes <- c(
          "cal-cell",
          if (d == today_it()) "cal-today",
          if (d == sel_date()) "cal-selected"
        )
        div(
          class = paste(classes, collapse = " "),
          onclick = sprintf(
            "Shiny.setInputValue('cal_day', '%s', {priority: 'event'})",
            d
          ),
          span(class = "cal-num", i),
          lapply(PEOPLE, function(p) {
            m <- t$minutes[t$person == p]
            if (length(m) == 0) {
              return(NULL)
            }
            span(
              class = "cal-badge",
              style = paste0("background:", PEOPLE_COLORS[[p]]),
              paste0(substr(p, 1, 1), " ", fmt_min(m))
            )
          })
        )
      })
    )
    div(class = "cal-grid", cells)
  })

  output$ma_plot <- renderPlot({
    ma <- daily_moving_avg(log_df(), k = 5)
    validate(need(
      nrow(ma) > 0,
      "Nessun dato ancora: aggiungi qualche attivita'."
    ))
    ggplot(ma, aes(work_date, avg, color = person)) +
      geom_line(linewidth = 1) +
      scale_color_manual(values = PEOPLE_COLORS) +
      labs(x = NULL, y = "Minuti/giorno (media mobile 5 gg)", color = NULL) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "top")
  })
}

shinyApp(ui, server)
