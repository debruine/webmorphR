suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(DT)
  library(dplyr)
  library(webmorphR)
})

imgdir <- getShinyOption("imgdir")
n_points <- getShinyOption("n_points")

# functions ----

# display debugging messages in R if local,
# or in the console log if remote
debug_msg <- function(...) {
  is_local <- Sys.getenv('SHINY_PORT') == ""
  txt <- paste(...)
  if (is_local) {
    message(txt)
  } else {
    shinyjs::logjs(txt)
  }
}

## datatable constants ----
dt_options <- list(
  info = FALSE,
  lengthChange = FALSE,
  paging = FALSE,
  ordering = FALSE,
  searching = FALSE,
  pageLength = 500,
  keys = TRUE
)

# tabs ----

## main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  actionButton("delin_clear", "Clear"),
  actionButton("delin_save", "Save"),
  actionButton("delin_finish", "Finish"),
  #downloadButton("finder_dl", "Download Files"),
  imageOutput("delin_img")
)


# UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "QuickDelin"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("QuickDelin", tabName = "main_tab"),
      numericInput("n_points", "Auto-Save Points", n_points, min = 0, max = 50),
      fileInput("finder_load", "Load Files", multiple = TRUE,
                width = "100%", accept = "image/*"),
      DTOutput("finder_table")
    )
  ),
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "custom.js")
    ),
    tabItems(
      main_tab
    )
  )
)

# server ----
server <- function(input, output, session) {
  v <- reactiveValues()

  if (!is.null(imgdir)) {
    debug_msg(imgdir)
    v$finder <- data.frame(
      file = list.files(imgdir)
    )
    v$paths <- list.files(imgdir, full.names = TRUE)
    v$delin_current <- 1
  } else {
    debug_msg("no imgdir")
  }

  ## finder_load ----
  observeEvent(input$finder_load, { debug_msg("load_files")
    v$finder <- data.frame(
      file = input$finder_load$name
    )

    tdir <- dirname(input$finder_load$datapath[[1]])
    v$paths <- file.path(tdir, input$finder_load$name)
    file.rename(input$finder_load$datapath, v$paths)
    v$delin_current <- 1
  }, ignoreNULL = TRUE)

  ## finder_table ----
  output$finder_table <- renderDT({
    v$finder
  },  escape = TRUE,
      rownames = FALSE,
      selection = "single",
      options = dt_options
  )

  ## finder_table_rows_selected ----
  observeEvent(input$finder_table_rows_selected, {
    debug_msg("finder_table_rows_selected")
    idx <- input$finder_table_rows_selected

    if (length(idx) == 0) {
      return()
    }

    v$delin_current <- idx
    updateTabItems(session, "tabs", selected = "delin_tab")
  })

  ## finder_dl ----
  output$finder_dl <- downloadHandler(
    filename = function() {
      debug_msg("finder_dl")
      "stuff.zip"
    },
    content = function(file) {
      tryCatch({
        tdir <- dirname(v$paths[[1]])
        setwd(tdir)
        utils::zip(file, list.files(tdir))
      }, error = function(e) {
        shinyjs::alert(e)
      })
    }
  )

  ## delin_img ----
  output$delin_img <- renderImage({
    list(src = v$paths[v$delin_current],
         alt = "Image failed to render")
  }, deleteFile = FALSE)

  observeEvent(v$delin_current, {
    # check for template
    tempath <- sub("\\..{3,4}$", ".tem", v$paths[v$delin_current])
    if (file.exists(tempath)) {
      tem <- read_stim(tempath)
      print(tem$points)
    }
  })

  ## delin_clear ----
  observeEvent(input$delin_clear, {
    runjs("delin_clear();")
  })

  # delin_finish ----
  observeEvent(input$delin_finish, {
    stopApp()
  })

  # save_delin ----
  save_delin <- function() {
    # construct webmorph template
    template <- matrix(input$pts, ncol = 2, byrow = TRUE) %>%
      apply(1, paste, collapse = "\t") %>%
      paste(collapse = "\n") %>%
      sprintf("%d\n%s\n0\n-1\n",  length(input$pts)/2, .)

    # save template with image name.tem
    path <- sub("\\..{3,4}$", ".tem", v$paths[v$delin_current])
    write(template, path)

    # clear the points
    runjs("delin_clear();")

    # increment image or quit
    if (v$delin_current >= length(v$paths)) {
      stopApp()
    } else {
      v$delin_current <- v$delin_current + 1
    }
  }

  ## save delin when n(pts) == n_points ----
  observeEvent(input$pts, {
    if (input$n_points > 0 && length(input$pts)/2 >= input$n_points) {
      save_delin()
    }
  }, ignoreNULL = TRUE)

  ## delin_save ----
  observeEvent(input$delin_save, {
    save_delin()
  })





} # end server()

shinyApp(ui, server)
