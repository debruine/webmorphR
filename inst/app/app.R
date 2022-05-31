suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(shinyWidgets)
  library(DT)
  library(dplyr)
  library(webmorphR)
})

imgdir <- getShinyOption("imgdir")


# tabs ----

## main_tab ----
main_tab <- tabItem(
  tabName = "main_tab",
  textOutput("quickhelp"),
  #fluidRow(
    #column(width = 6,
           #actionButton("delin_clear", "Clear Points"),
           actionButton("delin_reload", NULL, icon = icon("redo")),
           actionButton("prev_img", NULL, icon = icon("step-backward")),
           actionButton("delin_save", "Save", icon = icon("save")),
           actionButton("next_img", NULL, icon = icon("step-forward")),
    #),
    #column(width = 6,
           sliderTextInput("img_size", NULL,
              selected = 100, 
              grid = FALSE, 
              post = "%", 
              choices = c(seq(10,100, 10), seq(200, 1000, 100))),
    #)
  #),
  tags$div(id = "delin_box_box", 
    tags$div(id = "delin_box", imageOutput("delin_img", ))
  )
)


# UI ----
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "WebmorphR"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      #menuItem("QuickDelin", tabName = "main_tab"),
      hidden(downloadButton("tem_dl", "Templates")),
      actionButton("delin_finish", "Save and Back to R"),
      actionButton("delin_cancel", "Cancel without Saving"),
      fileInput("finder_load", "Load Files", multiple = TRUE,
                width = "100%", accept = c("image/*", ".tem")),
      DTOutput("finder_table")
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src= "https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"),
      tags$script(src = "keycodes.js"),
      tags$script(src = "custom.js")
    ),
    #tabItems(
      main_tab
    #)
  )
)

# server ----
server <- function(input, output, session) {
  v <- reactiveValues()

  if (!is.null(imgdir)) {
    output$quickhelp <- renderText("Loading stimuli from webmorphR")
    runjs("delin_clear();")
    v$stimuli <- read_stim(imgdir)
    v$delin_current <- 1
    output$quickhelp <- renderText({
      sprintf("%d images loaded", length(v$stimuli))
    })
    
    # hide desktop use functions
    hide("finder_load")
    hide("tem_dl")
  }

  ## finder_load ----
  observeEvent(input$finder_load, { debug_msg("finder_load")
    # rename files to original names in tempdir
    tdir <- dirname(input$finder_load$datapath[[1]])
    file.rename(input$finder_load$datapath, 
                file.path(tdir, input$finder_load$name))
    
    # load as stimlist
    runjs("delin_clear();")
    v$stimuli <- webmorphR::read_stim(tdir)
    v$delin_current <- 1
    
    show("tem_dl")
  }, ignoreNULL = TRUE)

  ## finder_table ----
  output$finder_table <- renderDT({
    req(v$stimuli)
    
    df <- data.frame(
      file = names(v$stimuli)
    )
    
    # my_col <- rep('black', nrow(df))
    # my_col[v$delin_current] <- 'white'
    # my_bg <- rep('white', nrow(df))
    # my_bg[v$delin_current] <- '#204969'
    
    datatable(df, escape = TRUE, 
              rownames = FALSE, 
              selection = 'single', 
              options = dt_options()) #|>
      # formatStyle(1, target = 'row',
      #             color = styleEqual(df$file, my_col),
      #             backgroundColor = styleEqual(df$file, my_bg))
  })

  ## finder_table_rows_selected ----
  observeEvent(input$finder_table_rows_selected, {
    debug_msg("finder_table_rows_selected")
    idx <- input$finder_table_rows_selected

    if (length(idx) == 0) { return() }

    v$delin_current <- idx
    updateTabItems(session, "tabs", selected = "delin_tab")
  })

  
  ## img_size ----
  observeEvent(input$img_size, {
    sprintf("resize = %f; delin_resize();", 
            input$img_size) |> 
      runjs()
  })
  
  # update save button on template change ----
  observeEvent(input$pts, { debug_msg("pts change")
    req(v$stimuli)
    img <- v$stimuli[[v$delin_current]]
    
    # check if same as delin
    needs_saved <- TRUE
    if (!is.null(img$points)) {
      saved_pts <- apply(img$points, 2, c) |> c() |> round(1)
      needs_saved <- !all(round(input$pts, 1) == saved_pts)
      # debug_msg("input: ", str(input$pts))
      # debug_msg("saved: ", str(saved_pts))
    }
    
    shinyjs::toggleClass("delin_save", "btn-danger", needs_saved)
  })

  ## delin_img ----
  output$delin_img <- renderImage({ debug_msg("delin_img")
    req(v$stimuli)
    img <- v$stimuli[[v$delin_current]]
    
    # update size
    w <- img$width * input$img_size/100
    h <- img$height * input$img_size/100
    js <- sprintf("$('#delin_img, #delin_box').css('width', %d).css('height', %d)", w, h)
    runjs(js)
    
    list(src = img$imgpath,
         alt = "Image failed to render",
         draggable = "false",
         width = w,
         height = h)
  }, deleteFile = FALSE)
  
  # draw tem points -----
  observeEvent(v$delin_current, {
    req(v$stimuli)
    img <- v$stimuli[[v$delin_current]]
    
    runjs("delin_clear();")
    
    if (is.null(img$points)) {
      output$quickhelp <- renderText("Hold down cmd/ctrl and shift to add points")
    } else {
      # add points with js 
      apply(img$points, 2, function(pt) {
        sprintf("new_pt({originalEvent: {layerX: %f, layerY: %f}});",
                pt[["x"]] * input$img_size / 100, 
                pt[["y"]] * input$img_size / 100) |>
          runjs()
      })
    }
  })
  
  ## delin_reload ----
  observeEvent(input$delin_reload, {
    req(v$stimuli)
    img <- v$stimuli[[v$delin_current]]
    
    runjs("delin_clear();")
    
    # add points with js 
    if (!is.null(img$points)) {
      apply(img$points, 2, function(pt) {
        sprintf("new_pt({originalEvent: {layerX: %f, layerY: %f}});",
                pt[["x"]] * input$img_size / 100, 
                pt[["y"]] * input$img_size / 100) |>
          runjs()
      })
    }
  })

  ## delin_clear ----
  observeEvent(input$delin_clear, {
    runjs("delin_clear();")
  })
  
  # next_img ----
  next_img <- function() {
    # increment image
    if (v$delin_current >= length(v$stimuli)) {
      v$delin_current <- 1
    } else {
      v$delin_current <- v$delin_current + 1
    }
  }
  
  observeEvent(input$next_img, { next_img() })
  
  # prev_img ----
  observeEvent(input$prev_img, { 
    if (v$delin_current == 1) {
      v$delin_current <- length(v$stimuli)
    } else {
      v$delin_current <- v$delin_current - 1
    }
  })
  
  # delin_save ----
  observeEvent(input$delin_save, {
    # add delin to v$stimuli
    v$stimuli[[v$delin_current]]$points <- 
      matrix(input$pts, 2, dimnames = list(c("x", "y")))
    
    next_img()
  })
  
  ## tem_dl ----
  output$tem_dl <- downloadHandler(
    filename = function() {
      debug_msg("tem_dl")
      "tems.zip"
    },
    content = function(file) {
      tryCatch({
        # change wd to tempdir for zipping
        oldwd <- getwd()
        on.exit(setwd(oldwd))
        tdir <- tempfile()
        write_stim(v$stimuli, tdir, overwrite = TRUE)
        setwd(tdir)
        utils::zip(file, list.files(pattern = "\\.tem$"))
      }, error = function(e) {
        alert(e)
      })
    },
    contentType = "application/zip"
  )
  
  # delin_finish ----
  observeEvent(input$delin_finish, {
    if (!is.null(imgdir)) {
      debug_msg("writing stimuli to ", imgdir)
      write_stim(v$stimuli, imgdir, overwrite = TRUE)
    }
    
    # clean up and signal done
    hide("delin_img")
    runjs("delin_clear();")
    showModal(modalDialog(title = "Images saved and returning to R",
                          "You can close this app now."))
    stopApp()
  })
  
  # delin_cancel ----
  modal_quit_confirm <- modalDialog(
    "This will not save any changes you made to the delineations.",
    title = "Cancel without Saving",
    footer = tagList(
      actionButton("quit_cancel", "Return to Delineator"),
      actionButton("quit_ok", "Cancel without Saving", class = "btn btn-danger")
    )
  )
  
  observeEvent(input$delin_cancel, { showModal(modal_quit_confirm) })
  observeEvent(input$quit_cancel, removeModal())
  
  observeEvent(input$quit_ok, {
    removeModal()
    stopApp()
  })
  
  

} # end server()

shinyApp(ui, server)
