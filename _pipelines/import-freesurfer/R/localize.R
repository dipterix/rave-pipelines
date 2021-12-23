shiny_ui <- function(){
  shiny::fluidPage(
    title = "Electrode Localization",
    shiny::fluidRow(
      shiny::absolutePanel(
        draggable = TRUE,
        width = "400px",
        height = "600px",
        left = 0,
        bottom = 0,
        fixed = FALSE,
        style = "z-index: 1000; overflow: scroll; background-color: white; padding: 15px; max-height: 100vh;",
        DT::DTOutput("table", width = "100%")
      ),
      threeBrain::threejsBrainOutput(
        "viewer", width = "100%", height = "100vh")
    )
  )
}

localize_electrodes <- function(subject, ct_path, surfaces = "pial", use_141 = TRUE, shiny_options = list(launch.browser = TRUE), ...){
  if(!isTRUE(file.exists(ct_path))){
    stop("CT file not exists")
  }
  subject <- raveio::as_rave_subject(subject, strict = FALSE)

  brain <- threeBrain::freesurfer_brain2(
    fs_subject_folder = subject$freesurfer_path,
    subject_name = subject$subject_code,
    surface_types = surfaces,
    use_141 = use_141,
    use_cache = TRUE,
    ...
  )

  if(is.null(brain)){
    return()
  }

  results <- dipsaus::fastmap2()
  # results$table

  shiny_server <- function(input, output, session){
    proxy <- threeBrain::brain_proxy(outputId = "viewer")

    local_reactive <- shiny::reactiveValues(
      table = NULL
    )
    output$viewer <- threeBrain::renderBrain({
      shiny::showNotification(p("Loading... Please wait"), type = 'message', closeButton = FALSE, duration = NULL, id = "notif")
      on.exit({
        shiny::removeNotification("notif")
      })
      brain$localize(ct_path, show_modal = FALSE, background = "#000000")
    })

    shiny::observeEvent(proxy$localization_table, {
      local_reactive$table <- proxy$localization_table
    })

    output$table <- DT::renderDT({
      DT::datatable(
        local_reactive$table,
        class = "compact cell-border stripe",
        editable = list(target = 'row', disable = list(
          columns = c(1, 3, 4, 5, 7, 9, 10:15) - 1)),
        rownames = FALSE,
        caption = "Please add the electrode/channel number before saving to RAVE",
        filter = "none",
        extensions = c('Buttons'),
        options = list(
          dom = 'Brtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          columnDefs = list(
            list(visible = FALSE, targets = c(1,3:5,10:15) - 1)
          )
        )
      )
    })

    onSessionEnded(function(){
      shiny::stopApp()
    })
  }

  raveio::catgl("Launching interactive viewer. When you finish the localization, make sure to click save button before closing the browser tab. The pipeline will resume once the localization tab is closed.\n", level = "INFO")
  app <- shiny::shinyApp(ui = shiny_ui, server = shiny_server, options = shiny_options)

  print(app)

}


subject <- raveio::as_rave_subject("test1/YAB")
settings <- list(ct_aligned_t1 = "~/Downloads/iELVis_Localization/YAS_CT_aligned1.nii")
localize_electrodes(subject, ct_path = settings$ct_aligned_t1)

