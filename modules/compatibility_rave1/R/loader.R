# UI components for loader
loader_html <- function(session = shiny::getDefaultReactiveDomain()){

  shiny::div(
    class = "container",
    shiny::fluidRow(
      shiny::column(width = 3L),
      shiny::column(
        width = 6L,
        ravedash::input_card(
          title = "Data Selection",
          class_header = "",

          ravedash::flex_group_box(
            title = "Project and Subject",

            shidashi::flex_item(
              loader_project$ui_func()
            ),
            shidashi::flex_item(
              loader_subject$ui_func()
            )
          ),

          footer = shiny::tagList(
            shiny::actionLink(
              inputId = ns("loader_sync_btn"),
              label = "Sync from [Import LFP] module"
            ),
            shiny::hr(),
            dipsaus::actionButtonStyled(
              inputId = ns("loader_ready_btn"),
              label = "Load subject",
              type = "primary",
              width = "100%"
            )
          )

        )
      )
    )
  )

}


# Server functions for loader
loader_server <- function(input, output, session, ...){

  # Triggers the event when `input$loader_ready_btn` is changed
  # i.e. loader button is pressed
  shiny::bindEvent(
    ravedash::safe_observe({
      # gather information from preset UIs
      settings <- component_container$collect_settings(
        ids = c(
          "loader_project_name",
          "loader_subject_code",
          "loader_electrode_text",
          "loader_epoch_name",
          "loader_reference_name"
        )
      )
      # TODO: add your own input values to the settings file

      # Save the variables into pipeline settings file
      pipeline$set_settings(.list = settings)

      # Check if user has asked to set the epoch & reference to be the default
      default_epoch <- isTRUE(loader_epoch$get_sub_element_input("default"))
      default_reference <- isTRUE(loader_epoch$get_sub_element_input("default"))

      # --------------------- Run the pipeline! ---------------------

      # Calculate the progress bar
      tarnames <- pipeline$target_table$Names
      count <- length(tarnames) + length(dipsaus::parse_svec(loader_electrodes$current_value)) + 4

      # Pop up alert to prevent user from making any changes (auto_close=FALSE)
      # This requires manually closing the alert window
      dipsaus::shiny_alert2(
        title = "Loading in progress",
        text = paste(
          "Everything takes time. Some might need more patience than others."
        ), icon = "info", auto_close = FALSE, buttons = FALSE
      )

      # Run the pipeline target `repository`
      # Use `as_promise=TRUE` to make result as a promise
      res <- pipeline$run(
        as_promise = TRUE,
        names = "repository",
        scheduler = "none",
        type = "smart",  # parallel
        # async = TRUE,
        callr_function = NULL,
        progress_quiet = TRUE
      )

      # The `res` contains a promise that might not have finished yet,
      # so register functions to run when the promise is resolved
      res$promise$then(

        # When data can be imported
        onFulfilled = function(e){

          # Set epoch and/or reference as default
          if(default_epoch || default_reference){
            repo <- pipeline$read("repository")
            if(default_epoch){
              repo$subject$set_default("epoch_name", repo$epoch_name)
            }
            if(default_reference) {
              repo$subject$set_default("reference_name", repo$reference_name)
            }
          }

          # Let the module know the data has been changed
          ravedash::fire_rave_event('data_changed', Sys.time())
          ravedash::logger("Data has been loaded loaded")

          # Close the alert
          dipsaus::close_alert2()
        },


        # this is what should happen when pipeline fails
        onRejected = function(e){

          # Close the alert
          dipsaus::close_alert2()

          # Immediately open a new alert showing the error messages
          dipsaus::shiny_alert2(
            title = "Errors",
            text = paste(
              "Found an error while loading the power data:\n\n",
              paste(e$message, collapse = "\n")
            ),
            icon = "error",
            danger_mode = TRUE,
            auto_close = FALSE
          )
        }
      )
    }),
    input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )


  shiny::bindEvent(
    ravedash::safe_observe({
      pipe2 <- raveio::pipeline_find("import_lfp_native")
      data <- structure(raveio::pipeline_read(c(
        "import_setup__project_name",
        "import_setup__subject_code"
      ), pipe_dir = pipe2),
      names = c("project_name", "subject_code"))

      shiny::updateSelectInput(
        session = session,
        inputId = loader_project$get_sub_element_id(),
        selected = data$project_name
      )

      shiny::updateSelectInput(
        session = session,
        inputId = loader_subject$get_sub_element_id(),
        selected = data$subject_code
      )

    }),
    input$loader_sync_btn, ignoreNULL = TRUE, ignoreInit = TRUE
  )



}
