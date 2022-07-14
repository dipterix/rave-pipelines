
module_server <- function(input, output, session, ...){


  # Local reactive values, used to store reactive event triggers
  local_reactives <- shiny::reactiveValues(
    update_outputs = NULL
  )

  # Local non-reactive values, used to store static variables
  local_data <- dipsaus::fastmap2()

  # get server tools to tweek
  server_tools <- get_default_handlers(session = session)


  shiny::bindEvent(
    ravedash::safe_observe({
      loaded_flag <- ravedash::watch_data_loaded()
      if(!loaded_flag){ return() }

      local_reactives$validation_results <- NULL
      shidashi::card_operate(title = "Data integrity check", method = "collapse")
      shidashi::card_operate(title = "Backward compatibility", method = "collapse")

    }, priority = 1001),
    ravedash::watch_data_loaded(),
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  # Register event: validate subject
  shiny::bindEvent(
    ravedash::safe_observe({

      subject <- component_container$data$subject
      version <- as.character(input$validation_version) %OF% c(2, 1)
      mode <- input$validation_mode %OF% c("normal", "basic")

      local_reactives$validation_results <- NULL

      if(mode == "normal") {
        dipsaus::shiny_alert2(
          title = "Validation in progress...",
          text = "Please wait...",
          icon = "info",
          auto_close = FALSE, buttons = FALSE
        )
        Sys.sleep(0.5)
        on.exit({
          dipsaus::close_alert2()
        }, add = TRUE, after = FALSE)
      }
      validation_results <- raveio::validate_subject(
        subject = subject$subject_id,
        method = mode,
        version = as.integer(version))

      local_reactives$validation_results <- validation_results
      return()

    }),
    server_tools$run_analysis_flag(),
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({

      tryCatch({
        subject <- component_container$data$subject
        if(!inherits(subject, "RAVESubject")) {
          stop("Subject is invalid. Please validate the subject first")
        }

        dipsaus::shiny_alert2(
          title = "Converting in progress",
          text = "This step might take a while...",
          buttons = FALSE, auto_close = FALSE,
          icon = "info"
        )
        Sys.sleep(0.5)
        raveio::rave_subject_format_conversion(subject = subject)

        dipsaus::close_alert2()
        dipsaus::shiny_alert2(
          title = "Conversion done!",
          icon = "success",
          buttons = list("OK" = TRUE),
          auto_close = TRUE,
          text = "The subject data is ready for RAVE 1.0 modules."
        )
      }, error = function(e) {

        dipsaus::close_alert2()
        ravedash::logger_error_condition(e)
        dipsaus::shiny_alert2(
          title = "Conversion failed",
          icon = "error",
          buttons = list("I got it" = TRUE),
          auto_close = FALSE,
          text = sprintf("Found the following error: %s...\n\nPlease check the console for details", paste(e$message, collapse = ""))
        )

      })


    }),
    input$compatibility_do,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::card_operate(title = "Data integrity check", method = "expand")
      shidashi::card_operate(title = "Backward compatibility", method = "collapse")
    }),
    input$quickaccess_data_integrity,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  shiny::bindEvent(
    ravedash::safe_observe({
      shidashi::card_operate(title = "Data integrity check", method = "collapse")
      shidashi::card_operate(title = "Backward compatibility", method = "expand")
    }),
    input$quickaccess_compatibility,
    ignoreNULL = TRUE, ignoreInit = TRUE
  )

  output$validation_check <- shiny::renderUI({
    validation_results <- local_reactives$validation_results
    if(is.null(validation_results)) { return(invisible()) }
    keys <- c("paths", "preprocess", "meta", "voltage_data",
              "power_phase_data", "epoch_tables", "reference_tables")
    re <- list()
    for(k in keys) {
      items <- validation_results[[k]]
      if(length(items)) {
        re0 <- lapply(names(items), function(nm) {
          item <- items[[nm]]
          s <- utils::capture.output({
            print(item, use_logger = FALSE)
          })

          if(isTRUE(item$valid)) {
            cls <- "hljs-comment"
          } else if(is.na(item$valid)) {
            cls <- "hljs-literal"
          } else {
            if(identical(item$severity, "minor")) {
              cls <- "hljs-literal"
            } else {
              cls <- "hljs-keyword"
            }
          }
          shiny::tags$code(class = cls, paste(s, collapse = "\n"))
        })
        re <- c(re, re0)
      }
    }
    re <- shiny::pre(
      class = "pre-compact bg-gray-90",
      re
    )
    re
  })




}
