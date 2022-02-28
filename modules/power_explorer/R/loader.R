# .module_id  <-  "power_explorer"
# if(interactive() && !dipsaus::shiny_is_running()){
#   setwd(rstudioapi::getActiveProject())
#   source('./modules/power_explorer/R/aaa.R')
# }

# Define components
loader_project <- ravedash::presets_loader_project()
loader_subject <- ravedash::presets_loader_subject()
loader_epoch <- ravedash::presets_loader_epoch()
loader_electrodes <- ravedash::presets_loader_electrodes()
loader_reference <- ravedash::presets_loader_reference()
loader_viewer <- ravedash::presets_loader_3dviewer()

component_container$add_components(
  loader_project, loader_subject, loader_epoch,
  loader_electrodes, loader_reference, loader_viewer
)


module_ui_loader <- function(session = shiny::getDefaultReactiveDomain()){


  ravedash::simple_layout(
    input_width = 4L,
    container_fixed = TRUE,
    container_style = 'max-width:1444px;',
    input_ui = {
      # project & subject
      ravedash::input_card(
        title = "Data Selection",

        ravedash::flex_group_box(
          title = "Project and Subject",

          shidashi::flex_item(
            loader_project$ui_func()
          ),
          shidashi::flex_item(
            loader_subject$ui_func()
          )
        ),

        loader_epoch$ui_func(),

        ravedash::flex_group_box(
          title = "Electrodes and Reference",

          loader_reference$ui_func(),
          shidashi::flex_break(),
          shidashi::flex_item(
            loader_electrodes$ui_func()
          ),
          shidashi::flex_item(
            shiny::fileInput(
              inputId = ns("loader_mask_file"),
              label = "or Mask file"
            ))
        ),

        footer = shiny::tagList(
          dipsaus::actionButtonStyled(
            inputId = ns("loader_ready_btn"),
            label = "Load subject",
            type = "primary",
            width = "100%"
          )
        )

      )
    },
    output_ui = {
      ravedash::output_card(
        title = "3D Viewer",
        class_body = "no-padding",
        loader_viewer$ui_func()
      )
    }
  )

}

server_loader <- function(input, output, session, ...){

  list2env(list(session = session, input = input), envir=globalenv())

  event_data <- register_session_events(session = session)

  # Add validator
  # session <- shiny::MockShinySession$new()
  loader_project$server_func(input, output, session)
  loader_subject$server_func(input, output, session)
  loader_epoch$server_func(input, output, session)
  loader_electrodes$server_func(input, output, session)
  loader_reference$server_func(input, output, session)
  loader_viewer$server_func(input, output, session)
  loader_validator_subject_code <- loader_subject$sv



  shiny::observe({
    # gather information
    pipeline_set(
      project_name = input[[loader_project$id]],
      subject_code = input[[loader_subject$id]],
      epoch_name = input$loader_epoch_name,
      trial_starts = -input$loader_epoch_name_pre,
      trial_ends = input$loader_epoch_name_post,
      reference_name = input[[loader_reference$id]],
      loaded_electrodes = input[[loader_electrodes$id]]
    )

    default_epoch <- isTRUE(input$loader_epoch_name_default)
    default_reference <- isTRUE(input$loader_reference_name_default)

    # Run the pipeline!
    tarnames <- raveio::pipeline_target_names(pipe_dir = pipeline_path)
    count <- length(tarnames) + dipsaus::parse_svec(input$loader_electrode_text) + 4

    dipsaus::shiny_alert2(
      title = "Loading in progress",
      text = paste(
        "Everything takes time. Some might need more patience than others."
      ), icon = "info", auto_close = FALSE, buttons = FALSE
    )

    tryCatch({
      raveio::pipeline_run(
        pipe_dir = pipeline_path,
        names = "repository",
        scheduler = "none",
        type = "smart",
        callr_function = NULL
      )
      # raveio::with_future_parallel(bquote({
      #   raveio::pipeline_run(
      #     pipe_dir = .(pipeline_path),
      #     names = "repository",
      #     type = "basic",
      #     use_future = FALSE
      #   )
      # }), quoted = TRUE, env = new.env(parent = asNamespace('raveio')))

      # load subject
      if(default_epoch || default_reference){
        repo <- raveio::pipeline_read("repository", pipe_dir = pipeline_path)
        if(default_epoch){
          repo$subject$set_default("epoch_name", repo$epoch_name)
        }
        if(default_reference) {
          repo$subject$set_default("reference_name", repo$reference_name)
        }
      }

      ravedash::fire_rave_event('data_changed', Sys.time())
      ravedash::logger("Data has been loaded loaded")
      dipsaus::close_alert2()
    }, error = function(e){
      dipsaus::close_alert2()
      dipsaus::shiny_alert2(
        title = "Errors",
        text = paste(
          "Found an error while loading the power data:\n\n",
          paste(e, collapse = "\n")
        ),
        icon = "error",
        danger_mode = TRUE,
        auto_close = FALSE
      )
    })

  }) |>
    shiny::bindEvent(input$loader_ready_btn, ignoreNULL = TRUE, ignoreInit = TRUE)



}
