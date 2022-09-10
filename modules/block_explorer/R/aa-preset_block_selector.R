

presets_loader_block <- function(
  id = "loader_block",
  varname = "block",
  label = "Block",
  loader_project_id = "loader_project_name",
  loader_subject_id = "loader_subject_code",
  multiple = FALSE
){
  comp <- ravedash:::RAVEShinyComponent$new(id = id, varname = varname)
  comp$depends <- c(loader_project_id, loader_subject_id)
  comp$no_save <- "default"
  force(multiple)

  comp$ui_func <- function(id, value, depends){
    shiny::selectInput(
      inputId = id,
      label = label,
      choices = character(0L),
      multiple = multiple
    )
  }

  comp$validators$add(list(
    sub_id = NULL,
    rule = function(value) {
      if(length(value)) {
        value <- value[!value %in% ""]
        if(length(value)) { return() }
      }
      return("Block cannot be blank")
    }
  ))

  comp$server_func <- function(input, output, session){
    loader_project <- comp$get_dependent_component(loader_project_id)
    loader_subject <- comp$get_dependent_component(loader_subject_id)

    # get external tool from component `loader_project`
    get_subject <- loader_subject$get_tool("get_subject")

    get_blocks <- function(){
      subject <- get_subject()
      choices <- character(0L)
      suggested <- character(0L)
      if(inherits(subject, "RAVESubject")) {
        # get all blocks
        choices <- as.character(subject$blocks)

        # get suggested block
        suggested <- subject$get_default(varname)

        # check current pipeline settings to obtain last entered
        last_project_name <- loader_project$get_settings_value()
        last_subject_code <- loader_subject$get_settings_value()
        if(
          identical(subject$project_name, last_project_name) &&
          identical(subject$subject_code, last_subject_code)
        ) {
          suggested <- comp$get_settings_value(key = varname, default = suggested)
        }

        if(!multiple) {
          suggested <- suggested %OF% choices
        } else if (!length(suggested)) {
          suggested <- choices
        }
      }

      return(list(
        choices = choices,
        suggested = suggested
      ))
    }

    shiny::bindEvent(
      observe({
        open_loader <- watch_loader_opened(session = session)
        if(!open_loader){ return() }
        if(!loader_subject$sv$is_valid()){ return() }
        blocks <- get_blocks()

        shiny::updateSelectInput(
          session = session,
          inputId = id,
          choices = blocks$choices,
          selected = blocks$suggested
        )
      }),
      loader_project$current_value,
      loader_subject$current_value,
      watch_loader_opened(session = session),
      ignoreNULL = TRUE
    )

    # validator_step1_inputs$add_rule(inputId = "loader_project_name", shinyvalidate::sv_regex("^[a-zA-Z0-9][a-zA-Z0-9_-]{0,}$", "The project name is invalid: can only contain letters, digits, dash (-), or underscore (_). The first letter should only contain letters and digits."))

  }

  comp

}
