library(dipsaus)

# options("shinyvalidate.verbose" = TRUE)

# Stores global variables
local_data <- fastmap::fastmap()
local_reactives <- shiny::reactiveValues()


# module_ui_loader <- function(){
#   shiny::tagList(
#     # ui_step1(),
#     ui_step2()
#   )
# }
#
# module_ui_main <- function(){
#   module_ui_loader()
# }


# ----- Presets ------
comp_import_setup <- ravedash::presets_import_setup_native()
comp_import_format <- ravedash::presets_import_setup_blocks()
comp_import_channels <- ravedash::presets_import_setup_channels()
# comp <- comp_import_channels
component_container <- ravedash:::RAVEShinyComponentContainer$new(
  module_id = module_id, pipeline_name = pipeline_name,
  pipeline_path = pipeline$pipeline_path)
component_container$add_components(
  comp_import_setup,
  comp_import_format,
  comp_import_channels
)
