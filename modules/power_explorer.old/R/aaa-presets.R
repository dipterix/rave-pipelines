if(getOption("rave.run.if_false", FALSE)){
  .module_id  <-  "power_explorer"
  if(interactive() && !dipsaus::shiny_is_running()){
    setwd(rstudioapi::getActiveProject())
    source('./modules/power_explorer/R/aa.R')
  }
}

component_container <- ravedash:::RAVEShinyComponentContainer$new(
  module_id = module_id, pipeline_name = pipeline$pipeline_name
)


# Define components
loader_project <- ravedash::presets_loader_project()
loader_subject <- ravedash::presets_loader_subject()
loader_epoch <- ravedash::presets_loader_epoch()
loader_electrodes <- ravedash::presets_loader_electrodes()
loader_reference <- ravedash::presets_loader_reference()
loader_viewer <- ravedash::presets_loader_3dviewer(height = "100%")

import_export_pipeline <- ravedash::presets_import_export_subject_pipeline()
electrode_selector <- ravedash::presets_analysis_electrode_selector2()
baseline_choices <- ravedash::presets_baseline_choices()
comp_condition_groups <- ravedash::presets_condition_groups()
comp_analysis_ranges <- ravedash::presets_analysis_ranges()

component_container$add_components(
  loader_project, loader_subject, loader_epoch,
  loader_electrodes, loader_reference, loader_viewer,
  electrode_selector, import_export_pipeline, baseline_choices,
  comp_condition_groups, comp_analysis_ranges
)


if(getOption("rave.run.if_false", FALSE)){
  session <- shiny::MockShinySession$new()
  input <- session$input
  output <- session$output
}