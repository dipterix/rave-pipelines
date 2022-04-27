

# 1. Information
# 2. Import (convert to nii)
# 3. Surface recon
# 4. CT co-registration

module_html <- function(){

  shiny::div(
    class = "container",
    shiny::fluidRow(

      shiny::column(
        width = 4L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll padding-bottom-70",
          shiny::column(
            width = 12L,

            ravedash::input_card(
              title = "Subject Information",
              class_header = "",
              shiny::uiOutput(ns("basic_info"))
            )

          )
        )
      ),

      shiny::column(
        width = 8L,
        shiny::div(
          class = "row screen-height overflow-y-scroll padding-bottom-70 output-wrapper",
          shiny::column(
            width = 12L,

            ravedash::output_card(
              title = "Import DICOM Images",
              start_collapsed = TRUE,
              tools = list(
                shidashi::as_badge("requires `dcm2niix`|bg-yellow")
              ),
              append_tools = FALSE,
              shiny::uiOutput(ns("panel_import_dicom"))
            ),

            ravedash::output_card(
              title = "Surface Reconstruction",
              start_collapsed = TRUE,
              tools = list(
                shidashi::as_badge("requires `FreeSurfer`|bg-yellow")
              ),
              append_tools = FALSE,
              'asda'
            ),

            ravedash::output_card(
              title = "Co-registration",
              start_collapsed = TRUE,
              tools = list(
                shidashi::as_badge("requires `FSL-flirt`|bg-yellow")
              ),
              append_tools = FALSE,
              'asda'
            )

          )
        )
      )
    )
  )


}
