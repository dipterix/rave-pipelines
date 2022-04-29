

# 1. Information
# 2. Import (convert to nii)
# 3. Surface recon
# 4. CT co-registration
dry_run <- raveio::is_dry_run()
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
              class_foot = "no-padding",
              append_tools = FALSE,
              footer = shiny::uiOutput(ns("panel_import_dicom")),
              shiny::div(
                "The following script uses ", shiny::pre(class="pre-compact no-padding display-inline", "dcm2niix"),
                " external library to convert DICOM images to Nifti format for later purposes. ",
                shiny::br(),
                "* The script requires Unix ",
                shiny::pre(class="pre-compact no-padding display-inline", "sh"),
                " terminal. If you are using Windows, please install and use Linux sub-system.",

                shiny::hr(),

                shiny::fluidRow(

                  shiny::column(
                    width = 5L,
                    shiny::div(
                      class = "display-inline",
                      shiny::selectInput(
                        inputId = ns("param_dcm2niix_merge"),
                        label = "Merge 2D slices from same series regardless of echo, exposure, etc.",
                        choices = c("Auto", "Yes", "No"),
                        selected = "Auto"
                      )
                    )
                  ),
                  shiny::column(
                    width = 5L,
                    shiny::selectInput(
                      inputId = ns("param_dcm2niix_float"),
                      label = "Merge 2D slices from same series regardless of echo, exposure, etc.",
                      choices = c("Yes", "No"),
                      selected = "Yes"
                    )
                  ),
                  shiny::column(
                    width = 2L,
                    shiny::selectInput(
                      inputId = ns("param_dcm2niix_crop"),
                      label = "Crop 3D acquisitions",
                      choices = c("Yes", "No", "Ignore"),
                      selected = "No"
                    )
                  )

                ),

                shiny::hr(),

                shiny::div(
                  class = "float-right",
                  shiny::div(
                    local({
                      if(dry_run) {
                        NULL
                      } else {
                        shiny::actionButton(ns("btn_dcm2niix_run"), "Run from RAVE")
                      }
                    }),
                    dipsaus::actionButtonStyled(ns("btn_dcm2niix_copy"), "Save & run by yourself")
                  )
                )
              )
            ),

            ravedash::output_card(
              title = "Surface Reconstruction",
              start_collapsed = TRUE,
              tools = list(
                shidashi::as_badge("requires `FreeSurfer`|bg-yellow")
              ),
              append_tools = FALSE,
              class_foot = "no-padding",
              footer = shiny::uiOutput(ns("panel_fs_recon")),
              shiny::div(
                "RAVE electrode localization depend on the ",
                shiny::pre(class="pre-compact no-padding display-inline", "FreeSurfer"),
                " outputs. ",
                "If you do not wish to reconstruct the surface files, which takes hours to run, ",
                "please select ",
                shiny::pre(class="pre-compact no-padding display-inline", "-autorecon1"),
                " in the ",
                shiny::pre(class="pre-compact no-padding display-inline", "Recon flag"),
                " drop-down menu. It usually takes around 10 minutes to run with this flag, ",
                "and several hours to run the rests. ",
                "For detailed documentation, please check the ",
                shiny::a(href = "https://surfer.nmr.mgh.harvard.edu/fswiki/recon-all",
                         target="_blank", "FreeSurfer website"), ".",
                shiny::br(),
                "* The script requires Unix ",
                shiny::pre(class="pre-compact no-padding display-inline", "bash"),
                " terminals. If you are using Windows, ",
                "please install and use the Linux sub-system.",

                shiny::hr(),

                shiny::fluidRow(

                  shiny::column(
                    width = 7L,
                    shiny::div(
                      shiny::selectInput(
                        inputId = ns("param_fs_infile"),
                        label = "MRI file",
                        choices = character(0L)
                      ),
                      shiny::actionLink(
                        inputId = ns("param_fs_refresh"),
                        label = "Refresh"
                      )
                    )
                  ),
                  shiny::column(
                    width = 5L,
                    shiny::selectInput(
                      inputId = ns("param_fs_steps"),
                      label = "Recon flag",
                      choices = autorecon_flags,
                      selected = "-all"
                    ),
                    shiny::checkboxInput(
                      inputId = ns("param_fs_fresh_start"),
                      label = "Remove existing work before running the command (if applicable)",
                      value = FALSE
                    )
                  )
                ),

                shiny::hr(),

                shiny::div(
                  class = "float-right",
                  shiny::div(
                    local({
                      if(dry_run) {
                        NULL
                      } else {
                        shiny::actionButton(ns("btn_recon_run"), "Run from RAVE")
                      }
                    }),
                    dipsaus::actionButtonStyled(ns("btn_recon_copy"), "Save & run by yourself")
                  )
                )
              )
            ),

            ravedash::output_card(
              title = "Co-registration CT to T1",
              start_collapsed = TRUE,
              tools = list(
                shidashi::as_badge("requires `FSL-flirt`|bg-yellow")
              ),
              append_tools = FALSE,
              class_foot = "no-padding",
              footer = shiny::uiOutput(ns("panel_coreg")),
              shiny::div(
                "* The script requires Unix ",
                shiny::pre(class="pre-compact no-padding display-inline", "bash"),
                " terminals. If you are using Windows, ",
                "please install and use the Linux sub-system.",

                shiny::hr(),

                shiny::fluidRow(

                  shiny::column(
                    width = 7L,
                    shiny::div(
                      shiny::selectInput(
                        inputId = ns("param_coreg_ct"),
                        label = "CT file",
                        choices = character(0L)
                      ),
                      shiny::actionLink(
                        inputId = ns("param_coreg_refresh"),
                        label = "Refresh"
                      )
                    )
                  )
                ),

                shiny::hr(),

                shiny::div(
                  class = "float-right",
                  shiny::div(
                    local({
                      if(dry_run) {
                        NULL
                      } else {
                        shiny::actionButton(ns("btn_coreg_run"), "Run from RAVE")
                      }
                    }),
                    dipsaus::actionButtonStyled(ns("btn_coreg_copy"), "Save & run by yourself")
                  )
                )
              )
            )

          )
        )
      )
    )
  )


}
