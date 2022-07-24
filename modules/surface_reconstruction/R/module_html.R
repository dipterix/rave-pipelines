

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
              # class_foot = "no-padding",
              append_tools = FALSE,
              footer = shiny::tagList(
                shiny::tags$details(
                  shiny::tags$summary("Terminal script - Import T1 MRI"),
                  shiny::uiOutput(ns("panel_import_T1"), container = shiny::p)
                ),
                shiny::tags$details(
                  shiny::tags$summary("Terminal script - Import CT"),
                  shiny::uiOutput(ns("panel_import_CT"), container = shiny::p)
                )
              ),
              shiny::div(
                "The following script uses ", shiny::pre(class="pre-compact no-padding display-inline", "dcm2niix"),
                " external library to convert DICOM images to Nifti format for later purposes. ",
                shiny::br(),
                "* The script requires Unix ",
                shiny::pre(class="pre-compact no-padding display-inline", "bash"),
                " terminal. If you are using Windows, please use Window sub-Linux system (WSL2).",

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
                        shiny::tagList(
                          shiny::actionButton(ns("btn_dcm2niix_run_t1"), "Run from RAVE (T1 MRI)"),
                          shiny::actionButton(ns("btn_dcm2niix_run_ct"), "Run from RAVE (CT)")
                        )
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
              # class_foot = "no-padding",
              footer = shiny::tags$details(
                shiny::tags$summary("Terminal script - FreeSurfer recon-all"),
                shiny::uiOutput(ns("panel_fs_recon"), container = shiny::p)
              ),
              shiny::div(
                "RAVE electrode localization depend on the ",
                shiny::pre(class="pre-compact no-padding display-inline", "FreeSurfer"),
                " outputs. The whole complete reconstruction will take several hours to run. ",
                "If you want to save time, please select flag: ",
                shiny::pre(class="pre-compact no-padding display-inline", "-autorecon1"),
                "This procedure only normalizes MRI and strips skulls, ",
                "hence only takes around 10 minutes. ",
                "For detailed documentation, please check the ",
                shiny::a(href = "https://surfer.nmr.mgh.harvard.edu/fswiki/recon-all",
                         target="_blank", "FreeSurfer website"), ".",
                shiny::br(),
                "* The script requires Unix ",
                shiny::pre(class="pre-compact no-padding display-inline", "bash"),
                " terminals. If you are using Windows, ",
                "please use Window sub-system for Linux [WSL2].",

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
                      selected = autorecon_flags[[1]]
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
                shidashi::as_badge("requires `AFNI or FSL`|bg-yellow")
              ),
              append_tools = FALSE,
              # class_foot = "no-padding",
              footer = shiny::tags$details(
                shiny::tags$summary("Terminal script - CT MRI co-registration"),
                shiny::uiOutput(ns("panel_coreg"), container = shiny::p)
              ),
              shiny::div(
                "* The script requires Unix ",
                shiny::pre(class="pre-compact no-padding display-inline", "bash"),
                " terminals. If you are using Windows, ",
                "please use the Windows sub-system for Linux (WSL2).",

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
                  ),

                  shiny::column(
                    width = 5L,
                    shiny::div(
                      shiny::selectInput(
                        inputId = ns("coreg_ct_program"),
                        label = "Program",
                        choices = c("AFNI", "FSL")
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
