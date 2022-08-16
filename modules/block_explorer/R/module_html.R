

module_html <- function(){

  shiny::fluidPage(
    shiny::fluidRow(

      shiny::column(
        width = 3L,
        shiny::div(
          # class = "row fancy-scroll-y stretch-inner-height",
          class = "row screen-height overflow-y-scroll padding-bottom-70",
          shiny::column(
            width = 12L,

            electrode_selector$ui_func(),

            comp_analysis_ranges$ui_func()

          )
        )
      ),

      shiny::column(
        width = 9L,
        shiny::div(
          class = "row screen-height overflow-y-scroll padding-bottom-70 output-wrapper",
          shiny::column(
            width = 12L,
            shidashi::card_tabset(
              inputId = ns("output_tab"),
              title = "",
              class_body = "fill-height padding-5",
              tools = list(
                card_tool(
                  widget = "collapse"
                ),
                card_tool(
                  widget = "maximize"
                )
              ),
              `Single electrode` = shiny::div(
                class = "fill-height min-height-vh70 height-700 resize-vertical",
                shiny::div(
                  class = "row fill-height",
                  shiny::div(
                    class = "col-sm-6 fill-height",
                    ravedash::group_box(
                      title = "Overview",
                      style = "height: calc(100% - 3rem);",
                      shiny::div(
                        class = "fill",
                        shiny::div(
                          style = "height: 40%;",
                          ravedash::output_gadget_container(
                            shiny::plotOutput(
                              ns("plot_single_overall_power"), width = '100%',
                              height = "100%",
                              brush = shiny::brushOpts(
                                id = ns("plot_single_overall_power__brush"),
                                direction = "xy",
                                clip = TRUE,
                                delayType = "debounce",
                                delay = 300,
                                opacity = 0.25,
                                resetOnNew = FALSE
                              )
                            )
                          )
                        ),
                        shiny::div(
                          style = "height: 30%;",
                          ravedash::output_gadget_container(
                            shiny::plotOutput(
                              ns("plot_single_overall_voltge"), width = '100%',
                              height = "100%")
                          )
                        ),
                        shiny::div(
                          style = "height: 30%;",
                          ravedash::output_gadget_container(
                            shiny::plotOutput(
                              ns("plot_single_overall_pwelch"), width = '100%',
                              height = "100%")
                          )
                        )
                      )
                    )
                  ),

                  shiny::div(
                    class = "col-sm-6 fill-height",
                    ravedash::group_box(
                      title = "Selected subset",
                      style = "height: calc(100% - 3rem);",
                      shiny::div(
                        class = "fill",
                        shiny::div(
                          style = "height: 40%;",
                          ravedash::output_gadget_container(
                            shiny::plotOutput(
                              ns("plot_single_sub_power"), width = '100%',
                              height = "100%"
                            )
                          )
                        ),
                        shiny::div(
                          style = "height: 30%;",
                          ravedash::output_gadget_container(
                            shiny::plotOutput(ns("plot_single_sub_voltge"), width = '100%',
                                              height = "100%")
                          )
                        ),
                        shiny::div(
                          style = "height: 30%;",
                          ravedash::output_gadget_container(
                            shiny::plotOutput(ns("plot_single_sub_pwelch"), width = '100%',
                                              height = "100%")
                          )
                        )
                      )
                    )
                  )

                )
              ),


              `Batch visualization` = shiny::div(
                class = "fill-height height-700 resize-vertical",
                shiny::div(
                  class = "row fill-height",
                  shiny::div(
                    class = "col-sm-6 fill-height",
                    ravedash::group_box(
                      title = "as"
                    )
                  )
                )
              )
            )
            # ravedash::output_card(
            #   'Collapsed over frequency',
            #   class_body = "no-padding fill-width height-450 min-height-450 resize-vertical",
            #   shiny::div(
            #     class = 'position-relative fill',
            #     shiny::plotOutput(ns("collapse_over_trial"), width = '100%', height = "100%")
            #   )
            # )
          )
        )
      )

    )
  )
}
