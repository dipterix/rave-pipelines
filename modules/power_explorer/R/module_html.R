



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

                        ravedash::input_card("Baseline",
                                             shiny::sliderInput(ns("baseline_window"), "Window", value = c(0,1),
                                                                min =0, max=1, step = .1, dragRange = TRUE),
                                             shiny::selectInput(ns("baseline_scope"), label = 'Baseline Scope',
                                                                selected=get_baseline_scope(names=TRUE)[1],
                                                                choices =get_baseline_scope(names=TRUE)),
                                             shiny::selectInput(ns("baseline_unit"), label = 'Unit of Analysis',
                                                                selected=get_unit_of_analysis(names=TRUE)[1],
                                                                choices =get_unit_of_analysis(names=TRUE))
                        ),

                        ravedash::input_card(class_header = "shidashi-anchor",
                                             title = "Analysis windows",
                                             dipsaus::compoundInput2(inputId = ns('ui_analysis_settings'),
                                                                     label = "Analysis Window",
                                                                     initial_ncomp = 1L, min_ncomp = 1L, max_ncomp = 5L,
                                                                     components = shiny::div(
                                                                         shiny::textInput(inputId = "label", label = "Label"),
                                                                         shiny::selectInput(inputId = "event", label = "Event", choices=NULL,selected = NULL),
                                                                         shiny::sliderInput(inputId = "time", label = "Time", min=0,max=1,value = c(0,1),step = .1),
                                                                         shiny::sliderInput(inputId = "frequency", label = "Frequency", min=0,max=200,value = c(70,100), step = 1)
                                                                     )
                                             )
                        ),

                        ravedash::input_card(class_header = "shidashi-anchor",
                                             title = "First grouping factor",
                                             dipsaus::compoundInput2(inputId = ns('first_condition_groupings'),
                                                                     label = "Trial Group", initial_ncomp = 1L, min_ncomp = 1L,
                                                                     max_ncomp = 15L,
                                                                     components = shiny::div(
                                                                         shiny::textInput(inputId = "label", label = "Label"),
                                                                         shiny::selectInput(inputId = "conditions", label = "Conditions", choices = "", multiple = TRUE)))
                        )


                        # baseline_choices$ui_func(),
                        # comp_analysis_ranges$ui_func()
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
                            title = "By Electrode",
                            class_body = "no-padding fill-width height-450 min-height-450 resize-vertical",
                            tools = list(
                                shidashi::card_tool(widget = "collapse")#,
                                # shidashi::card_tool(widget = "flip")
                            ),
                            `Over time` = shiny::plotOutput(ns('activity_over_time_by_electrode')),
                            `Statistics` = shiny::plotOutput(ns('per_electrode_statistics'))
                        ),
                        shidashi::card_tabset(
                            title='By Frequency',
                            class_body = "no-padding fill-width height-450 min-height-450 resize-vertical",
                            tools = list(shidashi::card_tool(widget = "collapse")),
                            `Over time` = shiny::plotOutput(ns("activity_over_time_by_frequency")),
                            `Correlations` = shiny::plotOutput(ns("frequency_correlation_plot"))
                        )
                        #   `card with flip` = shidashi::flip_box(
                        #     front = shidashi::info_box("Side A"),
                        #     back = shidashi::info_box("Side B"),
                        #     inputId = 'flip_box1')
                        # ),
                    )
                )
            )

        )
    )
}
