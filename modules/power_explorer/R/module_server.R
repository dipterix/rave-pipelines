module_server <- function(input, output, session, ...){


    # Local reactive values, used to store reactive event triggers
    local_reactives <- shiny::reactiveValues(
        update_outputs = NULL
    )

    # Local non-reactive values, used to store static variables
    local_data <- dipsaus::fastmap2()

    # get server tools to tweek
    server_tools <- get_default_handlers(session = session)

    # Run analysis once the following input IDs are changed
    # This is used by auto-recalculation feature
    server_tools$run_analysis_onchange(
        component_container$get_input_ids(c(
            "electrode_text"#,
            #"baseline_choices",
            #"analysis_ranges", #"condition_groups"
        ))
    )

    # Register event: main pipeline need to run
    shiny::bindEvent(
        ravedash::safe_observe({

            # Invalidate previous results (stop them because they are no longer needed)
            if(!is.null(local_data$results)) {
                local_data$results$invalidate()
                ravedash::logger("Invalidating previous run", level = "trace")
            }

            # Collect input data
            settings <- component_container$collect_settings(ids = c(
                "electrode_text"
                # "baseline_choices",
                # "condition_groups"#,
                # "analysis_ranges"
            ))

            pipeline$set_settings(
                baseline_settings = list(
                    window=list(input$baseline_window),
                    scope = input$baseline_scope,
                    unit_of_analysis = input$baseline_unit
                ),
                selected_electrodes = settings$analysis_electrodes,
                first_condition_groupings = input$first_condition_groupings,
                analysis_settings = input$ui_analysis_settings
            )

            # print(dput(pipeline$get_settings()))

            #' Run pipeline without blocking the main session
            #' The trick to speed up is to set
            #' `async=TRUE` will run the pipeline in the background
            #' `shortcut=TRUE` will ignore the dependencies and directly run `names`
            #' `names` are the target nodes to run
            #' `scheduler="none"` will try to avoid starting any schedulers and
            #' run targets sequentially. Combined with `callr_function=NULL`,
            #' scheduler's overhead can be removed.
            #' `type="smart"` will start `future` plan in the background, allowing
            #' multicore calculation
            results <- pipeline$run(
                as_promise = TRUE,
                scheduler = "none",
                type = "smart",
                callr_function = NULL,
                progress_title = "Calculating in progress",
                async = FALSE,
                check_interval = 0.1,
                shortcut = FALSE,
                names = c(
                    "settings", 'analysis_checks_passed', 'baselined_power'
                )
            )

            #local_data = list()
            local_data$results <- results
            ravedash::logger("Scheduled: ", pipeline$pipeline_name, level = 'debug', reset_timer = TRUE)

            results$promise$then(
                onFulfilled = function(...){

                    ravedash::logger("Fulfilled: ", pipeline$pipeline_name, level = 'debug')
                    shidashi::clear_notifications(class = "pipeline-error")
                    local_reactives$update_outputs <- Sys.time()
                    return(TRUE)
                },
                onRejected = function(e, ...){
                    msg <- paste(e$message, collapse = "\n")
                    if(inherits(e, "error")){
                        ravedash::logger(msg, level = 'error')
                        ravedash::logger(traceback(e), level = 'error', .sep = "\n")
                        shidashi::show_notification(
                            message = msg,
                            title = "Error while running pipeline", type = "danger",
                            autohide = FALSE, close = TRUE, class = "pipeline-error"
                        )
                    }
                    return(msg)
                }
            )

            local_data$env <- pipeline$eval(names = c('analysis_checks_passed', 'analysis_groups', 'pluriform_power',
                                                      'overall_tf_data', 'tf_correlation_data', 'by_electrode_tf_data',
                                                      'scatter_bar_data', 'analysis_data'))
            local_data$env$analysis_settings
            return()

        }),
        server_tools$run_analysis_flag(),
        ignoreNULL = TRUE, ignoreInit = TRUE
    )


    # (Optional) check whether the loaded data is valid
    shiny::bindEvent(
        ravedash::safe_observe({
            loaded_flag <- ravedash::watch_data_loaded()
            if(!loaded_flag){ return() }
            new_repository <- pipeline$read("repository")
            if(!inherits(new_repository, "rave_prepare_power")){
                ravedash::logger("Repository read from the pipeline, but it is not an instance of `rave_prepare_power`. Abort initialization", level = "warning")
                return()
            }
            ravedash::logger("Repository read from the pipeline; initializing the module UI", level = "debug")

            # check if the repository has the same subject as current one
            old_repository <- component_container$data$repository
            if(inherits(old_repository, "rave_prepare_power")){

                if( !attr(loaded_flag, "force") &&
                    identical(old_repository$signature, new_repository$signature) ){
                    ravedash::logger("The repository data remain unchanged ({new_repository$subject$subject_id}), skip initialization", level = "debug", use_glue = TRUE)
                    return()
                }
            }

            # TODO: reset UIs to default

            # Reset preset UI & data
            component_container$reset_data()
            component_container$data$repository <- new_repository

            #--handle input initialization
            # default preset initialization
            component_container$initialize_with_new_data()

            # custom input init
            # new_repository <- pipeline$read('repository')

            ##--loading the default baseline settings
            baseline_settings <- new_repository$subject$get_default('baseline_settings',
                                                                    default_if_missing = pipeline$get_settings('baseline_settings'), namespace = module_id)

            shiny::updateSliderInput(session = session, inputId = 'baseline_window',
                                     value = unname(unlist(baseline_settings$window)), min = min(new_repository$time_points),
                                     max = max(new_repository$time_points)
            )
            shiny::updateSelectInput(session = session, inputId = 'baseline_scope',
                                     selected = baseline_settings$scope
            )

            ##--loading the default analysis settings
            as <- new_repository$subject$get_default('analysis_settings',
                                                     default_if_missing = pipeline$get_settings('analysis_settings'), namespace = module_id)
            def <- list(
                list(label='A1', event='Trial Onset', time=0:1, frequency=c(70,150))
            )

            if(is.null(as) || !all(sapply(as, function(aa) all(c('event', 'frequency', 'label', 'time') %in% names(aa) )))) {
                as <- def
            }

            n_analysis <- length(as)
            dipsaus::updateCompoundInput2(session = session,
                                          inputId = 'ui_analysis_settings',
                                          initialization = list(
                                              event = list(selected = 'Trial Onset',
                                                           choices=get_available_events(columns=new_repository$epoch$columns)
                                              ),
                                              time = list(min=min(new_repository$time_points), max=max(new_repository$time_points)),
                                              frequency = list(min=min(new_repository$frequency), max=max(new_repository$frequency))
                                          ), value=as, ncomp = n_analysis)


            # shiny::textInput(inputId = "label", label = "Label"),
            # shiny::selectInput(inputId = "event", label = "Event", choices=NULL,selected = NULL),
            # shiny::sliderInput(inputId = "time", label = "Time", min=0,max=1,value = c(0,1),step = .1),
            # shiny::sliderInput(inputId = "frequency", label = "Frequency", min=0,max=200,value = c(70,100), step = 1)


            ## default condition groups
            cond_tbl <- table(new_repository$epoch$table$Condition)
            cond_tbl <- cond_tbl[order(names(cond_tbl))]
            conditions <- names(cond_tbl)
            def <- list(
                list(label = "All Conditions", conditions = conditions)
            )

            val <- new_repository$subject$get_default('first_condition_groupings')
            if (!length(val) || !is.list(val) || !all(val$conditions %in% conditions)) {
                val <- def
            }
            dipsaus::updateCompoundInput2(session = session,
                                          inputId = 'first_condition_groupings',
                                          initialization = list(conditions = list(choices = conditions)),
                                          value = val, ncomp = length(val))

            # Reset outputs
            shidashi::reset_output("collapse_over_trial")
            shidashi::reset_output("activity_over_time_by_electrode")

        }, priority = 1001),
        ravedash::watch_data_loaded(),
        ignoreNULL = FALSE,
        ignoreInit = FALSE
    )



    ###observe color palette changes

    basic_checks <- function() {
        shiny::validate(
            shiny::need(
                length(local_reactives$update_outputs) &&
                    !isFALSE(local_reactives$update_outputs),
                message = "Please run the module first"
            )
        )
        shiny::validate(
            shiny::need(
                isTRUE(local_data$results$valid),
                message = "One or more errors while executing pipeline. Please check the notification."
            )
        )
    }


    # decorator <- function(settings, )


    # Register outputs
    output$activity_over_time_by_electrode <- shiny::renderPlot({
        basic_checks()

        # check if we are in a multiple event situation
        etf_data <- local_data$env$by_electrode_tf_data
        repo <- local_data$env$repository
        analysis_groups <- local_data$env$analysis_groups
        analysis_settings <- local_data$env$analysis_settings
        baseline_settings <- local_data$env$baseline_settings

        if(all(c('data', 'xlab', 'ylab') %in% names(etf_data[[1]]))) {
            draw_many_heat_maps(
                hmaps = etf_data,
                meta_data = list(
                    subject_code = repo$subject$subject_code,
                    groups=analysis_groups,
                    analysis=analysis_settings,
                    baseline=baseline_settings
                ),
                PANEL.LAST = time_frequency_decorator(analysis_window_type = 'line')
            )
        } else {
            # we need to flatten the data before it can be plotted
            maps <- vector('list', length = prod(length(etf_data), length(etf_data[[1]])))
            curr_map = 1
            for(ii in seq_along(etf_data)) {
                for(jj in seq_along(etf_data[[ii]])) {
                    maps[[curr_map]] = etf_data[[ii]][[jj]]
                    curr_map = curr_map + 1
                }
            }

            draw_many_heat_maps(
                hmaps = maps,
                meta_data = list(
                    subject_code = repo$subject$subject_code,
                    groups=analysis_groups,
                    analysis=analysis_settings,
                    baseline=baseline_settings,
                ),
                PANEL.LAST = time_frequency_decorator(analysis_window_type = 'line')
            )
        }
    })

    output$activity_over_time_by_frequency <- shiny::renderPlot({
        basic_checks()

        # check if we are in a multiple event situation
        tf_data <- local_data$env$overall_tf_data
        repo <- local_data$env$repository
        analysis_groups <- local_data$env$analysis_groups
        analysis_settings <- local_data$env$analysis_settings
        baseline_settings <- local_data$env$baseline_settings

        if(1 == length(unique(sapply(analysis_settings, `[[`, 'event')))) {
            draw_many_heat_maps(
                hmaps = tf_data,
                meta_data = list(
                    subject_code = repo$subject$subject_code,
                    groups=analysis_groups,
                    analysis=analysis_settings,
                    baseline=baseline_settings
                ),
                PANEL.LAST = time_frequency_decorator()
            )
        }
    })


    output$frequency_correlation_plot <- shiny::renderPlot({
        basic_checks()

        # check if we are in a multiple event situation
        tf_data <- local_data$env$tf_co
        repo <- local_data$env$repository
        analysis_groups <- local_data$env$analysis_groups
        analysis_settings <- local_data$env$analysis_settings
        baseline_settings <- local_data$env$baseline_settings

        if(1 == length(unique(sapply(analysis_settings, `[[`, 'event')))) {
            draw_many_heat_maps(
                hmaps = tf_data,
                meta_data = list(
                    subject_code = repo$subject$subject_code,
                    groups=analysis_groups,
                    analysis=analysis_settings,
                    baseline=baseline_settings
                ),
                PANEL.LAST = time_frequency_decorator()
            )
        }
    })

    }
