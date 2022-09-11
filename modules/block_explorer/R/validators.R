# validators

validator_bandpass <- function(input, output, session) {
  sv <- shinyvalidate::InputValidator$new(session = session)
  # sv$add_rule(inputId = "filter_bandpass__filter_order", rule = function(value) {
  #   sample_rate <- component_container$data$sample_rate
  #   if(length(sample_rate) != 1) { return() }
  #   max_rate <- floor((sample_rate - 1) / 3)
  #   if(!length(value) || is.na(value) || value < 1 || value != round(value)) {
  #     return("Filter order must be a positive integer")
  #   }
  #   if(value > max_rate) {
  #     return(sprintf("Filter order must be smaller or equal to 1/3 of sample rate [%s]", sample_rate))
  #   }
  # }, session. = session)
  sv$add_rule(inputId = "filter_bandpass__freq_lb", rule = function(value) {
    sample_rate <- component_container$data$sample_rate
    if(length(sample_rate) != 1) { return() }
    max_rate <- floor(sample_rate / 2)
    if(!length(value) || is.na(value) || value < 0 ) {
      return("Band-passing filter must have positive lower-bound")
    }
    if(value > max_rate) {
      return(sprintf("Band-passing filter must have frequency less-equal than 1/2 of sample rate [%s]", sample_rate))
    }
  })
  sv$add_rule(inputId = "filter_bandpass__freq_ub", rule = function(value) {
    sample_rate <- component_container$data$sample_rate
    if(length(sample_rate) != 1) { return() }
    max_rate <- floor(sample_rate / 2)
    if(!length(value) || is.na(value) || value <= input$filter_bandpass__freq_lb ) {
      return("Band-passing filter upper-bound must be greater than the lower-bound")
    }
    if(value > max_rate) {
      return(sprintf("Band-passing filter must have frequency less-equal than 1/2 of sample rate [%s]", sample_rate))
    }
  })
  shiny::bindEvent(
    ravedash::safe_observe({
      if(isTRUE(input$filter_bandpass__enabled)) {
        sv$enable()
      } else {
        sv$disable()
      }
    }),
    input$filter_bandpass__enabled,
    ignoreNULL = TRUE, ignoreInit = FALSE
  )
  sv
}

validator_notch <- function(input, output, session) {
  sv <- shinyvalidate::InputValidator$new(session = session)
  sv$add_rule(inputId = "filter_notch__base_frequency", rule = function(value) {
    sample_rate <- component_container$data$sample_rate
    if(length(sample_rate) != 1) { return() }
    max_rate <- floor((sample_rate - 1) / 2)
    if(!length(value) || is.na(value) || value <= 0) {
      return("Base-frequency must be positive")
    }
    if(value > max_rate) {
      return(sprintf("Filter order must be smaller than 1/2 of sample rate [%s]", sample_rate))
    }
  }, session. = session)
  sv$add_rule(inputId = "filter_notch__harmonics", rule = function(value) {
    sample_rate <- component_container$data$sample_rate
    if(length(sample_rate) != 1) { return() }
    max_rate <- floor(sample_rate / 2)

    tms <- dipsaus::parse_svec(value, unique = FALSE)

    if(!length(tms) || any(tms <= 0) || any(duplicated(tms)) ) {
      return("Filter harmonics must be positive, non-duplicated values separated by comma")
    }
    base_freq <- input$filter_notch__base_frequency
    if(any(base_freq * tms > max_rate)) {
      return(sprintf("Base frequency x harmonics must be less-equal than 1/2 of sample rate [%s]", sample_rate))
    }
  })
  sv$add_rule(inputId = "filter_notch__bandwidths", rule = function(value) {
    sample_rate <- component_container$data$sample_rate
    if(length(sample_rate) != 1) { return() }
    max_rate <- floor(sample_rate / 2)

    base_freq <- input$filter_notch__base_frequency
    tms <- dipsaus::parse_svec(input$filter_notch__harmonics, unique = FALSE)
    half_bw <- dipsaus::parse_svec(value, unique = FALSE)

    if( length(half_bw) != length(tms) ) {
      return("Filter bandwidths must have the same length as the number of harmonics, separated by comma")
    }

    if(any(half_bw <= 0) ) {
      return("Filter bandwidths must be all positive, separated by comma")
    }

    if(any(base_freq * tms + half_bw > max_rate)) {
      return(sprintf("Base frequency x harmonics + half-bandwidth must be less-equal than 1/2 of sample rate [%s]. Try reducing the bandwidth", sample_rate))
    }
    if(any(base_freq * tms - half_bw <= 0)) {
      return("Base frequency x harmonics - half-bandwidth must be positive. Try reducing the bandwidth")
    }
  })
  shiny::bindEvent(
    ravedash::safe_observe({
      if(isTRUE(input$filter_notch__enabled)) {
        sv$enable()
      } else {
        sv$disable()
      }
    }),
    input$filter_notch__enabled,
    ignoreNULL = TRUE, ignoreInit = FALSE
  )
  sv
}

