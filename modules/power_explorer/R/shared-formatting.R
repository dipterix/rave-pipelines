


pretty_round <- function(x, allow_negative_round=FALSE) {
    max_x <- max(abs(x))
    dig = 0
    if(max_x < 1) {
        dig = abs(floor(log10(max_x)))
    }
    if(allow_negative_round && max_x > 100 ) {
        dig = -1
    }

    round(x, dig)
}
