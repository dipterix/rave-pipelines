`%OF%` <- dipsaus::`%OF%`

path_is_valid <- function(path, dir_ok = FALSE) {
  if(length(path) == 1 && !is.na(path) && file.exists(path)) {
    if(dir.exists(path)) {
      if(dir_ok) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    } else {
      return(TRUE)
    }
  }
  return(FALSE)
}
