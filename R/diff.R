#' Compute a patch that takes a diff of a recursive object (list or tracked_environment).
#'
#' @param old_object list or tracked_environment.
#' @param new_object list or tracked_environment.
diff <- function(old_object, new_object) {
  list(deletions = deletions, modifications = modifications, additions = additions) %>%
    invoke(old_object, new_object) %>%
    Filter(f = Negate(is.identity_patch)) %>%
    map_call(compose) %>%
    as.patch()
}

#' Compute a patch of deletions on a recursive object.
#' 
#' @inheritParams objectdiff
#' @export
deletions <- function(old_object, new_object) {
  UseMethod("deletions")
}

#' Compute a patch of modifications on a recursive object.
#' 
#' @inheritParams objectdiff
#' @export
modifications <- function(old_object, new_object) {
  UseMethod("modifications")
}

#' Compute a patch of additions on a recursive object.
#' 
#' @inheritParams objectdiff
#' @export
additions <- function(old_object, new_object) {
  UseMethod("additions")
}

