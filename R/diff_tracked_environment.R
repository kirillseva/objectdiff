#' Record what changes have been made to a tracked environment.
#'
#' @inheritParams objectdiff
#' @include objectdiff.R tracked_environment.R
setMethod('objectdiff', signature = c('tracked_environment', 'tracked_environment'),
  definition = function(old_object, new_object) {
    if (!identical(old_object, new_object))
      stop("tracked_environments can only be diffed against themselves")

    deletions <- setdiff(new_object%$%universe, ls(new_object, all = TRUE))

    if (length(deletions) == 0 &&
        (num_changed <- length(ls(new_object%$%ghost, all = TRUE))) == 0) {
      return(identity_patch()) # No changes!
    }

    patch <- function(object) { }
    append_body <- function(patch, line) {
      eval.parent(substitute(body(patch)[[length(body(patch)) + 1]] <- quote(line)))
    }

    if (length(deletions) > 0) {
      append_body(patch, rm(list = deletions, envir = object))
      environment(patch)$deletions <- deletions
    }

    if (num_changed > 0) {
      ghost <- new_object%$%ghost
      additions <- setdiff(ls(new_object, all = TRUE), new_object%$%universe)
      changed_objects <- setdiff(ls(ghost, all = TRUE), c(deletions, additions))
      change_patches <- setNames(nm = changed_objects,
        lapply(changed_objects, function(obj) {
          objectdiff(ghost[[obj]], new_object[[obj]])
        }))

      if (length(changed_objects > 0)) {
        append_body(patch, for (patch in names(change_patches)) {
          object[[patch]] <- change_patches[[patch]](object[[patch]])
        })
        environment(patch)$change_patches <- change_patches
      }

      if (length(additions) > 0) {
        append_body(patch, for (obj in names(new_objects)) {
          object[[obj]] <- new_objects[[obj]]
        })
        environment(patch)$new_objects <- setNames(nm = additions,
          lapply(additions, function(name) new_object[[name]]))
      }
    }

    append_body(patch, object)

    as.patch(patch)
  })

 
