# TODO: (RK) This fails on unnamed lists.
#' @include diff.R

#' @method deletions list
#' @export
deletions.list <- function(old_object, new_object) {
  deletions <- setdiff(names(old_object), names(new_object))

  if (length(deletions) == 0) { identity_patch() }
  else {
    patch_template(list(deletions = deletions), {
      object[setdiff(names(object), deletions)]
    })
  }
}

#' @method modifications list
#' @export
modifications.list <- function(old_object, new_object) {
  additions   <- setdiff(names(new_object), names(old_object))
  changes     <- setdiff(names(new_object), additions)

  if (length(changes) == 0) { identity_patch() } 
  else {
    # FIXME: (RK) Slow!
    if (identical(old_object[changes], new_object[changes])) {
      identity_patch()
    } else {
      patch_template(list(changed = changes, changes = new_object[changes]), {
        object[changed] <- changes
        object
      })
    }
  }
}

#' @method additions list
#' @export
additions.list <- function(old_object, new_object) {
  additions   <- setdiff(names(new_object), names(old_object))

  if (length(additions) == 0) { identity_patch() }
  else {
    patch_template(list(newnames = additions, additions = new_object[additions]), {
      object[newnames] <- additions
      object
    })
  }
}

