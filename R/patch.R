
identity_patch <- function() {
  patch <- function(...) ..1
  environment(patch) <- emptyenv()
  as.patch(patch)
}

trivial_patch <- function(object) as.patch(function(...) object)

#' Generate a patch for two atomic objects that are close in values.
#'
#' @param old_object atomic. 
#' @param new_object atomic. 
#' @param transition logical. Whether or not to use a transition depending
#'   on how many element do not match. Namely, if over 50% do not match in
#'   from a random sample of 100 elements (so most of \code{new_object} is
#'   probably different than \code{old_object}) then replace it completely
#'   with a trivial patch; otherwise, perform a more subtle calculation
#'   using \code{base::!=} and stores only exactly which elements changed.
#' @examples
#' x <- 1:10; y <- x; y[1] <- 5
#' patch <- objectdiff:::atomic_differences_patch(x, y) 
#' stopifnot(identical(y, patch(x)))
atomic_differences_patch <- function(old_object, new_object, transition = TRUE) {
  # Our first strategy is to sample 100 values and compare them.
  # If they match, the objects are "probably" the same.
  if (!isTRUE(transition))
    differences_patch(new_object, old_object, old_object != new_object)
  else {
    tested_indices <- sample(seq_len(length(new_object)), 100, replace = TRUE)

    use_trivial <- FALSE
    if (!identical(old_object[tested_indices], new_object[tested_indices])) {
      differences <- which(old_object[tested_indices] != new_object[tested_indices])
      # If most values are different, just patch with the new object.
      if (mean(differences) > 0.5) use_trivial <- TRUE
    }

    if (use_trivial) trivial_patch(new_object)
    else { # objects differ by a non-100% amount. Patch the differences.
      # TODO: (RK) Can we make this faster with C++? Need to be careful about 
      # attributes and class.
      differences_patch(new_object, old_object, old_object != new_object)
    }
  }
}

#' Generate a patch given a recording of differences between two objects.
#'
#' This patch will use another patch to record changes to attributes
#' (if any). Otherwise, given indices of changed object, it will 
#' generate a patch over those indices.
#' 
#' @inheritParams atomic_differences_patch
#' @param differences logical. The differences in first and second object.
#'   These should be calculated externally because a different approach
#'   could be used for different objects (e.g., lists versus atomic;
#'   in the former we would need \code{base::identical} on each element,
#'   whereas in the latter we could use \code{base::`!=`}).
differences_patch <- function(old_object, new_object, differences) {
  if (sum(differences) == 0) {
    # Patch only attributes / class.
    attributes_patch(old_object, new_object)
  } else {
    patch <- new('function')
    formals(patch) <- alist(object = )
    body(patch) <- quote({ object[differences] <- new_values })
    environment(patch) <- new.env(parent = baseenv())
    environment(patch)$new_values <- new_object[differences]
    environment(patch)$differences <- differences

    # If attributes do not match, patch the attributes as well.
    if (!identical(attributes(new_object), attributes(old_object))) {
      body(patch)[[3]] <- quote(patch_attributes(object))
      environment(patch)$patch_attributes <-
        attributes_patch(old_object, new_object)
    } else body(patch)[[3]] <- quote(object)

    as.patch(patch)
  }
}

#' Assume two objects are identical and only patch their attributes.
#'
#' @inheritParams atomic_differences_patch
attributes_patch <- function(old_object, new_object) {
  patch <- function(object) {
    attributes(object) <- patch_attributes(attributes(object))
    object
  }
  environment(patch) <- new.env(parent = baseenv())
  environment(patch)$patch_attributes <-
    objectdiff(attributes(old_object), attributes(new_object))
  as.patch(patch)
}

#' @rdname patch
as.patch <- function(x) {
  stopifnot(is.function(x))
  class(x) <- c('patch', class(x))
  x
}