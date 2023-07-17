with_docstring <- function(f) {
  doc <- body(f)[2L][[1L]]
  attr(f, "__doc__") <- structure(`if`(is.character(doc), doc, ""),
                                  class = "docstring")
  class(f) <- c("with_docstring", class(f))
  return(f)
}
print.docstring <- function(x, ...) {
  cat(x)
  invisible(x)
}

help <- function() {
  UseMethod("help")
}
help.default <- utils::help
help.with_docstring <- function() {
  print(attr(topic, "__doc__", exact = TRUE))
}
formals(help) <-
  formals(help.with_docstring) <- formals(utils::help)
