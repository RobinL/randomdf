
categorical <- function(options=rep(c(letters,LETTERS),11), howmany=10) {
  return(sample(options, howmany, replace=TRUE))
}

discrete <- function(options=1:100, howmany=10) {
  return(sample(options, howmany, replace=TRUE))
}

continuous <- function(howmany=10) {
  return(runif(howmany))
}

col_generators <- list(cat=categorical, dis=discrete, con=continuous)

#' @export
random_df_generator <- function(num_columns=10, num_rows=10) {

  list_of_columns <- list()
  col_counts <- rep(0,3)
  names(col_counts) <- names(col_generators)

  for (i in 1:num_columns) {
    this_generator <- sample(col_generators,1)
    basename <- names(this_generator)
    colname <- sprintf("%s_%s",basename, col_counts[basename])
    col_counts[basename] = col_counts[basename] + 1
    col_contents <- this_generator[[basename]](howmany=num_rows)
    list_of_columns[[colname]] <- col_contents
  }

  random_df <- data.frame(list_of_columns)
  random_df
}
