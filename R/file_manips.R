#' Split a file into multiple sheets/files
#'
#' With a file or dataframe as input, possibility to ssplit it on a given variable and write to disk
#'
#' @param file input file path or a dataframe
#' @param out_file output file path(s)
#' @param into one of "sheets" or "files"
#' @return A File is written on provided path(s)
#' @examples
#'
#' split_file_into(iris, out_file = "iris_splitted.xlsx", split_by = "Species", into = "sheets")
#' split_file_into(iris, out_file = "iris_splitted_%s.xlsx", split_by = "Species", into = "files")
#'
#' # Giving a file path works too
#' rio::export(iris, "iris.xlsx")
#' split_file_into("iris.xlsx", out_file = "iris_splitted_%s.xlsx", split_by = "Species", into = "files")
#'
#' # takes a single file and splits it by the column split_by.
#' # then write multiple files or a single file with multiple sheets
#'
#' @export

split_file_into <- function(df, out_file = NULL, split_by = NULL, into = "sheets"){
  if( is.character(df) ){df <- rio::import(df)}

  df <- dplyr::group_by_at(df, split_by)
  df <- tidyr::nest(df)

  X <- purrr::set_names(df[["data"]], dplyr::pull(df, split_by))

  switch(into,
         "files" = rio::export_list(X, file = out_file),
         "sheets" = rio::export(X, file = out_file))
}
