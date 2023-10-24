change_file <- function(filetochange){

  lines <- readLines(filetochange)
  lines[1] <- paste0("var url ='", url ,"';")
  writeLines(lines, filetochange)
}