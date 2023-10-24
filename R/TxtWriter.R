# Txt writer
save_txt <- function(filename, info_to_write){
  sink(filename)
  print(info_to_write)
  sink()
}
