#FTP NOTES

library(RCurl)

ftp_url <- "ftp://cran.r-project.org/pub/R/web/packages/normfluodbf/"

get_files <- getURL(ftp_url, dirlistonly = FALSE)
extracted_file_names <- str_split(get_files, "\r\n")[[1]]
extracted_html_filenames <- unlist(str_extract_all(extracted_file_names, ".+(.html)"))


curlhandle <- getCurlHandle(ftp.use.epsv = FALSE)

ftpdownloader <- function(filename, folder, handle){
  
  dir.create(folder, showWarnings = FALSE)
  
  fileurl <- str_c(ftp_url, filename)
  
  if(!file.exists(str_c(folder,"/",filename))){
    
    file_name <- try(getURL(fileurl, curl = handle))
    
    write(file_name, str_c(folder,"/", filename))
    
    Sys.sleep(1)
    
  }
}

l_ply(extracted_html_filenames, ftpdownloader, folder="ftpfunctionfolder", handle=Curlhandle )


