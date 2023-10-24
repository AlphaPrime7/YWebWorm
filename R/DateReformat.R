# 1. MAIN
date_reformat <- function(x, form = NULL){
  date_reformat_df <- data.frame()
  for(i in x){
    date_split <- str_split(i, boundary("word")) #produces a 2 dimensional array
    for (j in date_split) {
      new_date <- paste0(as.numeric(Mo2Num(j[1])), sep = "/", as.numeric(j[2]), sep = "/", as.numeric(j[3]))
      date_reformat_df <- rbind(date_reformat_df, new_date)
    }
  }
  colnames(date_reformat_df) <- c("reformated_date")
  if(is.null(form)){
    return(date_reformat_df)
  } else if(form == "R" && !is.null(form)){
    date_reformat_df <- as.data.frame(lapply(date_reformat_df[1:ncol(date_reformat_df)], string_date_reformat))
    return(date_reformat_df)
  }
}

#test
review_dates_R <- date_reformat(review_dates, form = "R")
review_dates_nonR <- date_reformat(review_dates)
date_reformat(review_dates, form = "R")
for (i in review_dates_nonR[,1]) {
  print(class(i))
}
for (i in review_dates_R[,1]) {
  print(class(i))
}
# 2. SUBORDINATE
Mo2Num <- function(x) match(tolower(x), tolower(month.abb))

#test
Mo2Num(c("jan", "JAN", "Feb", "SEP"))

# 3. SUBORDINATE
string_date_reformat <- function(x){
  as.Date(x, format="%m/%d/%Y")
}

#PROJECT-THINK OF ALL THE POSSIBLE DATE FORMATS