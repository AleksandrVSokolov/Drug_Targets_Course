setwd("...")
My_wd_path = "..."
My_drugs = readLines('https://raw.githubusercontent.com/AleksandrVSokolov/Drug_Targets_Course/main/Approved_drugs.txt')
My_drugs = My_drugs[sample(x = 1:771, size = 50, replace = FALSE)]
'%!in%' = function(x,y){!('%in%'(x,y))}

############# URL inspection function ############# 
valid_url = function(URL, t = 2){
  Connection = url(URL)
  Check = suppressWarnings(try(open.connection(Connection, open = "rt", timeout = t), silent = TRUE)[1])
  suppressWarnings(try(close.connection(Connection), silent = TRUE))
  ifelse(is.null(Check), TRUE, FALSE)
}


############# Test ############# 
valid_url('https://www.google.com/')
valid_url('https://www.not_existing_nonsense.com/')


############# Package installation and import ############# 
install.packages('gdata')
install.packages('stringi')
install.packages('stringr')
install.packages('wordcloud2')
install.packages('webshot')
install.packages('htmlwidgets')
library('gdata')
library('stringi')
library('stringr')
library('wordcloud2')
library('webshot')
library('htmlwidgets')


############# Main script part ############# 

# Preparing search terms
Search_terms = My_drugs

# Curating search terms
Search_terms = str_trim(Search_terms)
Search_terms = Search_terms[Search_terms!= '']
Search_terms = Search_terms[!is.na(Search_terms)]
Search_terms = toupper(Search_terms)
Search_terms = unique(Search_terms)

# Save curated terms in the backup variable
Terms_initial = Search_terms

# Preparing main URL requests
Search_terms = stri_replace_all_fixed(str = Search_terms, pattern = ' ', replacement = '+')
Base_string_1_main = 'https://clinicaltrials.gov/ct2/results/download_fields?term='
Base_string_2_main = '&type=Intr&down_count=10000&down_fmt=csv&down_chunk=1&down_flds=all'
URLs = paste0(Base_string_1_main, Search_terms, Base_string_2_main)

# Preparing secondary URLs to check the validity of the request
Base_string_1_check = 'https://clinicaltrials.gov/ct2/results?cond=&term='
Base_string_2_check = '&type=Intr'
URLs_check = paste0(Base_string_1_check, Search_terms, Base_string_2_check)


# The main loop
index = 1
while (index <= length(URLs)){
  if (valid_url(URLs_check[index])){
    Destination = paste0(folder, '/', Terms_initial[index],'.csv')
    Error_flag = FALSE
    tryCatch({download.file(URLs[index], Destination)}
             , error = function(e) {Error_flag <<- TRUE})
    if (Error_flag){
      warning(paste0(index,'__','ERROR_DETECTED', '__at__', Sys.time()), immediate. = TRUE)
      Sys.sleep(5)
      download.file(URLs[index], Destination)
    }
  }
  percent = round((index/length(URLs))*100, 2)
  writeLines(paste0(index, '__', percent,'_%_complete', '__at__', Sys.time()))
  index = index + 1
  Sys.sleep(1)
}

# File detection
Files = list.files(folder)
if(length(Files) < 1){print('0 trials found')}

# File uploading
Paths = paste0(folder, '/', Files)
Files_list = lapply(Paths, function(x) read.csv(x, header = TRUE, encoding = 'UTF-8'))
Trial_data = do.call(rbind, Files_list)

# Preparing conditions
Conditions = Trial_data$Conditions
Conditions = stri_split_fixed(str = Conditions, pattern = '|')
Conditions = unlist(Conditions)

# Preparing frequency table
Frequency_table = as.data.frame(table(Conditions))
Frequency_table = Frequency_table[order(Frequency_table$Freq, decreasing = TRUE),]
Frequency_table = Frequency_table[1:100,]

# Saving widget
widget = wordcloud2(Frequency_table, size = 1, shape = 'cicular', color = 'random-dark', rotateRatio = 0.9, gridSize = 4, shuffle = TRUE)
saveWidget(widget, "tmp.html", selfcontained = FALSE)
webshot("tmp.html", "wordcloud.png", delay = 10, vwidth = 2000, vheight = 2000)
