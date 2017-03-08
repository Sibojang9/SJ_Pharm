### --------- Function List ------------#######

print ('Sibo Functions loaded')

sibo_functions <- function()  {
 print ('read_csv_in_NM')
 print ('write_csv_comm')
}




##------------- write csv with comments on the first row ---------------##
write_csv_comm <- function(dataset,filename,comments='No Comments')  {
  head_com <- paste('#',comments)      # add '#' for NONMEM to ignore
  write(head_com ,filename)                   # add comments first
  write(colnames(dataset),filename,ncolumns=ncol(dataset),append = TRUE,sep=',' )
  write_csv(dataset,filename,append = TRUE, na='.')
}


read_csv_in_NM <- function(mod_path)  {
    if (!file.exists(mod_path) )   {winDialog( "ok", "Mod File Dose Not Exist") }
  ##------------------- find csv path inside mod --------------------##
  csv_name <- read_file(mod_path) %>% str_extract(., "FILE=\\S+[Cc][Ss][Vv]") %>% str_sub(., 6)
  csv_dir   <-  dirname(mod_path)
  csv_path <- paste0(csv_dir ,'/',  csv_name ) %>% print
   if (!file.exists(csv_path) )   {winDialog( "ok", "Csv File Dose Not Exist") }

  ##----------------- back CSV file ---------------------##
  file.copy(csv_path,paste0(getwd(),'/',csv_name ,'-backup', '.csv'))


  ##--------------- read csv sim/fit file from MOD ----------------------##
  return ( na.omit(as.data.frame(sapply(read_csv(csv_path,skip=1), as.numeric))))

}


outersect <- function(x, y) {
  cat ('only left:')
  print (setdiff(x, y))
  cat ('only  right:')
  print    ( setdiff(y, x))
  cat ('Combined:')
  return(  sort(c(setdiff(x, y), setdiff(y, x)))           )
}


con_para <- function(para)   {   
 
## remove space and line return
para  <- para %>%  gsub(" ", "",., fixed = TRUE) %>%   gsub("\n", "",., fixed = TRUE)

## convert to list 
para_rawlist <-   as.list(strsplit(para, ",")[[1]])

## blank list variable to add thing on
paralist <- list()
 
## evaluate every element in the list 
for (i in 1:length(para_rawlist) )   {
  newlist <- eval(parse(text=paste('list(', para_rawlist[[i]], ')')))  # great new list other than last
  assign (  names( newlist)[1],newlist[[1]] )  # evaluate before
  paralist <- append(paralist,eval(parse(text=paste('list(', para_rawlist[[i]], ')')))  )  
} 

new <-  as.numeric(  paste( paralist   ) )
names(new)   <- names(paralist)
return(new)
  }




