##------------- write csv with comments on the first row ---------------##
write_csv_comm <- function(dataset,filename,comments='No Comments')  {
  head_com <- paste('#',comments)      # add '#' for NONMEM to ignore
  write(head_com ,filename)                   # add comments first
  write(colnames(dataset),filename,ncolumns=ncol(dataset),append = TRUE,sep=',' )
  write_csv(dataset,filename,append = TRUE, na='.')
}
