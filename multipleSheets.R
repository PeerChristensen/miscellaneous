

file <- 'working excel_update.xlsx'

sheets <- excel_sheets(file)
frames=list()
for (i in 1:length(sheets)){ 
  frames=append(frames,data.frame(read_excel(file, sheet = i)))
}

a=read_excel(file, sheet = 7)
