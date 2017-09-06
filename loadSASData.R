library(foreign,quietly = TRUE)
library(dplyr,quietly = TRUE)

###############load SAS data #########################
setwd("F:\\Maggie\\work\\Rdata")


loaddata = function(sasdata){
  sasdataload=read.ssd(sashome,paste0(sasdata),
                       sascmd="F:/Program Files/SASHome/SASFoundation/9.4/sas.exe")
  save(sasdataload,file=paste0(sasdata,".Rdata"))
}

sashome<- "F:/Maggie/work/SAS"
# loaddata

# should have orgid
loaddata("thysrg")
load("thysrg.Rdata")
dt <- sasdataload

#output to excel
outpath = "F:\\Maggie\\work\\ThyroidC\\"
fname <- paste0(outpath,"thysrg",
               ".xlsx")

if(file.exists(fname)) file.remove(fname)

xls<- loadWorkbook(fname,create=TRUE)

name = "Surg"
createSheet(xls,name=name)

writeWorksheet(xls,dt,name,
               startRow = 1,startCol = 1
               )
saveWorkbook(xls)

###########output to excel##################
library(XLConnect,quietly = TRUE)
 
data <- read.csv("F:\\Maggie\\work\\ThyroidC\\surgDetailT.csv",stringsAsFactors = FALSE)

outpath = "F:\\Maggie\\work\\ThyroidC\\"
fname <- paste0(outpath,"surgDetailT",
                ".xlsx")

out2excel <- function(fname,wname,data){

  if(file.exists(fname)) file.remove(fname)

  xls<- loadWorkbook(fname,create=TRUE)
  
  name = wname
  createSheet(xls,name=name)
  
  writeWorksheet(xls,data,name,
                 startRow = 1,startCol = 1
  )
  saveWorkbook(xls)
}