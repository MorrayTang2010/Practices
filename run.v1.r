#Delphi function
#Data:2016.04.22
setwd("E:\\R\\coding\\Delphi")
rm(list=ls())
require(RMySQL)
require(DBI)
####9
source("E:\\R\\coding\\造数测试\\predict.01.read.r")
source("Rule.r")
#9
file.name.9<-"pred.9.csv"
pred.9.dataset<-read.csv(file.name.9,header=TRUE,stringsAsFactors = FALSE)
###595
file.name<-"delphi.csv"
#sample.data<-read.csv(file.name,header=TRUE,stringsAsFactors = FALSE)
sample.data=pred.9.dataset
char.lst<-c("SALE_TYPE","ADDR","DEGREE","MAR_STA","COOP_SINC","SERV_CENTER","PLAT",
            "OTH_BUSI" ,"POLICY" ,"ENT_COURT","ADMIN_COURT","LPER_COURT")
feature.names<-names(sample.data)[2:(length(sample.data)-1)]
quan.lst<-setdiff(feature.names,char.lst)

###Delphi
output.data=Delphi(input.data=pred.9.dataset,char.lst=char.lst,quan.lst=quan.lst)

iniFile <- read.table("mysql_info.ini", sep="=", as.is=T)
dataset.list=ReadDB_pred9(iniFile, input.tablename="model_output_copy1")

