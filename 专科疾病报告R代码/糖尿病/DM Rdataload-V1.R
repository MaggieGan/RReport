library(sas7bdat)
library(ggplot2)
library(plyr)
library(dplyr)
library(grid)
library(scales)
library(VennDiagram)
library(sas7bdat)
library(wordcloud)
library(reshape)
library(maps)
library(mapdata)
library(maptools)
library(XLConnect)
library(Hmisc)

####################
setwd("D:/HQMS/HQMS_20160630/DM/R")
getwd()
library(foreign)
loaddata=function(sasdata){
  sasdataload=read.ssd(sashome,paste0(sasdata),
                       sascmd="C:/Program Files/SASHome/SASFoundation/9.4/sas.exe")
  save(sasdataload,file=paste0(sasdata,".Rdata"))
}
sashome <- "D:/HQMS/HQMS_20160630/DM"
#Globalid;
loaddata("dmlist")

#Hospital Information;
loaddata("orgfdm")
#For Accesment;
loaddata("acsmdm")
#Hospital Accesment;
loaddata("dmaccsm")

##年龄分组
AgeGing=function(AgeD){
  AgeGing=ddply(AgeD,NULL,transform,ageg=ifelse(AGE_FIN>17 & AGE_FIN<35,1,
                                                       ifelse(AGE_FIN>=35 & AGE_FIN<45,2,
                                                              ifelse(AGE_FIN>=45 & AGE_FIN<65,3,
                                                                     ifelse(AGE_FIN>=65 & AGE_FIN<75,4,
                                                                            ifelse(AGE_FIN>=75 & AGE_FIN<85,5,
                                                                                   ifelse(AGE_FIN>=85 & AGE_FIN<120,6,NA)))))))
  return(AgeGing)
}

#

departmentGing=function(DepartD){
  AgeGing=ddply(DepartD,NULL,transform,admsDp2=substr(P23,1,2),dischDp2=substr(P26,1,2))%>%
    mutate(admsDp=ifelse(admsDp2=='03',1,
                              ifelse(admsDp2=='04',2,
                                            ifelse(admsDp2=='50',3,
                                                   ifelse(admsDp2=='02',4,
                                                          ifelse(admsDp2=='19',5,
                                                                 ifelse(admsDp2=='05',6,
                                                                        ifelse(admsDp2=='16',7,
                                                                               ifelse(admsDp2=='28',8,
                                                                                      ifelse(admsDp2=='20',9,
                                                                                             ifelse(admsDp2=='10',10,11)))))))))),
           dischDp=ifelse(dischDp2=='03',1,
                               ifelse(dischDp2=='04',2,
                                             ifelse(dischDp2=='50',3,
                                                    ifelse(dischDp2=='02',4,
                                                           ifelse(dischDp2=='19',5,
                                                                  ifelse(dischDp2=='05',6,
                                                                         ifelse(dischDp2=='16',7,
                                                                                ifelse(dischDp2=='28',8,
                                                                                       ifelse(dischDp2=='20',9,
                                                                                              ifelse(admsDp2=='10',10,11)))))))))),
           ASubDp=ifelse(P23=='0306',1,
                               ifelse(P23=='0301',2,
                                      ifelse(P23=='0302',3,
                                             ifelse(P23=='0303',4,
                                                    ifelse(P23=='0304',5,
                                                           ifelse(P23=='0305',6,
                                                                  ifelse(P23=='0307',7,
                                                                         ifelse(P23=='0308',8,
                                                                                ifelse(P23=='0309',9,
                                                                                       ifelse(P23=='0310',10,
                                                                                              ifelse(P23=='0311',11,NA))))))))))) ,
          DSubDp=ifelse(P26=='0306',1,
                              ifelse(P26=='0301',2,
                                     ifelse(P26=='0302',3,
                                            ifelse(P26=='0303',4,
                                                   ifelse(P26=='0304',5,
                                                          ifelse(P26=='0305',6,
                                                                 ifelse(P26=='0307',7,
                                                                        ifelse(P26=='0308',8,
                                                                               ifelse(P26=='0309',9,
                                                                                      ifelse(P26=='0310',10,
                                                                                             ifelse(P26=='0311',11,NA)))))))))))
          

                
          
                
  )
  gc()
  print("test1")
  AgeGing=subset(AgeGing,select=-c(P23,P26,.id,admsDp2,dischDp2))
 #, division=ifelse(is.na(ASubDp)|is.na(DSubDp),3,ifelse(ASubDp==6&DSubDp==6,1,2))
  print(tally(group_by(AgeGing,dischDp)))
 print(tally(group_by(AgeGing,admsDp)))
#  print(tally(group_by(AgeGing,division)))
  return(AgeGing)
} 

CITYGing=function(DepartD){
  AgeGing=ddply(DepartD,NULL,transform,
                FCITY=ifelse(substr(FROM_CIT,1,2)=='北京',1,
                             ifelse(substr(FROM_CIT,1,2)=='天津',2,
                                    ifelse(substr(FROM_CIT,1,2)=='石家',3,
                                           ifelse(substr(FROM_CIT,1,2)=='太原',4,
                                                  ifelse(substr(FROM_CIT,1,2)=='呼和',5,
                                                         ifelse(substr(FROM_CIT,1,2)=='沈阳',6,
                                                                ifelse(substr(FROM_CIT,1,2)=='长春',7,
                                                                       ifelse(substr(FROM_CIT,1,2)=='哈尔滨',8,
                                                                              ifelse(substr(FROM_CIT,1,2)=='上海',9,
                                                                                     ifelse(substr(FROM_CIT,1,2)=='南京',10,
                                                                                            ifelse(substr(FROM_CIT,1,2)=='杭州',11,
                                                                                                   ifelse(substr(FROM_CIT,1,2)=='合肥',12,
                                                                                                          ifelse(substr(FROM_CIT,1,2)=='福州',13,
                                                                                                                 ifelse(substr(FROM_CIT,1,2)=='南昌',14,
                                                                                                                        ifelse(substr(FROM_CIT,1,2)=='济南',15,
                                                                                                                               ifelse(substr(FROM_CIT,1,2)=='郑州',16,
                                                                                                                                      ifelse(substr(FROM_CIT,1,2)=='武汉',17,
                                                                                                                                             ifelse(substr(FROM_CIT,1,2)=='长沙',18,
                                                                                                                                                    ifelse(substr(FROM_CIT,1,2)=='广州',19,
                                                                                                                                                           ifelse(substr(FROM_CIT,1,2)=='南宁',20,
                                                                                                                                                                  ifelse(substr(FROM_CIT,1,2)=='海口',21,
                                                                                                                                                                         ifelse(substr(FROM_CIT,1,2)=='重庆',22,
                                                                                                                                                                                ifelse(substr(FROM_CIT,1,2)=='成都',23,
                                                                                                                                                                                       ifelse(substr(FROM_CIT,1,2)=='贵阳',24,
                                                                                                                                                                                              ifelse(substr(FROM_CIT,1,2)=='昆明',25,
                                                                                                                                                                                                     ifelse(substr(FROM_CIT,1,2)=='拉萨',26,
                                                                                                                                                                                                            ifelse(substr(FROM_CIT,1,2)=='西安',27,
                                                                                                                                                                                                                   ifelse(substr(FROM_CIT,1,2)=='兰州',28,
                                                                                                                                                                                                                          ifelse(substr(FROM_CIT,1,2)=='西宁',29,
                                                                                                                                                                                                                                 ifelse(substr(FROM_CIT,1,2)=='银川',30,
                                                                                                                                                                                                                                        ifelse(substr(FROM_CIT,1,2)=='乌鲁',31,
                                                                                                                                                                                                                                               NA))))))))))))))))))))))))))))))))
                                                                                                                                                                                                                                               
  AgeGing=subset(AgeGing,select=-c(.id))
  print(tally(group_by(AgeGing,FCITY)))
  return(AgeGing)
}    


RegionGring=function(RegionD){
  Dtmp=ddply(RegionD,NULL,transform,RegionF=ifelse(FROM_PRO %in% c(1,2,3,4,5),1,
                                                   ifelse(FROM_PRO %in% c(6,7,8),2,
                                                          ifelse(FROM_PRO %in% c(9,10,11,12,13,14,15,32),3,
                                                                 ifelse(FROM_PRO %in% c(16,17,18),4,
                                                                        ifelse(FROM_PRO %in% c(19,20,21,33,34),5,
                                                                               ifelse(FROM_PRO %in% c(22,23,24,25,26),6,
                                                                                      ifelse(FROM_PRO %in% c(27,28,29,30,31),7,NA)))))))
             )
  Dtmp=subset(Dtmp,select=-c(.id))
  print(tally(group_by(Dtmp,RegionF)))
  return(Dtmp)
}

##7大分区
RegionGring1=function(RegionD){
  Dtmp=ddply(RegionD,NULL,transform,Region=ifelse(PRO_ID %in% c(1,2,3,4,5),1,
                                                  ifelse(PRO_ID %in% c(6,7,8),2,
                                                         ifelse(PRO_ID %in% c(9,10,11,12,13,14,15,32),3,
                                                                ifelse(PRO_ID %in% c(16,17,18),4,
                                                                       ifelse(PRO_ID %in% c(19,20,21,33,34),5,
                                                                              ifelse(PRO_ID %in% c(22,23,24,25,26),6,
                                                                                     ifelse(PRO_ID %in% c(27,28,29,30,31),7,NA)))))))
  )
  Dtmp=subset(Dtmp,select=-c(.id))
  print(tally(group_by(Dtmp,Region)))
  return(Dtmp)
}

CITYMTGing=function(DepartD){
  AgeGing=ddply(DepartD,NULL,transform,
                TCITY=ifelse(substr(CITYNAME,1,2)=='北京',1,
                             ifelse(substr(CITYNAME,1,2)=='天津',2,
                                    ifelse(substr(CITYNAME,1,2)=='石家',3,
                                           ifelse(substr(CITYNAME,1,2)=='太原',4,
                                                  ifelse(substr(CITYNAME,1,2)=='呼和',5,
                                                         ifelse(substr(CITYNAME,1,2)=='沈阳',6,
                                                                ifelse(substr(CITYNAME,1,2)=='长春',7,
                                                                       ifelse(substr(CITYNAME,1,2)=='哈尔滨',8,
                                                                              ifelse(substr(CITYNAME,1,2)=='上海',9,
                                                                                     ifelse(substr(CITYNAME,1,2)=='南京',10,
                                                                                            ifelse(substr(CITYNAME,1,2)=='杭州',11,
                                                                                                   ifelse(substr(CITYNAME,1,2)=='合肥',12,
                                                                                                          ifelse(substr(CITYNAME,1,2)=='福州',13,
                                                                                                                 ifelse(substr(CITYNAME,1,2)=='南昌',14,
                                                                                                                        ifelse(substr(CITYNAME,1,2)=='济南',15,
                                                                                                                               ifelse(substr(CITYNAME,1,2)=='郑州',16,
                                                                                                                                      ifelse(substr(CITYNAME,1,2)=='武汉',17,
                                                                                                                                             ifelse(substr(CITYNAME,1,2)=='长沙',18,
                                                                                                                                                    ifelse(substr(CITYNAME,1,2)=='广州',19,
                                                                                                                                                           ifelse(substr(CITYNAME,1,2)=='南宁',20,
                                                                                                                                                                  ifelse(substr(CITYNAME,1,2)=='海口',21,
                                                                                                                                                                         ifelse(substr(CITYNAME,1,2)=='重庆',22,
                                                                                                                                                                                ifelse(substr(CITYNAME,1,2)=='成都',23,
                                                                                                                                                                                       ifelse(substr(CITYNAME,1,2)=='贵阳',24,
                                                                                                                                                                                              ifelse(substr(CITYNAME,1,2)=='昆明',25,
                                                                                                                                                                                                     ifelse(substr(CITYNAME,1,2)=='拉萨',26,
                                                                                                                                                                                                            ifelse(substr(CITYNAME,1,2)=='西安',27,
                                                                                                                                                                                                                   ifelse(substr(CITYNAME,1,2)=='兰州',28,
                                                                                                                                                                                                                          ifelse(substr(CITYNAME,1,2)=='西宁',29,
                                                                                                                                                                                                                                 ifelse(substr(CITYNAME,1,2)=='银川',30,
                                                                                                                                                                                                                                        ifelse(substr(CITYNAME,1,2)=='乌鲁',31,
                                                                                                                                                                                                                                               NA)))))))))))))))))))))))))))))))
               )
  print(tally(group_by(AgeGing,TCITY)))
  AgeGing=ddply(AgeGing,NULL,transform,
                CITYMT=ifelse(!is.na(TCITY)&!is.na(FCITY)&FCITY==TCITY,1,
                              ifelse(!is.na(TCITY)&!is.na(FROM_PRO)&TCITY==FROM_PRO,2,
                                     ifelse(!is.na(FROM_PRO),3,
                                            NA))))
  AgeGing=subset(AgeGing,select=-c(.id))
  print(tally(group_by(AgeGing,TCITY,CITYMT)))
  return(AgeGing)
}  
####根据合并症
GetCMPT=function(Fdata,GV,levels,ANAnow="Rate"){
  for(i in 1:length(levels)){
    if(ANAnow=="Rate"){Vlist=c("GLOBALID","GENDER","ageg",GV,levels[i])}
    else if(ANAnow=="MORTALITY"){Vlist=c("GLOBALID","STATUS",GV,levels[i])}
    else if(ANAnow=="LOS"){Vlist=c("GLOBALID","SJDAYS",GV,levels[i])}
    else if(ANAnow=="COST"){Vlist=c("GLOBALID","FEE_TOTA",GV,levels[i])}
    CMPT1=data.frame(with(Fdata,mget(Vlist)))
    Vnames=names(CMPT1)
    Vnames[length(Vnames)]="cmpt"
    names(CMPT1)=Vnames 
    if(ANAnow!="Rate"){ CMPT1=filter(CMPT1,cmpt==1)  }
    CMPT1$cmptid=i
    if(i==1){CMPT=CMPT1}
    else {    CMPT=rbind(CMPT,CMPT1)}
  }
  return(CMPT)
}
####根据并发症
GetCMB=function(Fdata,GV,levels,ANAnow="Rate"){
  for(i in 1:length(levels)){
    if(ANAnow=="Rate"){Vlist=c("GLOBALID","GENDER","ageg",GV,levels[i])}
    else if(ANAnow=="MORTALITY"){Vlist=c("GLOBALID","STATUS",GV,levels[i])}
    else if(ANAnow=="LOS"){Vlist=c("GLOBALID","SJDAYS",GV,levels[i])}
    else if(ANAnow=="COST"){Vlist=c("GLOBALID","FEE_TOTA",GV,levels[i])}
    CMPT1=data.frame(with(Fdata,mget(Vlist)))
    Vnames=names(CMPT1)
    Vnames[length(Vnames)]="cmb"
    names(CMPT1)=Vnames 
    if(ANAnow!="Rate"){ CMPT1=filter(CMPT1,cmb==1)  }
    CMPT1$cmbid=i
    if(i==1){CMPT=CMPT1}
    else {    CMPT=rbind(CMPT,CMPT1)}
  }
  return(CMPT)
}
####根据分析治疗
GetSUGY=function(Fdata,GV,levels,ANAnow="Rate"){
  for(i in 1:length(levels)){
    if(ANAnow=="Rate"){Vlist=c("GLOBALID","GENDER","ageg",GV,levels[i])}
    else if(ANAnow=="MORTALITY"){Vlist=c("GLOBALID","STATUS",GV,levels[i])}
    else if(ANAnow=="LOS"){Vlist=c("GLOBALID","SJDAYS",GV,levels[i])}
    else if(ANAnow=="COST"){Vlist=c("GLOBALID","FEE_TOTA",GV,levels[i])}
    CMPT1=data.frame(with(Fdata,mget(Vlist)))
    Vnames=names(CMPT1)
    Vnames[length(Vnames)]="surg"
    names(CMPT1)=Vnames 
    if(ANAnow!="Rate"){ CMPT1=filter(CMPT1,surg==1)  }
    CMPT1$surgid=i
    if(i==1){CMPT=CMPT1}
    else {    CMPT=rbind(CMPT,CMPT1)}
  }
  return(CMPT)
}

#风险人群分析库--min
RiskGet<-function(HQMSID,Yearnote){
  load(paste0("D:/work/RData/HQMSDR",HQMSID,".Rdata"))
  HQMSTMP=get(paste0("HQMSDR",HQMSID))
  HQMSTMP=subset(HQMSTMP,select=c(GLOBALID,PRO_ID,FROM_PRO,FROM_CIT,ETHNIC,RSDENCE,AGE_FIN))
  #HQMSTMP=subset(HQMSTMP,select=c(GLOBALID,PRO_ID,CYCLE,FROM_PRO,FROM_CIT,ETHNIC,RSDENCE,AGE_FIN))%>%
  #  muate(ADSM=CYCLE-floor(CYCLE/100)*100)
  #HQMSTMP=subset(HQMSTMP,select=-c(CYCLE))
  Tmp=filter(Drisk,YEAR %in% Yearnote)
  HQMSTMP=left_join(Tmp,HQMSTMP)
  HQMSTMP=CITYGing(HQMSTMP)
  HQMSTMP=RegionGring(HQMSTMP)
  HQMSTMP=RegionGring1(HQMSTMP)
  return(HQMSTMP)
}

load("D:/HQMS/HQMS_20160630/DM/R/dmlist.Rdata")
#剔除thesameperson为空且所有疾病均不纳入分析的病例
Drisk=subset(filter(sasdataload,FIRSTREC ==1),select=c(GLOBALID,YEAR,HT,DM,IGT))
rm(sasdataload)
gc()
RiskTmp4=RiskGet(4,c(2015))
gc()
RiskTmp3=RiskGet(3,c(2014))
gc()
RiskTmp2=RiskGet(2,c(2013))
RiskTmp1=RiskGet(1,c(2010,2011,2012))

RiskDMtmp=rbind(RiskTmp1,RiskTmp2,RiskTmp3,RiskTmp4)
gc()
#HQMS
RiskDMtmp1=RiskDMtmp
RiskDMtmp1$RiskDM=1
#HT
RiskDMtmp2=filter(RiskDMtmp,HT==1)
RiskDMtmp2$RiskDM=2
RiskDM=rbind(subset(RiskDMtmp1,select=-c(HT)),subset(RiskDMtmp2,select=-c(HT)))
save(RiskDM,file="D:/HQMS/HQMS_20160630/DM/R/RiskDM.Rdata")

rm(Drisk,RiskTmp1,RiskTmp2,RiskTmp3,RiskTmp4,RiskDMtmp,RiskDMtmp1,RiskDMtmp2,RiskDM)

#分析库(如果包含年龄缺失的需另外补充)
load("D:/HQMS/HQMS_20160630/DM/R/dmlist.Rdata")
DMTMP=subset(filter(sasdataload,YEAR==2015&FIRSTREC==1&(DM==1|IGT==1)),
             select=-c(YEAR,FIRSTREC))	
rm(sasdataload)

load("D:/work/RData/HQMSDR4.Rdata")
DMTMP=left_join(DMTMP,HQMSDR4)
rm(HQMSDR4)

DMTMP=AgeGing(DMTMP)
DMTMP=CITYGing(DMTMP)
DMTMP=CITYMTGing(DMTMP)
DMTMP=RegionGring1(DMTMP)
DMTMP=RegionGring(DMTMP)
DMTMP=departmentGing(DMTMP)
#分组1--DM/IGT
DMall=subset(filter(DMTMP,DM==1),select=-c(DM,IGT,DM1,DM2,GDM,DM_OTHER))
DMall$DMC=1
IGT=subset(filter(DMTMP,IGT==1),select=-c(DM,IGT,DM1,DM2,GDM,DM_OTHER))
IGT$DMC=2
DMC=rbind(DMall,IGT)
rm(DMall,IGT)
save(DMC,file="D:/HQMS/HQMS_20160630/DM/R/DMC.Rdata")
#分组2--DM1,DM2,GDM,DM_other
DM1=subset(filter(DMTMP,DM1==1),select=-c(DM,IGT,DM1,DM2,GDM,DM_OTHER))
DM1$DMG=1
DM2=subset(filter(DMTMP,DM2==1),select=-c(DM,IGT,DM1,DM2,GDM,DM_OTHER))
DM2$DMG=2
GDM=subset(filter(DMTMP,GDM==1),select=-c(DM,IGT,DM1,DM2,GDM,DM_OTHER))
GDM$DMG=3
DMother=subset(filter(DMTMP,DM_OTHER==1),select=-c(DM,IGT,DM1,DM2,GDM,DM_OTHER))
DMother$DMG=4

DMG=rbind(DM1,DM2,GDM,DMother)
rm(DM1,DM2,GDM,DMother)
save(DMG,file="D:/HQMS/HQMS_20160630/DM/R/DMG.Rdata")

#wrGite.table(SLETable,file="clipboard")
###############地图数据集#######################
#######省份比例##########
Mapdata=function(DPlot,ByID,Vnow,NameList=""){
   D2015=data.frame(with(DPlot,mget(c(paste0(ByID),paste0(Vnow)))))
   names(D2015)=c("ByID","Dmark")
DMT=rename(tally(group_by(D2015,ByID)),c(n="DMT"))
NMT=rename(tally(group_by(filter(D2015,Dmark==1),ByID)),c(n="NMT"))
FMap=left_join(DMT,NMT)%>%
		mutate(P=NMT/DMT)
FMap=FMap[,c(1,4)]
if(length(NameList)==1){
 if(ByID=="FROM_PRO"){names(FMap)=c("ByID",paste0("PF",Vnow)) }
 else {names(FMap)=c("ByID",paste0("P",Vnow))}
}
else{
  names(FMap)=NameList
}
 return(FMap)
}
##DM
load("D:/HQMS/HQMS_20160630/DM/R/RiskDM.Rdata")
DM2016=filter(RiskDM,YEAR==2015&RiskDM==1)
DMMaptmp=Mapdata(DM2016,"PRO_ID","DM")
DMFMaptmp=Mapdata(DM2016,"FROM_PRO","DM")
DMmapProtmp=left_join(DMFMaptmp,DMMaptmp)
##IGT
IGTMaptmp=Mapdata(DM2016,"PRO_ID","IGT")
IGTFMaptmp=Mapdata(DM2016,"FROM_PRO","IGT")
IGTmapProtmp=left_join(IGTFMaptmp,IGTMaptmp)
###
DMFMap=left_join(DMmapProtmp,IGTmapProtmp)
write.table(DMFMap,file="clipboard")
rm(DMMaptmp,DMFMaptmp,IGTMaptmp,IGTFMaptmp,DMFMap)
#######区域比例##########
##DM
DM2016=filter(RiskDM,YEAR==2015&RiskDM==1)
DMMaptmp=Mapdata(DM2016,"Region","DM")
DMFMaptmp=Mapdata(DM2016,"RegionF","DM")
DMmapProtmp=left_join(DMFMaptmp,DMMaptmp)
##IGT
IGTMaptmp=Mapdata(DM2016,"Region","IGT")
IGTFMaptmp=Mapdata(DM2016,"RegionF","IGT")
IGTmapProtmp=left_join(IGTFMaptmp,IGTMaptmp)
###
DMRegionMap=left_join(DMmapProtmp,IGTmapProtmp)
write.table(DMRegionMap,file="clipboard")

rm(DMMaptmp,DMFMaptmp,IGTMaptmp,IGTFMaptmp,DMRegionMap)
####################
#######省份院内死亡比例##########
##DM整体
D_DMMrlty=filter(DMC,STATUS %in% c(1,2) & DMC==1)
DMMaptmp=Mapdata(D_DMMrlty,"PRO_ID","STATUS",c("PRO_ID","PDM"))
##Reference Line
DMMlty=dim(filter(D_DMMrlty,STATUS==1))[1]/dim(D_DMMrlty)[1]
##DM本地
D_DMMrlty=filter(D_DMMrlty,TRAVEL==2)
DMLocaltmp=Mapdata(D_DMMrlty,"PRO_ID","STATUS",c("PRO_ID","PLocolDM"))
DMMrltyMap=left_join(DMMaptmp,DMLocaltmp)
rm(D_DMMrlty,DMLocaltmp,DMMaptmp)
##IGT整体
D_IGTMrlty=filter(DMC,STATUS %in% c(1,2) & DMC==2)
IGTMaptmp=Mapdata(D_IGTMrlty,"PRO_ID","STATUS",c("PRO_ID","PIGT"))
##Reference Line
IGTMlty=dim(filter(D_IGTMrlty,STATUS==1))[1]/dim(D_IGTMrlty)[1]
##DM本地
D_IGTMrlty=filter(D_IGTMrlty,TRAVEL==2)
IGTLocaltmp=Mapdata(D_IGTMrlty,"PRO_ID","STATUS",c("PRO_ID","PLocalIGT"))
IGTMrltyMap=left_join(IGTMaptmp,IGTLocaltmp)
rm(D_IGTMrlty,IGTMaptmp,IGTLocaltmp)
###########
MrltyFmap=left_join(DMMrltyMap,IGTMrltyMap)
write.table(MrltyFmap,file="clipboard")
rm(DMMrltyMap)


