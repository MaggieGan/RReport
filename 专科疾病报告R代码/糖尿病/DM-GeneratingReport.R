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
library(ReporteRs)
library(tab)

#Generating  :D:\work\HQMS\DM\Rcode\ReportTmp
setwd("D:/work/HQMS/DM/Rcode/ReportTmp")
getwd()

windowsFonts(myFont1=windowsFont("Times New Roman"),myFont2=windowsFont("微软雅黑"),myFont3=windowsFont("Helvetica"))
options(stringsAsFactors=FALSE)

FigW0=6;FigW1=6.6;mapW=6.6
FigH0=2.7;FigH1=3.3;FigH2=3.6;FigH3=4.2;mapH=6.2
ProExclud=c(26,30,32,33,34)
boxMdnPoi="图中红色点为平均值。"
blankLine=function(){doc = addParagraph(doc, "", stylename = "BlankLine")}
PageBreak=function(){doc = addPageBreak(doc)}
PlotAdd=function(Plot0,FigW=FigW0,FigH=FigH0){doc = addPlot(doc = doc, fun = print, x=Plot0,  width =FigW, height = FigH)}
TableAdd=function(Table0,blankLine=TRUE){
  doc = addFlexTable(doc = doc,Table0)
  if(blankLine){blankLine()}}
NoteAdd=function(i,Note,blankLine=FALSE){
  doc = addParagraph( doc, paste0(i,". ",Note),stylename="rRawOutput")
  if(blankLine){blankLine()}
  }
PLegend=function(Note,blankLine=TRUE){
  doc = addParagraph( doc, value=Note, stylename = "rPlotLegend")
  if(blankLine){blankLine()}
  }
TLegend=function(Note){doc = addParagraph( doc, value=Note, stylename = "rTableLegend")}
Title1=function(Note){
  doc = addTitle( doc, Note, level = 1 )
  blankLine()
  doc = addPageBreak(doc)
}
Title2=function(Note,PageBreak=FALSE){
  if(PageBreak){doc = addPageBreak(doc)}
  doc = addTitle( doc, Note, level = 2 )
  }
Title3=function(Note,PageBreak=TRUE){  
  if(PageBreak){doc = addPageBreak(doc)}
  doc = addTitle( doc, Note, level = 3 )}

Title4=function(Note,PageBreak=FALSE){
  if(PageBreak){doc = addPageBreak(doc)}
  doc = addTitle( doc, Note, level = 4)}

ADDHline=function(yintCpt){
  return(geom_hline(yintercept=yintCpt,colour="#AB2E23",linetype = "longdash",alpha=0.5))
}

#Generating Reports
setwd("D:/work/HQMS/DM/Rcode/ReportTmp")
getwd()
fileN=0

################
fileN=fileN +1
doc.filename=paste0(filesave,Sys.Date( ),"-",fileN,".docx")
doc =docx(title = "ADR", template="D:/work/HQMS/DM/DM报告说明1.docx")
#doc =docx(title = "ADR", template="DiseaseAnalysisTemplate.docx")
options("ReporteRs-fontsize"=9, "ReporteRs-default-font"="微软雅黑")
#doc = addPageBreak(doc)
doc = addParagraph(doc, "目      录", stylename = "TitleDoc")
blankLine()
doc = addTOC(doc)
PageBreak()
##################第一章  概况  ################################################
#charpter 1
Title1(paste0(PTuse,"患者概况"))
#########
#####
load("D:/HQMS/HQMS_20160630/DM/R/RiskDM.Rdata")
RiskDM$YEAR=RiskDM$YEAR-2009
#tally(group_by(RiskDM,YEAR))

D_RISK=c("RiskDM","RiskDM")
D_RISKVnow=c("DM","IGT")
DMMAP<-read.csv("D:/work/HQMS/DM/Rcode/Rdata/DMMAP.csv", header = TRUE, sep = ",", quote="\"", dec=".",fill = TRUE)
DMRegionMap<-read.csv("D:/work/HQMS/DM/Rcode/Rdata/DMRegionMap.csv", header = TRUE, sep = ",", quote="\"", dec=".",fill = TRUE)
DMMltyMAP<-read.csv("D:/work/HQMS/DM/Rcode/Rdata/DMMltyMAP.csv", header = TRUE, sep = ",", quote="\"", dec=".",fill = TRUE)

##
Title2("趋势分析（2010-2015年）")
for (i in 1:length(D_RISK)){
#################  
  Vnow="RiskDM"
  labPTnow=get(paste0("lab_",D_RISKVnow[i]))
  PTnow=D_RISKVnow[i]
  Dnow=get(D_RISK[i])
########### 
Title3(paste0(PTnow,"患者在HQMS中的比例变化趋势"),PageBreak=FALSE)
PlotAdd(D21Rate(Dnow,"YEAR",Vnow,PTnow,1,"line"))
PLegend(paste0("2010-2015年",labPTnow,"患者比例变化趋势"))
TLegend(paste0("2010-2015年",labPTnow,"患者比例（N(%)）"))
TableAdd(TableRate2(Dnow,"YEAR",Vnow,PTnow,V_Frate=1))
##HQMS Only######
Dnow=filter(Dnow,RiskDM==1)
###
Title4("地理分区")
PlotAdd(D21Rate(Dnow,"YEAR","Region",PTnow,1,"line"))
PLegend(paste0("2010-2015年不同地理分区HQMS患者中",labPTnow,"比例趋势"))
TLegend(paste0("2010-2015年不同地理分区HQMS患者中",labPTnow,"比例（N(%)）"))
TableAdd(TableRate2(Dnow,"YEAR","Region",PTnow,V_Frate=1))

####  
Title4("省份")
PlotAdd(D21Rate(filter(Dnow,YEAR>3 & !(PRO_ID %in% ProExclud)),"PRO_ID","YEAR",PTnow,1), FigW =FigW1)
PLegend(paste0("2013-2015年不同地区",labPTnow,"患者比例趋势"))
TLegend(paste0("2010-2015年不同地区",labPTnow,"患者比例（N(%)）"))
TableAdd(TableRate2(Dnow,"YEAR","PRO_ID",PTnow,V_Frate=1))
####
Title4(lab_RSDENCE)
PlotAdd(D21Rate(Dnow,"YEAR","RSDENCE",PTnow,1,"line"))
PLegend(paste0(lab_RSDENCE,"HQMS患者中",labPTnow,"比例趋势"))
TLegend(paste0(lab_RSDENCE,"HQMS患者中",labPTnow,"比例趋势（N(%)）"))
TableAdd(TableRate2(Dnow,"YEAR","RSDENCE",PTnow,V_Frate=1),blankLine = FALSE)
NoteAdd(1,vAbn_NaN(Dnow,Vnow,"RSDENCE","患者城乡来源"),blankLine = TRUE)

#####
Title4(lab_ETHNIC,PageBreak = TRUE)
PlotAdd(D21Rate(filter(Dnow,YEAR>3),"YEAR","ETHNIC",PTnow,1), FigW =FigW1)
PLegend(paste0("2013-2015年不同",lab_ETHNIC,labPTnow,"患者比例"))
TLegend(paste0("2010-2015年不同",lab_ETHNIC,labPTnow,"患者比例（N(%)）"))
TableAdd(TableRate2(Dnow,"YEAR","ETHNIC",PTnow,V_Frate=1),blankLine = FALSE)
NoteAdd(1,vAbn_NaN(Dnow,Vnow,"ETHNIC","患者民族"),blankLine = TRUE)
}
###
Title2(paste0("2015年",PTuse,"患者比例"),PageBreak = TRUE)
for (i in 1:length(D_RISK)){
  #################  
  Vnow="RiskDM"
  labPTnow=get(paste0("lab_",D_RISKVnow[i]))
  PTnow=D_RISKVnow[i]
  Dnow=filter(get(D_RISK[i]),YEAR==6&RiskDM==1)
  #####
Title3(paste0("2015年",labPTnow,"患者在HQMS中的比例"),PageBreak=FALSE)
Title4(lab_Region)
#Map
PlotAdd( MapPlot(DMRegionMap,"ByID",PTnow,map_color_use=map7,decimal=i,PRO=FALSE),FigW=mapW,FigH=mapH)
PLegend(paste0("2015年不同",lab_Region,"诊治",labPTnow,"患者比例"))
TLegend(paste0("2015年不同",lab_Region,"诊治",labPTnow,"患者比例（N(%)）"))
TableAdd(TableRate2(Dnow,Vnow,"Region",PTnow,V_Frate=1))

Title4(lab_PRO_ID)
#Map
PlotAdd( MapPlot(DMMAP,"ByID",PTnow,map_color_use=map_blue,decimal=i),FigW=mapW,FigH=mapH)
PLegend(paste0("2015年不同省份","诊治",labPTnow,"患者比例"))
TLegend(paste0("2015年不同省份","诊治",labPTnow,"患者比例（N(%)）"))
TableAdd(TableRate2(Dnow,Vnow,"PRO_ID",PTnow,V_Frate=1))

#Title4(lab_ADSM)
#PlotAdd(D21Rate(Dnow,"ADSM",Vnow,PTnow,1), FigW =FigW1)
#PLegend(paste0("不同",lab_ADSM,"中",labPTnow,"患者比例"))
#TLegend(paste0("不同",lab_ADSM,"中",labPTnow,"患者比例（N(%)）"))
#TableAdd(TableRate2(Dnow,Vnow,"ADSM",PTnow,V_Frate=1))
}

###
#rm(DMMAP,RiskDM)
##################第二章 CKD患者就诊   ##################################
load("D:/HQMS/HQMS_20160630/DM/R/DMC.Rdata")
load("D:/HQMS/HQMS_20160630/DM/R/DMG.Rdata")
#妊娠糖尿病患者大于50岁，设为异常
DMG=ddply(DMG,NULL,transform,ageg=ifelse(DMG==3&AGE_FIN>50,NA,ageg),
          AGE_FIN=ifelse(DMG==3&AGE_FIN>50,NA,AGE_FIN))
ANA_levels=c("DMC","DMG")
######
Title1(paste0(PTuse,"患者就诊"))
Title2(paste0(PTuse,"患者就医流向"))
for(i in 1:2){
  ######
  Dnow=get(ANA_levels[i])
  Vnow=ANA_levels[i]
  labnow=get(paste0("lab_",ANA_levels[i]))
  ####
  CAUSECh=data.frame(with(Dnow,mget(c("GLOBALID","FROM_PRO","PRO_ID","TRAVEL",Vnow))))
  names(CAUSECh)=c("GLOBALID","FROM_PRO","PRO_ID","TRAVEL","CAUSE")
  #CAUSECh=subset(CAUSE,select=c(GLOBALID,FROM_PRO,PRO_ID,TRAVEL,CAUSE))
  CAUSECh1=CAUSECh;CAUSECh1$ProNote=1;CAUSECh1$PRO=CAUSECh1$PRO_ID
  CAUSECh2=CAUSECh;CAUSECh2$ProNote=2;CAUSECh2$PRO=CAUSECh2$FROM_PRO
  CAUSECh=rbind(CAUSECh1,CAUSECh2)
  rm(CAUSECh1,CAUSECh2)
####
  CAUSE_levels=get(paste0(Vnow,"_levels"))
####
Title3(labnow,PageBreak = FALSE)
for(CAUSEId in 1:length(get(paste0(ANA_levels[i],"_levels")))){
  LabelNow=paste0(CAUSE_levels[CAUSEId])
  CAUSENow=filter(CAUSECh,CAUSE==CAUSEId)
  HLine=dim(filter(CAUSENow,TRAVEL==1))[1]/dim(filter(CAUSENow,TRAVEL %in% c(1,2)))[1]
  
  PlotAdd(D21Rate(filter(CAUSENow,!(PRO %in% ProExclud)),"ProNote","PRO","TRAVEL",1,"NULL")+ADDHline(HLine),FigW=FigW1)
  PLegend(paste0(LabelNow,"患者的就医流向"),blankLine = FALSE)
  NoteAdd(1,paste0("参考线说明：整体",LabelNow,"异地就医比例（",Rate_legend(HLine),"）。"))
  NoteAdd(2,"西藏、宁夏及港澳台地区不展示。",blankLine=TRUE)
  
TLegend(paste0(LabelNow,"患者的就医流向（N(%)）"))
TableAdd(MedicalT1(CAUSENow,"ProNote","PRO","TRAVEL"),blankLine = FALSE)
NoteAdd(1,vAbn_NaN(filter(CAUSENow,ProNote==2),"CAUSE","FROM_PRO","常住省份(直辖市)"),blankLine=TRUE)
#NoteAdd(2,"西藏、宁夏及港澳台地区不展示。")
}}
####################
rm(CAUSECh,CAUSENow)
#################
Title2(paste0(PTuse,"患者出入院科别"))

for(i in 1:2){
  ######
  Dnow=get(ANA_levels[i])
  Vnow=ANA_levels[i]
  labnow=get(paste0("lab_",ANA_levels[i]))
  ###
  Title3(labnow,PageBreak = FALSE)
PlotAdd(D11PCT(Dnow,Vnow,"admsDp")  )
PLegend(paste0(labnow,"患者入院科别"))
TLegend(paste0(labnow,"患者入院科别（N(%)）"))
TableAdd(TablePCT1(Dnow,Vnow,"admsDp"),blankLine = FALSE)
NoteAdd(1,"按入院科别比例，除TOP10外合并为其他。",blankLine = TRUE)

PlotAdd(D11PCT(Dnow,Vnow,"dischDp")  )
PLegend(paste0(labnow,"患者出院科别"))
TLegend(paste0(labnow,"患者出院科别（N(%)）"))
TableAdd(TablePCT1(Dnow,Vnow,"dischDp"),blankLine = FALSE)
NoteAdd(1,"按入院科别比例，除TOP10外合并为其他。",blankLine = TRUE)
}

##################第三章 CH3 患者特征  ########################################
Title1(paste0(PTuse,"患者概况"))
for(i in 1:2){
  ######
  Dnow=get(ANA_levels[i])
  Vnow=ANA_levels[i]
  LabelNow=get(paste0("lab_",ANA_levels[i]))
  ###
  Title2(LabelNow)
#性别与年龄
Title3("性别年龄",PageBreak = FALSE)
PlotAdd(D11PCT(Dnow,Vnow,"GENDER"))
PLegend(paste0(LabelNow,"患者性别分布"))
TLegend(paste0(LabelNow,"患者性别分布（N(%)）"))
TableAdd(TablePCT1(Dnow,Vnow,"GENDER"))

PlotAdd(D11Stat(Dnow,Vnow,"AGE_FIN",Violin = TRUE))
PLegend(paste0(LabelNow,"患者年龄分布",blankLine = FALSE))
if(i==1){blankLine()}else{NoteAdd(1,boxMdnPoi,blankLine=TRUE)}

TLegend(paste0(LabelNow,"患者年龄分布"))
TableAdd(TableSTAT1(Dnow,Vnow,"AGE_FIN"),blankLine = FALSE)
NoteAdd(1,vAbn_NaN(Dnow,Vnow,"ageg","患者年龄"),blankLine = TRUE)
PlotAdd(D21PCT(filter(Dnow,GENDER!=3),Vnow,"GENDER","ageg"),FigH=FigH1)
PLegend(paste0("不同性别",LabelNow,"患者年龄段分布"))
TLegend(paste0("不同性别",LabelNow,"患者年龄段分布（N(%)）"))
TableAdd(TableRate2(Dnow,Vnow,"GENDER","ageg"),blankLine = FALSE)
NoteAdd(1,vAbn_NaN(Dnow,Vnow,"ageg","患者年龄"),blankLine = TRUE)

#Title3("入院月份",PageBreak = FALSE)
#PlotAdd(D11PCT(Dnow,Vnow,"ADSM"))
#PLegend(paste0(LabelNow,"患者入院月份分布"))
#TLegend(paste0(LabelNow,"患者入院月份分布（N(%)）"))
#TableAdd(TablePCT1(Dnow,Vnow,"ADSM"))

#城乡分布
Title3(lab_RSDENCE,PageBreak = FALSE)
PlotAdd(D11PCT(Dnow,Vnow,"RSDENCE"))
PLegend(paste0(LabelNow,"患者城乡来源分布"))
TLegend(paste0(LabelNow,"患者城乡来源分布（N(%)）"))
TableAdd(TablePCT1(Dnow,Vnow,"RSDENCE"),blankLine = FALSE)
NoteAdd(1,vAbn_NaN(Dnow,Vnow,"RSDENCE","患者城乡来源"),blankLine = TRUE)

Title3(lab_OCCUP,PageBreak = FALSE)
PlotAdd(D11PCT(Dnow,Vnow,"OCCUP"))
PLegend(paste0(LabelNow,"患者职业分布"))
TLegend(paste0(LabelNow,"患者职业分布（N(%)）"))
TableAdd(TablePCT1(Dnow,Vnow,"OCCUP"),blankLine = FALSE)
NoteAdd(1,vAbn_NaN(Dnow,Vnow,"OCCUP","患者职业"),blankLine = TRUE)

#医疗保险
Title3(lab_INSURANC,PageBreak = FALSE)
PlotAdd(D11PCT(Dnow,Vnow,"INSURANC"))
PLegend(paste0(LabelNow,"患者付款方式分布"))
TLegend(paste0(LabelNow,"患者付款方式分布（N(%)）"))
TableAdd(TablePCT1(Dnow,Vnow,"INSURANC"),blankLine = FALSE)
NoteAdd(1,vAbn_NaN(Dnow,Vnow,"INSURANC","付款方式"),blankLine = TRUE)
}

################## 第四章 并发症 ############################################

Title1(paste0(PTuse,"患者并发症"))
for(i in 1:2){
  ######
  Dnow=get(ANA_levels[i])
  Vnow=ANA_levels[i]
  labnow=get(paste0("lab_",ANA_levels[i]))
  if(i==1){
    Dnow=filter(Dnow,DMC==1)
    labnow="糖尿病"
    }
  ###
  Title2(labnow)
  DMPTnow=GetCMPT(Dnow,Vnow,cmptid)
  gc()
  
  PlotAdd(D21Rate(DMPTnow,Vnow,"cmptid","cmpt",1,"NULL"),FigW = FigW1)
  PLegend(paste0(labnow,"患者并发症"))
  TLegend(paste0(labnow,"患者并发症（N(%)）"))
  TableAdd(TableRate2(DMPTnow,Vnow,"cmptid","cmpt",V_Frate=1))
  ######
  rm(DMPTnow);gc()
  ######
  #性别与年龄  
  #Title2("性别年龄")
  for(j in 1:length(cmptid)){
    PlotAdd(D31Rate(filter(Dnow,GENDER!=3),"ageg","GENDER",Vnow,cmptid[j],1),FigW=FigW1,FigH =FigH1  )
    PLegend(paste0("不同性别年龄段",labnow,"患者并发",get(paste0("lab_",cmptid[j])),"的比例"))
    TLegend(paste0("不同性别年龄段",labnow,"患者并发",get(paste0("lab_",cmptid[j])),"的比例（N(%)）"))
    TableAdd(TableRate3(filter(Dnow,GENDER!=3),Vnow,"GENDER","ageg",cmptid[j],V_Frate=1))
  }
}
################## 第四章 合并症############################################
  
  Title1(paste0(PTuse,"患者合并症"))
  for(i in 1:2){
    ######
    Dnow=get(ANA_levels[i])
    Vnow=ANA_levels[i]
    labnow=get(paste0("lab_",ANA_levels[i]))
    if(i==1){
      Dnow=filter(Dnow,DMC==1)
      labnow="糖尿病"
    }
    ###
    Title2(labnow)
    CMBnow=GetCMB(Dnow,Vnow,cmbid)
    gc()
    
   
    PlotAdd(D21Rate(CMBnow,Vnow,"cmbid","cmb",1,"NULL"),FigW = FigW1)
    PLegend(paste0(labnow,"患者合并症"))
    TLegend(paste0(labnow,"患者合并症（N(%)）"))
    TableAdd(TableRate2(CMBnow,Vnow,"cmbid","cmb",V_Frate=1))
    ######
    rm(CMBnow);gc()
    ######
    #性别与年龄  
    #Title2("性别年龄")
    for(j in 1:length(cmbid)){
      PlotAdd(D31Rate(filter(Dnow,GENDER!=3),"ageg","GENDER",Vnow,cmbid[j],1),FigW=FigW1,FigH =FigH1  )
      PLegend(paste0("不同性别年龄段",labnow,"患者合并",get(paste0("lab_",cmbid[j])),"的比例"))
      TLegend(paste0("不同性别年龄段",labnow,"患者合并",get(paste0("lab_",cmbid[j])),"的比例（N(%)）"))
      TableAdd(TableRate3(filter(Dnow,GENDER!=3),Vnow,"GENDER","ageg",cmbid[j],V_Frate=1))
    }
}
################## 第四章患者治疗 ############################################
Title1(paste0(PTuse,"患者治疗"))

for(i in 1:2){
  ######
  Dnow=get(ANA_levels[i])
  Vnow=ANA_levels[i]
  labnow=get(paste0("lab_",ANA_levels[i]))
  if(i==1){
    Dnow=filter(Dnow,DMC==1)
    labnow="糖尿病"
  }
  ###
  Title2(labnow)
  SUGYnow=GetSUGY(Dnow,Vnow,surgid)
  gc()

  PlotAdd(D21Rate(SUGYnow,Vnow,"surgid","surg",1,"NULL"),FigW = FigW1)
  PLegend(paste0(labnow,"患者行心血管手术的比例"))
  TLegend(paste0(labnow,"患者行心血管手术的比例（N(%)）"))
  TableAdd(TableRate2(SUGYnow,Vnow,"surgid","surg",V_Frate=1))
  ######
  rm(SUGYnow);gc()
  ######
  #性别与年龄  
  #Title2("性别年龄")
  for(j in 1:length(surgid)){
    PlotAdd(D31Rate(filter(Dnow,GENDER!=3),"ageg","GENDER",Vnow,surgid[j],1),FigW=FigW1,FigH =FigH1  )
    PLegend(paste0("不同性别年龄段",labnow,"患者行",get(paste0("lab_",surgid[j])),"的比例"))
    TLegend(paste0("不同性别年龄段",labnow,"患者行",get(paste0("lab_",surgid[j])),"的比例（N(%)）"))
    TableAdd( TableRate3(filter(Dnow,GENDER!=3),Vnow,"GENDER","ageg",surgid[j],V_Frate=1) )
  }}
  ###############
 
##################第5章--院内死亡##################################################
######
Title1(paste0(PTuse,"患者院内死亡"))
for(i in 1 :length(ANA_levels)){
  Vnow=ANA_levels[i]
  Dnow=get(paste0(ANA_levels[i]))
  labnow=get(paste0("lab_",ANA_levels[i]))
  
  Title2(labnow)
  PlotAdd(D11Rate(Dnow,Vnow,"STATUS",1) )
  PLegend(paste0(labnow,"患者院内死亡比例"))
  TLegend(paste0(labnow,"患者院内死亡比例（N(%)"))
  TableAdd(TablePCT1(Dnow,Vnow,"STATUS",PFrate=TRUE,TRPS=TRUE),blankLine=FALSE)
  NoteAdd(1,vAbn_NaN(Dnow,Vnow,"STATUS","出院时生存状态"),blankLine=TRUE)
  #性别与年龄  length(CMPTlevels)
  Title3("性别年龄",PageBreak = FALSE)
    PlotAdd(D31Rate(filter(Dnow,GENDER!=3),"ageg","GENDER",Vnow,"STATUS",1),FigW=FigW1,FigH =FigH1 )
    PLegend(paste0("不同性别年龄段",labnow,"患者院内死亡比例"))
    TLegend(paste0("不同性别年龄段",labnow,"患者院内死亡比例（N(%)）"))
    TableAdd(TableRate3(filter(Dnow,GENDER!=3),Vnow,"GENDER","ageg","STATUS",1),blankLine=FALSE )
    NoteAdd(1,vAbn_NaN(Dnow,Vnow,"STATUS","出院时生存状态"),blankLine=TRUE)

    Title3("并发症",PageBreak = FALSE)
    CMPTnow=GetCMPT(get(ANA_levels[i]),ANA_levels[i],cmptid,ANAnow="MORTALITY")
    if(i==1){CMPTnow=filter(CMPTnow,DMC==1)} 
    PlotAdd(D21Rate(CMPTnow,Vnow,"cmptid","STATUS",V_Frate=1),FigW = FigW1)
    PLegend(paste0("并发不同疾病的",labnow,"患者院内死亡比例"))
    TLegend(paste0("并发不同疾病的",labnow,"患者院内死亡比例（N(%)）"))
    TableAdd(TableRate2(CMPTnow,"cmptid",Vnow,"STATUS",V_Frate=1),blankLine=FALSE )
    NoteAdd(1,vAbn_NaN2(CMPTnow,"cmptid",Vnow,"STATUS","出院时生存状态"))
    ###############
    rm(CMPTnow)
    ##############  
    Title3("合并症",PageBreak = FALSE)
    CMBnow=GetCMB(get(ANA_levels[i]),ANA_levels[i],cmbid,ANAnow="MORTALITY")
    if(i==1){CMPTnow=filter(CMBnow,DMC==1)} 
    PlotAdd(D21Rate(CMBnow,Vnow,"cmbid","STATUS",V_Frate=1),FigW = FigW1)
    PLegend(paste0("合并不同疾病的",labnow,"患者院内死亡比例"))
    TLegend(paste0("合并不同疾病的",labnow,"患者院内死亡比例（N(%)）"))
    TableAdd(TableRate2(CMBnow,"cmbid",Vnow,"STATUS",V_Frate=1),blankLine=FALSE )
    NoteAdd(1,vAbn_NaN2(CMBnow,"cmbid",Vnow,"STATUS","出院时生存状态"))
    ###############
    rm(CMBnow)
    ##############  
  Title3("治疗",PageBreak = FALSE)
   SUGYnow=GetSUGY(get(ANA_levels[i]),ANA_levels[i],surgid,ANAnow="MORTALITY")
   if(i==1){SUGYnow=filter(SUGYnow,DMC==1)} 
   PlotAdd(D21Rate(SUGYnow,Vnow,"surgid","STATUS",V_Frate=1),FigW = FigW1)
   PLegend(paste0(labnow,"行不同治疗的院内死亡比例"))
   TLegend(paste0(labnow,"行不同治疗的院内死亡比例（N(%)）"))
   TableAdd(TableRate2(SUGYnow,"surgid",Vnow,"STATUS",V_Frate=1),blankLine=FALSE )
   NoteAdd(1,vAbn_NaN2(SUGYnow,"surgid",Vnow,"STATUS","出院时生存状态"))
###############
  rm(SUGYnow)
##############
  if(i==1){
  Title3("地区")
   PlotAdd( MapPlot(DMMltyMAP,"ByID",paste0(DMC_levels[1],"MLTY"),map_color_use=map_red),FigW=mapW,FigH=mapH)
   PLegend(paste0("各省（直辖市）",labnow,"患者院内死亡比例"),blankLine = FALSE)
   NoteAdd(1,"西藏、宁夏及港澳台地区不展示。")

   PlotAdd( MapPlot(DMMltyMAP,"ByID",paste0(DMC_levels[1],"MLTY1"),map_color_use=map_red),FigW=mapW,FigH=mapH)
   PLegend(paste0("各省（直辖市）诊治本地",labnow,"患者院内死亡比例"),blankLine=FALSE)
   NoteAdd(1,"西藏、宁夏及港澳台地区不展示。")
   #######
   Dnow=filter(Dnow,DMC==1) 
   CAUSECh=data.frame(with(Dnow,mget(c("GLOBALID","STATUS","PRO_ID","TRAVEL",Vnow))))
   names(CAUSECh)=c("GLOBALID","STATUS","PRO_ID","TRAVEL","CAUSE")
   CAUSECh1=filter(CAUSECh,CAUSE==1);CAUSECh1$TreatNote=1;
   CAUSECh2=filter(CAUSECh,CAUSE==1&TRAVEL==2);CAUSECh2$TreatNote=2;
   CAUSECh=rbind(CAUSECh1,CAUSECh2)
   rm(CAUSECh1,CAUSECh2)
   tally(group_by(CAUSECh,TreatNote))
   ######
   PlotAdd(D21Rate(CAUSECh,"TreatNote","PRO_ID","STATUS",1,"NULL")+ADDHline(get(paste0(DMC_levels[1],"Mlty"))),FigW=FigW1)
   PLegend(paste0("各省（直辖市）诊治",labnow,"患者院内死亡比例"),blankLine=FALSE)
   NoteAdd(1,paste0("参考线说明：整体",labnow,"患者院内死亡比例（",Rate_legend(get(paste0(DMC_levels[1],"Mlty"))),"）。"),blankLine=FALSE)
   TLegend(paste0("各省（直辖市）诊治",labnow,"患者院内死亡比例（N(%)）"))
   TableAdd(TableRate2(CAUSECh,"TreatNote","PRO_ID","STATUS",V_Frate=1))

}}
##################第6章--住院天数##################################################
######
Title1(paste0(PTuse,"患者住院天数"))
for(i in 1 :length(ANA_levels)){
  Vnow=ANA_levels[i]
  Dnow=get(paste0(ANA_levels[i]))
  labnow=get(paste0("lab_",ANA_levels[i]))
  
  Title2(labnow)
  PlotAdd(D11Stat(Dnow,Vnow,"SJDAYS") )
  PLegend(paste0(labnow,"患者住院天数"),blankLine=FALSE)
  if(i==1){blankLine()}else{NoteAdd(1,boxMdnPoi,blankLine=TRUE)}
  TLegend(paste0(labnow,"患者住院天数"))
  TableAdd(TableSTAT1(Dnow,Vnow,"SJDAYS"),blankLine=FALSE )
  NoteAdd(1,vAbn_NaN(Dnow,Vnow,"SJDAYS","住院天数"),blankLine=TRUE)
  
  Title3("并发症",PageBreak = FALSE)
  CMPTnow=GetCMPT(get(ANA_levels[i]),ANA_levels[i],cmptid,ANAnow="LOS")
  if(i==1){CMPTnow=filter(CMPTnow,DMC==1)} 
  PlotAdd(D21Stat(CMPTnow,"cmptid",Vnow,"SJDAYS"),FigW = FigW1)
  PLegend(paste0("并发不同疾病的",labnow,"患者住院天数"))
  TLegend(paste0("并发不同疾病的",labnow,"患者住院天数（Median(Q1-Q3)）"))
  TableAdd(TableSTAT2(CMPTnow,"cmptid",Vnow,"SJDAYS"),blankLine=FALSE )
  NoteAdd(1,vAbn_NaN2(CMPTnow,"cmptid",Vnow,"SJDAYS","住院天数"))
  ###############
  rm(CMPTnow)
  ##############  
  Title3("合并症",PageBreak = FALSE)
  CMBnow=GetCMB(get(ANA_levels[i]),ANA_levels[i],cmbid,ANAnow="LOS")
  if(i==1){CMPTnow=filter(CMBnow,DMC==1)} 
  PlotAdd(D21Stat(CMBnow,"cmbid",Vnow,"SJDAYS"),FigW = FigW1)
  PLegend(paste0("合并不同疾病的",labnow,"患者住院天数"))
  TLegend(paste0("合并不同疾病的",labnow,"患者住院天数（Median(Q1-Q3)）"))
  TableAdd(TableSTAT2(CMBnow,"cmbid",Vnow,"SJDAYS"),blankLine=FALSE )
  NoteAdd(1,vAbn_NaN2(CMBnow,"cmbid",Vnow,"SJDAYS","住院天数"))
  ###############
  rm(CMBnow)
  #########
  Title3(paste0("治疗"))
  SUGYnow=GetSUGY(get(ANA_levels[i]),ANA_levels[i],surgid,ANAnow="LOS")
  if(i==1){SUGYnow=filter(SUGYnow,DMC==1)} 
  PlotAdd(D21Stat(SUGYnow,"surgid",Vnow,"SJDAYS"),FigW =FigW1 )
  PLegend(paste0("行不同治疗的",labnow,"患者住院天数"),blankLine=FALSE)
  NoteAdd(1,boxMdnPoi,blankLine=TRUE)
  TLegend(paste0("行不同治疗的",labnow,"患者住院天数（Median(Q1-Q3)）"))
  TableAdd(TableSTAT2(SUGYnow,"surgid",Vnow,"SJDAYS"),blankLine=FALSE )
  NoteAdd(1,vAbn_NaN2(SUGYnow,"surgid",Vnow,"SJDAYS","住院天数"),blankLine=TRUE)

  ##########
  rm(SUGYnow)
  ##########
  
  Title3(paste0("地区"))
  PlotAdd(D21Stat(Dnow,"PRO_ID",Vnow,"SJDAYS"),FigW =FigW1)
  PLegend(paste0("不同地区",labnow,"患者住院天数"))
  TLegend(paste0("不同地区",labnow,"患者住院天数（Median(Q1-Q3)）"))
  TableAdd(TableSTAT2(Dnow,Vnow,"PRO_ID","SJDAYS"),blankLine=FALSE )
  NoteAdd(1,vAbn_NaN(Dnow,Vnow,"SJDAYS","住院天数"),blankLine=TRUE)
}

##################第8章--住院费用##################################################
######
Title1("慢性肾脏病患者住院费用")
Title2(paste0("付款方式与费用构成"))
for(i in 1 :length(ANA_levels)){
  Vnow=ANA_levels[i]
  Dnow=get(paste0(ANA_levels[i]))
  labnow=get(paste0("lab_",ANA_levels[i]))
  
  Title3(labnow,PageBreak=FALSE)
  PlotAdd(D11PCT(Dnow,Vnow,"INSURANC"),FigW=FigW1 )
  PLegend(paste0(labnow,"患者付款方式"),blankLine=FALSE)
  TLegend(paste0(labnow,"患者付款方式"))
  TableAdd(TablePCT1(Dnow,Vnow,"INSURANC"),blankLine=FALSE)
  
  PlotAdd(PLotCOST(Dnow,Vnow))
  PLegend(paste0(labnow,"患者费用构成"))
  TLegend(paste0(labnow,"患者费用构成（百万元(%)）"))
  TableAdd(COSTcompose(Dnow,Vnow),blankLine=FALSE)
  NoteAdd(1,vAbn_NaN(Dnow,Vnow,"FEE_TOTA","住院总费用"))
  NoteAdd(2,"分项费用占比=分项费用/分项费用之和*100%。")
  NoteAdd(3,"总费用：指患者住院总费用。各病例分项费用之和与总费用允许存在误差(<1.2)）。",blankLine=TRUE)
}
Title2(paste0("住院总费用"))
  for(i in 1 :length(ANA_levels)){
    Vnow=ANA_levels[i]
    Dnow=get(paste0(ANA_levels[i]))
    labnow=get(paste0("lab_",ANA_levels[i]))
    
    Title3(labnow,PageBreak = FALSE)   
  PlotAdd(D11Stat(Dnow,Vnow,"FEE_TOTA",Violin=TRUE) )
  PLegend(paste0(labnow,"患者住院总费用"),blankLine=FALSE)
  if(i==1){blankLine()}else{NoteAdd(1,boxMdnPoi,blankLine=TRUE)}
  TLegend(paste0(labnow,"患者住院总费用"))
  TableAdd(TableSTAT1(Dnow,Vnow,"FEE_TOTA"),blankLine=FALSE )
  NoteAdd(1,vAbn_NaN(Dnow,Vnow,"FEE_TOTA","住院总费用"),blankLine=TRUE)
  
  Title4(lab_INSURANC)
  PlotAdd(D21Stat(Dnow,"INSURANC",Vnow,"FEE_TOTA"),FigW=FigW1)
  PLegend(paste0(labnow,"不同付款方式住院总费用"),blankLine=FALSE)
  NoteAdd(1,boxMdnPoi,blankLine=TRUE)
  TLegend(paste0(labnow,"不同付款方式住院总费用（Median(Q1-Q3)）"))
  TableAdd(TableSTAT2(Dnow,Vnow,"INSURANC","FEE_TOTA"),blankLine=FALSE )
  NoteAdd(1,vAbn_NaN(Dnow,Vnow,"FEE_TOTA","住院总费用"),blankLine=TRUE)
  ###
  Title4("并发症",PageBreak = FALSE)
  CMPTnow=GetCMPT(get(ANA_levels[i]),ANA_levels[i],cmptid,ANAnow="COST")
  if(i==1){CMPTnow=filter(CMPTnow,DMC==1)} 
  PlotAdd(D21Stat(CMPTnow,"cmptid",Vnow,"FEE_TOTA"),FigW = FigW1)
  PLegend(paste0("并发不同疾病的",labnow,"患者住院总费用"))
  TLegend(paste0("并发不同疾病的",labnow,"患者住院总费用（Median(Q1-Q3)））"))
  TableAdd(TableSTAT2(CMPTnow,Vnow,"cmptid","FEE_TOTA"),blankLine=FALSE )
  NoteAdd(1,vAbn_NaN2(CMPTnow,Vnow,"cmptid","FEE_TOTA","住院总费用"))
  ###############
  rm(CMPTnow)
  ##############  
  Title4("合并症",PageBreak = FALSE)
  CMBnow=GetCMB(get(ANA_levels[i]),ANA_levels[i],cmbid,ANAnow="COST")
  if(i==1){CMPTnow=filter(CMBnow,DMC==1)} 
  PlotAdd(D21Stat(CMBnow,"cmbid",Vnow,"FEE_TOTA"),FigW = FigW1)
  PLegend(paste0("合并不同疾病的",labnow,"患者住院总费用"))
  TLegend(paste0("合并不同疾病的",labnow,"患者住院总费用（Median(Q1-Q3)）"))
  TableAdd(TableSTAT2(CMBnow,Vnow,"cmbid","FEE_TOTA"),blankLine=FALSE )
  NoteAdd(1,vAbn_NaN2(CMBnow,Vnow,"cmbid","FEE_TOTA","住院总费用"))
  ###############
  rm(CMBnow)

  Title4(paste0("不同治疗"))
  SUGYnow=GetSUGY(get(ANA_levels[i]),ANA_levels[i],surgid_levels,ANAnow="COST")
  if(i==1){SUGYnow=filter(SUGYnow,DMC==1)} 
  PlotAdd(D21Stat(SUGYnow,"surgid",Vnow,"FEE_TOTA"),FigW=FigW1)
  PLegend(paste0("行不同治疗的",labnow,"患者住院总费用"),blankLine=FALSE)
  NoteAdd(1,boxMdnPoi,blankLine=TRUE)
  TLegend(paste0("行不同治疗的",labnow,"患者住院总费用（Median(Q1-Q3)）"))
  TableAdd(TableSTAT2(SUGYnow,Vnow,"surgid","FEE_TOTA"),blankLine=FALSE )
  NoteAdd(1,vAbn_NaN2(SUGYnow,Vnow,"surgid","FEE_TOTA","住院总费用"),blankLine=TRUE)

  Title4(paste0("地区"))
  PlotAdd(D21Stat(Dnow,"PRO_ID",Vnow,"FEE_TOTA"),FigW =FigW1)
  PLegend(paste0("不同地区",labnow,"患者住院总费用"))
  TLegend(paste0("不同地区",labnow,"患者住院总费用（Median(Q1-Q3)）"))
  TableAdd(TableSTAT2(Dnow,Vnow,"PRO_ID","FEE_TOTA"),blankLine=FALSE )
  NoteAdd(1,vAbn_NaN(Dnow,Vnow,"FEE_TOTA","住院总费用"),blankLine=TRUE)
}

##################第9章--患者差异--城乡##################################################
######
###################################
DM=filter(DMC,DMC==1)
DM1=filter(DMG,DMG==1)
DM2=filter(DMG,DMG==2)
ANAMain_levels=c("DM","DM1","DM2")
lab_ANAMain=c("糖尿病","I型糖尿病","II型糖尿病")
labnow=lab_RSDENCE
Vnow="RSDENCE"
####################################

Title1(paste0(PTuse,"患者城乡差异"))
for(i in 1 :length(ANAMain_levels)){
  Dnow=get(paste0(ANAMain_levels[i]))
  labPTnow=lab_ANAMain[i]
  PTnow=ifelse(i==1,"DMC","DMG")
  
Title2(labPTnow)  
Title3("入院科别")
PlotAdd(D11PCT(Dnow,Vnow,"admsDp")  )
PLegend(paste0("城乡",labPTnow,"患者入院科别分布"))
TLegend(paste0("城乡",labPTnow,"患者入院科别分布（N(%)）"))
TableAdd(TablePCT1(Dnow,Vnow,"admsDp"),blankLine = FALSE)
NoteAdd(1,"根据付款方式识别患者城乡来源。")
NoteAdd(2,vAbn_NaN(Dnow,PTnow,Vnow,"患者城乡来源"))
NoteAdd(3,'仅列出入院科别Top10科别，其余归入"其他"。')
NoteAdd(4,ChisqNote1(Dnow,Vnow,"admsDp"),blankLine = TRUE)


Title3("并发症")
CMPTnow=GetCMPT(Dnow,Vnow,cmptid)
PlotAdd(D21Rate(CMPTnow,Vnow,"cmptid","cmpt",1))
PLegend(paste0(labnow,labPTnow,"患者并发疾病"))
TLegend(paste0(labnow,labPTnow,"患者并发疾病比例（N(%)）"))
TableAdd(TablePCT1Vec(Dnow,Vnow,cmptid,V_Frate=1),blankLine = FALSE)
NoteAdd(1,"根据付款方式识别患者城乡来源。")
NoteAdd(2,vAbn_NaN(Dnow,PTnow,Vnow,"患者城乡来源"))
NoteAdd(3,ChisqNote1Vec(Dnow,Vnow,cmptid,"行不同治疗的比例"),blankLine = TRUE)
######
rm(CMPTnow)
######
Title3("合并症")
CMBnow=GetCMB(Dnow,Vnow,cmbid)
PlotAdd(D21Rate(CMBnow,Vnow,"cmbid","cmb",1))
PLegend(paste0(labnow,labPTnow,"患者合并疾病"))
TLegend(paste0(labnow,labPTnow,"患者合并疾病比例（N(%)）"))
TableAdd(TablePCT1Vec(Dnow,Vnow,cmbid,V_Frate=1),blankLine = FALSE)
NoteAdd(1,"根据付款方式识别患者城乡来源。")
NoteAdd(2,vAbn_NaN(Dnow,PTnow,Vnow,"患者城乡来源"))
NoteAdd(3,ChisqNote1Vec(Dnow,Vnow,cmbid,"行不同治疗的比例"),blankLine = TRUE)
######
rm(CMPTnow)
######
Title3("治疗")
SUGYnow=GetSUGY(Dnow,Vnow,surgid)
PlotAdd(D21Rate(SUGYnow,Vnow,"surgid","surg",1))
PLegend(paste0(labnow,labPTnow,"患者治疗"))
TLegend(paste0(labnow,labPTnow,"治疗比例（N(%)）"))
TableAdd(TablePCT1Vec(Dnow,Vnow,surgid_levels,V_Frate=1),blankLine = FALSE)
NoteAdd(1,"根据付款方式识别患者城乡来源。")
NoteAdd(2,vAbn_NaN(Dnow,PTnow,Vnow,"患者城乡来源"))
NoteAdd(3,ChisqNote1Vec(Dnow,Vnow,surgid_levels,"行不同治疗的比例"),blankLine = TRUE)
######
rm(SUGYnow)
######
Title3("诊疗",PageBreak = FALSE)
PlotAdd(CostLOSM1(Dnow,Vnow,labPTnow))
PLegend(paste0("城乡",labPTnow,"患者诊疗"))
TLegend(paste0("城乡",labPTnow,"患者诊疗"))
TableAdd(TCostFeeM1(Dnow,Vnow,labPTnow),blankLine = FALSE)
NoteAdd(1,"根据付款方式识别患者城乡来源。")
NoteAdd(2,paste0(vAbn_NaN(filter(Dnow,!is.na(RSDENCE)),"RSDENCE","STATUS","患者出院时生存状态"),ChisqNote1(Dnow,Vnow,"STATUS")))
NoteAdd(3,paste0(vAbn_NaN(filter(Dnow,!is.na(RSDENCE)),"RSDENCE","SJDAYS","患者住院天数"),TestSTAT1(Dnow,Vnow,"SJDAYS")))
NoteAdd(4,paste0(vAbn_NaN(filter(Dnow,!is.na(RSDENCE)),"RSDENCE","FEE_TOTA","患者住院总费用"),TestSTAT1(Dnow,Vnow,"FEE_TOTA")))


PlotAdd(D11PCT(Dnow,Vnow,"DSCWAY"))
PLegend(paste0("城乡",labPTnow,"离院方式"))
TLegend(paste0("城乡",labPTnow,"离院方式（N(%)）"))
TableAdd(TablePCT1(Dnow,Vnow,"DSCWAY"),blankLine = FALSE)
NoteAdd(1,"根据付款方式识别患者城乡来源。")
NoteAdd(2,vAbn2_NaN(Dnow,PTnow,"DSCWAY",Vnow,"患者离院方式或城乡来源"))
NoteAdd(3,ChisqNote1(Dnow,Vnow,"DSCWAY"),blankLine = TRUE)
}

#writeDoc( doc, file = doc.filename )

################第九章：医疗服务评估###################
#################
load("D:/HQMS/HQMS_20160630/DM/R/orgfdm.Rdata")
OrgidDM=sasdataload
ORGSPECuse=subset(OrgidDM,select=c(ORGID,ORGSPEC,LEVEL))

load("D:/HQMS/HQMS_20160630/DM/R/dmaccsm.Rdata")
Accsment=sasdataload

ACCSHQMS=subset(filter(Accsment,ASSCM==1&ORGSPEC==1),select=c(ORGID,COST,LOS,MRLTY,NOTCURE,MEDANTI,REHOS,AVEN,TRAVEL,NOTADVC))
ACCSDM=subset(filter(Accsment,ASSCM!=1&ORGSPEC==1&AVEN>30),select=c(ORGID,COST,LOS,MRLTY,NOTCURE,MEDANTI,REHOS,AVEN,TRAVEL,NOTADVC,ASSCM))
names(ACCSDM)=c("ORGID","COST1","LOS1","MRLTY1","NOTCURE1","MEDANTI1","REHOS1","AVEN1","TRAVELT1","NOTADVC1","ASSCM")
ACSMHos=left_join(ACCSHQMS,ACCSDM)%>%
  mutate(TRAVELT=TRAVEL)
ACSMHos=left_join(ACSMHos,ORGSPECuse)%>%
  mutate(AVENG=ifelse(AVEN1<=300,ceiling(AVEN1/50),ifelse(AVEN1<=500,7,ifelse(AVEN1<=1000,8,9))))

###
OrgidInfo=OrgidDM
####
rm(sasdataload,Accsment,ACCSDM,ACCSHQMS,ORGSPECuse,OrgidDM)
################

Title1(paste0(PTuse,"医疗服务评估"))
Title2("HQMS备案医院")
##肾脏病医疗服务评估（仅针对综合医院）
#综合医院
PlotAdd(D11PCT(OrgidInfo,"PRO_ID","ORGSPEC")+xlab("")+ylab(""),FigW = FigW1)
PLegend("HQMS备案医院中医院类型分布")
TLegend("HQMS备案医院中医院类型分布（N(%)）")
TableAdd(TablePCT1(OrgidInfo,"PRO_ID","ORGSPEC",ALL=TRUE,TRPS=TRUE))

Title2("就诊医院分布")
#########HQMS基准线
load("D:/HQMS/HQMS_20160630/DM/R/acsmdm.Rdata")
ACSMDM=subset(filter(sasdataload,DM==1)%>%
  mutate(ASSCM=2,TRAVELT=ifelse(TRAVEL==0,2,TRAVEL)),select=-c(DM,IGT,TRAVEL))

ACSMIGT=subset(filter(sasdataload,IGT==1)%>%
  mutate(ASSCM=3,TRAVELT=ifelse(TRAVEL==0,2,TRAVEL)),select=-c(DM,IGT,TRAVEL))

ACSMnow=rbind(ACSMDM,ACSMIGT)
#
rm(sasdataload)
##################
PlotAdd( D11PCT(ACSMnow,"ASSCM","ORGSPEC") )
PLegend(paste0(PTuse,"患者就诊医院类型"))
TLegend(paste0(PTuse,"患者就诊医院类型（N(%)）"))
TableAdd(TablePCT1(ACSMnow,"ASSCM","ORGSPEC"))

PlotAdd(D11PCT(ACSMnow,"ASSCM","LEVEL"))
PLegend(paste0(PTuse,"患者就诊医院级别"))
TLegend(paste0(PTuse,"患者就诊医院级别（N(%)）"))
TableAdd(TablePCT1(ACSMnow,"ASSCM","LEVEL"))

#综合医院
#ACSMCKD=filter(ACSMCKD,ORGSPEC==1)
#PlotAdd(D21PCT(ACSMCKD,"LEVEL","NEPHNEW","FIRSTREC"))
#PLegend("CKD患者就诊医院类型")
#TLegend("CKD患者就诊医院类型（N(%)）")
#TableAdd(TableRate2(ACSMCKD,"LEVEL","NEPHNEW","FIRSTREC",ALL=TRUE))

Title2("医疗服务评估",PageBreak=FALSE)
##服务能力:综合医院月均诊治患者人次
Title3("服务能力",PageBreak=FALSE)
Title4("月均诊治CKD患者人次")
####

PlotAdd(D21PCT(filter(ACSMHos,ORGSPEC==1),"ASSCM","LEVEL","AVENG")+ylab(""))
PLegend("综合医院月均诊治糖尿病患者人次")
TLegend("综合医院月均诊治糖尿病患者人次（医院：N(%)）")
TableAdd(TableRate2(ACSMHos,"ASSCM","LEVEL","AVENG"))

Title4("收治异地患者")
PlotAdd(DPACSM("TRAVELT"),FigW = FigW1,FigH = FigH3)
PLegend(paste0("不同医院收治异地",PTuse,"患者比例"),blankLine = FALSE)
NoteAdd(1,paste0("参考线说明：蓝色虚线为不同医院收治异地",ASSCM_levels[2],"患者比例的25%、75%分位数。"))
NoteAdd(2,paste0("参考线说明：红色虚线为不同医院收治异地",ASSCM_levels[3],"患者比例的25%、75%分位数。"))
NoteAdd(3,"参考线说明：灰色实线为y=x。",blankLine=TRUE)
TLegend(paste0("不同地区诊治异地",ASSCM_levels[2],"患者比例"))
TableAdd(TableRate2(ACSMDM,"LEVEL","PRO_ID","TRAVELT",V_Frate=1,ALL=TRUE),blankLine=FALSE )
NoteAdd(1,vAbn_NaN(ACSMDM,"LEVEL","TRAVELT","常住地"),blankLine=TRUE)
TLegend(paste0("不同地区诊治异地",ASSCM_levels[3],"比例（N(%)）"))
TableAdd(TableRate2(ACSMIGT,"LEVEL","PRO_ID","TRAVELT",V_Frate=1,ALL=TRUE),blankLine=FALSE )
NoteAdd(1,vAbn_NaN(ACSMIGT,"LEVEL","TRAVELT","常住地"),blankLine=TRUE)

#病学专科诊治患者：
#服务能力---月均例数
#服务效率---住院时间、住院费用
Title3("服务效率")
Title4("费用")
PlotAdd(DPACSM("FEE_TOTA"),FigW = FigW1,FigH = FigH3)
PLegend(paste0("不同医院",PTuse,"患者次均住院总费用"),blankLine=FALSE)
NoteAdd(1,paste0("参考线说明：蓝色虚线为不同医院",ASSCM_levels[2],"患者次均住院总费用25%、75%分位数。"))
NoteAdd(2,paste0("参考线说明：红色虚线为不同医院",ASSCM_levels[3],"患者次均住院总费用的25%、75%分位数。"))
NoteAdd(3,"参考线说明：灰色实线为y=x。",blankLine=TRUE)
TLegend(paste0("不同地区诊治",ASSCM_levels[2],"患者次均住院总费用（元）（Mean(STD)）"))
TableAdd(TableSTAT2(ACSMDM,"LEVEL","PRO_ID","FEE_TOTA",ALL=TRUE,Type="Mean"),blankLine=FALSE )
NoteAdd(1,vAbn_NaN(ACSMDM,"LEVEL","FEE_TOTA","住院总费用"),blankLine=TRUE)
TLegend(paste0("不同地区诊治",ASSCM_levels[3],"患者次均住院总费用（元）（Mean(STD)）"))
TableAdd(TableSTAT2(ACSMIGT,"LEVEL","PRO_ID","FEE_TOTA",ALL=TRUE,Type="Mean"),blankLine=FALSE )
NoteAdd(1,vAbn_NaN(ACSMIGT,"LEVEL","FEE_TOTA","住院总费用"),blankLine=TRUE)

Title4("住院天数")
PlotAdd(DPACSM("SJDAYS"),FigW = FigW1,FigH = FigH3)
PLegend(paste0("不同医院",PTuse,"患者次均住院天数"),blankLine=FALSE)
NoteAdd(1,paste0("参考线说明：蓝色虚线为不同医院",ASSCM_levels[2],"患者次均住院天数25%、75%分位数。"))
NoteAdd(2,paste0("参考线说明：红色虚线为不同医院",ASSCM_levels[3],"患者次均住院天数的25%、75%分位数。"))
NoteAdd(3,"参考线说明：灰色实线为y=x。",blankLine=TRUE)
TLegend(paste0("不同地区诊治",ASSCM_levels[2],"患者住院天数（Mean(STD)）"))
TableAdd(TableSTAT2(ACSMDM,"LEVEL","PRO_ID","SJDAYS",ALL=TRUE,Type="Mean"),blankLine=FALSE )
NoteAdd(1,vAbn_NaN(ACSMDM,"LEVEL","SJDAYS","住院天数"),blankLine=TRUE)
TLegend(paste0("不同地区诊治",ASSCM_levels[3],"患者住院天数（Mean(STD)）"))
TableAdd(TableSTAT2(ACSMIGT,"LEVEL","PRO_ID","SJDAYS",ALL=TRUE,Type="Mean"),blankLine=FALSE )
NoteAdd(1,vAbn_NaN(ACSMIGT,"LEVEL","SJDAYS","住院天数"),blankLine=TRUE)


#服务质量---院内死亡比例、未救治率、非预期再住院率、抗菌药物使用比例
Title3("服务质量")
Title4("院内死亡率")
PlotAdd(DPACSM("STATUS"),FigW = FigW1,FigH = FigH3)
PLegend(paste0("不同医院",PTuse,"患者院内死亡比例"),blankLine=FALSE)
NoteAdd(1,paste0("参考线说明：蓝色虚线为不同医院",ASSCM_levels[2],"患者院内死亡比例数的25%、75%分位数。"))
NoteAdd(2,paste0("参考线说明：红色虚线为不同医院",ASSCM_levels[3],"患者院内死亡比例的25%、75%分位数。"))
NoteAdd(3,"参考线说明：灰色实线为y=x。",blankLine=TRUE)
TLegend(paste0("不同地区",ASSCM_levels[2],"患者院内死亡比例"))
TableAdd(TableRate2(ACSMDM,"LEVEL","PRO_ID","STATUS",V_Frate=1,ALL=TRUE),blankLine=FALSE )
NoteAdd(1,vAbn_NaN(ACSMDM,"LEVEL","STATUS","出院时生存状态"),blankLine=TRUE)
TLegend(paste0("不同地区",ASSCM_levels[3],"患者院内死亡比例（N(%)）"))
TableAdd(TableRate2(ACSMIGT,"LEVEL","PRO_ID","STATUS",V_Frate=1,ALL=TRUE),blankLine=FALSE )
NoteAdd(1,vAbn_NaN(ACSMIGT,"LEVEL","STATUS","出院时生存状态"),blankLine=TRUE)

Title4("未救治率")
PlotAdd(DPACSM("NOTCURE"),FigW = FigW1,FigH = FigH3)
PLegend(paste0("不同医院",PTuse,"患者未救治比例"),blankLine=FALSE)
NoteAdd(1,paste0("参考线说明：蓝色虚线为不同医院",ASSCM_levels[2],"患者未救治比例的25%、75%分位数。"))
NoteAdd(2,paste0("参考线说明：红色虚线为不同医院",ASSCM_levels[3],"患者未救治比例的25%、75%分位数。"))
NoteAdd(3,"参考线说明：灰色实线为y=x。",blankLine=TRUE)
TLegend(paste0("不同地区",ASSCM_levels[2],"患者未救治率"))
TableAdd(TableRate2(ACSMDM,"LEVEL","PRO_ID","NOTCURE",V_Frate=1,ALL=TRUE))
TLegend(paste0("不同地区",ASSCM_levels[3],"患者未救治率（N(%)）"))
TableAdd(TableRate2(ACSMIGT,"LEVEL","PRO_ID","NOTCURE",V_Frate=1,ALL=TRUE))

Title4("非医嘱离院率")
PlotAdd(DPACSM("NOTADVC"),FigW = FigW1,FigH = FigH3)
PLegend(paste0("不同医院",PTuse,"患者非医嘱离院比例"),blankLine=FALSE)
NoteAdd(1,paste0("参考线说明：蓝色虚线为不同医院",ASSCM_levels[2],"患者非医嘱离院比例的25%、75%分位数。"))
NoteAdd(2,paste0("参考线说明：红色虚线为不同医院",ASSCM_levels[3],"患者非医嘱离院比例的25%、75%分位数。"))
NoteAdd(3,"参考线说明：灰色实线为y=x。",blankLine=TRUE)
TLegend(paste0("不同地区",ASSCM_levels[2],"患者非医嘱离院比例（N(%)）"))
TableAdd(TableRate2(ACSMDM,"LEVEL","PRO_ID","NOTADVC",V_Frate=1,ALL=TRUE))
TLegend(paste0("不同地区",ASSCM_levels[3],"患者非医嘱离院（N(%)）"))
TableAdd(TableRate2(ACSMIGT,"LEVEL","PRO_ID","NOTADVC",V_Frate=1,ALL=TRUE))

Title4("31天内非计划再住院率")
PlotAdd(DPACSM("REHOS"),FigW = FigW1,FigH = FigH3)
PLegend(paste0("不同医院",PTuse,"患者31天内非计划再住院率"),blankLine=FALSE)
NoteAdd(1,paste0("参考线说明：蓝色虚线为不同医院",ASSCM_levels[2],"患者31天内非计划再住院比例的25%、75%分位数。"))
NoteAdd(2,paste0("参考线说明：红色虚线为不同医院",ASSCM_levels[3],"患者31天内非计划再住院比例的25%、75%分位数。"))
NoteAdd(3,"参考线说明：灰色实线为y=x。",blankLine=TRUE)
TLegend(paste0("不同地区",ASSCM_levels[2],"患者 31天内非计划再住院率（N(%)）"))
TableAdd(TableRate2(ACSMDM,"LEVEL","PRO_ID","REHOS",V_Frate=1,ALL=TRUE))
TLegend(paste0("不同地区",ASSCM_levels[3],"患者31天内非计划再住院率（N(%)）"))
TableAdd(TableRate2(ACSMIGT,"LEVEL","PRO_ID","REHOS",V_Frate=1,ALL=TRUE))

Title4("抗菌药物使用率")
PlotAdd(DPACSM("MEDANTI"),FigW = FigW1,FigH = FigH3)
PLegend(paste0("不同医院",PTuse,"患者使用抗菌药物比例"),blankLine=FALSE)
NoteAdd(1,paste0("参考线说明：蓝色虚线为不同医院",ASSCM_levels[2],"患者使用抗菌药物比例的25%、75%分位数。"))
NoteAdd(2,paste0("参考线说明：红色虚线为不同医院",ASSCM_levels[3],"患者使用抗菌药物比例的25%、75%分位数。"))
NoteAdd(3,"参考线说明：灰色实线为y=x。",blankLine=TRUE)
TLegend(paste0("不同地区",ASSCM_levels[2],"患者使用抗菌药物比例"))
TableAdd(TableRate2(ACSMDM,"LEVEL","PRO_ID","MEDANTI",V_Frate=1,ALL=TRUE),blankLine=FALSE )
NoteAdd(1,vAbn_NaN(ACSMDM,"LEVEL","MEDANTI","住院总费用"),blankLine=TRUE)
TLegend(paste0("不同地区",ASSCM_levels[3],"患者使用抗菌药物比例（N(%)）"))
TableAdd(TableRate2(ACSMIGT,"LEVEL","PRO_ID","MEDANTI",V_Frate=1,ALL=TRUE),blankLine=FALSE )
NoteAdd(1,vAbn_NaN(ACSMIGT,"LEVEL","MEDANTI","住院总费用"),blankLine=TRUE)

writeDoc( doc, file = doc.filename )
rm(ACSMDM,ACSMIGT,ACSMnow,ACSMHos)
rm(DMC,DMG,DM,DM1,DM2,CAUSECh,CMBnow,RiskDM,Dnow,OrgidInfo)





