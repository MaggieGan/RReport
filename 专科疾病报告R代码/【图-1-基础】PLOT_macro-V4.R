#参数定义
#1.vgroup:分组-ggplot2中指定group参数
#2.vsubgroup:次分组-ggplot2中指定position参数（如goem_bar:stack\fill\dodge)
#3.vfact:分面
#4.vstat:统计的连续变量
#标签使用N_legend\Rate_legend统一定义

###缺失
Abnormal_paste=function(DAbnormal,basicNote,NPT){
  for(i in 1:NPT){
    ADD_note=paste0(basicNote,DAbnormal$note[i])
    basicNote=paste0(ADD_note,"；")
  }
  ADD_note=paste0(ADD_note,"。")
  return(ADD_note)
}
#用NaN标注缺失异常的变量
vAbn_NaN=function(Dplot,group_levels,vlabel){
  #患者人数-分母
  DMT=tally(group_by(Dplot,vgroup))
  #缺失人数-分子
  NMRT=tally(group_by(filter(Dplot,is.na(vAbn)),vgroup))
  #缺失比例
  Abnormal_tmp=left_join(rename(DMT,c(n="DMT")),rename(NMRT,c(n="NMRT")))
  Abnormal_tmp=filter(Abnormal_tmp,!is.na(NMRT))
  levelupdate=grouporder(Abnormal_tmp,group_levels)
  Abnormal=levelupdate[[1]]
  Abnormal=ddply(Abnormal,NULL,transform,note=paste0(vgroup,"：",NMRT,"(",Abnormalnote=Rate_legend(NMRT/DMT),")"))
  ##print(Abnormal)
  Ngroup=length(levelupdate[[2]])
  if(Ngroup>=1){
    Abnormal_note=paste0(vlabel,"缺失或异常的患者病例不纳入分析，")
    Abnormal_note=Abnormal_paste(Abnormal,Abnormal_note,Ngroup)
  }
  else{
    Abnormal_note=paste0(vlabel,"缺失或异常的患者病例不纳入分析，暂无缺失或异常。")
  }
  return(Abnormal_note)
}
#vAbn_NaN(rename(risk_use,c(risk="vgroup",from_pro="vAbn")),risk_levels,"常住地")
#vAbn_NaN(rename(OLDCVD_cause,c(R_order="vgroup",fee_own="vAbn")),OLDCVD_cause_levels,"常住地")
#特殊缺失

Speci_NaN=function(Dplot,group_levels,vlabel){
  #患者人数-分母
  DMT=tally(group_by(Dplot,vgroup))
  #缺失人数-分子
  NMRT=tally(group_by(filter(Dplot,is.na(vAbn)),vgroup))
  #缺失比例
  Abnormal_tmp=left_join(rename(DMT,c(n="DMT")),rename(NMRT,c(n="NMRT")))
  Abnormal_tmp=filter(Abnormal_tmp,!is.na(NMRT))
  levelupdate=grouporder(Abnormal_tmp,group_levels)
  Abnormal=levelupdate[[1]]
  Abnormal=ddply(Abnormal,NULL,transform,note=paste0(vgroup,"：",NMRT,"(",Abnormalnote=Rate_legend(NMRT/DMT),")"))
  ##print(Abnormal)
  Ngroup=length(levelupdate[[2]])
  if(Ngroup>=1){
    Abnormal_note=paste0(vlabel,"的病例不纳入分析，")
    Abnormal_note=Abnormal_paste(Abnormal,Abnormal_note,Ngroup)
  }
  else{
    Abnormal_note=paste0(vlabel,"的病例不纳入分析，暂无缺失或异常。")
  }
  return(Abnormal_note)
}
#Speci_NaN(rename(risk_use,c(risk="vgroup",status="vAbn")),,"***")
#需用2个维度定义分析患者群体
vAbn_NaN2=function(Dplot,group_levels,sub_levels,vlabel){
  #患者人数-分母
  DMT=tally(group_by(Dplot,vgroup,vsubgroup))
  ##print(DMT)
  #缺失人数-分子
  NMRT=tally(group_by(filter(Dplot,is.na(vAbn)),vgroup,vsubgroup))
  #print(NMRT)
  #缺失比例
  Abnormal_tmp=left_join(rename(DMT,c(n="DMT")),rename(NMRT,c(n="NMRT")))
  # print(Abnormal_tmp)
  Abnormal_tmp=filter(Abnormal_tmp,!is.na(NMRT))
  Abnormal=grouporder(Abnormal_tmp,group_levels)[[1]]
  Abnormal=suborder(Abnormal,sub_levels)[[1]]
  Abnormal=ddply(Abnormal,NULL,transform,note=paste0(vgroup,"-",vsubgroup,"：",NMRT,"(",Abnormalnote=Rate_legend(NMRT/DMT),")"))
  # print(Abnormal)
  Ngroup=dim(Abnormal)[1]
  # print(Ngroup)
  if(Ngroup>=1){
    Abnormal_note=paste0(vlabel,"缺失或异常的患者病例不纳入分析，")
    Abnormal_note=Abnormal_paste(Abnormal,Abnormal_note,Ngroup)
  }
  else{
    Abnormal_note=paste0(vlabel,"缺失或异常的患者病例不纳入分析，暂无缺失或异常。")
  }
  return(Abnormal_note)
}

#vAbn_NaN2(rename(therapy2,c(R_order="vgroup",thrpy_order="vsubgroup",sjdays="vAbn")),OLDCHD_levels,thrpy_levels,"出院时生存状态")
#vAbn_NaN2(rename(filter(therapy5_POSTC,POSTC==1),c(R_order="vgroup",POSTC_order="vsubgroup",status="vAbn")),TT_levels,POSTC_levels,"出院时生存状态")
#vAbn_NaN2(rename(therapy2_thrpy,c(R_order="vgroup",thrpy_order="vsubgroup",sjdays="vAbn")),OLDCHD_levels,thrpy_levels,"出院时生存状态")

#2个参数联合缺失;
vAbn2_NaN=function(Dplot,group_levels,vlabel){
  #患者人数-分母
  DMT=tally(group_by(Dplot,vgroup))
  #缺失人数-分子
  NMRT=tally(group_by(filter(Dplot,is.na(vAbn1)|is.na(vAbn2)),vgroup))
  #缺失比例
  Abnormal_tmp=left_join(rename(DMT,c(n="DMT")),rename(NMRT,c(n="NMRT")))
  Abnormal_tmp=filter(Abnormal_tmp,!is.na(NMRT))
  levelupdate=grouporder(Abnormal_tmp,group_levels)
  Abnormal=levelupdate[[1]]
  Abnormal=ddply(Abnormal,NULL,transform,note=paste0(vgroup,"：",NMRT,"(",Abnormalnote=Rate_legend(NMRT/DMT),")"))
  #print(Abnormal)
  Ngroup=length(levelupdate[[2]])
  if(Ngroup>=1){
    Abnormal_note=paste0(vlabel,"缺失或异常的患者病例不纳入分析，")
    Abnormal_note=Abnormal_paste(Abnormal,Abnormal_note,Ngroup)
  }
  else{
    Abnormal_note=paste0(vlabel,"缺失或异常的患者病例不纳入分析，暂无缺失或异常。")
  }
  return(Abnormal_note)
}

#vAbn2_NaN(rename(OLDCHD,c(R_order="vgroup",sjdays="vAbn1",occup="vAbn2")),OLDCHD_levels,"住院天数或职业")

#各风险人群中患者比例
#base function for plots
library("scales")
times_trans <- function(base=2) {
  trans <- function(x) x^base
  inv <- function(x) x^(1/base)
  trans_new(paste0("times_trans-", format(base)), trans, inv, breaks=trans_breaks( function(x) x^base, function(x) x^(1/base)),domain = c(0,1))
}

#百分比显示方式，小于0.1%则显示千分之。。
Rate_legend=function(Rateuse){
  Rprint<-ifelse(Rateuse==0,'0%',ifelse(Rateuse<10^-4,'<0.1‰',ifelse(Rateuse<10^-3,paste0(round(Rateuse*1000,1),'‰'),ifelse(Rateuse<10^-2,paste0(round(Rateuse*1000,0),'‰'),paste0(round(Rateuse*100,1),'%')))))
  return(Rprint)
}
N_legend=function(ntrans){
  #n1000=N/1000
  Nprint<-ifelse(ntrans<0.1,'<0.1',ifelse(ntrans<1,paste0(round(ntrans,1)),paste0(round(ntrans,0))))
  return(Nprint)
}
#变量排序
grouporder=function(datain,group_levels){
  vgrouplevel=tally(group_by(datain,vgroup))
  #print(vgrouplevel)
  #有值的分组
  group_levels=group_levels[vgrouplevel$vgroup]
  #print(group_levels)
  #转换成Label
  datain$vgroup<-factor(datain$vgroup,labels=group_levels)
  #X轴legend识别
  if(max(nchar(group_levels,type = "width"))>100/length(group_levels)){rotate1=1}
  else{rotate1=0}
  #print(paste0("MaxChar:",max(nchar(group_levels,type = "width"))," validChar:",100/length(group_levels)))
  datain<-within(datain, vgroup <- factor(vgroup, levels=group_levels))
  
  result=list(data=datain,levelsupdated=group_levels,rotate=rotate1)
  
  return(result)
}
suborder=function(datain,sub_levels){
  vsubgrouplevel=tally(group_by(datain,vsubgroup))
  #print(vsubgrouplevel)
  sub_levels=sub_levels[vsubgrouplevel$vsubgroup]
  datain$vsubgroup<-factor(datain$vsubgroup,labels=sub_levels)
  datain<-within(datain, vsubgroup <- factor(vsubgroup, levels=sub_levels))
  result=list(data=datain,levelsupdated=sub_levels)
  return(result)
}
factorder=function(datain,vfact_levels){
  vfactlevel=tally(group_by(datain,vfact))
  #print(vfact_levels)
  vfact_levels=vfact_levels[vfactlevel$vfact]
  datain$vfact<-factor(datain$vfact,labels=vfact_levels)
  datain<-within(datain, vfact <- factor(vfact, levels=vfact_levels))
  result=list(data=datain,levelsupdated=vfact_levels)
  return(result)
}
#DPlot_m=grouporder(DPlot_m,group_levels)
#DPlot_m=factorder(DPlot_m,vfact_levels)
#DPlot_m=suborder(DPlot_m,sub_levels)

#缺失或异常患者人次比例

#2维累计百分比图
Pstackhist2=function(DPlot,colouruse,group_levels,sub_levels,ylabuse,xlabuse,labuse,legendtrans){
  DPlot_m_tmp<-tally(group_by(DPlot,vgroup,vsubgroup))
  DPlot_DMT<-tally(group_by(DPlot,vgroup))
  #print(DPlot_DMT)
  DPlot_m<-left_join(rename(DPlot_m_tmp,c(n="N")),rename(DPlot_DMT,c(n="DMT")))
  DPlot_m$Rate=DPlot_m$N/DPlot_m$DMT
  #print(DPlot_m)
  #图标签
  DPlot_m$ntrans<-DPlot_m$N/legendtrans
  #标签顺序
  levelgupdate=grouporder(DPlot_m,group_levels)
  DPlot_m=levelgupdate[[1]]
  levelsubupdate=suborder(DPlot_m,sub_levels)
  DPlot_m=levelsubupdate[[1]]
  
  # print(DPlot_m)
  #如某组比例80%，如民族中的汉族，不做展示
  # DPlot_m <- ddply(DPlot_m, .(vsubgroup), transform, subRate=min(Rate))
  #if(length(levelsubupdate[[2]])>15){ DPlot_m<-filter(DPlot_m,subRate<0.80)}
  #分析民族时，忽略汉族
  #if(DPlot_m$vsubgroup[1]=="汉族"){
  DPlot_m<-filter(DPlot_m,vsubgroup!="汉族")
  #}
  #print(DPlot_m)
  DPlot_m1 <- ddply(DPlot_m, .(vgroup), transform, pos = (cumsum(Rate) - 0.5*Rate),sumpos=cumsum(Rate))
  DPlot_m1$labymax=max(DPlot_m1$sumpos)
  #防止标签重叠
  Ngroup<-length(levelgupdate[[2]]) 
  Nsub=length(levelsubupdate[[2]])
  #分组>12则仅显示比例
  if(Ngroup>12){  DPlot_m1$label<-paste0(Rate_legend(DPlot_m1$Rate))  }
  else {  DPlot_m1$label<-paste0(N_legend(DPlot_m1$ntrans),"; ",Rate_legend(DPlot_m1$Rate))  }
  
  if (Ngroup>9) {fontsize=1.5}    else{fontsize=2.5}
  if(levelgupdate[[3]]==1){themeuse=chart_x270}  else {themeuse=chart_legend}
  DPlot_m1 <- ddply(DPlot_m1, .(vgroup), transform, label1=ifelse(is.na(lag(pos))|pos-lag(pos,1)>=labymax/15|Rate>labymax/20,label,""))
  #print(DPlot_m1)
  
  DPlot_plot<-ggplot(DPlot_m1,aes(x=vgroup, y=N,fill=vsubgroup))+geom_bar(stat ="identity",position="dodge",width=0.1^(1/Ngroup))+
    ylab(ylabuse)+xlab(xlabuse)+colouruse+labs(fill=labuse)+scale_y_continuous(labels =percent)+
    themeuse
  if(Ngroup>5){DPlot_plot<-DPlot_plot+theme(legend.position="bottom")+guides(fill = guide_legend(nrow=ceiling(Nsub/10)))}
  else if(Nsub>5) {DPlot_plot<-DPlot_plot+guides(fill = guide_legend(ncol=ceiling(Nsub/10)))}
  else{DPlot_plot<-DPlot_plot+theme(legend.position="bottom")}
  return(DPlot_plot)
}

#3维-某+分组+比例柱图
P_bar3=function(DPlot,V_FRate,colouruse,group_levels,sub_levels,ylabuse,xlabuse,labuse){
  #分母Denominator
  Plot_DMT_tmp<-tally(group_by(DPlot,vgroup,vsubgroup))
  
  Plot_DMT<-data.frame(vgroup=Plot_DMT_tmp$vgroup,vsubgroup=Plot_DMT_tmp$vsubgroup,DMT=Plot_DMT_tmp$n)
  #print(head(Plot_DMT))
  #分子numerator
  Plot_n<-filter(tally(group_by(DPlot,vgroup,vsubgroup,Vrate)),Vrate==V_FRate)
  Plot_join<-left_join(Plot_n,Plot_DMT)
  Plot_m<-data.frame(Plot_join,Rate=Plot_join$n/Plot_join$DMT)
  
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  sublevelgupdate=suborder(Plot_m,sub_levels)
  Plot_m=sublevelgupdate[[1]]
  Ngroup=length(levelgupdate[[2]])
  Nsub=length(sublevelgupdate[[2]])
  
  if(Ngroup*Nsub<=20){Plot_m$label1<-paste0(N_legend(Plot_m$n/1000),"; ",Rate_legend(Plot_m$Rate))}
  else{Plot_m$label1<-Rate_legend(Plot_m$Rate)  }
  if(Ngroup>20){
    Plot_m<-Plot_m[order(Plot_m$vgroup,-Plot_m$Rate),]
    Plot_m$labymax=max(Plot_m$Rate)-min(Plot_m$Rate)
    #防止标签重叠
    Plot_m <- ddply(Plot_m, .(vgroup), transform, label1=ifelse(is.na(lag(Rate))|abs(Rate-lag(Rate,1))>=labymax/10,paste0(label1),""))
  }
  if(levelgupdate[[3]]==1){themeuse=chart_x270}  else {themeuse=chart_legend}
  
  if(Ngroup>=20|Ngroup*Nsub>=15){fonesize=1.5}
  else{ fonesize=2.5}
  #print(head(Plot_m))
  plot1<-ggplot(Plot_m,aes(x=vgroup, y=Rate,fill=vsubgroup,label=label1))+geom_bar(stat ="identity",position="dodge")+
    ylab(ylabuse)+xlab(xlabuse)+themeuse+colouruse+scale_y_continuous(labels =percent)+theme(legend.position="bottom")+labs(fill=labuse)+
    geom_text(position = position_dodge(width=0.95),vjust=0.05,size=fonesize)
  return(plot1)
}



#3维-某+分组+比例柱图
P_bar3_DMT=function(DPlot,DMTuse,colouruse,group_levels,sub_levels,ylabuse,xlabuse,labuse){
  #分子numerator
  Plot_n<-tally(group_by(DPlot,vgroup,vsubgroup))
  Plot_m<-data.frame(Plot_n,Rate=Plot_n$n/DMTuse)
  
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  if(levelgupdate[[3]]==1){themeuse=chart_x270}  else {themeuse=chart_legend}
  sublevelgupdate=suborder(Plot_m,sub_levels)
  Plot_m=sublevelgupdate[[1]]
  Ngroup=length(levelgupdate[[2]])
  Nsub=length(sublevelgupdate[[2]])
  
  if(Ngroup*Nsub<=20){Plot_m$label1<-paste0(N_legend(Plot_m$n/1000),"; ",Rate_legend(Plot_m$Rate))}
  else{Plot_m$label1<-Rate_legend(Plot_m$Rate)  }
  if(Ngroup>20){
    Plot_m<-Plot_m[order(Plot_m$vgroup,-Plot_m$Rate),]
    Plot_m$labymax=max(Plot_m$Rate)-min(Plot_m$Rate)
    #防止标签重叠
    Plot_m <- ddply(Plot_m, .(vgroup), transform, label1=ifelse(is.na(lag(Rate))|abs(Rate-lag(Rate,1))>=labymax/10,paste0(label1),""))
  }
  if(Ngroup>=20|Ngroup*Nsub>=15){fonesize=1.5}
  else{ fonesize=2.5}
  #print(head(Plot_m))
  plot1<-ggplot(Plot_m,aes(x=vgroup, y=Rate,fill=vsubgroup,label=label1))+geom_bar(stat ="identity",position="dodge")+
    ylab(ylabuse)+xlab(xlabuse)+themeuse+colouruse+scale_y_continuous(labels =percent)+theme(legend.position="bottom")+labs(fill=labuse)+
    geom_text(position = position_dodge(width=0.95),vjust=0.05,size=fonesize)
  return(plot1)
}

#3维-某+分组+比例线图
P_line3=function(DPlot,V_FRate,group_levels,sub_levels,ylabuse,xlabuse,labcolour){
  #分母Denominator
  Plot_DMT_tmp<-tally(group_by(DPlot,vgroup,vsubgroup))
  
  Plot_DMT<-data.frame(vgroup=Plot_DMT_tmp$vgroup,vsubgroup=Plot_DMT_tmp$vsubgroup,DMT=Plot_DMT_tmp$n)
  #分子numerator
  Plot_n<-filter(tally(group_by(DPlot,vgroup,vsubgroup,Vrate)),Vrate==V_FRate)
  Plot_join<-left_join(Plot_n,Plot_DMT)
  Plot_join<-na.omit(Plot_join,vgroup)
  Plot_m<-data.frame(Plot_join,Rate=Plot_join$n/Plot_join$DMT)
  
  #标签选择--间距太小仅显示最大值，否则显示最大最小值
  Plot_m$label<-Rate_legend(Plot_m$Rate)
  Plot_m<-Plot_m[order(Plot_m$vgroup,-Plot_m$Rate),]
  Plot_m$labymax=max(Plot_m$Rate)-min(Plot_m$Rate)
  
  #防止标签重叠
  Plot_m <- ddply(Plot_m, .(vgroup), transform, label1=ifelse(is.na(lag(Rate))|abs(Rate-lag(Rate,1))>=labymax/10,label,""))
  #print(head(Plot_m))
  levelupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelupdate[[1]]
  levelsubupdate=suborder(Plot_m,sub_levels)
  Plot_m=levelsubupdate[[1]]
  Nsub=ceiling(length(levelsubupdate[[2]])/6)
  Ngroup=length(levelupdate[[2]])
  if(Ngroup>20|Ngroup*Nsub>12){ fonesize=2  }  else{ fonesize=2.5  }
  if(levelupdate[[3]]==1){themeuse=chart_linex270}  else {themeuse=chart_linex}
  plot1<-ggplot(Plot_m, aes(x=vgroup,y=Rate,group=vsubgroup,label=label1))+
    geom_line(aes(colour=vsubgroup))+geom_point(aes(colour=vsubgroup))+
    ylab(ylabuse)+xlab(xlabuse)+scale_y_continuous(labels =percent)+themeuse+
    theme(legend.position="bottom")+geom_text(size=fonesize,vjust=0.05)+guides(col = guide_legend(nrow=Nsub))+
    scale_colour_discrete(name=labcolour)
  return(plot1)
}

#4维-某+分组+比例线图
P_line4=function(DPlot,V_FRate,group_levels,fact_levels,sub_levels,ylabuse,xlabuse,labfact,labsub){
  #分母Denominator
  Plot_DMT_tmp<-tally(group_by(DPlot,vgroup,vsubgroup,vfact))
  Plot_DMT<-data.frame(vgroup=Plot_DMT_tmp$vgroup,vsubgroup=Plot_DMT_tmp$vsubgroup,vfact=Plot_DMT_tmp$vfact,DMT=Plot_DMT_tmp$n)
  #分子numerator
  Plot_n<-filter(tally(group_by(DPlot,vgroup,vsubgroup,vfact,Vrate)),Vrate==V_FRate)
  Plot_join<-left_join(Plot_n,Plot_DMT)
  Plot_join<-na.omit(Plot_join,vgroup)
  Plot_m<-data.frame(Plot_join,Rate=Plot_join$n/Plot_join$DMT)
  
  #标签选择--间距太小仅显示最大值，否则显示最大最小值
  Plot_m$label<-Rate_legend(Plot_m$Rate)
  Plot_m<-Plot_m[order(Plot_m$vgroup,-Plot_m$Rate),]
  Plot_m$labymax=max(Plot_m$Rate)-min(Plot_m$Rate)
  
  #防止标签重叠
  Plot_m <- ddply(Plot_m, .(vgroup), transform, label1=ifelse(is.na(lag(Rate))|abs(Rate-lag(Rate,1))>=labymax/10,label,""))
  Plot_m$subfact=paste0(Plot_m$vsubgroup,Plot_m$vfact)
  #print(head(Plot_m))
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  levelsubupdate=suborder(Plot_m,sub_levels)
  Plot_m=levelsubupdate[[1]]
  factlevelupdate=factorder(Plot_m,fact_levels)
  Plot_m=factlevelupdate[[1]]
  Nfact=length(factlevelupdate[[2]])
  Nsub=length(levelsubupdate[[2]])
  #线型样式-分组少，颜色样式分组多
  if(Nfact<=Nsub){
    Plot_m<-rename(Plot_m,c(vfact="lineT",vsubgroup="lineC"))
    lablineT=labfact
    lablineC=labsub
  }
  else{ 
    Plot_m<-rename(Plot_m,c(vfact="lineC",vsubgroup="lineT")) 
    lablineT=labsub
    lablineC=labfact
  }
  
  if(levelgupdate[[3]]==1){themeuse=chart_linex270}  else {themeuse=chart_linex}
  
  plot1<-ggplot(Plot_m, aes(x=vgroup,y=Rate,group=subfact,label=label1))+
    geom_line(aes(colour=lineC,linetype=lineT))+geom_point(aes(colour=lineC))+
    ylab(ylabuse)+xlab(xlabuse)+scale_y_continuous(labels =percent)+themeuse+
    theme(legend.position="bottom")+geom_text(size=2,vjust=0.05)+guides(col = guide_legend(nrow=ceiling(Nsub/6)))+
    scale_colour_discrete(name=lablineC)
  if(Nfact>1){plot1<-plot1+scale_linetype_discrete(name=lablineT)}
  else{plot1<-plot1+scale_linetype(guide = FALSE)}
  if(Nfact+Nsub<7){
    plot1<-plot1+theme(legend.position="bottom",legend.box="horizontal")
  } 
  return(plot1)
}

#5维-某+分组+比例线图
P_linefact4=function(DPlot,group_levels,fact_levels,sub1_levels,sub2_levels,V_FRate,ylabuse,xlabuse,labcolour,lablinetype){
  #分母Denominator
  Plot_DMT_tmp<-tally(group_by(DPlot,vgroup,vsub1group,vsub2group,vfact))
  Plot_DMT<-data.frame(vgroup=Plot_DMT_tmp$vgroup,vsub1group=Plot_DMT_tmp$vsub1group,vsub2group=Plot_DMT_tmp$vsub2group,vfact=Plot_DMT_tmp$vfact,DMT=Plot_DMT_tmp$n)
  #print(Plot_DMT)
  #分子numerator
  Plot_n<-filter(tally(group_by(DPlot,vgroup,vsub1group,vsub2group,vfact,Vrate)),Vrate==V_FRate)
  Plot_join<-left_join(Plot_n,Plot_DMT)
  Plot_join<-na.omit(Plot_join,vgroup)
  Plot_m<-data.frame(Plot_join,Rate=Plot_join$n/Plot_join$DMT)
  
  #标签选择--间距太小仅显示最大值，否则显示最大最小值
  Plot_m$label<-Rate_legend(Plot_m$Rate)
  Plot_m<-Plot_m[order(Plot_m$vfact,Plot_m$vgroup,-Plot_m$Rate),]
  Plot_m$labymax=max(Plot_m$Rate)-min(Plot_m$Rate)
  
  #防止标签重叠
  Plot_m <- ddply(Plot_m, .(vfact,vgroup), transform, label1=ifelse(is.na(lag(Rate))|abs(Rate-lag(Rate,1))>=labymax/10,label,""))
  Plot_m$subfact=paste0(Plot_m$vsub1group,Plot_m$vsub2group)
  #print(head(Plot_m))
  Plot_m=levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  
  Plot_m=rename(Plot_m,c(vsub1group="vsubgroup"))
  sub1levelupdate=suborder(Plot_m,sub1_levels)
  Plot_m=rename(sub1levelupdate[[1]],c(vsubgroup="vsub1group"))
  Nsub1=length(sub1levelupdate[[2]])
  Nsub=ceiling(Nsub1/6)
  
  Plot_m=rename(Plot_m,c(vsub2group="vsubgroup"))
  sub2levelupdate=suborder(Plot_m,sub2_levels)
  Plot_m=rename(sub2levelupdate[[1]],c(vsubgroup="vsub2group"))
  Nsub2=length(sub2levelupdate[[2]])
  
  factlevelupdate=factorder(Plot_m,fact_levels)
  Plot_m=factlevelupdate[[1]]
  
  if(levelgupdate[[3]]==1){themeuse=chart_linex270}  else {themeuse=chart_linex}
  plot1<-ggplot(Plot_m, aes(x=vgroup,y=Rate,group=subfact,label=label1))+
    geom_line(aes(colour=vsub1group,linetype=vsub2group))+geom_point(aes(colour=vsub1group))+
    ylab(ylabuse)+xlab(xlabuse)+scale_y_continuous(labels =percent)+themeuse+facet_grid(.~vfact)+
    theme(legend.position="bottom")+geom_text(size=2,vjust=0.05)+guides(col = guide_legend(nrow=Nsub))+
    scale_colour_discrete(name=labcolour)
  if(Nsub2>1){plot1<-plot1+scale_linetype_discrete(name=lablinetype)}
  else{plot1<-plot1+scale_linetype(guide = FALSE)}
  if(Nsub1+Nsub2<7){plot1<-plot1+theme(legend.position="bottom",legend.box="horizontal")}
  return(plot1)
}  


#1维-例数柱图（-分母）
P_DMT1=function(DPlot,group_levels,DMTuse,ylabuse,legendtrans){
  Plot_m<-tally(group_by(DPlot,vgroup))
  Plot_m$Rate<-Plot_m$n/DMTuse
  Plot_m$ntrans<-Plot_m$n/legendtrans
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  #Plot_m$vgroup<-factor(Plot_m$vgroup,labels=group_levels)
  Ngroup<-length(levelgupdate[[2]])
  foneSize=2.5
  if(Ngroup<15) {
    Plot_m$label1<-paste0(N_legend(Plot_m$ntrans),"; ",Rate_legend(Plot_m$Rate))
  }
  else{
    Plot_m$label1<-paste0(Rate_legend(Plot_m$Rate))
    if(Ngroup>25){      foneSize=1.5    } 
  }
  if(levelgupdate[[3]]==1){themeuse=chart_x270}  else {themeuse=chart_legend}
  plot1<-ggplot(Plot_m,aes(x=vgroup, y=Rate,label=label1))+geom_bar(stat ="identity",fill=color_main,width=0.1^(1/Ngroup))+
    ylab(ylabuse)+xlab("")+themeuse+geom_text(position = position_dodge(width=0.9),vjust=0.05,size=foneSize)+scale_y_continuous(labels =percent)
  return(plot1) 
}
#N_DMT1(get(paste0(PTlegenddatause[2])),chart_legend,get(paste0(PTlegenddatause[2],'_levels')),get(paste0(PTuseE,'_DMT')),ylab_ptn,1000)
# N_DMT1(OLDCVD_cause,chart_legend,OLDCVD_cause_levels,CVD_DMT,ylab_ptp,1000)

#2维-比例线图（-分母）
P_line2=function(DPlot,DMTuse,group_levels,sub_levels,ylabuse,xlabuse,labuse){
  #分母Denominator
  Plot_DMT<-rename(DMTuse,c(n='DMT'))
  #分子numerator
  Plot_n<-na.omit(tally(group_by(DPlot,vgroup,vsubgroup)),vgroup)
  #print(head(Plot_n))
  Plot_join<-left_join(Plot_n,Plot_DMT)
  #print(head(Plot_join))
  Plot_m<-data.frame(Plot_join,Rate=round(Plot_join$n/Plot_join$DMT,3))
  #print(head(Plot_m))
  #标签选择--显示最大值+与上一高点间距大于坐标轴1/10的值
  Plot_m$label<-Rate_legend(Plot_m$Rate)
  
  Plot_m<-Plot_m[order(Plot_m$vgroup,-Plot_m$Rate),]
  Plot_m$labymax=max(Plot_m$Rate)-min(Plot_m$Rate)
  #防止标签重叠
  Plot_m <- ddply(Plot_m, .(vgroup), transform, label1=ifelse(is.na(lag(Rate))|abs(Rate-lag(Rate,1))>=labymax/25,label,""))
  
  # print(head(Plot_m))
  #label
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  sublevelupdate=suborder(Plot_m,sub_levels)
  Plot_m=sublevelupdate[[1]]
  #print(head(Plot_m))
  Ngroup=length(levelgupdate[[2]])
  Nsub=length(sublevelupdate[[2]])
  if(Ngroup>20|Ngroup*Nsub>15){fonesize=1.5}  else{ fonesize=2.5}
  if(Nsub<6){coloruse=c5} else if(Nsub<11){coloruse=c10} else {coloruse=c19}
  if(levelgupdate[[3]]==1){themeuse=chart_linex270}  else {themeuse=chart_linex}
  # print(head(Plot_m))
  plot1<-ggplot(Plot_m, aes(x=vgroup,y=Rate,group=vsubgroup,label=label1))+
    geom_line(aes(colour=vsubgroup,alpha=0.5))+geom_point(aes(colour=vsubgroup,alpha=0.5))+themeuse+
    scale_y_continuous(labels =percent)+
    theme(legend.position="bottom")+guides(col = guide_legend(nrow = ceiling(Nsub/7)))+geom_text(size=fonesize,vjust=0.05)+
    ylab(ylabuse)+xlab(xlabuse)+scale_alpha(guide = FALSE)
  if(Nsub<20) {
    plot1=plot1+scale_colour_manual(values=coloruse,name=labuse)
  }
  
  return(plot1)
}

##1维-频数线图
N_line1=function(DPlot,ylabuse,xlabuse,lengendtrans){
  Abnormal_tmp<-ddply(DPlot, NULL, summarise,mean=mean(vstat,na.rm=TRUE),std=sd(vstat,na.rm=TRUE),min=quantile(vstat,0,na.rm=TRUE),q1=quantile(vstat,0.25,na.rm=TRUE),median=quantile(vstat,0.5,na.rm=TRUE),
                      q3=quantile(vstat,0.75,na.rm=TRUE),q9=quantile(vstat,0.90,na.rm=TRUE),max=quantile(vstat,1,na.rm=TRUE))
  #Abnormal<-Abnormal_tmp$q3+Abnormal_tmp$std
  Abnormal<-min(max(Abnormal_tmp$q9,900),200)
  #print(Abnormal)
  #取整数计算N
  DPlot$vstat=round(DPlot$vstat,0)
  plot_m<-filter(tally(group_by(DPlot,vstat)),vstat<Abnormal)
  plot_m$ntrans<-plot_m$n/lengendtrans
  
  notemean=paste0("Mean(SD):",round(Abnormal_tmp$mean,1),"(",round(Abnormal_tmp$std,1),")")
  notemedian=paste0("Median(Q1-Q3):",round(Abnormal_tmp$median,0),"(",round(Abnormal_tmp$q1,0),"-",round(Abnormal_tmp$q3,0),")")
  note=c(notemean,notemedian)
  notex=0.97*c(max(plot_m$vstat),max(plot_m$vstat))
  notey=c(max(plot_m$ntrans),0.93*max(plot_m$ntrans))
  data_note=data.frame(notex,notey,note)
  #标签：偶数且>=0.1
  if(max(plot_m$vstat)-min(plot_m$vstat)>30){
    plot_m$label1<-ifelse((plot_m$vstat/2==ceiling(plot_m$vstat/2) & plot_m$ntrans>0.9),N_legend(plot_m$ntrans),"")
    break1<-c(ceiling(min(plot_m$vstat)/10):ceiling(max(plot_m$vstat)/10))*10
  }
  else{
    plot_m$label1<-N_legend(plot_m$ntrans)
    break1<-c(1:30)
  }
   #print(plot_m)
  plot1<-ggplot(plot_m, aes(x=vstat,y=ntrans,label=label1))+
    geom_line(colour=color_main)+geom_point(colour=color_main)+ylab(ylabuse)+xlab(xlabuse)+chart_legend+
    geom_text(size=2.5,hjust=0.5,vjust=-0.5)+scale_x_continuous(breaks=break1)+
    geom_text(data =data_note, aes(x=notex,y=notey,label=note),size=2.5,hjust=1,vjust=0)
  
  return(plot1)
}

##2维-频数线图
N_line2=function(DPlot,group_levels,ylabuse,xlabuse,labcolour,lengendtrans){
  Abnormal_tmp<-ddply(DPlot, .(vgroup), summarise,mean=mean(vstat,na.rm=TRUE),std=sd(vstat,na.rm=TRUE),min=quantile(vstat,0,na.rm=TRUE),q1=quantile(vstat,0.25,na.rm=TRUE),median=quantile(vstat,0.5,na.rm=TRUE),
                      q3=quantile(vstat,0.75,na.rm=TRUE),q9=quantile(vstat,0.90,na.rm=TRUE),max=quantile(vstat,1,na.rm=TRUE))
  #Abnormal<-max(Abnormal_tmp$mean+Abnormal_tmp$std)
  Abnormal<-min(max(Abnormal_tmp$q9,900),200)
  #print(Abnormal)
  #取整数计算N
  DPlot$vstat=round(DPlot$vstat,0)
  plot_m<-filter(tally(group_by(DPlot,vgroup,vstat)),vstat<Abnormal)
  plot_m$ntrans<-plot_m$n/lengendtrans
  levelgupdate=grouporder(plot_m,group_levels)
  plot_m=levelgupdate[[1]]
  
  notemean=paste0(round(Abnormal_tmp$mean,0),"(",round(Abnormal_tmp$std,1),")")
  notemedian=paste0(round(Abnormal_tmp$median,0),"(",round(Abnormal_tmp$q1,0),"-",round(Abnormal_tmp$q3,0),")")
  
  data_note=paste0(levelgupdate[[2]],":",notemean," / ",notemedian)
  plot_m$note[1]="Mean(SD) / Median(Q1-Q3)"
  plot_m$note[2]=data_note[1]
  plot_m$note[3]=data_note[2]
  
  plot_m$labymax=max(plot_m$ntrans)-min(plot_m$ntrans)
  plot_m$labxrange=ceiling((max(plot_m$vstat)-min(plot_m$vstat))/25)
  
  plot_m<- ddply(plot_m, .(vstat), transform, label0=ifelse(is.na(lag(ntrans))|abs(ntrans-lag(ntrans,1))>=labymax/15,N_legend(ntrans),""))
  #plot_m<-plot_m[order(plot_m$vgroup,plot_m$vstat),]
  plot_m <- ddply(plot_m, .(vgroup), transform, label1=ifelse(is.na(lag(ntrans))|abs(ntrans-lag(ntrans,1))>=labymax/10|ceiling(vstat/labxrange)==vstat/labxrange,paste0(label0),""))
  
  #标签：偶数且>=0.1
  # plot_m$label1<-ifelse(plot_m$vstat/2==ceiling(plot_m$vstat/2),N_legend(plot_m$ntrans),"")
  #标签选择--间距太小仅显示最大值，否则显示最大最小值
  # plot_m<-plot_m[order(plot_m$vstat,-plot_m$ntrans),]
  #plot_m$labymax=max(plot_m$ntrans)-min(plot_m$ntrans)
  
  #防止标签重叠
  # plot_m <- ddply(plot_m, .(vstat), transform, label1=ifelse(is.na(lag(ntrans))|abs(ntrans-lag(ntrans,1))>=labymax/10,label1,""))
  
  break1<-c(ceiling(min(plot_m$vstat)/10):ceiling(max(plot_m$vstat)/10))*10
  # print(break1)
  #print(head(plot_m))
  plot1<-ggplot(plot_m, aes(x=vstat,y=ntrans,group=vgroup,label=label1))+
    geom_line(aes(colour=vgroup))+geom_point(aes(colour=vgroup))+
    geom_text(size=2.5)+scale_x_continuous(breaks=break1)+chart_linex+
    ylab(ylabuse)+xlab(xlabuse)+theme(legend.position="bottom") +
    scale_colour_discrete(name=labcolour)+
    geom_text(aes(x=0.97*max(vstat),y=0.97*max(ntrans),label=note[1]),size=2.5,hjust=1)+
    geom_text(aes(x=0.97*max(vstat),y=0.90*max(ntrans),label=note[2]),size=2.5,hjust=1)+
    geom_text(aes(x=0.97*max(vstat),y=0.83*max(ntrans),label=note[3]),size=2.5,hjust=1)
  
  return(plot1)
}

##2维-频数线图
P_Cline2=function(DPlot,group_levels,ylabuse,xlabuse,labcolour){
  Abnormal_tmp<-ddply(DPlot, .(vgroup), summarise,mean=mean(vstat,na.rm=TRUE),std=sd(vstat,na.rm=TRUE),min=quantile(vstat,0,na.rm=TRUE),q1=quantile(vstat,0.25,na.rm=TRUE),median=quantile(vstat,0.5,na.rm=TRUE),
                      q3=quantile(vstat,0.75,na.rm=TRUE),q9=quantile(vstat,0.90,na.rm=TRUE),max=quantile(vstat,1,na.rm=TRUE))
  #Abnormal<-max(Abnormal_tmp$mean+Abnormal_tmp$std)
  Abnormal<-min(max(Abnormal_tmp$q9,900),200)
  #print(Abnormal)
  #取整数计算N&%
  DPlot$vstat=round(DPlot$vstat,0)
  plot_m<-tally(group_by(DPlot,vgroup,vstat))
  #计算比例
  plot_m<-ddply(plot_m,.(vgroup),transform,Rate=n/sum(n))
  plot_m=filter(plot_m,vstat<Abnormal)
  
  levelgupdate=grouporder(plot_m,group_levels)
  plot_m=levelgupdate[[1]]
  
  notemean=paste0(round(Abnormal_tmp$mean,0),"(",round(Abnormal_tmp$std,1),")")
  notemedian=paste0(round(Abnormal_tmp$median,0),"(",round(Abnormal_tmp$q1,0),"-",round(Abnormal_tmp$q3,0),")")
  
  data_note=paste0(levelgupdate[[2]],":",notemean," / ",notemedian)
  #print(data_note)
  plot_m$note[1]="Mean(SD) / Median(Q1-Q3)"
  plot_m$note[2]=data_note[1]
  plot_m$note[3]=data_note[2]
  #标签：偶数且>=0.1
  #plot_m$label1<-ifelse(plot_m$vstat/2==ceiling(plot_m$vstat/2),Rate_legend(plot_m$Rate),"")
  #标签选择--间距太小仅显示最大值，否则显示最大最小值
  plot_m<-plot_m[order(plot_m$vstat,-plot_m$Rate),]
  plot_m$labymax=max(plot_m$Rate)-min(plot_m$Rate)
  plot_m$labxrange=ceiling((max(plot_m$vstat)-min(plot_m$vstat))/25)
  #防止标签重叠
  plot_m<- ddply(plot_m, .(vstat), transform, label0=ifelse(is.na(lag(Rate))|abs(Rate-lag(Rate,1))>=labymax/15,Rate_legend(Rate),""))
  #plot_m<-plot_m[order(plot_m$vgroup,plot_m$vstat),]
  plot_m <- ddply(plot_m, .(vgroup), transform, label1=ifelse(is.na(lag(Rate))|abs(Rate-lag(Rate,1))>=labymax/10|ceiling(vstat/labxrange)==vstat/labxrange,paste0(label0),""))
  
  break1<-c(ceiling(min(plot_m$vstat)/10):ceiling(max(plot_m$vstat)/10))*10
  # #print(break1)
  #print(head(plot_m))
  plot1<-ggplot(plot_m, aes(x=vstat,y=Rate,group=vgroup,label=label1))+
    geom_line(aes(colour=vgroup))+geom_point(aes(colour=vgroup))+scale_y_continuous(labels =percent)+
    geom_text(size=2.5,hjust=0.5,vjust=0)+scale_x_continuous(breaks=break1)+chart_linex+theme(legend.position="bottom")+
    ylab(ylabuse)+xlab(xlabuse)+scale_colour_discrete(name=labcolour)+
    geom_text(aes(x=0.97*max(vstat),y=0.97*max(Rate),label=note[1]),size=2.5,hjust=1,vjust=0)+
    geom_text(aes(x=0.97*max(vstat),y=0.90*max(Rate),label=note[2]),size=2.5,hjust=1,vjust=0)+
    geom_text(aes(x=0.97*max(vstat),y=0.83*max(Rate),label=note[3]),size=2.5,hjust=1,vjust=0)
  return(plot1)
}

##1维box
BOXPLOT1=function(DPlot,cntrlylim,group_levels,ylabuse,xlabuse){
  Plot_m<-ddply(DPlot, "vgroup", summarise,mean=mean(vstat,na.rm=TRUE),std=sd(vstat,na.rm=TRUE),min=quantile(vstat,0,na.rm=TRUE),q1=quantile(vstat,0.25,na.rm=TRUE),median=quantile(vstat,0.5,na.rm=TRUE),
                q3=quantile(vstat,0.75,na.rm=TRUE),max=quantile(vstat,1,na.rm=TRUE))
  #plot_m<-na.omit(plot_m,vgroup)
  lim_y=max(30,max(Plot_m$q3)+5)
  Plot_m$max1<-ifelse(cntrlylim==1&Plot_m$max>lim_y,lim_y,Plot_m$max)
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  Ngroup<-length(levelgupdate[[2]]) 
  Plot_m$notey=min(Plot_m$min)-0.05*max(Plot_m$max1-Plot_m$min)
  if(Ngroup>12){fonesize=1.5  }  else {fonesize=2.5}
  if(levelgupdate[[3]]==1){themeuse=chart_x270}  else {themeuse=chart_legend}
  Plot_m$note=paste0(round(Plot_m$mean,0),"/",round(Plot_m$median,0))
  #print(Plot_m)
  plot1<- ggplot(Plot_m, aes(x =vgroup,y=mean,ymin =min, lower =q1,middle =median, upper =q3, ymax =max1))
  if(cntrlylim!=1){
    plot1<-plot1+geom_errorbar(aes(ymin=min,ymax=max))
  }
  plot1<-plot1+geom_boxplot(stat = "identity",fill=color_main,show.legend=FALSE,notchwidth = 0.5, varwidth = TRUE,)+labs(fill="")+
    xlab(xlabuse)+themeuse+ylab(ylabuse)+
    stat_summary(fun.y=mean,shape=1,colour='red',size=1.5,geom='point')+
    theme(legend.position="bottom")+guides(fill = guide_legend(nrow=ceiling(Ngroup/9)))+
    geom_text(aes(x=vgroup,y=notey,label=note),size=fonesize,hjust = 0.5, vjust = 0.5)
  
  return(plot1)
}


#P_age1(OLDCVD,chart_legend)
##1维-饼图
P_pie1=function(DPlot,colouruse,group_levels,labuse,legendtrans){
  Plot_m<-na.omit(tally(group_by(DPlot,vgroup)),vgroup)
  Plot_m$ntrans<-Plot_m$n/legendtrans
  # #print(plot_m)
  #标签
  Plot_m$Rate<-Plot_m$n/sum(Plot_m$n)
  Plot_m$label<-paste0(N_legend(Plot_m$ntrans),"; ",Rate_legend(Plot_m$Rate))
  #标签顺序
  Plot_m=grouporder(Plot_m,group_levels)[[1]]
  
  Plot_m$lagRate<-lag(Plot_m$Rate,1)
  Plot_m<-ddply(Plot_m,NULL, transform, pos = (cumsum(Rate) - 0.5*Rate))
  Plot_m <- ddply(Plot_m, NULL, transform, label1=ifelse(is.na(lag(pos))|((pos-lag(pos,1)>=0.1|Rate>0.05)&pos[1]+1-pos>0.1),label,""))
  #plot_m$pos<-ifelse(is.na(plot_m$lagRate),0.5*plot_m$Rate,plot_m$Rate+plot_m$lagRate- 0.5*plot_m$Rate)
  #print(Plot_m)
  plot1<-ggplot(Plot_m, aes(x="",y=ntrans,fill=vgroup,label=label1)) +geom_bar(stat = "identity",position="fill",width=1)+
    ylab("")+xlab("")+scale_y_continuous("",breaks = NULL)+colouruse+
    chart_legend+labs(fill=labuse)+coord_polar("y", start = pi/3)+ geom_text(aes(y=pos,label=label1),size=2.5)
  #scale_y_continuous(labels =percent,breaks=c(0.15,0.25,0.35,0.5,0.65,0.75,0.85,1))
  return(plot1)
}

##2维-分面饼图
P_piefact=function(DPlot,colouruse,group_levels,sub_levels,labuse,legendtrans){
  #分子
  Plot_n<-tally(group_by(DPlot,vgroup,vsubgroup))
  Plot_n$ntrans<-Plot_n$n/legendtrans
  #分母
  Plot_DMT<-tally(group_by(DPlot,vgroup))
  Plot_DMT<-rename(Plot_DMT,c(n="DMT"))
  #left join
  Plot_join<-left_join(Plot_n,Plot_DMT)
  Plot_m<-data.frame(Plot_join,Rate=Plot_join$n/Plot_join$DMT)
  
  Plot_m$label<-paste0(N_legend(Plot_m$ntrans),"; ",Rate_legend(Plot_m$Rate))
  #print(Plot_m)
  #标签顺序
  levelupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelupdate[[1]]
  levelsubupdate=suborder(Plot_m,sub_levels)
  Plot_m=levelsubupdate[[1]]
  Plot_m<-ddply(Plot_m, .(vgroup), transform, pos = (cumsum(Rate) - 0.5*Rate))
  Plot_m <- ddply(Plot_m, .(vgroup), transform, label1=ifelse(is.na(lag(pos))|((pos-lag(pos,1)>=0.1|Rate>0.05)&pos[1]+1-pos>0.1),label,""))
  #print(Plot_m)
  # #print(Plot_m)
  Plot1<-ggplot(Plot_m, aes(x="",y=ntrans,fill=vsubgroup,label=label1)) +geom_bar(stat = "identity",position="fill",width=1)+
    ylab("")+xlab("")+scale_y_continuous("",breaks = NULL)+colouruse+
    chart_legend+labs(fill=labuse)+coord_polar("y", start = pi/3)+ geom_text(aes(y=pos,label=label1),size=2.5)+facet_grid(.~vgroup)
  #scale_y_continuous(labels =percent,breaks=c(0.15,0.25,0.35,0.5,0.65,0.75,0.85,1))
  if(length(levelupdate[[2]])>3){Plot1=Plot1+theme(legend.position="bottom")}
  return(Plot1)
}

##3维-分面圆环
P_ringfact=function(DPlot,colouruse,vfact_levels,group_levels,sub_levels,labuse,legendtrans){
  #分子
  Plot_n<-tally(group_by(DPlot,vfact,vgroup,vsubgroup))
  Plot_n$ntrans<-Plot_n$n/legendtrans
  #分母
  Plot_DMT<-tally(group_by(DPlot,vfact,vgroup))
  Plot_DMT<-rename(Plot_DMT,c(n="DMT"))
  #left join
  Plot_join<-left_join(Plot_n,Plot_DMT)
  Plot_m<-data.frame(Plot_join,Rate=Plot_join$n/Plot_join$DMT)
  
  #标签顺序
  levelupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelupdate[[1]]
  Ngroup=length(levelupdate[[2]])
  
  levelfactupdate=factorder(Plot_m,vfact_levels)
  Plot_m=levelfactupdate[[1]]
  Nfact=length(levelfactupdate[[2]])
  
  levelsubupdate=suborder(Plot_m,sub_levels)
  Plot_m=levelsubupdate[[1]]
  
  if(Ngroup>2|Nfact>3){
    fontsize=1.5
    Plot_m$label<-Rate_legend(Plot_m$Rate)
    }
  else {
    fontsize=2.5
    Plot_m$label<-paste0(N_legend(Plot_m$ntrans),"; ",Rate_legend(Plot_m$Rate))
    }
  #print(Plot_m)
  
  Plot_m <- ddply(Plot_m, .(vgroup,vfact), transform, pos = (cumsum(Rate) - 0.5*Rate),sumpos=cumsum(Rate))
  Plot_m$labymax=max(Plot_m$sumpos)
  ##print(head(DPlot_m1))
  #防止标签重叠
  Plot_m <- ddply(Plot_m, .(vgroup,vfact), transform, label1=ifelse(is.na(lag(pos))|pos-lag(pos,1)>=labymax/15|Rate>labymax/20,label,""))
  #print(Plot_m)

  Plot1<-ggplot(Plot_m,aes(x=vgroup,y=Rate,fill=vsubgroup,label=label1)) +
    geom_bar(stat ="identity",position="fill")+facet_grid(.~vfact)+colouruse+coord_polar("y")+
    ylab("")+scale_y_continuous("",breaks = NULL)+xlab("")+chart_legend+labs(fill=labuse)+
    coord_polar("y", start = pi/3)+ geom_text(aes(y=pos,label=label1),size=2.5)
  if(Nfact>3){Plot1=Plot1+theme(legend.position="bottom")}
  return(Plot1)
}

##2维-分面累计比例柱图
P_barfact=function(DPlot,colouruse,group_levels,vfact_levels,sub_levels,labuse,legendtrans){
  #分子
  Plot_n<-tally(group_by(DPlot,vgroup,vfact,vsubgroup))
  Plot_n$ntrans<-Plot_n$n/legendtrans
  ##print(Plot_n)
  #Plot_n$labeltmp1<-ifelse(Plot_n$n1000<0.1,'',ifelse(Plot_n$n1000<1,round(Plot_n$n1000,1),round(Plot_n$n1000,0)))
  #分母
  Plot_DMT<-tally(group_by(DPlot,vgroup,vfact))
  Plot_DMT<-rename(Plot_DMT,c(n="DMT"))
  #left join
  Plot_join<-left_join(Plot_n,Plot_DMT)
  Plot_m<-data.frame(Plot_join,Rate=Plot_join$n/Plot_join$DMT)
  
  #标签顺序
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  Plot_m=factorder(Plot_m,vfact_levels)[[1]]
  Plot_m=suborder(Plot_m,sub_levels)[[1]]
  Ngroup<-length(levelgupdate[[2]])
  Nsub=ceiling(length(Ngroup)/10)
  #分组>12则仅显示比例,且X轴legend竖向
  if(Ngroup>20){  Plot_m$label<-paste0(Rate_legend(Plot_m$Rate))
  }
  else {Plot_m$label<-paste0(N_legend(Plot_m$ntrans),"; ",Rate_legend(Plot_m$Rate))}
  if (Ngroup>12) { fontsize=1.5}  else{fontsize=2.5}
  if(levelgupdate[[3]]==1){themeuse=chart_x270}  else {themeuse=chart_legend}
  
  Plot_m <- ddply(Plot_m, .(vgroup,vfact), transform, pos = (cumsum(Rate) - 0.5*Rate),sumpos=cumsum(Rate))
  Plot_m$labymax=max(Plot_m$sumpos)
  ##print(head(DPlot_m1))
  #防止标签重叠
  Plot_m <- ddply(Plot_m, .(vgroup), transform, label1=ifelse(is.na(lag(pos))|pos-lag(pos,1)>=labymax/15|Rate>labymax/20,label,""))
  # #print(Plot_m)
  
  # #print(Plot_m)
  Plot1<-ggplot(Plot_m, aes(x=vgroup,y=Rate,fill=vsubgroup,label=label1)) +geom_bar(stat = "identity",position="fill",width=0.1^(1/Ngroup))+
    ylab("")+xlab("")+scale_y_continuous(labels =percent)+colouruse+themeuse+labs(fill=labuse)+ 
    geom_text(aes(y=pos,label=label1),size=fontsize)+facet_grid(vfact~.)+theme(legend.position="bottom")+guides(fill = guide_legend(nrow=Nsub))
  return(Plot1)
} 

##2维-分面图
P_factbar=function(DPlot,colouruse,group_levels,vfact_levels,sub_levels,labuse,legendtrans){
  #分子
  Plot_n<-tally(group_by(DPlot,vgroup,vfact,vsubgroup))
  Plot_n$ntrans<-Plot_n$n/legendtrans
  ##print(Plot_n)
  #Plot_n$labeltmp1<-ifelse(Plot_n$n1000<0.1,'',ifelse(Plot_n$n1000<1,round(Plot_n$n1000,1),round(Plot_n$n1000,0)))
  #分母
  Plot_DMT<-tally(group_by(DPlot,vgroup,vfact))
  Plot_DMT<-rename(Plot_DMT,c(n="DMT"))
  #left join
  Plot_join<-left_join(Plot_n,Plot_DMT)
  Plot_m<-data.frame(Plot_join,Rate=Plot_join$n/Plot_join$DMT)
  #
  Plot_m$label<-paste0(N_legend(Plot_m$ntrans),"; ",Rate_legend(Plot_m$Rate))
  # #print(Plot_m)
  #标签顺序
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  vfactupdate=factorder(Plot_m,vfact_levels)
  Plot_m=vfactupdate[[1]]
  
  Plot_m=suborder(Plot_m,sub_levels)[[1]]
  
  Plot_m <- ddply(Plot_m, .(vgroup,vfact), transform, pos = (cumsum(Rate) - 0.5*Rate),sumpos=cumsum(Rate))
  Plot_m$labymax=max(Plot_m$sumpos)
  ##print(head(DPlot_m1))
  #防止标签重叠
  Plot_m <- ddply(Plot_m, .(vgroup), transform, label1=ifelse(is.na(lag(pos))|pos-lag(pos,1)>=labymax/15|Rate>labymax/20,label,""))
  # #print(Plot_m)
  Ngroup<-length(levelgupdate[[2]])
  Nsub=ceiling(length(Ngroup)/10)
  Nvfact<-length(vfactupdate[[2]])
  if(Ngroup*Nvfact>12){fonesize=2} else{fonesize=2.5}
  if(levelgupdate[[3]]==1){themeuse=chart_x270}  else {themeuse=chart_legend}
  ##print(sub_levels)
  ##print(Plot_m)
  Plot1<-ggplot(Plot_m, aes(x=vfact,y=Rate,fill=vsubgroup,label=label1)) +geom_bar(stat = "identity",position="fill")+
    ylab("")+xlab("")+scale_y_continuous(labels =percent)+colouruse+
    themeuse+labs(fill=labuse)+ geom_text(aes(y=pos,label=label1),size=fonesize)+facet_grid(.~vgroup)
  if(Ngroup>3){
    Plot1<-Plot1+theme(legend.position="bottom")+guides(fill = guide_legend(nrow=Nsub))
  }
  return(Plot1)
}

#1维-例数柱图
Nbar1=function(DPlot,group_levels,ylabuse,xlabuse,legendtrans){
  plot_m<-na.omit(tally(group_by(DPlot,vgroup)),vgroup)
  plot_m$ntrans<-plot_m$n/legendtrans
  #print(plot_m)
  plot_m$Rate<-plot_m$n/sum(plot_m$n)
  plot_m$label1<-paste0(N_legend(plot_m$ntrans),"; ",Rate_legend(plot_m$Rate))
  #print(plot_m)
  #标签顺序
  levelgupdate=grouporder(plot_m,group_levels)
  plot_m=levelgupdate[[1]]
  Ngroup<-length(levelgupdate[[2]])
  if(Ngroup<15){fonesize=2.5}  else(fonesize=1.5)
  if(levelgupdate[[3]]==1){themeuse=chart_x270}  else {themeuse=chart_legend}
  plot1<-ggplot(plot_m, aes(x=vgroup,y=ntrans,label=label1)) +geom_bar(fill=color_main,show_guide=FALSE,stat = "identity",width=0.1^(1/Ngroup))+
    ylab(ylabuse)+xlab(xlabuse)+themeuse+ geom_text(position = position_dodge(width=0.85),size=fonesize,vjust=-0.05)
  if(max(plot_m$Rate>0.95)){plot1<-plot1+scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x)(10^(-3:3)))}
  return(plot1)
}

#百分比分组柱图

#2维累计百分比图
Phist2=function(DPlot,group_levels,DMTuse,ylabuse,xlabuse,legendtrans){
  Plot_m1<-tally(group_by(DPlot,vgroup))
  Plot_m<-left_join(Plot_m1,DMTuse)
  Plot_m$Rate<-Plot_m$n/Plot_m$DMT
  #print(Plot_m)
  Plot_m$ntrans<-Plot_m$n/legendtrans
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  Ngroup<-length(levelgupdate[[2]])
  if(Ngroup>14){
    Plot_m$label1<-paste0(Rate_legend(Plot_m$Rate))
  } 
  else{
    Plot_m$label1<-paste0(N_legend(Plot_m$ntrans),"; ",Rate_legend(Plot_m$Rate))
  } 
  if(Ngroup>10){fonesize=2}
  else{fonesize=2.5}
  if(levelgupdate[[3]]==1){themeuse=chart_x270}  else {themeuse=chart_legend}
  
  plot1<-ggplot(Plot_m,aes(x=vgroup, y=Rate,label=label1))+geom_bar(stat ="identity",fill=color_main,width=0.1^(1/Ngroup))+
    ylab(ylabuse)+xlab(xlabuse)+labs(fill="")+themeuse+geom_text(position = position_dodge(width=0.9),vjust=0.05,size=fonesize)+scale_y_continuous(labels =percent)
  return(plot1) 
}

##分面dodge图
P_dodgebarfact=function(DPlot,V_FRate,colouruse,group_levels,vfact_levels,sub_levels,ylabuse,xlabuse,labuse){
  #分母Denominator
  Plot_DMT_tmp<-tally(group_by(DPlot,vgroup,vfact,vsubgroup))
  Plot_DMT<-rename(Plot_DMT_tmp,c(n='DMT'))
  #分子numerator
  Plot_n<-filter(tally(group_by(DPlot,vgroup,vfact,vsubgroup,Vrate)),Vrate==V_FRate)
  Plot_join<-left_join(Plot_n,Plot_DMT)
  Plot_m<-data.frame(Plot_join,Rate=Plot_join$n/Plot_join$DMT)
  Plot_m$label1<-Rate_legend(Plot_m$Rate)
  Plot_m$labymax<-max(Plot_m$Rate)
  
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  Plot_m=factorder(Plot_m,vfact_levels)[[1]]
  levelsubupdate=suborder(Plot_m,sub_levels)
  Plot_m=levelsubupdate[[1]]
  
  Ngroup=length(levelgupdate[[2]])
  Nsub=length(levelsubupdate[[2]])
  if(Ngroup*Nsub>40){
    #防止标签重叠
    Plot_m <- ddply(Plot_m, .(vfact,vgroup), transform, label1=ifelse(is.na(lag(Rate))|abs(Rate-lag(Rate,1))>=labymax/20,paste0(label1),""))
  }
  if (Ngroup*Nsub>20){fonesize=1.4} else {fonesize=2}
  if(levelgupdate[[3]]==1){themeuse=chart_x270}  else {themeuse=chart_legend}
  #print(head(Plot_m))
  plot1<-ggplot(Plot_m,aes(x=vgroup, y=Rate,fill=vsubgroup,label=label1))+geom_bar(stat ="identity",position="dodge")+facet_grid(vfact~.)+
    ylab(ylabuse)+xlab(xlabuse)+themeuse+colouruse+scale_y_continuous(labels =percent)+theme(legend.position="bottom")+
    geom_text(position = position_dodge(width=0.95),vjust=0.05,size=fonesize)
  if(Nsub>1){ plot1<- plot1+labs(fill=labuse)}
  else{ plot1<- plot1+labs(fill=labNull)}
  return(plot1)
}

##2维度连续线图
ClinePLOT1=function(DPlot,group_levels,sub_levels,ylabuse,xlabuse,labcolour){
  Plot_tmp<-ddply(DPlot, .(vgroup,vsubgroup), summarise,mean=mean(vstat,na.rm=TRUE),median=quantile(vstat,0.5,na.rm=TRUE))
  Plot_m1<-data.frame(vgroup=Plot_tmp$vgroup,vsubgroup=Plot_tmp$vsubgroup,Vstat="Mean",stat=Plot_tmp$mean)
  Plot_m2<-data.frame(vgroup=Plot_tmp$vgroup,vsubgroup=Plot_tmp$vsubgroup,Vstat="Median",stat=Plot_tmp$median)
  Plot_m=rbind(Plot_m1,Plot_m2)
  #print(head(Plot_m))
  Plot_m$vsubstat<-paste0(Plot_m$vsubgroup,Plot_m$Vstat)
  #print(head(Plot_m))
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  levelsubupdate=suborder(Plot_m,sub_levels)
  Plot_m=levelsubupdate[[1]]
  
  Ngroup<-length(levelsubupdate[[2]]) 
  #一行legend最多为8个 
  Nsub<-ceiling(Ngroup/8)  
  if(levelgupdate[[3]]==1){themeuse=chart_linex270}  else {themeuse=chart_linex}
  ##print(Ngroup)
  Plot_m$label<-paste0(round(Plot_m$stat,1))
  #防止标签重叠
  Plot_m$labymax=max(Plot_m$stat)-min(Plot_m$stat)
  if(Ngroup>4){
    Plot_m<-Plot_m[order(Plot_m$vgroup,Plot_m$Vstat,-Plot_m$stat),]
    Plot_m <- ddply(Plot_m, .(vgroup,Vstat), transform, label1=ifelse(is.na(lag(stat))|abs(stat-lag(stat,1))>=labymax/20,label,""))
    plot1<-ggplot(Plot_m, aes(x=vgroup,y=stat,group=vsubstat,label=label1))+themeuse+
      geom_line(aes(colour=vsubgroup,alpha=0.5))+geom_point(aes(colour=vsubgroup,alpha=0.5))+facet_grid(Vstat~.)+
      theme(legend.position="bottom")+guides(col = guide_legend(nrow=Nsub))+ 
      scale_colour_discrete(name=labcolour)+scale_alpha(guide = FALSE)
  }
  else{
    Plot_m<-Plot_m[order(Plot_m$vgroup,-Plot_m$stat),]
    Plot_m <- ddply(Plot_m, .(vgroup), transform, label1=ifelse(is.na(lag(stat))|abs(stat-lag(stat,1))>=labymax/20,label,""))
    plot1<-ggplot(Plot_m, aes(x=vgroup,y=stat,group=vsubstat,label=label1))+
      geom_line(aes(colour=vsubgroup,linetype=Vstat,alpha=0.5))+geom_point(aes(colour=vsubgroup,alpha=0.5))+
      themeuse+theme(legend.position="bottom",legend.box="horizontal")+ 
      scale_colour_discrete(name=labcolour)+scale_linetype_discrete(name=labstatline)+scale_alpha(guide = FALSE)
    
  }
  plot1<-plot1+ylab(ylabuse)+xlab(xlabuse)+geom_text(size=2.5,vjust=0.05)
  
  
  return(plot1)
}

BOXPLOT2=function(DPlot,group_levels,sub_levels,colouruse,ylabuse,xlabuse,labuse){
  Plot_m<-ddply(DPlot, .(vgroup,vsubgroup), summarise,mean=mean(vstat,na.rm=TRUE),std=sd(vstat,na.rm=TRUE),min=quantile(vstat,0,na.rm=TRUE),q1=quantile(vstat,0.25,na.rm=TRUE),median=quantile(vstat,0.5,na.rm=TRUE),
                q3=quantile(vstat,0.75,na.rm=TRUE),max=quantile(vstat,1,na.rm=TRUE))
  Plot_m<-na.omit(Plot_m,vgroup)
  lim_y=max(30,max(Plot_m$q3)+5)
  Plot_m$max1<-ifelse(Plot_m$max>lim_y,lim_y,Plot_m$max)
  #lim_y<-max(Plot_m$mean+Plot_m$std*3)
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  levleusbupdate=suborder(Plot_m,sub_levels)
  Plot_m=levleusbupdate[[1]]
  Nsub=length(levleusbupdate[[2]])
  #print(Nsub)
  Ngroup=length(levelgupdate[[2]])
  if(Ngroup+Nsub>10){fonesize=1.5} 
  else {fonesize=2.5}
  if(levelgupdate[[3]]==1){themeuse=chart_linex270}  else {themeuse=chart_linex}
  Plot_m$label1=paste0(round(Plot_m$mean,0),"/",round(Plot_m$median,0))
  ##print(Plot_m)
  Plot_m$notey=min(Plot_m$min)-0.05*max(Plot_m$max1-Plot_m$min)
  
  plot1<- ggplot(Plot_m, aes(x =vgroup,fill=vsubgroup,ymin =min, lower =q1,middle =median, upper =q3, ymax =max1))+
    geom_boxplot(stat = "identity")+themeuse+colouruse+ylab(ylabuse)+xlab(xlabuse)+labs(fill=labuse)+
    theme(legend.position="bottom")+guides(fill = guide_legend(nrow=ceiling(Nsub/9)))+
    geom_text(position=position_dodge(width=0.9),aes(x=vgroup,y=notey,label=label1),vjust=0.05,size=fonesize)+
    geom_text(position=position_dodge(width=0.9),aes(x=vgroup,y=mean,label="●"),vjust=0,hjust=0,colour='red',size=1.5)
  
  return(plot1)
}
#分面box

BOXPLOT2fact=function(DPlot,group_levels,vfact_levels,sub_levels,colouruse,ylabuse,xlabuse,labuse){
  Plot_m<-ddply(DPlot, .(vgroup,vfact,vsubgroup), summarise,mean=mean(vstat,na.rm=TRUE),std=sd(vstat,na.rm=TRUE),min=quantile(vstat,0,na.rm=TRUE),q1=quantile(vstat,0.25,na.rm=TRUE),median=quantile(vstat,0.5,na.rm=TRUE),
                q3=quantile(vstat,0.75,na.rm=TRUE),max=quantile(vstat,1,na.rm=TRUE))
  Plot_m<-na.omit(Plot_m,vgroup)
  lim_y=max(30,max(Plot_m$q3)+5)
  Plot_m$max1<-ifelse(Plot_m$max>lim_y,lim_y,Plot_m$max)
  #lim_y<-max(Plot_m$mean+Plot_m$std*3)
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  Plot_m=factorder(Plot_m,vfact_levels)[[1]]
  levleusbupdate=suborder(Plot_m,sub_levels)
  Plot_m=levleusbupdate[[1]]
  Nsub=length(levleusbupdate[[2]])
  Ngroup=length(levelgupdate[[2]])
  if(Ngroup+Nsub>10){fonesize=1.5}    else {fonesize=2.5}
  Plot_m$label1=paste0(round(Plot_m$mean,0),"/",round(Plot_m$median,0))
  #print(Plot_m)
  Plot_m$notey=min(Plot_m$min)-0.05*max(Plot_m$max1-Plot_m$min)
  if(levelgupdate[[3]]==1){themeuse=chart_linex270}  else {themeuse=chart_linex}
  
  plot1<- ggplot(Plot_m, aes(x =vgroup,fill=vsubgroup,ymin =min, lower =q1,middle =median, upper =q3, ymax =max1))+facet_grid(vfact~.)+
    geom_boxplot(stat = "identity")+themeuse+colouruse+ylab(ylabuse)+xlab(xlabuse)+labs(fill=labuse)+
    theme(legend.position="bottom")+guides(fill = guide_legend(nrow=ceiling(Nsub/9)))+
    geom_text(position=position_dodge(width=0.9),aes(x=vgroup,y=notey,label=label1),vjust=0.05,size=fonesize)+
    geom_text(position=position_dodge(width=0.9),aes(x=vgroup,y=mean,label="●"),colour='red',size=2.5)
  
  return(plot1)
}
#连续变量分面中位数、均值图
ClinePLOTfacet=function(DPlot,group_levels,vfact_levels,sub_levels,ylabuse,xlabuse,labcolour){
  Plot_tmp<-ddply(DPlot, .(vgroup,vfact,vsubgroup), summarise,mean=mean(vstat,na.rm=TRUE),median=quantile(vstat,0.5,na.rm=TRUE))
  Plot_m1<-data.frame(vgroup=Plot_tmp$vgroup,vfact=Plot_tmp$vfact,vsubgroup=Plot_tmp$vsubgroup,Vstat="Mean",stat=Plot_tmp$mean)
  Plot_m2<-data.frame(vgroup=Plot_tmp$vgroup,vfact=Plot_tmp$vfact,vsubgroup=Plot_tmp$vsubgroup,Vstat="median",stat=Plot_tmp$median)
  Plot_m=rbind(Plot_m1,Plot_m2)
  #print(head(Plot_m))
  Plot_m$vsubstat<-paste0(Plot_m$vsubgroup,Plot_m$Vstat)
  #print(head(Plot_m))
  levelgupdate=grouporder(Plot_m,group_levels)
  Plot_m=levelgupdate[[1]]
  Plot_m=factorder(Plot_m,vfact_levels)[[1]]
  levelsubupdate=suborder(Plot_m,sub_levels)
  Plot_m=levelsubupdate[[1]]
  
  Ngroup<-length(levelgupdate[[2]]) 
  Nsub<-length(levelsubupdate[[2]])
   
  Plot_m$label<-paste0(round(Plot_m$stat,1))
  #防止标签重叠
  Plot_m$labymax=max(Plot_m$stat)-min(Plot_m$stat)
  if(levelgupdate[[3]]==1){themeuse=chart_linex270}  else {themeuse=chart_linex}
  
  Plot_m=Plot_m[order(Plot_m$vgroup,-Plot_m$stat),]
  Plot_m <- ddply(Plot_m, .(vgroup), transform, label1=ifelse(is.na(lag(stat))|abs(stat-lag(stat,1))>=labymax/20,label,""))
  plot1<-ggplot(Plot_m, aes(x=vgroup,y=stat,group=vsubstat,label=label1))+geom_line(aes(colour=vsubgroup,linetype=Vstat))+
    geom_point(aes(colour=vsubgroup))+themeuse+
    facet_grid(vfact~.)+ylab(ylabuse)+xlab(xlabuse)+geom_text(size=2.5,vjust=0.05)+
    scale_colour_discrete(name=labcolour)+scale_linetype_discrete(name=labstatline)
  if(Ngroup+Nsub<9){plot1=plot1+theme(legend.position="bottom",legend.box="horizontal")}
  else if(Ngroup>9){plot1=plot1+theme(legend.position="bottom")+guides(col = guide_legend(nrow=ceiling(Nsub/9)))}
  else {plot1=plot1+theme(legend.position="bottom")}
  return(plot1)
}
