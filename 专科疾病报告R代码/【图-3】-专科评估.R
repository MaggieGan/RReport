##评价体系

DPACSM=function(Note,DPlot=ACSMHos,FBaseL=ACSMnow){
  COMLevel=c("LEVEL","ASSCM")
  if(Note=="FEE_TOTA") {
    Vlist=c("COST","COST1",COMLevel)
    DPlot$COST= DPlot$COST/1000
    DPlot$COST1= DPlot$COST1/1000
    FBaseL$FEE_TOTA=FBaseL$FEE_TOTA/1000
    }
  else if (Note=="SJDAYS"){Vlist=c("LOS","LOS1",COMLevel)}
  else {
    if (Note=="STATUS"){Vlist=c("MRLTY","MRLTY1",COMLevel)}
    else {Vlist=c(Note,paste0(Note,1),COMLevel)}
  } 
  
  DPlot=data.frame(with(DPlot,mget(Vlist)))
  names(DPlot)=c("ACSHQMS","ACSDs",COMLevel)


  BaseLine1=ddply(DPlot,.(ASSCM),summarise,Mean=mean(ACSDs,na.rm=TRUE),Q1=quantile(ACSDs,0.25,na.rm=TRUE),Q3=quantile(ACSDs,0.75,na.rm=TRUE))
  print(BaseLine1)
  #DPlot$LEVEL<-factor(DPlot$LEVEL,labels=LEVEL_levels)
  #DPlot$ASSCM<-factor(DPlot$ASSCM,labels=ASSCM_levels[2:3])

  #BaseLine
  FBaseL=data.frame(with(FBaseL,mget(c("ASSCM",Note))))
  names(FBaseL)=c("ASSCM","AcsmNow")
  if(!(Note %in% c("FEE_TOTA","SJDAYS"))){
    FBaseL$AcsmNow=ifelse( FBaseL$AcsmNow==2,0, FBaseL$AcsmNow)
  }
  BaseLine=ddply(FBaseL,.(ASSCM),summarise,Mean=mean(AcsmNow,na.rm=TRUE),Q1=quantile(AcsmNow,0.25,na.rm=TRUE),Q3=quantile(AcsmNow,0.75,na.rm=TRUE))
 #print(BaseLine)
  
  FLineXY=c(0,max(max(DPlot$ACSHQMS,na.rm=TRUE),BaseLine1$Q3,na.rm=TRUE))
  DPlot$Flinx=DPlot$ACSHQMS
  DPlot$Fliny=DPlot$ACSHQMS
  DPlot$Flinx[1:2]=FLineXY
  DPlot$Fliny[1:2]=FLineXY
  print(head(DPlot))
  plot_out=ggplot(DPlot,aes(x=ACSDs, y=ACSHQMS))+geom_point(aes(colour=as.factor(LEVEL),shape=as.factor(ASSCM),alpha=0.9))+
  geom_line(aes(x=Flinx,y=Fliny,alpha=0.7))+chart_legend+scale_alpha(guide = FALSE)+
  theme(legend.position="bottom")+theme(legend.box="horizontal")+
    #geom_hline(yintercept=BaseLine1$Q1[1],colour=c3[1],linetype = "longdash")+
    #geom_hline(yintercept=BaseLine1$Q1[2],colour=c3[2],linetype = "longdash")+
    #geom_hline(yintercept=BaseLine1$Q3[1],colour=c3[1],linetype = "longdash")+
    #geom_hline(yintercept=BaseLine1$Q3[2],colour=c3[2],linetype = "longdash")+
    geom_vline(xintercept=BaseLine1$Q3[2],colour=c3[2],linetype = "longdash")+
    geom_vline(xintercept=BaseLine1$Q1[1],colour=c3[1],linetype = "longdash")+
    geom_vline(xintercept=BaseLine1$Q1[2],colour=c3[2],linetype = "longdash")+
    geom_vline(xintercept=BaseLine1$Q3[1],colour=c3[1],linetype = "longdash")+
    scale_colour_manual(values=c3,name=lab_LEVEL,labels =LEVEL_levels)+ scale_shape_manual(values=c(16,18),name=lab_ASSCM,labels =ASSCM_levels[2:3])
  if(Note=="TRAVELT"){
    plot_out=plot_out+
      xlab(paste0("诊治异地",PTuse,"患者比例"))+ylab("诊治异地患者比例")+
      scale_y_continuous(labels =percent)+scale_x_continuous(labels =percent)
  }
  else if(!(Note %in% c("FEE_TOTA","SJDAYS"))){
    plot_out=plot_out+scale_y_continuous(labels =percent)+scale_x_continuous(labels =percent)+
    xlab(paste0(PTuse,"患者",get(paste0("lab_",Note)),"比例"))+ylab(paste0("HQMS",get(paste0("lab_",Note)),"比例"))
  }
  else{
    plot_out=plot_out+
      xlab(paste0(PTuse,"患者",get(paste0("lab_",Note))))+ylab(paste0("HQMS",get(paste0("lab_",Note))))
  }
  return(plot_out)
}

DPACSM("FEE_TOTA")
DPACSM("SJDAYS")
DPACSM("STATUS")


#1维1个连续变量
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
  plot_m$note=""
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

ACSMHos$AVENG=ceiling(ACSMHos$AVEN1/50)
filter(ACSMHos,ORGID==13163)
ggplot(ACSMHos,aes(as.factor(AVENG)))+geom_bar(aes(fill =as.factor(ASSCM)),position="dodge")

 ggplot(mpg, aes(class))+geom_bar(aes(fill = drv), position = "dodge")

, aes(AVEN1,fill=as.factor(ASSCM))) +  geom_freqpoly(binwidth = 50)+

ggplot(diamonds, aes(price, colour = cut))

 geom_histogram(bins = 200)
 
 ACSMHos$AVENG=ceiling(ACSMHos$AVEN1/100)
 
ggplot(ACSMHos, aes(x=vstat,y=Rate,group=vgroup,label=label1))+
  geom_line(aes(colour=vgroup))+geom_point(aes(colour=vgroup))+scale_y_continuous(labels =percent)+
  geom_text(size=2.5,hjust=0.5,vjust=0)+scale_x_continuous(breaks=break1)+chart_linex+theme(legend.position="bottom")+
  ylab(ylabuse)+xlab(xlabuse)+scale_colour_discrete(name=labcolour)
  
