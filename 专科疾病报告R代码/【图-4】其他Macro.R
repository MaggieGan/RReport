### Macro
risk_bubble=function(PT,h,v){
  risk_q3=quantile(PT$p_risk,0.25)
  ds_q3=quantile(PT$p_ds,0.50)
  PT$worduse=ifelse((PT$p_risk>risk_q3|PT$p_ds>ds_q3)&PT$n>500,PT$words,"")
  #PT$worduse=PT$word
  max_n=log10(10^ceiling(log10(max(PT$n)/100)))+1
  PT<-filter(PT,note %in% c("b","c"))
  #PT$point_size<-log10(PT$DMT/100)
  PT$point_size<-PT$p_ds*10
  Ylim=c(0,ceiling(max(PT$p_risk)*100)/100)
  Xlim=c(0,max(PT$n)*1.05)
  print(Xlim)
  print(Ylim)
  ylab1=paste0("不同风险人群中",PTuse,"的比例")
  xlab1=paste0("不同风险人群中",PTuse,"患者人次")
  risk_gg<-ggplot(PT,aes(x=n, y=p_risk,label=words))+
    geom_point(aes(color=note, size=point_size,stroke = point_size),show.legend = FALSE)+
    xlab(xlab1)+ylab(ylab1)+chart_legend+colour3+
    geom_text(size=2,hjust=h,vjust=v)+scale_y_continuous(labels =percent,limits=Ylim)+
	scale_x_continuous(labels =comma,limits=Xlim)
  
  return(risk_gg)
}   
#risk_bubble(riskBublle,0.5,-1)
Trisk_bubble=function(PT){
  TLout=PT[order(PT$note,-PT$p_ds),]
  TLout=data.frame(with(TLout,mget(c("tmp","n","DMT","p_ds","p_risk"))))
  TLout=ddply(TLout,NULL,transform,Pd1=paste0(round(p_ds*100,2)),Pd2=paste0(round(p_risk*100,2)))
   TLout=data.frame(with(TLout,mget(c("tmp","n","Pd2","Pd1"))))
  names(TLout)=c("患者人群","CKD患者人数","CKD患者比例（%）","占CKD患者比例（%）") 
  TLout=Tmplt(TLout,1)
  return(TLout)}
 Trisk_bubble(riskBublle)

FontCount=function(CN,Spec=TRUE){
  a=length(get(paste0(CN,"_levels")))
  if(Spec){ fontSnow=ifelse(CN=="CAUSE"|a>6,8, ifelse(a>4,9,10))}
  else{fontSnow=ifelse(a>6,8, ifelse(a>=4,9,10)) }
  return(fontSnow)}

#异地来诊--异地就诊
MedicalT1=function(Tdta,GV,SubV,Vnote){
  #整体异地就医情况
  HLine=TablePCT1(Tdta,"ProNote","TRAVEL",Template=FALSE)
  HLine[,1]="Overall"
  NameNow=names(HLine)
  NameNow[1]="Variable"
  names(HLine)=NameNow
  
  #Tdta=filter(Tdta,!(PRO %in% ProExclud))
  a=TableRate2(Tdta,GV,SubV,Vnote,1,Template=FALSE)
  a=rbind(HLine[1,],a)
  print(head(a))
  Tdta=filter(Tdta,!(PRO %in% ProExclud))
  b=TableRate2(Tdta,GV,SubV,Vnote,2,Template=FALSE)
  b=rbind(HLine[2,],b)
  print(head(b))
  NameNow=names(b)
  NameNow[2:3]=c("本地来诊","本地就诊")
  names(b)=NameNow
  C=left_join(a,b)
  C$blankCol=""
  D=C[,c(1,4,2,6,5,3,6)]
  D[,2]=ifelse(is.na(D[,2]),"--", D[,2])
  D[,5]=ifelse(is.na(D[,5]),"--", D[,5])
  
  MyFTable = FlexTable( data = D, header.columns = FALSE,
                        header.cell.props = cellProperties( background.color = "#BFBFBF",border.bottom.style="solid"),
                        header.text.props = textProperties( color = "black",font.size = 10, font.weight = "bold" ),
                        header.par.props = parProperties(text.align = "center",padding=0),
                        body.text.props = textProperties( font.size = 10 )
  )
  #合并的表头
  MyFTable = addHeaderRow( MyFTable,
                           value = c( "","医院收治患者","", "常住患者",""),
                           colspan = c( 1,2,1, 2,1)
  )
  
  MyFTable = addHeaderRow( MyFTable,
                           value=c("省份", "本地来诊", "异地来诊","", "本地就诊", "异地就诊","")
  )
  # zebra stripes - alternate colored backgrounds on table rows
  MyFTable = setZebraStyle( MyFTable, odd = "#F2F2F2", even = "white" )
  # applies a border grid on table
  MyFTable = setFlexTableBorders(MyFTable,
                                 inner.vertical =borderNone(),
                                 inner.horizontal = borderNone(),
                                 outer.vertical =borderNone(),
                                 outer.horizontal = borderProperties( color = "#404040",
                                                                      style = "solid", width = 2 )
  )
  
  MyFTable[,1:7]= parProperties(text.align = "center")
  #
  MyFTable[1,]=cellProperties(border.bottom=borderProperties(color="#404040", style="solid", width=1),
                                border.left=borderNone(),border.right=borderNone(),border.top=borderNone())

  MyFTable = setFlexTableWidths( MyFTable,
                                 widths = c(1,1.2,1.2,0.5,1.2,1.2,0.5) )
  #  widthTmp=rep(1,dim2)
  #  TLin=as.data.frame(TLin,stringsAsFactors=FALSE)
  # for (i in 1:dim2){
  #   widthTmp[i]=max(nchar(TLin[,i]))
  # }
  #  widthTmp=widthTmp/sum(widthTmp)
  #print(widthTmp)
  #  MyFTable = setFlexTableWidths( MyFTable,widths=Tlenth*widthTmp)
  return(MyFTable)
  # add MyFTable into document
}




##############################MapPlot###################
#PNote--省份ID名
#Note-对应有PNote:用于展示的比例;RNote:填色分组（升序）;
# mapLabNote_levels:图例（填色分组）;mapLabNote:图例标题
MapPlot<-function(Dplot,PNote,Note,map_color_use=map_blue,decimal=1,Chinese=TRUE,PRO=TRUE){
  #提取关注的疾病
  Vlist=c(PNote,paste0("P",Note),paste0("R",Note))
  print(Vlist)
  pro_summ=data.frame(with(Dplot,mget(Vlist)))
 if(PRO){ names(pro_summ)=c("pro_Nid","n","order")}
  else{ names(pro_summ)=c("region_Nid","n","order")}
  
  ##显示的是百分比还是例数
  if(max(pro_summ$n)<1){
    pro_summ$legend1=paste0(round(pro_summ$n*100,decimal),"%")
  }
  else{  pro_summ$legend1=paste0(pro_summ$n)}
  #print(pro_summ)

  maplabel1<-left_join(maplabel,pro_summ) #合并两个数据框
  if(PRO){#按省份填充
    map_legend2<-get(paste0("mapLab",Note,"_levels"))
    map_legend1<-c(1:6)

  if(Chinese){  maplabel1$lable<-paste(maplabel1$province,ifelse(is.na(maplabel1$legend1),"-",maplabel1$legend1))}
  else{maplabel1$lable<-paste(maplabel1$proE,ifelse(is.na(maplabel1$legend1),"-",maplabel1$legend1))  }
  }
  else{#按区域填充
    map_legend1<-c(1:7)
    if(Chinese){  maplabel1$lable<-ifelse(maplabel1$region!="",paste(maplabel1$region,ifelse(is.na(maplabel1$legend1),"-",maplabel1$legend1)),"")}
    else{maplabel1$lable<-ifelse(maplabel1$regionE!="",paste(maplabel1$regionE,ifelse(is.na(maplabel1$legend1),"-",maplabel1$legend1)),"")   }
   }
  
  print(maplabel1)
  
  map_color<-data.frame(NAME=maplabel1$NAME,color=maplabel1$order)
  #print(map_color)
  xs1<-left_join(xs,map_color) 
  
  china_mapdata<-join(china_map1, xs1, type = "full") #合并两个数据框
  ylabuse=get(paste0(paste0("mapLab",Note)))
  map_plot<-ggplot(china_mapdata, aes(x = long, y = lat, group = group,fill=factor(color)))+
    geom_polygon( )+
    chart_linex+theme(legend.position=c(0.15,0.35))+labs(fill=ylabuse)+
    scale_x_continuous("",breaks = NULL)+scale_y_continuous("",breaks = NULL)+xlab("")+ylab("")
  
  if(PRO){map_plot=map_plot+geom_path(colour = "#FFFFFF",size = 0.6)+
    scale_fill_manual(values=map_color_use,na.value = "#F5F5F5",breaks =factor(map_legend1),labels =map_legend2)}
  else{map_plot=map_plot+geom_path(colour = "#FFFFFF",size = 0.5)+
    scale_fill_manual(values=map_color_use,na.value = "#F5F5F5",breaks =factor(map_legend1),guide = FALSE)}
  map_plot<-map_plot+annotate("text",x=maplabel1$map_x,y=maplabel1$map_y,label=maplabel1$lable,size = 2.5)
  return(map_plot)
}

################费用构成###############
#fee_med用药费fee_check诊断类 fee_sugy 手术费fee_opt操作费fee_Nur护理费fee_supl耗材费fee_other  其他费用

#费用构成
COSTcompose=function(DPlot,GV,template=TRUE){
  DPlot_tmp=data.frame(with(DPlot,mget(c(paste0(GV),"FEE_TOTA","FEE_SUGY","FEE_OPT","FEE_CHEC","FEE_MED","FEE_NUR","FEE_SUPL","FEE_OTHE"))))
  names(DPlot_tmp)=c("vgroup","fee_total","fee_sugy","fee_opt","fee_check","fee_med","fee_Nur","fee_supl","fee_other")


  DPlot_tmp=filter(DPlot_tmp,fee_total>0)
  #DPlot_tmp=rename(DPlot_tmp,c(R_order="vgroup"))
  fee_tmp=ddply(DPlot_tmp,.(vgroup),summarise,fee0=sum(fee_total,na.rm=TRUE),
                fee1=sum(fee_med,na.rm=TRUE),
                fee2=sum(fee_check,na.rm=TRUE),
                fee3=sum(fee_sugy,na.rm=TRUE),fee4=sum(fee_opt,na.rm=TRUE),
               fee5=sum(fee_Nur,na.rm=TRUE),fee6=sum(fee_supl,na.rm=TRUE),fee7=sum(fee_other,na.rm=TRUE))

  gnote=tally(group_by(fee_tmp,vgroup))$vgroup
  COST_tmp=fee_tmp[,1:2]
  Ngroup=length(gnote)

  fee_trans=NULL
  for(i in 1:Ngroup){
    fee_trans_tmp=t(subset(fee_tmp,vgroup==gnote[i]))
    
    fee_trans_tmp1=data.frame(vgroup=gnote[i],vsubgroup=c(1:7),fee=fee_trans_tmp[-c(1,2)])
    fee_trans=rbind(fee_trans,fee_trans_tmp1)
  }
  fee_trans=ddply(fee_trans,.(vgroup),transform,ntrans=fee/10000000,Rate=fee/sum(fee))
                  
  #标签
  fee_trans <- ddply(fee_trans, .(vgroup), transform,
                     label0=paste0(N_legend(ntrans),"(",Rate_legend(Rate),")"),
                     label1=paste0(Rate_legend(Rate)))
  #组别标签
  #print(fee_trans)
  if(template){
      for(i in 1:Ngroup){
      CostComp1=subset(filter(fee_trans,vgroup==gnote[i]),select=c("vsubgroup","label0"))
      names(CostComp1)=c("vsubgroup",paste0("label",i))
      if(i==1){CostComp=CostComp1}
      else{CostComp=left_join(CostComp,CostComp1)}
      }
    CostComp[8,1]=8
    CostComp[8,(1:Ngroup)+1]=N_legend(t(COST_tmp[,2])/10000000)
    
    CostComp=suborder(CostComp,fee_levels)[[1]]
    group_levels=get(paste0(GV,"_levels"))[gnote]
   print(group_levels)
   print(CostComp)
    names(CostComp)=c("费用构成",group_levels)
    CostComp[,1]=fee_levels
    CostComp=Tmplt(CostComp,1)
    return(CostComp)
}
  else{return(fee_trans)}
}

# COSTcompose(CAUSE,"CAUSE")

##图
PLotCOST=function(DPlot,GV){
  fee_trans=COSTcompose(DPlot,GV,template=FALSE)
  group_levels=get(paste0(GV,"_levels"))

  #组别标签
  levelgupdate=grouporder(fee_trans,group_levels)
  fee_trans=levelgupdate[[1]]
  Ngroup=length(levelgupdate[[2]])
 # print(levelgupdate[[2]])
  fee_trans=suborder(fee_trans,fee_levels)[[1]]
  if(levelgupdate[[3]]==1){themeuse=chart_x270}  else {themeuse=chart_legend}
  
  if(Ngroup==1){
    plot1<-ggplot(fee_trans, aes(x=vsubgroup,y=Rate)) +geom_bar(fill=color_main,show_guide=FALSE,stat = "identity",width=0.1^(1/7))+
      ylab(lab_Null)+xlab(lab_compose)+themeuse+geom_text(aes(label=label1),size=2.5,vjust=0.05)+scale_y_continuous(labels =percent)
  }
  else{
    #防止标签重叠
    if(Ngroup>12){fontsize=1.5}else{fontsize=2.5}
    fee_trans <- ddply(fee_trans, .(vgroup), transform, pos = (cumsum(Rate) - 0.5*Rate),sumpos=cumsum(Rate))
    fee_trans$labymax=max(fee_trans$sumpos)

    fee_trans<- ddply(fee_trans, .(vgroup), transform, label2=ifelse(is.na(lag(pos))|pos-lag(pos,1)>=labymax/15|Rate>labymax/20,
                                                                       paste0(Rate_legend(Rate)),""))
  
  # print(fee_trans)
    plot1<-ggplot(fee_trans,aes(x=vgroup, y=Rate,fill=vsubgroup,label=label2))+geom_bar(stat ="identity",position="stack",width=0.1^(1/Ngroup))+
      ylab(lab_Null)+xlab(lab_Null)+colour9+labs(fill=lab_compose)+scale_y_continuous(labels =percent)+
      themeuse+geom_text(aes(x=vgroup,y=pos),size=fontsize)
    if(Ngroup>5){plot1<-plot1+theme(legend.position="bottom")+guides(fill = guide_legend(nrow=1))}
  }
  return(plot1)
  
}

#PLotCOST(CAUSE,"CAUSE")
