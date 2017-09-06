#1维1个变量分布
D11PCT<-function(DPlot,group_levels,lab_Gnote,Vnote){
  #维度变量的分组数
  Cnt_group=length(tally(group_by(DPlot,vgroup))$vgroup)
  labG=lab_Gnote
  #分析变量的分组数及label
  Cnt_sub=length(tally(group_by(DPlot,vsubgroup))$vsubgroup)
  vsub_levels=get(paste0(Vnote,"_levels"))
  #样式设定：颜色、ylabuse,xlabuse,labuse
  colornow=get(paste0("colour",Cnt_sub))
  labsub=get(paste0("lab_",Vnote))
  if(Cnt_group==1){#维度变量只有1组
    DPlot<-rename(DPlot,c(vgroup='Notuse',vsubgroup='vgroup'))
    if(Cnt_sub<6){ #分析变量在6组以下--饼图
      plot_out<-P_pie1(DPlot,colornow,vsub_levels,labsub,1000)
    }
    else {#分析变量在5组及以上--Bar
      plot_out<-Nbar1(DPlot,vsub_levels,lab_PTN,labsub,1000)
    }
  }
  else if(Cnt_group<4 & Cnt_sub<=4){#维度变量2-3组且分析变量在3组以下--左右分面饼图
    plot_out<-P_piefact(DPlot,colornow,group_levels,vsub_levels,labsub,1000)
  }
  else{#1-维度变量5组以下，分析变量分组5组以下-横向StackBar
    #2-StackBar
    plot_out<-Pstackhist2(DPlot,colornow,group_levels,vsub_levels,lab_PTP,labG,labsub,1000)
  }
  return(plot_out)
}

#2维1个变量分布

D21PCT<-function(DPlot,group_levels,vfact_levels,lab_Gnote,Fnote,Vnote){
  #第一维度变量的分组数
  Cnt_group=length(tally(group_by(DPlot,vgroup))$vgroup)
  labG=lab_Gnote
  #第二维度变量的分组数
  Cnt_fact=length(tally(group_by(DPlot,vfact))$vfact)
  labfact=get(paste0("lab_",Fnote))
  #分析变量的分组数及label
  Cnt_sub=length(tally(group_by(DPlot,vsubgroup))$vsubgroup)
  vsub_levels=get(paste0(Vnote,"_levels"))
  colornow=get(paste0("colour",Cnt_sub))
  labsub=get(paste0("lab_",Vnote))
  if(Cnt_group==1){#第一维度变量只有1组
    if (Cnt_fact==1){ #第二维度变量1组--饼图 
      DPlot<-rename(DPlot,c(vgroup='Notuse',vsubgroup='vgroup'))
      DPlot<-D11PCT(DPlot,vsub_levels,Vnote)
    }
    else if(Cnt_fact<4 & Cnt_sub<6){ #第二维度变量2-3组--左右分面饼图 
      DPlot<-rename(DPlot,c(vgroup='Notuse',vfact='vgroup'))
      plot_out<-P_piefact(DPlot,colornow,vfact_levels,vsub_levels,labsub,1000)
    }
    else {#分析变量在4组及以上--stackBar
      DPlot<-rename(DPlot,c(vgroup='Notuse',vfact='vgroup'))
      plot_out<-Pstackhist2(DPlot,colornow,vfact_levels,vsub_levels,labfact,labNull,labsub,1000)
    }
  }
  else if(Cnt_fact==1){#第二维度变量1组
    if(Cnt_group<4 & Cnt_sub<6 ){ #第一维度变量2-3组--左右分面饼图 
      plot_out<-P_piefact(DPlot,colornow,group_levels,vsub_levels,labsub,1000)
    }
    else {#分析变量在4组及以上--stackBar
      plot_out<-Pstackhist2(DPlot,colornow,group_levels,vsub_levels,labG,labNull,labsub,1000)
    }
  }
  else if(Cnt_group<4){#维度变量2-3组且分析变量在3组以下--左右分面饼图
    plot_out<-P_factbar(DPlot,colornow,group_levels,vfact_levels,vsub_levels,labsub,1000)  
  }
  else if(Cnt_fact<4){#维度变量2-3组且分析变量在3组以下--左右分面饼图
    plot_out<-P_barfact(DPlot,colornow,group_levels,vfact_levels,vsub_levels,labsub,1000)
  }
  else{print("warming:维度太多")  }
  return(plot_out)
}
#1维1个变量发生率
D11Rate <-function(DPlot,GV,SubV,V_Frate,Vnote){
  #DPlot,group_levels,V_Frate,lab_Gnote,Vnote
  DPlot=data.frame(with(DPlot,mget(c("GLOBALID",paste0(GV),paste0(SubV)))))
  names(DPlot)=c("GLOBALID","vgroup","vsubgroup") 
  group_levels=get(paste0(GV,"_levels"))
  lab_Gnote=get(paste0("lab_",GV))
  vsub_levels=get(paste0(SubV,"_levels"))
  #维度变量的分组数
  
  Cnt_group=length(tally(group_by(DPlot,vgroup))$vgroup)
  labG=lab_Gnote
  #分析变量的分组数及label
  Cnt_sub=length(tally(group_by(DPlot,vsubgroup))$vsubgroup)
  #vsub_levels=get(paste0(Vnote,"_levels"))
  
  #样式设定：颜色、ylabuse,xlabuse,labuse
  colornow=get(paste0("colour",Cnt_sub))
  labnow=get(paste0("lab_",Vnote))
  if(Cnt_group==1){#维度变量只有1组--饼图
    DPlot<-rename(DPlot,c(vgroup='Notuse',vsubgroup='vgroup'))
    plot_out<-P_pie1(DPlot,colornow,vsub_levels,labnow,1000)
  }
  else if(Cnt_group<4){#维度变量2-3组--左右分面饼图
    plot_out<-P_piefact(DPlot,colornow,group_levels,vsub_levels,labnow,1000)
  }
  else{#Bar
    DMTnow=rename(tally(group_by(DPlot,vgroup)),c(n="DMT"))
    labnow=paste0(labnow,"比例(%)")
    DPlot<-filter(DPlot,vsubgroup==V_Frate)  
    plot_out<-Phist2(DPlot,group_levels,DMTnow,labnow,labG,1000)
  }
  return(plot_out)
}
#D11Rate(risk_use,"risk","THYROIDC",1,"PT")

#2维1个变量发生率
D21Rate<-function(DPlot,GV,FactV,SubV,V_Frate,Vnote){
  #DPlot,group_levels,V_Frate,lab_Gnote,Vnote
  DPlot=data.frame(with(DPlot,mget(c("GLOBALID",paste0(GV),paste0(FactV),paste0(SubV)))))
  names(DPlot)=c("GLOBALID","vgroup","vfact","vsubgroup") 
  DPlot=filter(DPlot,!is.na(vgroup)&!is.na(vfact))
  #标签
  group_levels=get(paste0(GV,"_levels"))
  labG=get(paste0("lab_",GV))
  vfact_levels=get(paste0(FactV,"_levels"))
  labfact=get(paste0("lab_",FactV))
  vsub_levels=get(paste0(SubV,"_levels"))
  labsub=get(paste0("lab_",SubV))
  
  #DPlot,group_levels,vfact_levels,V_Frate,lab_Gnote,Fnote,Vnote){
  #第一维度变量的分组数
  Cnt_group=length(tally(group_by(DPlot,vgroup))$vgroup)
  #第二维度变量的分组数
  Cnt_fact=length(tally(group_by(DPlot,vfact))$vfact)
  
  if(Cnt_fact>Cnt_group & Cnt_fact>10){
    Cnt_group0=Cnt_group
    Cnt_group=Cnt_fact
    Cnt_fact=Cnt_group0
    labG0=labG
    labG=labfact
    labfact=labG0
    group_levels0=group_levels
    group_levels=vfact_levels
    vfact_levels=group_levels0
    DPlot=rename(rename(DPlot,c(vgroup="vgroup0",vfact="vfact0")),c(vgroup0="vfact",vfact0="vgroup"))
  }
  #分析变量的分组数及label
  Cnt_sub=length(tally(group_by(DPlot,vsubgroup))$vsubgroup)
  #样式设定：颜色、ylabuse,xlabuse,labuse
  if(Cnt_group==1){#第一维度变量只有1组
    DPlot<-rename(DPlot,c(vgroup='Notuse',vfact='vgroup'))
    if(Cnt_fact<3 | (Cnt_fact==3 & Cnt_sub<4)){#第二维度变量<4组--左右分面饼图
      colornow=get(paste0("colour",Cnt_sub))
      plot_out<-P_piefact(DPlot,colornow,vfact_levels,vsub_levels,labsub,1000)
    }
    else{#第二维度变量4组以上--Bar
      DMTnow=rename(tally(group_by(DPlot,vgroup)),c(n="DMT"))
      DPlot<-filter(DPlot,vsubgroup==V_Frate)  
      labnow=paste0(labsub,"比例(%)")
      plot_out<-Phist2(DPlot,vfact_levels,DMTnow,labnow,labfact,1000)
    }
  }
  else if (Cnt_fact==1){#第二维度变量只有1组
    if(Cnt_group<4){#第一维度变量<4组--左右分面饼图
      colornow=get(paste0("colour",Cnt_sub))
      plot_out<-P_piefact(DPlot,colornow,group_levels,vsub_levels,labsub,1000)
    }
    else{#第一维度变量4组以上--Bar
      DMTnow=rename(tally(group_by(DPlot,vgroup)),c(n="DMT"))
      DPlot<-filter(DPlot,vsubgroup==V_Frate)  
      labnow=paste0(labsub,"比例(%)")
      plot_out<-Phist2(DPlot,group_levels,DMTnow,labnow,labG,1000)
    }
  }
  else if(Cnt_group<4&Cnt_fact<4){#第一第二位都只有2-3组 ring pie
    colornow=get(paste0("colour",Cnt_sub))
    plot_out<-P_ringfact(DPlot,colornow,vfact_levels,group_levels,vsub_levels,labsub,1000)
  }
  else if(Cnt_fact*Cnt_group<45){#第二维度变量2-3组--dodge bar
    DPlot<-rename(DPlot,c(vsubgroup="Vrate",vfact='vsubgroup'))
    colornow=get(paste0("colour",Cnt_fact))
    labnow=paste0(labsub,"比例(%)")
    plot_out<-P_bar3(DPlot,1,colornow,group_levels,vfact_levels,labnow,labG,labfact)
  }
  else{#线图
    DPlot<-rename(DPlot,c(vsubgroup="Vrate",vfact='vsubgroup'))
    labnow=paste0(labsub,"比例(%)")
    plot_out<-P_line3(DPlot,1,group_levels,vfact_levels,labnow,labG,labfact)
    
  }
  return(plot_out)
}
#D21Rate(risk_use,"risk","PRO_ID",PTM,1,"PT")

#3维1个变量发生率
D31Rate<-function(DPlot,group_levels,vfact_levels,vsub_levels,V_Frate,lab_Gnote,Fnote,Snote,Vnote){
  #第一维度变量的分组数
  Cnt_group=length(tally(group_by(DPlot,vgroup))$vgroup)
  labG=lab_Gnote
  #第二维度变量的分组数
  Cnt_fact=length(tally(group_by(DPlot,vfact))$vfact)
  labfact=get(paste0("lab_",Fnote))
  #第三维度变量的分组数
  Cnt_sub=length(tally(group_by(DPlot,vsubgroup))$vsubgroup)
  labsub=get(paste0("lab_",Snote))
  #分析变量
  labvnote=get(paste0("lab_",Vnote))
  #样式设定：颜色、ylabuse,xlabuse,labuse
  if(Cnt_group==1){#第一维度变量只有1组--降至2维
    DPlot=rename(DPlot,c(vgroup="Notuse",vfact="vgroup",vsubgroup="vfact",Vrate="vsubgroup"))
    plot_out<-D21Rate(DPlot,vfact_levels,vsub_levels,V_Frate,labfact,Snote,Vnote)
  }
  else if(Cnt_fact==1){#第二维度变量只有1组--降至2维
    DPlot=rename(DPlot,c(vfact="Notuse",vsubgroup="vfact",Vrate="vsubgroup"))
    plot_out<-D21Rate(DPlot,group_levels,vsub_levels,V_Frate,labG,Snote,Vnote)
  }
  else if (Cnt_sub==1){#第三维度变量只有1组--降至2维
    DPlot=rename(DPlot,c(vsubgroup="Notuse",Vrate="vsubgroup"))
    plot_out<-D21Rate(DPlot,group_levels,vfact_levels,V_Frate,labG,Fnote,Vnote)
  }
  else if (Cnt_fact==2){#第2维度变量(分面)2组
    colornow=get(paste0("colour",Cnt_sub))
    labnow=paste0(labvnote,"比例(%)")
    plot_out<-P_dodgebarfact(DPlot,1,colornow,group_levels,vfact_levels,vsub_levels,labnow,labG,labsub)
  }
  else if (Cnt_sub==2){#第三维度变量2组
    DPlot=rename(DPlot,c(vfact="Notuse",vsubgroup="vfact"))
    DPlot=rename(DPlot,c(Notuse="vsubgroup"))
    colornow=get(paste0("colour",Cnt_fact))
    labnow=paste0(labvnote,"比例(%)")
    
    plot_out<-P_dodgebarfact(DPlot,1,colornow,group_levels,vsub_levels,vfact_levels,labnow,labG,labfact)
  }
  else if (Cnt_group==2){#第一维度变量2组
    DPlot=rename(DPlot,c(vgroup="Notuse",vfact="vgroup"))
    DPlot=rename(DPlot,c(Notuse="vfact"))
    colornow=get(paste0("colour",Cnt_fact))
    labnow=paste0(labvnote,"比例(%)")
    plot_out<-P_dodgebarfact(DPlot,1,colornow,vfact_levels,group_levels,vsub_levels,labnow,labfact,labG)
  }
  else{
    plot_out<-P_line4(DPlot,1,group_levels,vfact_levels,vsub_levels,lab_thrpy0P,labG,labfact,labsub)
  } 
  return(plot_out)
}


#4维1个变量发生率
D41Rate<-function(DPlot,group_levels,vfact_levels,vsub1_levels,vsub2_levels,V_Frate,Gnote,Fnote,S1note,S2note,Vnote){
  #第一维度变量的分组数
  Cnt_group=length(tally(group_by(DPlot,vgroup))$vgroup)
  labG=get(paste0("lab_",Gnote))
  #第二维度变量的分组数
  Cnt_fact=length(tally(group_by(DPlot,vfact))$vfact)
  labfact=get(paste0("lab_",Fnote))
  #第三维度变量的分组数
  Cnt_sub1=length(tally(group_by(DPlot,vsub1group))$vsub1group)
  labsub1=get(paste0("lab_",S1note))
  #第四维度变量的分组数
  Cnt_sub2=length(tally(group_by(DPlot,vsub2group))$vsub2group)
  labsub2=get(paste0("lab_",S2note))
  #分析变量
  #样式设定：颜色、ylabuse,xlabuse,labuse
  if(Cnt_group==1){#第一维度变量只有1组--降至3维
    DPlot=rename(DPlot,c(vgroup="Notuse",vfact="vgroup",vsub1group="vfact",vsub2group="vsubgroup"))
    plot_out<-D31Rate(DPlot,vfact_levels,vsub1_levels,vsub2_levels,V_Frate,Fnote,S1note,S2note,Vnote)
  }
  else if(Cnt_fact==1){#第二维度变量只有1组--降至3维
    DPlot=rename(DPlot,c(vfact="Notuse",vsub1group="vfact",vsub2group="vsubgroup"))
    plot_out<-D31Rate(DPlot,group_levels,vsub1_levels,vsub2_levels,V_Frate,Gnote,S1note,S2note,Vnote)
  }
  else if (Cnt_sub1==1){#第三维度变量只有1组--降至2维
    DPlot=rename(DPlot,c(vsub2group="vsubgroup"))
    plot_out<-D31Rate(DPlot,group_levels,vfact_levels,vsub2_levels,V_Frate,Gnote,Fnote,S2note,Vnote)
  }
  else if (Cnt_sub2==1){#第三维度变量只有1组--降至2维
    DPlot=rename(DPlot,c(vsub1group="vsubgroup"))
    plot_out<-D31Rate(DPlot,group_levels,vfact_levels,vsub1_levels,V_Frate,Gnote,Fnote,S1note,Vnote)
  }
  else{
    plot_out<-P_linefact4(DPlot,group_levels,vfact_levels,vsub1_levels,vsub2_levels,1,lab_PTP,labG,labsub1,labsub2)
  } 
  return(plot_out)
}

#1维1个连续变量
D11Stat<-function(DPlot,group_levels,lab_Gnote,Vnote){
  #第一维度变量的分组数
  g_tally=tally(group_by(DPlot,vgroup))
  Cnt_group=length(g_tally$vgroup)
  labG=lab_Gnote
  labV=get(paste0("lab_",Vnote))
  if(Cnt_group==1){#维度变量只有1组-单线
    plot_out<-N_line1(DPlot,lab_PTN,labV,1000)
  }
  else if (Cnt_group==2){#维度变量只有1组-双线
    if(max(g_tally$n/sum(g_tally$n))>0.65){#分组分布不均匀
      plot_out<-P_Cline2(DPlot,group_levels,lab_PTP,labV,labG)
    }
    else{ plot_out<-N_line2(DPlot,group_levels,lab_PTN,labV,labG,1000)}
  }
  else{#维度变量3组以上，box
    plot_out<-BOXPLOT1(DPlot,1,group_levels,labV,labG)
  }
  return(plot_out)
}
#2维1个连续变量
D21Stat<-function(DPlot,group_levels,sub_levels,lab_Gnote,subnote,Vnote){
  #第一维度变量的分组数
  g_tally=tally(group_by(DPlot,vgroup))
  Cnt_group=length(g_tally$vgroup)
  labG=lab_Gnote
  #第二维度变量的分组数
  sub_tally=tally(group_by(DPlot,vsubgroup))
  Cnt_sub=length(sub_tally$vsubgroup)
  labsub=get(paste0("lab_",subnote))
  #分析变量的legend Title
  labV=get(paste0("lab_",Vnote))
  if(Cnt_sub>Cnt_group & Cnt_sub>10){
    Cnt_group0=Cnt_group
    Cnt_group=Cnt_sub
    Cnt_sub=Cnt_group0
    labG0=labG
    labG=labsub
    labsub=labG0
    group_levels0=group_levels
    group_levels=sub_levels
    sub_levels=group_levels0
    DPlot=rename(rename(DPlot,c(vgroup="vgroup0",vsubgroup="vsubgroup0")),c(vgroup0="vsubgroup",vsubgroup0="vgroup"))
  }
  if(Cnt_group==1){#第一维度变量只有1组
    if (Cnt_sub==1){#第二维度变量只有1组 单线
      plot_out<-N_line1(DPlot,lab_PTN,labV,1000)
    }
    else if (Cnt_sub==2){#第二维度变量有2组 双线
      DPlot=rename(DPlot,c(vgroup="Notuse",vsubgroup="vgroup"))
      if(max(sub_tally$n/sum(sub_tally$n))>0.6){#分组分布不均为
        plot_out<-P_Cline2(DPlot,sub_levels,lab_PTP,labV,labsub)
      }
      else{
        plot_out<-N_line2(DPlot,sub_levels,lab_PTN,labV,labsub,1000)
      }
    }
    else{#第二维度变量3组以上，box
      DPlot=rename(DPlot,c(vgroup="Notuse",vsubgroup="vgroup"))
      plot_out<-BOXPLOT1(DPlot,1,sub_levels,labV,labsub)
    }
  }
  else if (Cnt_sub==1){#第二维度变量1组
    if(Cnt_group==2 ){#第一维度1组，双线
      if(max(g_tally$n/sum(g_tally$n))>0.6){#分组分布不均匀
        plot_out<-P_Cline2(DPlot,group_levels,lab_PTP,labV,labsub)
      }
      else{
        plot_out<-N_line2(DPlot,group_levels,lab_PTN,labV,labG,1000)
      }
    }
    else {#第一维度变量3组以上，box
      print("test")
      plot_out<-BOXPLOT1(DPlot,1,group_levels,labV,labG)
    }
  }
  else if (Cnt_group*Cnt_sub<41){#分组box
    colornow=get(paste0("colour",Cnt_sub))
    plot_out<-BOXPLOT2(DPlot,group_levels,sub_levels,colornow,labV,labG,labsub)
  }
  else {#Cnt_group*Cnt_sub>=41 Median\Mean 线图
    plot_out<-ClinePLOT1(DPlot,group_levels,sub_levels,labV,labG,labsub)
  }
  return(plot_out)
}
#3维1个连续变量
D31Stat<-function(DPlot,group_levels,vfact_levels,vsub_levels,Gnote,Fnote,Snote,Vnote){
  #第一维度变量的分组数
  Cnt_group=length(tally(group_by(DPlot,vgroup))$vgroup)
  labG=get(paste0("lab_",Gnote))
  #第二维度变量的分组数
  Cnt_fact=length(tally(group_by(DPlot,vfact))$vfact)
  labfact=get(paste0("lab_",Fnote))
  #第三维度变量的分组数
  Cnt_sub=length(tally(group_by(DPlot,vsubgroup))$vsubgroup)
  labsub=get(paste0("lab_",Snote))
  #分析变量
  labvnote=get(paste0("lab_",Vnote))
  #样式设定：颜色、ylabuse,xlabuse,labuse
  if(Cnt_group==1){#第一维度变量只有1组--降至2维
    DPlot=rename(DPlot,c(vgroup="Notuse",vfact="vgroup"))
    plot_out<-D21Stat(DPlot,vfact_levels,vsub_levels,labfact,Snote,Vnote)
  }
  else if(Cnt_fact==1){#第二维度变量只有1组--降至2维
    plot_out<-D21Stat(DPlot,group_levels,vsub_levels,labG,Snote,Vnote)
  }
  else if (Cnt_sub==1){#第三维度变量只有1组--降至2维
    DPlot=rename(DPlot,c(vsubgroup="Notuse",Vrate="vsubgroup"))
    plot_out<-D21Stat(DPlot,group_levels,vfact_levels,labG,Fnote,Vnote)
  }
  else if (Cnt_group*Cnt_sub<40){#可最多容纳箱子图
    colornow=get(paste0("colour",Cnt_sub))
    plot_out<-BOXPLOT2fact(DPlot,group_levels,vfact_levels,vsub_levels,colornow,labvnote,labG,labsub)
  }
  else{
    plot_out<-ClinePLOTfacet(DPlot,group_levels,vfact_levels,vsub_levels,labG,labfact,labsub)  
  }
  return(plot_out)
}
