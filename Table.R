
#a=TableRate3(RiskCKD,"YEAR","RSDENCE","RiskCKD","CKD",1)
#GV-表头,FactV+SubV行, Vnote需计算的比例
TableRate3=function(Tdta,GV,FactV,SubV,Vnote,V_Frate,ALL=FALSE,Pval=FALSE){
  Tdta=data.frame(with(Tdta,mget(c(paste0(GV),paste0(FactV),paste0(SubV),paste0(Vnote)))))
  names(Tdta)=c("vgroup","vfact","vsubgroup","Vrate") 
  
  Tdta=filter(Tdta,!(is.na(vgroup)|is.na(vfact)|is.na(vsubgroup)|is.na(Vrate)))
  #第一维度变量的分组数
  Gorder=tally(group_by(Tdta,vgroup))$vgroup
  Cnt_group=length(Gorder)
  G0_levels=get(paste0(GV,"_levels"))
  group_levels=G0_levels[Gorder]
  labG=get(paste0("lab_",GV))

  #第二维度变量的分组数
  factorder=tally(group_by(Tdta,vfact))$vfact

  Cnt_fact=length(factorder)
  vfact_levels=get(paste0(FactV,"_levels"))[factorder]
  labfact=get(paste0("lab_",FactV))

  #第三维度变量的分组数
  suborder=tally(group_by(Tdta,vsubgroup))$vsubgroup
  Cnt_sub=length(suborder)
  vsub_levels=get(paste0(SubV,"_levels"))[suborder]
  labsub=get(paste0("lab_",SubV))
    
  #分析变量
  VRorder=tally(group_by(Tdta,Vrate))$Vrate
  Cnt_VR=length(VRorder)
  Vrate_levels=get(paste0(Vnote,"_levels"))[VRorder]
  labVrate=get(paste0("lab_",Vnote))
  #表头
   if(!is.na(V_Frate)){
  TMP0=tabfreq(x =Tdta$vgroup,y=Tdta$Vrate,ylevels=Vrate_levels,yname="HQMS",xlevels=group_levels,
                  p.include=FALSE,n.headings=FALSE,overall.column=ALL,compress=TRUE,compress.val=V_Frate)

  TMP0=as.data.frame(TMP0)
  TMP0[2:(Cnt_group+1)]="--"
  }
for(i in 1:Cnt_fact){
  factlab=vfact_levels[factorder[i]]
  for(j in 1:Cnt_sub){
    sublab=vsub_levels[suborder[j]]
    TLnow=filter(Tdta,vfact==factorder[i]&vsubgroup==suborder[j])
    if(dim(TLnow)[1]>0){
		#假如某个Gorder缺了需要补“--”
		Gorder1=tally(group_by(TLnow,vgroup))$vgroup 
		VRorder1=tally(group_by(TLnow,Vrate))$Vrate 
		group1_levels=G0_levels[Gorder1]
		#print("test1")
		print(group1_levels)
		if(!is.na(V_Frate)){
			if(V_Frate %in% VRorder1){
				TMP=tabfreq(x =TLnow$vgroup,y=TLnow$Vrate,ylevels=Vrate_levels,yname="HQMS",xlevels=group1_levels,
					p.include=Pval,n.headings=FALSE,overall.column=ALL,compress=TRUE,compress.val=V_Frate)
			TMP=data.frame(col1=factlab,col2=sublab,TMP)
			TMP=TMP[,-3]
			#NameNow=names(TMP)
			if(ALL){NameNow=c("col1","col2","Overall",group1_levels)}
			else{	NameNow=c("col1","col2",group1_levels)}
			names(TMP)=NameNow
			}
			else{ TMP=TMP0 	}	
		}
		else {
			TMP=tabfreq(x =TLnow$vgroup,y=TLnow$Vrate,ylevels=Vrate_levels,yname="HQMS",xlevels=group1_levels,
                  p.include=Pval,n.headings=FALSE,overall.column=ALL)
			TMP=data.frame(col1=factlab,col2=sublab,TMP)[-1,]
			if(ALL){	names(TMP)=c("col1","col2","Variable","Overall",group1_levels)}
			else{	names(TMP)=c("col1","col2","Variable",group1_levels)}
		}
		Cnt_group1=length(Gorder1)
##########	
		#print(TMP)
		#print(c(i,j,Cnt_group1,Cnt_group))

	#如有缺失分组，补充列“--”
		if(Cnt_group1<Cnt_group & (V_Frate & V_Frate %in% VRorder1)){
			colAddN=Cnt_group-Cnt_group1
			colNowN=dim(TMP)[2]
			colAll=colNowN+colAddN
			name1=names(TMP)
			TMP[,c((colNowN+1):colAll)]="--"
			NameNow=c(name1,paste0(G0_levels[Gorder[!(Gorder %in% Gorder1)]]))
			names(TMP)=NameNow
			#列顺序按照原始顺序
			if(i==1&j==1){
			colMarkN=colAll-Cnt_group
			ReOdrCol=c(1:Cnt_group)
			for(k in 1:Cnt_group){	
			print("test");	 print(NameNow) ; print(group_levels)
			ReOdrCol[k]=which(NameNow==group_levels[k])}
			TMP=TMP[,c(1:colMarkN,ReOdrCol)]
			}
		}
		if(i==1&j==1){		TLout=TMP}
		else{     TLout=rbind(TLout,TMP)}
    }}}
  fontSnow=FontCount(GV)
  nameNow=names(TLout)
  if(!is.na(V_Frate)){ 
    nameNow[1:2]=c(labfact,labsub)
    names(TLout)=nameNow
   # print(TLout)
    TLout=Tmplt(TLout,2,fontSize=fontSnow)
    }
  else{ 
    nameNow[1:3]=c(labfact,labsub,fontSize=labVrate)
    names(TLout)=nameNow
    TLout=Tmplt(TLout,3,fontSnow)
    }
  
  return(TLout)
}

#TableAdd(TableRate2(Dnow,"YEAR",Vnow,PTnow,V_Frate=1))
#GV-表头,SubV行, Vnote需计算的比例
TableRate2=function(Tdta,GV,SubV,Vnote,V_Frate=NA,ALL=FALSE,Template=TRUE){
  Tdta=data.frame(with(Tdta,mget(c(paste0(GV),paste0(SubV),paste0(Vnote)))))
  names(Tdta)=c("vgroup","vsubgroup","Vrate") 
  Tdta=filter(Tdta,!(is.na(vgroup)|is.na(vsubgroup)|is.na(Vrate)))
  #第一维度变量的分组数
  Gorder=tally(group_by(Tdta,vgroup))$vgroup
  Cnt_group=length(Gorder)
  group_levels=get(paste0(GV,"_levels"))[Gorder]
  labG=get(paste0("lab_",GV))
  G0_levels=get(paste0(GV,"_levels"))
  #第三维度变量的分组数
  suborder=tally(group_by(Tdta,vsubgroup))$vsubgroup
  Cnt_sub=length(suborder)
  vsub_levels=get(paste0(SubV,"_levels"))
  labsub=get(paste0("lab_",SubV))
  
  #分析变量
  VRorder=tally(group_by(Tdta,Vrate))$Vrate
  Cnt_VR=length(VRorder)
  V0_levels=get(paste0(Vnote,"_levels"))
  Vrate_levels=V0_levels[VRorder]
  labVrate=get(paste0("lab_",Vnote))

  #表头
   if(!is.na(V_Frate)){
  TMP0=tabfreq(x =Tdta$vgroup,y=Tdta$Vrate,ylevels=Vrate_levels,yname="HQMS",xlevels=group_levels,
                  p.include=FALSE,n.headings=FALSE,overall.column=ALL,compress=TRUE,compress.val=V_Frate)

  TMP0=as.data.frame(TMP0)
   # print(TMP0) 
  }
    for(j in 1:Cnt_sub){
      sublab=vsub_levels[suborder[j]]
      TLnow=filter(Tdta,vsubgroup==suborder[j])
      if(dim(TLnow)[1]>0){
        #假如某个Gorder缺了需要补“--”
        Gorder1=tally(group_by(TLnow,vgroup))$vgroup 
        VRorder1=tally(group_by(TLnow,Vrate))$Vrate 
        group1_levels=G0_levels[Gorder1]
        Vrate1_levels=V0_levels[VRorder1]
	  if(!is.na(V_Frate)){
	  if(V_Frate %in% VRorder1){
      TMP=tabfreq(x =TLnow$vgroup,y=TLnow$Vrate,ylevels=Vrate1_levels,yname="HQMS",xlevels=group1_levels,
                  p.include=FALSE,n.headings=FALSE,overall.column=ALL,compress=TRUE,compress.val=V_Frate)
      TMP[,1]=sublab
      TMP=as.data.frame(TMP)
		}
	  else{	
	  print(tally(group_by(TLnow,vgroup,Vrate)))
		TMP=TMP0
		TMP[]="--"
	    TMP[,1]=sublab
		
	  }
      }
      else {
      TMP=tabfreq(x =TLnow$vgroup,y=TLnow$Vrate,ylevels=Vrate1_levels,yname="HQMS",xlevels=group1_levels,
                    p.include=FALSE,n.headings=FALSE,overall.column=ALL) 
      TMP=data.frame(col1=sublab,TMP)[-1,]
	if(ALL){	names(TMP)=c("col1","Variable","Overall",group1_levels)}
		else{	names(TMP)=c("col1","Variable",group1_levels)}
       }
      Cnt_group1=length(Gorder1)
	#如有缺失分组，补充列“--”
      if(Cnt_group1<Cnt_group & ((V_Frate & V_Frate %in% VRorder1)|is.na(V_Frate))){
        colAddN=Cnt_group-Cnt_group1
        colNowN=dim(TMP)[2]
		colAll=colNowN+colAddN
        name1=names(TMP)
        TMP[,c((colNowN+1):colAll)]="--"
        NameNow=c(name1,paste0(G0_levels[Gorder[!(Gorder %in% Gorder1)]]))
		names(TMP)=NameNow
	#列顺序按照原始顺序
		if(j==1){
		colMarkN=colAll-Cnt_group
		ReOdrCol=c(1:Cnt_group)
		for(k in 1:Cnt_group){	ReOdrCol[k]=which(NameNow==group_levels[k])}
		print(ReOdrCol)
		TMP=TMP[,c(1:colMarkN,ReOdrCol)]
      }
	}
	print(TMP)
	
      if(j==1){        TLout=TMP      }
      else{
        TLout=rbind(TLout,TMP)
       # TLlabel=rbind(TLlabel,LableTMP)
      }
   }}
  #
  print(head(TLout))
  if(Template){
  fontSnow=FontCount(GV)
  NameNow=names(TLout)

  if(!is.na(V_Frate)){ 
    NameNow[1]=labsub
    names(TLout)=NameNow
    TLout=Tmplt(TLout,1,fontSize=fontSnow)}
     else{ 
         NameNow[1:2]=c(labsub,labVrate)
         names(TLout)=NameNow
         TLout=Tmplt(TLout,2,fontSize=fontSnow)}
}
  return(TLout)
}

##GV-表头,SubV行,SubV/GV
TablePCT1=function(Tdta,GV,SubV,ALL=FALSE,PFrate=FALSE,Frate=1,TRPS=FALSE,Pval=FALSE,Dcmal=1,Template=TRUE){
  Tdta=data.frame(with(Tdta,mget(c(paste0(GV),paste0(SubV)))))
  names(Tdta)=c("vgroup","vsubgroup") 
  
  TLnow=filter(Tdta,!(is.na(vgroup)|is.na(vsubgroup)))
  #print(tally(group_by(TLnow,vgroup,vsubgroup)))
  #第一维度变量的分组数
  Gorder=tally(group_by(TLnow,vgroup))$vgroup
  Cnt_group=length(Gorder)
  group_levels=get(paste0(GV,"_levels"))[Gorder]
  labG=get(paste0("lab_",GV))
  
  #第三维度变量的分组数
  suborder=tally(group_by(TLnow,vsubgroup))$vsubgroup
  Cnt_sub=length(suborder)
  vsub_levels=get(paste0(SubV,"_levels"))[suborder]
  labsub=get(paste0("lab_",SubV))

  TLout=tabfreq(x =TLnow$vgroup,y=TLnow$vsubgroup,ylevels=vsub_levels,yname="HQMS",xlevels=group_levels,
                decimals=Dcmal,p.include=Pval,n.headings=FALSE,overall.column=ALL,compress=PFrate,compress.val=Frate)
 # print(TLout)
  TLout=as.data.frame(TLout)
  if(Pval){#默认不计算Chisq test P 值
  Pvalue=TLout$P[1]
  labG=paste0(labG,",",Pvalue)
  TLout=subset(TLout,select=-c(P))
 # print(TLout)
 # print(labG)
  TLout=rename(TLout,c(Variable=paste0(labsub,",",Pvalue)))
   }
  TLout=rename(TLout,c(Variable=paste0(labsub)))
  if(!PFrate){TLout=TLout[-1,]}
  if(Template){ #默认套用表格样式
  fontSnow=FontCount(GV)
    if(TRPS){ #默认不转置表格
      TLout=as.data.frame(t(TLout))
      if(ALL){TLout$NameCol=c("RowName","Overall",group_levels)}
      else {TLout$NameCol=c("RowName",group_levels)} 
      NameNow=TLout[1,]
      names(TLout)=NameNow
      TLout=TLout[-1,]
      #print(TLout)
      Ncol=dim(TLout)[2]
      TLout=TLout[,c(Ncol,c(1:(Ncol-1)))]
      if(PFrate){ names(TLout)=c(labG,"N（%）")     }
      else{names(TLout)=c(labG,NameNow[1:(Ncol-1)])}
     }
 print(TLout)
  if(Cnt_group==1){
    names(TLout)=c(labsub,"N（%）")
    TLout=Tmplt(TLout,1,fontSnow)
  }
   else{ TLout=Tmplt(TLout,1,fontSnow)}
    }
  return(TLout)
}

#多个1维率
TablePCT1Vec=function(Tdta,GV,SubVec,ALL1=FALSE,V_Frate){
  Tdta1=data.frame(with(Tdta,mget(c(paste0(GV)))))
  names(Tdta1)=c("vgroup") 
  TableVec=filter(tally(group_by(Tdta1,vgroup)),!is.na(vgroup))
  sumN=sum(TableVec$n)
  group_levels=get(paste0(GV,"_levels"))
  if(ALL1){
    TableVec=data.frame(vgroup="N","Overall"=sumN,t(grouporder(TableVec,group_levels)[[1]])) 
    names(TableVec)=c("vgroup","Overall",group_levels)
    }
  else{  
    TableVec=data.frame(vgroup="N",t(grouporder(TableVec,group_levels)[[1]]))
  names(TableVec)=c("vgroup",group_levels)
  }
  TableVec=TableVec[-1,]
  for(i in 1:length(SubVec)){
    #print(TableVec)
    TableVecTmp=TablePCT1(Tdta,GV,SubVec[i],ALL=ALL1,PFrate=TRUE,Frate=V_Frate,Template=FALSE)
    NameNow=names(TableVecTmp)
    NameNow[1]="vgroup"
    names(TableVecTmp)=NameNow
    TableVecTmp[,1]=get(paste0("lab_",SubVec[i]))
    TableVec=rbind(TableVec,TableVecTmp)
  }
  NameNow=names(TableVec)
  NameNow[1]=""
  names(TableVec)=NameNow
  fontSnow=FontCount(GV)
  TableVec=Tmplt(TableVec,1,fontSnow)
  #TLout = setFlexTableWidths( TLout,widths = c(0.8,1,2,2,1) )
  return(TableVec)
}
  
#TablePCT1Vec(Cause,"CAUSE",CMPTlevels)
#1个分层连续变量
##GV-表头,SubV行,SubV/GV
#TableSTAT1(Cause,"CAUSE","AGE_FIN")
TableSTAT1=function(Tdta,GV,Vstat,ALL=FALSE){
  Tdta=data.frame(with(Tdta,mget(c(paste0(GV),paste0(Vstat)))))
  names(Tdta)=c("vgroup","vstat") 
  
  TLnow=filter(Tdta,!(is.na(vgroup)|is.na(vstat)))
  #print(tally(group_by(TLnow,vgroup)))
  #第一维度变量的分组数
  Gorder=tally(group_by(TLnow,vgroup))$vgroup
  Cnt_group=length(Gorder)
  group_levels=get(paste0(GV,"_levels"))[Gorder]
  labG=get(paste0("lab_",GV))
  
  TLout1=tabmeans(x =TLnow$vgroup,y=TLnow$vstat,xlevels=group_levels,yname="Mean(SD)",
                p.include=FALSE,n.headings=FALSE,overall.column=ALL)
  TLout2=tabmedians(x =TLnow$vgroup,y=TLnow$vstat,xlevels=group_levels,yname="Median(Q1-Q3)",decimals=0,
                  p.include=FALSE,n.headings=FALSE,parenth="q1q3",overall.column=ALL)
  TLout=as.data.frame(t(rbind(TLout1,TLout2)))
  TLout$names=row.names(TLout)

  TLout=TLout[-1,]
  TLout=TLout[,c(3,1,2)]
  names(TLout)=c(labG,"Mean(SD)","Median(Q1-Q3)")
  TLout=Tmplt(TLout,1)
  return(TLout)
}


TableSTAT2=function(Tdta,GV,SubV,Vstat,ALL=FALSE,Type="Median"){
  Tdta=data.frame(with(Tdta,mget(c(paste0(GV),paste0(SubV),paste0(Vstat)))))
  names(Tdta)=c("vgroup","vsubgroup","vstat") 
  if(Vstat=="FEE_TOTA"){Dcmal=0}
  else {Dcmal=1}
  Tdta=filter(Tdta,!(is.na(vgroup)|is.na(vsubgroup)|is.na(vstat)))
  #print(tally(group_by(Tdta,vgroup)))

  #第一维度变量的分组数
  Gorder=tally(group_by(Tdta,vgroup))$vgroup
  Cnt_group=length(Gorder)
  group_levels=get(paste0(GV,"_levels"))[Gorder]
  labG=get(paste0("lab_",GV))
  G0_levels=get(paste0(GV,"_levels"))
  
  #第三维度变量的分组数
  suborder=tally(group_by(Tdta,vsubgroup))$vsubgroup
  Cnt_sub=length(suborder)
  vsub_levels=get(paste0(SubV,"_levels"))
  labsub=get(paste0("lab_",SubV))

  for(j in 1:Cnt_sub){
      sublab=vsub_levels[suborder[j]]
      TLnow=filter(Tdta,vsubgroup==suborder[j])
    if(dim(TLnow)[1]>0){
      #假如某个Gorder缺了需要补“--”
	   # print(tally(group_by(TLnow,vgroup)))
		Gorder1=tally(group_by(TLnow,vgroup))$vgroup 
        group1_levels=G0_levels[Gorder1]
      
    if(Type=="Median") {
	 TMP=tabmedians(x =TLnow$vgroup,y=TLnow$vstat,xlevels=group1_levels,yname=sublab,decimals=0,
                    p.include=FALSE,n.headings=FALSE,parenth="q1q3",overall.column=ALL)
					}
	else{
	
	    TMP=tabmeans(x =TLnow$vgroup,y=TLnow$vstat,xlevels=group1_levels,yname=sublab,decimals=Dcmal,
                  p.include=FALSE,n.headings=FALSE,overall.column=ALL)
   }				
    TMP[,1]=sublab
    TMP=as.data.frame(TMP)
 
	Cnt_group1=length(Gorder1)
    if(Cnt_group1<Cnt_group){
        colAddN=Cnt_group-Cnt_group1
        colNowN=dim(TMP)[2]
        name1=names(TMP)
        TMP[,colNowN+c(1:colAddN)]="--"
        names(TMP)=c(name1,paste0(G0_levels[Gorder[!(Gorder %in% Gorder1)]]))
      }
#print(TMP)
    if(j==1){        TLout=TMP      }
      else{      TLout=rbind(TLout,TMP)  	} 
	 }} 
  #TLout=TLout[-1,]
  #TLout=TLout[,c(3,1,2)]
  fontSnow=FontCount(GV)
  Namenow=names(TLout)
  Namenow[1]=labsub
  names(TLout)=Namenow
  TLout=Tmplt(TLout,1,fontSnow)
  return(TLout)
}


TCostFeeM1=function(Tdta,GV,AllNote){
  #提取分析用的变量与数据
  Tdta=data.frame(with(Tdta,mget(c(paste0(GV),"FEE_TOTA","SJDAYS","STATUS"))))
  names(Tdta)=c("vgroup","FEE_TOTA","SJDAYS","STATUS") 
  Tdta1<-filter(Tdta,!is.na(vgroup))
  group_levels=get(paste0(GV,"_levels"))
  
  TdtaAll=Tdta
  TdtaAll$vgroup=length(group_levels)+1
  TLnow=rbind(Tdta1,TdtaAll)
  group_levels=c(group_levels,AllNote)
  TCount=tally(group_by(TLnow,vgroup))

  TLStatus=filter(TLnow,STATUS %in% c(1,2))
  TMlty=tabfreq(x =TLStatus$vgroup,y=TLStatus$STATUS,ylevels=STATUS_levels,yname="院内死亡比例",xlevels=group_levels,
                p.include=FALSE,n.headings=FALSE,overall.column=FALSE,compress=TRUE,compress.val=1)
TMlty[1,1]="院内死亡比例，N (%)"
TN=TMlty
TN[1,1]="N"
for(i in 1:dim(TCount)[1]){TN[1,(i+1)]=paste0(TCount$n[i])}
TMlty=rbind(TN,TMlty)
  TLOS=tabmeans(x =TLnow$vgroup,y=TLnow$SJDAYS,xlevels=group_levels,yname="住院天数",
                p.include=FALSE,n.headings=FALSE,overall.column=FALSE)
 TLout=rbind(TMlty,TLOS)	
  TCOST=tabmeans(x =TLnow$vgroup,y=TLnow$FEE_TOTA,xlevels=group_levels,yname="住院总费用",
                p.include=FALSE,n.headings=FALSE,overall.column=FALSE)
 TLout=as.data.frame(rbind(TLout,TCOST))
  Namenow=names(TLout)
  Namenow[1]=""
  names(TLout)=Namenow
    TLout=Tmplt(TLout,1)
	return(TLout)
}
#Table Template
Tmplt=function(TLin,nameCOl,fontSize=10){
  dim2O=dim(TLin)[2]
  Namenow=names(TLin)
  NoteLine=TLin[1,1]
  #print(NoteLine)
  if(dim2O==2){
    TLin$blankCol=""
    TLin=TLin[,c(3,1,2,3)]
    Namenow=c("",Namenow,"")
  }
   else if (dim2O==3){
    TLin$blankCol=""
    TLin=TLin[,c(4,1,2,3,4)]
    Namenow=c("",Namenow,"")
  }
  else if (dim2O==4){
    TLin$blankCol=""
    TLin=TLin[,c(1,2,3,4,5)]
    Namenow=c(Namenow,"")
  }
  else if (dim2O==5){
    TLin$blankCol=""
    TLin=TLin[,c(1,2,3,4,5,6)]
    Namenow=c(Namenow,"")
  #  print(TLin)
  #  print(dim(TLin))
  }
  names(TLin)=Namenow
  NameLen=nchar(Namenow,type = "bytes")
  print(NameLen)
# print(dim(TLin))
 dim2=dim(TLin)[2]
  widthTmp=rep(1,dim2)
  TLin1=as.data.frame(TLin,stringsAsFactors=FALSE)
  for (i in 1:dim2){
    widthTmp[i]=max(NameLen[i],max(nchar(TLin1[,i],type = "bytes")))
	#first blank col
	widthTmp[i]=ifelse(widthTmp[i]==0,ifelse(i==1,2,ifelse(dim2O<=5,4,ifelse(i==dim2,2,1))),widthTmp[i])
  }
    print(widthTmp)
  widthSum=sum(widthTmp)
  #标注列加宽--
 NotecolID=c(1:nameCOl)+ifelse(dim2O<4,1,0)
 widthTmp[NotecolID]=ifelse(widthTmp[NotecolID]<26,
							widthTmp[NotecolID]*(5**(1/widthTmp[NotecolID])),
							widthTmp[NotecolID]/(1.5-1/widthTmp[NotecolID]))
  #else{widthTmp[1:nameCOl]=widthTmp[1:nameCOl]*(8**(1/dim2O))}
  print(widthTmp)
  # widthTmp=widthTmp/sum(widthTmp)
  if(widthSum<70){fontSize=10}
  else if ( widthSum<80){fontSize=9}
  else if (widthSum<90){fontSize=8}
  else if (widthSum<110){fontSize=7}
  else{fontSize=6}
  print(c(widthSum,widthTmp/sum(widthTmp)*6.8,fontSize))
  MyFTable = FlexTable( data = TLin,
                        header.cell.props = cellProperties( background.color = "#BFBFBF",border.bottom.style="solid"),
                        header.text.props = textProperties( color = "black",font.size = fontSize, font.weight = "bold" ),
                        header.par.props = parProperties(text.align = "right",padding=0),
                        body.text.props = textProperties( font.size = fontSize )
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
  #  print("test1")
  #涉及多个分组是，每个分组加底线分割
  if(nameCOl>1){
    TLVgroup=TLin[,1:2+ifelse(dim2O<4,1,0)]
	print(TLVgroup)
    names(TLVgroup)=c("Vgroup","NotUse")
    AddlineNote=tally(group_by(TLVgroup,Vgroup))
   # 除最后一个外都需要划分界线
	lastNote=TLVgroup[dim(TLVgroup)[1],1]
    print(AddlineNote)
    for(k in 1:(dim(AddlineNote)[1])){
      NoteNow=AddlineNote$Vgroup[k]
	  if(NoteNow!=lastNote){
    Indexmax=max(which(TLin[,1+ifelse(dim2O<4,1,0)]==NoteNow,arr.ind=T))
    MyFTable[Indexmax,1:dim(TLin)[2]]=cellProperties(border.bottom=borderProperties(color="#404040", style="solid", width=1),
                                             border.left=borderNone(),border.right=borderNone(),border.top=borderNone()
                                             )
     }}
  }
  else if( NoteLine %in% c("N","Overall")){
    MyFTable[1,]=cellProperties(border.bottom=borderProperties(color="#404040", style="solid", width=1),
                                border.left=borderNone(),border.right=borderNone(),border.top=borderNone()
    )
  }
  #if(widthSum<=80){}
MyFTable=setFlexTableWidths( MyFTable,widths = c(widthTmp/sum(widthTmp)*6.8))
 MyFTable[,1:dim(TLin)[2]] = parProperties(text.align = "right")
  return(MyFTable)

}




