##图大小设置
FigW0=6;FigW1=6.6;mapW=6.6
FigH0=2.8;FigH1=3.3;FigH2=3.6;FigH3=4.2;mapH=6.2
ProExclud=c(26,30,32,33,34)
boxMdnPoi="图中红色点为平均值。"
MapExcNote=function(i,blankLine=FALSE){NoteAdd(i,"西藏、宁夏及港澳台地区不展示",blankLine=blankLine)}
#公用参数
labstatline="统计量"
lab_PT="患者"
lab_Null=""
lab_PTN="患者人次(千)"
lab_PTP="患者比例(%)"
Medicaltravel_levels=c('收治异地患者比例','患者异地就医比例')
PTrsdence<-c("收治患者人次（千）","常住患者人次（千）")
#hqmsadult_levels=c("HQMS")
###################常规分析参数#################################
#民族
ETHNIC_levels=c('汉族','壮族','满族','回族','苗族','维吾尔族','土家族','彝族','蒙古族 ','藏族','侗族','布依族','瑶族','白族','朝鲜族','哈尼族','黎族','哈萨克族','傣族','其他')
lab_ETHNIC="民族"
#病理#切口愈合等级
lab_incision="切口愈合等级"
incision_levels<-c("I/甲","其他","0类切口")
#职业
OCCUP_levels=c('国家公务员','专业技术人员','职员','企业管理人员','工人','农民','学生','现役军人','自由职业者','个体经营者','无业人员',
               '退（离）休人员','其他')
lab_OCCUP="职业"
#婚姻状态##感染##压疮##输血反应
#医院所在省份#常住省份
maplabel<-read.csv("C:/Users/LONGJIANYAN/Desktop/DiseaseAnalysis/Map/MapIDV2.csv", header = TRUE, sep = ",", quote="\"", dec=".",fill = TRUE)
pro_levels=c('北京','天津','河北','山西','内蒙古','辽宁','吉林','黑龙江','上海','江苏','浙江','安徽','福建','江西','山东','河南','湖北','湖南','广东','广西','海南','重庆','四川','贵州','云南','西藏','陕西','甘肃','青海','宁夏','新疆','台湾','香港','澳门')
PRO_ID_levels=pro_levels; lab_PRO_ID="就诊地"
FROM_PRO_levels=pro_levels; lab_FROM_PRO="常住省（直辖市）"
##常住城市
#异地就医
TRAVEL_levels=c('是','否');lab_TRAVEL="异地就医"
TreatNote=c( "All","本地")
lab_TreatNote="诊治的患者"
#医院等级
LEVEL_levels=c('甲等','乙等','其他'); lab_LEVEL="医院等级"
#医院性质
#再住院
#因同一种病再住院
#非预期重返
#非预期重返间隔
#性别
GENDER_levels=c('男','女','未知');lab_GENDER="性别"
###Levels特异定义年龄
lab_AGE_FIN="年龄"
lab_ageg="年龄"
#/*出入院日期年月；入院季节、入院月份、工作日入院*/
#lab_adsTime="入院时间"
season_levels=c('春','夏','秋','冬'); lab_season="入院季节"
ADSM_levels=c('1月','2月','3月','4月','5月','6月','7月','8月','9月','10月','11月','12月')
lab_ADSM="入院月份"
Vacation_levels<-c('非工作日入院','工作日入院')
lab_Vacation<-"入院时间"
#/*入院途径*/
ADMST_levels=c("急诊","门诊","其他医疗机构转入","其他"); lab_ADMST="入院途径"
###Levels特异定义/*入院科别*//*出院科别*/
#dischDp_levels=department_levels;#admsDp_levels=department_levels
lab_admsDp="入院科别"
lab_dischDp="出院科别"
lab_dischDp="出院科别"
lab_ASubDp="内科亚科"
#/*院内死亡*/
STATUS_levels=c('是','否') ;lab_STATUS="院内死亡"
lab_MRTY="院内死亡"
#离院方式
DSCWAY_levels=c("医嘱离院","医嘱转院","医嘱转社区/卫生院","非医嘱离院","死亡","其他"); lab_DSCWAY="离院方式"
#/*患者来源-城乡*/
RSDENCE_levels=c('城市','农村');lab_RSDENCE="城乡"
#/*p1医疗付款方式*/
#INSURANC_levels=c("城镇职工医保","城镇居民医保","新农合","贫困救助","商业医疗保险","全公费","全自费","其他社会保险","其他")
INSURANC_levels=c("职工医保","居民医保","新农合","贫困救助","商业保险","全公费","全自费","其他保险","其他")
lab_INSURANC="付款方式"
#/*住院天数
lab_SJDAYS="住院天数"
#住院总费用,手术费,操作费,诊断类,用药费,护理费,耗材费,其他费用,西药类（抗菌药物）,中药类,细胞因子费用,自费*/
lab_FEE_TOTA="住院总费用(千元)"
#/*是否曾入住ICU、#/*ICU住院天数*/
ICU_levels<-c("有","无"); lab_ICU="重症监护"

####################医疗服务评估###############################
NERehos_levels=c("当天","1-15天","16-31天")
lab_NERehos="31天非预期再住院"
lab_NOTCURE="未救治"
NOTCURE_levels=c("是","否")
REHOS_levels=c("有","无")
lab_REHOS="31天非预期再住院"
MEDANTI_levels=c("有","无")
lab_MEDANTI="使用抗菌药物"
lab_AVEN1="月均病例数"
lab_NOTADVC="非医嘱离院"
NOTADVC_levels=c("是","否")
FIRSTREC_levels=c("首次住院","再住院","未知")
lab_FIRSTREC="当年住院"
#AVENG_levels=c(paste0(c(0:25)*50+1,"-",c(1:26)*50))
lab_AVENG="月均住院人次"
TRAVELT_levels=c("异地患者","本地患者")
lab_TRAVELT="诊治异地患者"
#######################################################################

pro_note_levels=c('收治','输出')
#Medicaltraval_label=paste0('异地就医比例,',round(Medicaltraval_Rate*100,1),"%")
lab_travelnote="异地就医人群"
ylab_travelnote="占异地就医患者比例"

lab_hoslevel='医院等级'
HOSPT_levels=c('1-100','101-200','201-300','301-400','401-500','501-600','601-700','701-800','801-900','901+')
lab_HOSPT="医院平均每月收治患者人数"



diagmain_levels<-c("主要诊断","次要诊断")
fee_levels<-c('用药费','诊断类','手术费','操作费','护理费','耗材费','其他',"总费用")
med_levels<-c('西药类(非抗菌药物)','西药类(抗菌药物)','中药类')
feeown_levels<-c('自费','非自费')
feecel_levels=c("细胞因子费用","其他")
readmis_levels=c("再次住院","首次住院","当天再住院","未知住院间隔")
admistimes_levels=c("2014年住院次数","2014年因同一种病住院次数")
day_period_levels=c("0天","1-15天","15-31天")
period_levels=paste0("≤",c(0:12),"个月")
period_levels[1]="1-15天"
lab_period=lab_readmisPeriod
nsm_infection_levels<-c('有','无')


lab_Adverse<-"不良事件"


lab_case="病例分型"
lab_rehos="31天非预期重返"
lab_rehosR="31天非预期重返率比例"
lab_day_period="非预期重返间隔"
lab_ICU="重症监护"
#ylab_status="院内死亡 (%)"

lab_martality0="院内死亡"
lab_feeown="自费比例"
lab_feecel="细胞因子费用比例"
lab_fee="住院费用(千元)"
xlab_MeanLOS="平均住院天数(天)"
lab_RenalF="肾衰患者比例"
ylab_sgry="手术患者比例"
lab_status="院内死亡"


#疾病库
library(sas7bdat)
icdR=read.sas7bdat(file="D:/HQMS/常用数据库/icdR.sas7bdat")
head(icdR)
#icdR=filter(icdR,ICD_focus==1)
library(Hmisc)
ICD=data.frame(maindiag=capitalize(tolower(icdR$icd4)),icd_disease=icdR$icd_disease,name_disease=icdR$name_disease,ICD_focus=icdR$ICD_focus,Death_focus=icdR$Death_focus)
head(ICD)

Intruction_Ch1=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0('本章从',PTuse,"在特定风险人群的比例切入，以期从整体的角度理解",PTuse,"患者的疾病负担。"), stylename = 'a')
  doc = addParagraph(doc,paste0('具体的，首先讨论',PTuse,"患者在特定风险人群中的占比，然后对医院所在地、患者所在地、性别、年龄分组、城乡来源、民族、入院季节等进行亚组分析。"), stylename = 'a')
  doc = addParagraph(doc, "", stylename = "a")
}
Intruction_Ch2=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0('本章内容分为两个部分，一是患者构成，包括不同地区整体',PTuse,"在HQMS成人患者中的占比。然后是各亚组的",PTuse,"占整体",PTuse,"的比例。"), stylename = 'a')
  doc = addParagraph(doc,paste0('二是体',PTuse,"的人口学特征，就急性冠脉综合征患者的性别、年龄、城乡来源、职业等分布展开讨论。"), stylename = 'a')
  doc = addParagraph(doc, "", stylename = "a")
}
Intruction_Ch3=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0("本章从",PTuse,"的就诊情况切入。"), stylename = 'a')
  doc = addParagraph(doc,paste0("首先讨论患者异地就医情况。从各地（省/直辖市）收治异地患者占收治患者的比例、各地患者异地就医的比例，两个角度进行分析。"), stylename = 'a')
  doc = addParagraph(doc,paste0("其次，讨论各医院月均收治急性冠脉综合征患者人次。"), stylename = 'a')
  doc = addParagraph(doc,paste0("最后，就急性冠脉综合征患者的出入院情况，包括入院途径、出入院科别、离院方式、以及是否曾入重症监护室等展开讨论。"), stylename = 'a')
  doc = addParagraph(doc, "", stylename = "a")
}
Intruction_Ch41=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0('本章讨论',PTuse,"并发疾病的情况。"), stylename = 'a')
  doc = addParagraph(doc, "", stylename = "a")
}
Intruction_Ch42=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0('本章讨论',PTuse,"合并其他疾病的情况。"), stylename = 'a')
  doc = addParagraph(doc, "", stylename = "a")
}
Intruction_Ch51=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0('本章讨论',PTuse,"接受治疗情况。首先展示整体患者接受治疗的比例，然后对医院所在地、性别、年龄分组、城乡来源、职业、付款方式、入院季节、医院等级等进行亚组分析。"), stylename = 'a')
  doc = addParagraph(doc, "", stylename = "a")
}
Intruction_Ch52=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0('本章就',PTuse,"治疗后发生的并发症情况进行展示。"), stylename = 'a')
  doc = addParagraph(doc, "", stylename = "a")
}
Intruction_Ch6=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0("本章就",PTuse,"院内死亡情况展开讨论。"), stylename = 'a')
  doc = addParagraph(doc,paste0("首先讨论",PTuse,"院内死亡的比例。除整体、合并疾病、接受治疗的患者院内死亡比例外，还按医院所在地、性别、年龄分组、入院科别、入院季节等进行亚组分析。"), stylename = 'a')
  doc = addParagraph(doc,paste0("再者探讨",PTuse,"的诊断与院内死亡的关系。一是从诊断依据（主要诊断/其他诊断)出发，讨论了不同诊断依据的患者发生院内死亡比例，和发生院内死亡的患者的诊断依据分布。二则关注发生院内死亡的患者的主要诊断。"), stylename = 'a')
  doc = addParagraph(doc, "", stylename = "a")
}
Intruction_Ch7=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0("本章就",PTuse,"的住院天数展开讨论。并按合并疾病、治疗、医院所在地、性别、年龄分组、职业、入院季节、付款方式等进行亚组分析。"), stylename = 'a')
  doc = addParagraph(doc,paste0(" 此外，还对曾受重症监护的患者与不曾受监护的",PTuse,"的住院天数进行比较。"), stylename = 'a')
  doc = addParagraph(doc, "", stylename = "a")
}
Intruction_Ch8=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0("本章就",PTuse,"的住院费展开讨论。"), stylename = 'a')
  doc = addParagraph(doc,paste0("首先讨论",PTuse,"的付款方式、自费情况、费用的构成。"), stylename = 'a')
  doc = addParagraph(doc,paste0("然后是",PTuse,"住院总费用。并按合并疾病、治疗、医院所在地、性别、年龄分组、职业、入院季节、付款方式等进行亚组分析。"), stylename = 'a')
  doc = addParagraph(doc, "", stylename = "a")
}
Intruction_Ch9=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0("本章讨论",PTuse,"再住院情况。"), stylename = 'a')
  doc = addParagraph(doc,paste0("因同一种病再住院，指的是患者再住院的主要诊断亚目(ICD编码前四位)与上一次住院相同。本章讨论的再住院，特指因同一种病再住院。"), stylename = 'a')
  
  doc = addParagraph(doc, "", stylename = "a")
}
Intruction_ChI=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0("本章讨论",PTuse,"发生院内感染的情况。"), stylename = 'a')
  doc = addParagraph(doc, "", stylename = "a")
}
Intruction_ChI1=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0("异地就医患者，指的是常住地（省或直辖市）与就诊医院所在地（省或直辖市）不同的患者。"), stylename = 'a')
  doc = addParagraph(doc,paste0("本章讨论的异地就医，一是以异地就医的",PTuse,"为主体，分析内容有三个："), stylename = 'a')
  doc = addParagraph(doc,paste0("1. 各地收治收治、输出的异地就医患者占异地就医患者的比例；"), stylename = 'a')
  doc = addParagraph(doc,paste0("2. 收治异地患者最多的省份（直辖市）患者来院，与输出患者最多的省份（直辖市）患者去向；"), stylename = 'a')
  doc = addParagraph(doc,paste0("3. 所有异地患者的来源与去向。"), stylename = 'a')
  doc = addParagraph(doc,paste0("二是以某个亚组的患者为主体（如接受男性患者），展示该组患者异地就医的比例。展开的维度包括治疗、性别、年龄、职业、入院季节、付款方式。"), stylename = 'a')
  doc = addParagraph(doc, "", stylename = "rRawOutput")
}
Intruction_ChI3=function(){
  doc = addParagraph(doc, "", stylename = "a")
  doc = addParagraph(doc,paste0("本章着重比较周末及法定节假日入院，与工作日入院的",PTuse,"。"), stylename = 'a')
}

Intruction_ChI4=function(){
  doc = addParagraph(doc,paste0("是否有完善病理诊断患者的差异；"), stylename = 'a')
}
Intruction_ChI5=function(){
  doc = addParagraph(doc,paste0("不同TNM分期患者的差异。"), stylename = 'a')
}