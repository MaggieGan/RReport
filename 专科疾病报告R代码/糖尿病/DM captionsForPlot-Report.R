#分析患者
PTuse="糖尿病"
filesave="DM"
reportname="糖尿病2015年度科学报告"

####################风险人群##################
D_RISK=c("RiskDM")
#高血压  HT HQMS
RiskDM_levels<-c('HQMS','HT')
RiskPTuse="患者人群"
lab_RiskDM="患者人群"
DM_levels<-c('DM','Non-DM')
lab_DM="糖尿病"
IGT_levels<-c('IGT','Non-DM')
lab_IGT="糖耐量异常"

####################分析分组###################
DMC_levels=c('DM','IGT')
lab_DMC="糖尿病"

DMG_levels=c('DM1','DM2',"GDM","OtherDM")
lab_DMG="不同类型糖尿病"

####################并发症###################	
#昏迷		Coma  1
#酮症酸中毒		DKA  2
#糖尿病肾病		DN  3
#糖尿病眼病		DE  4
#神经并发症		DNP  5
#糖尿病足		DF   6
#周围循环并发症	DPC 7
#其他并发症		Other 8
cmptid=c("COMA","DKA","DN","DE","DNP","DF","DPC","DA")
cmptid_levels=c("昏迷","酮症酸中毒","糖尿病肾病","糖尿病眼病","神经并发症","糖尿病足","周围循环并发症","糖尿病关节病","其他")
lab_cmptid="并发症"
cmpt_levels=c("有","无")
lab_cmpt="并发症"
COMA_levels=c('昏迷','其他');	lab_COMA='昏迷'
DKA_levels=c('酮症酸中毒','其他');	lab_DKA='酮症酸中毒'
DN_levels=c('糖尿病肾病','其他');	lab_DN='糖尿病肾病'
DE_levels=c('糖尿病眼病','其他');	lab_DE='糖尿病眼病'
DNP_levels=c('神经并发症','其他');	lab_DNP='神经并发症'
DF_levels=c('糖尿病足','其他');	lab_DF='糖尿病足'
DPC_levels=c('周围循环并发症','其他');	lab_DPC='周围循环并发症'
DA_levels=c('糖尿病关节病','其他');	lab_DA='糖尿病关节病'
COMP_OTH_levels=c('其他并发症','其他');	lab_COMP_OTH='其他并发症'

# 合并症
#高血压  1	HT
#肝炎	2	Hepatitis
#感染	3	Infection
#肿瘤	4	cancer
#急性冠脉综合征	5	ACS
#其他冠状动脉性心脏病	6	CIHD
#心力衰竭	7	CHF
#其他心脏病	8	Other HD
#脑卒中	9	Stroke
cmbid=c("HT","HEPATITI","INFECTIO","CANCER","ACS","CIHD","CHF","HD_OTHER","STROKE")
cmbid_levels=c("高血压","肝炎","感染","肿瘤","急性冠脉综合征","急性冠脉综合征","心力衰竭","其他心脏病","卒中")
lab_cmbid="合并症"
cmb_levels=c("有","无")
lab_cmb="合并症"
HT_levels=c("HT",'Other'); lab_HT="高血压"
HEPATITI_levels=c('Hepatitis','Other');	lab_HEPATITI='肝炎'
INFECTIO_levels=c('Infection','Other');	lab_INFECTIO='感染'
CANCER_levels=c('Cancer','Other');	lab_CANCER='肿瘤'
ACS_levels=c('ACS','Other');	lab_ACS='急性冠脉综合征'
CIHD_levels=c('CIHD','Other');	lab_CIHD='其他冠状动脉性心脏病'
CHF_levels=c('CHF','Other');	lab_CHF='心力衰竭'
HD_OTHER_levels=c('HD other','Other');	lab_HD_OTHER='其他心脏病'
STROKE_levels=c('Stroke','Other');	lab_STROKE='脑卒中'


#治疗
#########手术########################
lab_CAG="冠脉造影术"
lab_PCI="经皮冠状动脉介入术"
lab_CABG="冠状动脉旁路移植术"
CAG_levels=c("行CAG","无")
PCI_levels=c("行PCI","无")
CABG_levels=c("行CABG","无")

surgid=c("CAG","PCI","CABG")
surgid_levels=c("CAG","PCI","CABG")
lab_surgid="操作/手术"
surg_levels=c("有","无")
lab_surg="操作/手术"

#############
#特异性分组
ageg_levels=c('18-34','35-44','45-64','65-74','75-84','85+')
agegN=length(ageg_levels)
department_levels=c('内科','外科','中医科','全科医疗科','肿瘤科','妇产科','传染科','重症医学科','急诊医学科','眼科','其他')
admsDp_levels=department_levels
dischDp_levels=department_levels

lab_TreatNote="诊治的患者"
TreatNote_levels=c("All","本地")
##### 医疗服务评估
ORGSPEC_levels=c("综合医院","专科医院")
lab_ORGSPEC="医院类型"
AVENG_levels=c(paste0(c(0:5)*50+1,"-",c(1:6)*50),"301-500","501-1000","1000+")
ASSCM_levels=c("HQMS","DM","IGT")
lab_ASSCM="患者类型"
#####
FCITY_levels=c('北京','天津','石家庄','太原','呼和浩特','沈阳','长春','哈尔滨','上海','南京','杭州','合肥','福州','南昌','济南','郑州','武汉','长沙','广州','南宁','海口','重庆','成都','贵阳','昆明','拉萨','西安','兰州','西宁','银川','乌鲁木齐')
lab_FCITY="省会(常住地)"
YEAR_levels=c('2010','2011','2012','2013','2014','2015')
lab_YEAR="出院年份"
RegionF_levels=c('华北','东北','华东','华中','华南','西南','西北')
lab_RegionF="地理分区"

ADSM_levels=c('1月','2月','3月','4月','5月','6月','7月','8月','9月','10月','11月','12月')
lab_ADSM="入院月份"

CITYMT_levels=c("本市","本省外市","外省")
lab_CITYMT="诊治患者来源"

TCITY_levels=c('北京','天津','石家庄','太原','呼和浩特','沈阳','长春','哈尔滨','上海','南京','杭州','合肥','福州','南昌','济南','郑州','武汉','长沙','广州','南宁','海口','重庆','成都','贵阳','昆明','拉萨','西安','兰州','西宁','银川','乌鲁木齐')
lab_TCITY="省会(就诊地)"


########################### MAP##########################
#就诊省份
mapLabDM_levels=c("<10%","10%-11%","11%-12%","12%-13%","13%-14%","14%-25%")
mapLabDM="2015年诊治患者中DM比例"

mapLabFDM_levels=mapLabDM_levels
mapLabFDM="2015年常住地患者中DM比例"

#
mapLabIGT_levels=c("<0.3%","0.3%-0.4%","0.4%-0.5%","0.5%-0.6%","0.6%-0.8%","0.8%-0.9%")
mapLabFIGT_levels=mapLabSTROKE_levels
mapLabIGT="2015年诊治患者中IGT比例"
mapLabFIGT="2015年诊治患者中IGT比例"

# MAP of DM Mortality
mapLabDMMLTY_levels=c("<0.4%","0.4%-0.6%","0.6%-0.8%","0.8%-1.0%","1.0%-1.4%","1.4%-2.0%")
mapLabDMMLTY1_levels=mapLabDMMLTY_levels
mapLabDMMLTY=paste0(lab_DM,"患者院内死亡率")
mapLabDMMLTY1=paste0("本地",lab_DM,"患者院内死亡率") 
#Map of IGT Mortality
mapLabIGTMLTY_levels=c("0%","<0.1%","0.1%-0.2%","0.2%-0.4%","0.4%-1.0%","1.0%-2.0%")
mapLabIGTMLTY1_levels=mapLabIGTMLTY_levels
mapLabIGTMLTY=paste0(lab_IGT,"患者院内死亡率")
mapLabIGTMLTY1=paste0("本地",lab_IGT,"患者院内死亡率") 

##Accesment
ASSCM_levels=c("HQMS","DM","IGT")

##DMT 分析人群数
load("D:/HQMS/HQMS_20160630/Thyroid/R/THYROIDC.Rdata")
summary(THYROIDC$FEE_TOTA)

total_DMT<-tally(group_by(THYROIDC,THYROIDC))$n
(THYROIDC_DMT=total_DMT)
(PRO_ID_DMT<-rename(tally(group_by(THYROIDC,PRO_ID)),c(PRO_ID="vgroup")))
(FROM_PRO_DMT<-rename(na.omit(tally(group_by(THYROIDC,FROM_PRO)),FROM_PRO),c(FROM_PRO="vgroup")))
#total_season_DMT<-rename(tally(group_by(burn,adsSeason)),c(adsSeason="vgroup"))

##全国比较-异地就医比例
travelTmp=filter(tally(group_by(THYROIDC,TRAVEL)),!is.na(TRAVEL))
(Medicaltravel_Rate<-(travelTmp$n/sum(travelTmp$n))[1])

##全国比较-院内死亡率
StatusTmp=filter(tally(group_by(THYROIDC,STATUS)),!is.na(STATUS))
(MartalityR=(StatusTmp$n/sum(StatusTmp$n))[1])



#患者具体流向--词云
#install.packages("wordcloud")
library(wordcloud)

#install.packages("Hmisc")
library(Hmisc)
#DPlot<-ddply(DPlot,NULL,transform,travel=ifelse(is.na(from_pro),NA,ifelse(pro_id==from_pro,2,1)))
medical_migrate_tmp<-filter(THYRC,TRAVEL==1)
medical_migrate_tmp1<-tally(group_by(medical_migrate_tmp,PRO_ID,FROM_PRO))

map_note1<-data.frame(PRO_ID=maplabel$pro_Nid,NAME=maplabel$province)
map_from<-data.frame(FROM_PRO=maplabel$pro_Nid,NAME_from=maplabel$province)
medical_migrate_tmp2<-left_join(medical_migrate_tmp1,map_note1)
medical_migrate<-left_join(medical_migrate_tmp2,map_from)
medical_migrate$Rate=medical_migrate$n/sum(medical_migrate$n)

medical_migrate$label1=Rate_legend(medical_migrate$Rate)

ggplot(medical_migrate, aes(x=NAME_from,y=NAME))+chart_x270+
  geom_point(aes(color="red",size=Rate),alpha=0.5)+
  scale_size_continuous(range = c(3,12))+
  theme(legend.position="bottom")+ylab(lab_pro1)+xlab(lab_pro2)


medical_migrate$words<-paste0(medical_migrate$NAME_from,"-",medical_migrate$NAME,":",Rate_legend(medical_migrate$Rate))
mycolors <- brewer.pal(8,"Dark2")

tiff(file=paste0(PTuse,"异地就医.tiff"), width=10, height=8, units="in", compression="lzw", res=150)
wordcloud(medical_migrate$words,medical_migrate$n,random.order=FALSE,random.color=FALSE,colors=mycolors)
dev.off() 


