
library(ggplot2)
library(dplyr)
library(grid)
library(scales)
library(plyr)


windowsFonts(myFont1=windowsFont("Times New Roman"),myFont2=windowsFont("微软雅黑"),myFont3=windowsFont("Helvetica"))
colour25<-scale_fill_manual(values = c('#9013FE','#00FFC6','#9B9B9B','#990719','#E52451','#458AC9','#7FCDF4','#8BC997','#CCE199','#EA68A2','#F19EC2','#C491BF','#8D80BE','#8D97CB','#FFF799','#FACC89','#458AC9','#F7B37F','#EC6942','#E52114',"#46bcf7","#0283C5","#F0457D","#c8ec9b","#70BC9E","#9ECA94"))
colour21<-scale_fill_manual(values = c('#9013FE','#00FFC6','#9B9B9B','#990719','#E52451','#458AC9','#7FCDF4','#8BC997','#CCE199','#EA68A2','#F19EC2','#C491BF','#8D80BE','#8D97CB','#FFF799','#FACC89','#458AC9','#F7B37F','#EC6942','#E52114',"#46bcf7"))

colour20<-scale_fill_manual(values = c('#9013FE','#00FFC6','#9B9B9B','#990719','#E52451','#458AC9','#7FCDF4','#8BC997','#CCE199','#EA68A2','#F19EC2','#C491BF','#8D80BE','#8D97CB','#FFF799','#FACC89','#458AC9','#F7B37F','#EC6942','#E52114'))
colour19<-scale_fill_manual(values = c('#9013FE','#00FFC6','#9B9B9B','#990719','#E52451','#F19EC2','#7FCDF4','#8BC997','#CCE199','#EA68A2','#E52114','#C491BF','#8D80BE','#8D97CB','#FFF799','#FACC89','#458AC9','#F7B37F','#EC6942'))
colour13<-scale_fill_manual(values = c("#0E7EB8","#29A8E1", "#08913B","#8FC320","#F8B84B","#E95412","#9D020A","#E5008F","#c8ec9b","#C8D4DC","#0283C5","#F0457D","#2d71e1"))
#colour11<-scale_fill_manual(values = c("#0E7EB8","#29A8E1", "#08913B","#8FC320","#F8B84B","#E95412","#9D020A","#E5008F","#c8ec9b","#C8D4DC","#2d71e1"))
#colour10<-scale_fill_manual(values = c("#0E7EB8","#29A8E1", "#08913B","#8FC320","#F8B84B","#E95412","#9D020A","#E5008F","#c8ec9b","#C8D4DC"))
#colour9<-scale_fill_manual(values = c("#29A8E1","#c8ec9b", "#F0457D","#08913B","#F8B84B","#E95412","#9D020A","#E5008F","#C8D4DC"))
colour6<-scale_fill_manual(values = c("#0283C5","#F0457D","#c8ec9b","#70BC9E","#9ECA94","#C8D4DC"))
colour5<-scale_fill_manual(values = c("#0283C5","#F0457D","#c8ec9b","#70BC9E","#9ECA94"))
colour4<-scale_fill_manual(values = c("#2d71e1","#46bcf7","#f8e45b","#aedc88"))
#colour4l<-scale_fill_manual(values = c("#c468e7","#17a0e5","#66c49f","#f8e45b"))
#colour3<-scale_fill_manual(values = c("#2d71e1","#46bcf7","#aedc88"))
colour3<-scale_fill_manual(values = c("#67ccff","#f0457d","#c7fb8b"))
colour2<-scale_fill_manual(values = c("#46bcf7","#aedc88"))
colour1<-scale_fill_manual(values = c("#46bcf7"))
#colour2<-scale_fill_manual(values = c("#a58fc3","#4dafe5"))
colour7=colour13
colour8=colour13
colour9=colour13
colour10=colour13
colour11=colour13
colour12=colour13
colour14=colour20
colour15=colour20
colour16=colour20
colour17=colour20
colour18=colour20
colour19=colour20
colour22=colour25
colour23=colour25
colour24=colour25
#map_green<-c('6'='#006834','5'='#009543','4'='#00AB8D','3'='#00A968','2'='#44B034','1'='#8EC31E')
#map_blue<-c('6'='#085298','5'='#0070B4','4'='#268FCF','3'='#48B1DF','2'='#A0CAEC','1'='#AADCBD')
map_green<-c('6'='#0fa473','5'='#00d0c8','4'='#00d0c8','3'='#12d1a4','2'='#86d476','1'='#bfe154')
map_blue<-c('6'='#0d76c2','5'='#0099de','4'='#5cc4fc','3'='#69daff','2'='#cbf4ff','1'='#c6fde1')
map_red<-c('1'='#D7D5EB','2'='#A8CD47','3'='#FBD331','4'='#F4A877','5'='#D94C7A','6'='#AB2E23')

map7<-c('1'='#D7D5EB','2'='#69DAFF','3'='#A8CD47','4'='#FBD331','5'='#F4A877','6'='#D94C7A','7'='#AB2E23')



c19=c('#9013FE','#00FFC6','#9B9B9B','#990719','#E52451','#F19EC2','#7FCDF4','#8BC997','#CCE199','#EA68A2','#E52114','#C491BF','#8D80BE','#8D97CB','#FFF799','#FACC89','#458AC9','#F7B37F','#EC6942')
c10= c("#0E7EB8","#29A8E1", "#08913B","#8FC320","#F8B84B","#E95412","#9D020A","#E5008F","#c8ec9b","#C8D4DC")
c5<-c("#0283C5","#F0457D","#c8ec9b","#70BC9E","#9ECA94")
c3<-c("#67ccff","#f0457d","#c7fb8b")

#xx<-PT_Rsdence2_pie(PT,chart_legend,colour2_new)



colour3<-scale_fill_manual(values = c("#c8ec9b","#F0457D","#0283C5"))
color_main<-"#46BCF7"
  #"#75BDA0"

##坐标设定
#scale_y_continuous(labels =percent)
#library(ggthemes)
chart_x270<-theme_minimal()+theme_update(
  strip.text=element_text(family="myFont2", size=8),
  axis.line = element_line(size = 0.3, colour = "darkgrey", linetype = 1),
  axis.ticks = element_line(size = 0.3, colour = "darkgrey", linetype = 1),
  axis.line.x = element_line(size = 0.3, colour = "darkgrey", linetype =1),
  axis.ticks.x = element_line(size = 0.3, colour = "darkgrey", linetype = 1),
  axis.text.x=element_text(family="myFont2",angle=270,hjust=0,vjust=0,size=7, colour="black"),
  axis.title.x=element_text(family="myFont2", size=8,face="bold"),
  axis.line.y = element_line(size = 0.3, colour = "darkgrey", linetype =1),
  axis.ticks.y = element_line(size = 0.3, colour = "darkgrey", linetype = 1),
  axis.text.y=element_text(family="myFont2",size=7,colour="black"),
  axis.title.y=element_text(family="myFont2", size=8, angle=90,colour="black",face="bold"),
  legend.text = element_text(family="myFont2",size=7),
  legend.title=element_text(family="myFont2", size=8),
  legend.key.width=unit(0.1,"cm"),
  legend.key.height=unit(0.4,"cm"),
  #legend.title=element_blank(),
  panel.background = element_blank(),
  legend.background = element_blank())
  #strip.background =element_blank())
#检查样式是否已生效：x轴文字竖向，图例宽度为0.1CM
ggplot(mpg, aes(manufacturer, fill=class))+ geom_bar()+chart_x270

chart_legend<-theme_minimal()+theme_update(
  strip.text=element_text(family="myFont2", size=8) , 
  axis.line= element_line(size = 0.3, colour = "darkgrey", linetype = 1),
  axis.ticks = element_line(size = 0.3, colour = "darkgrey", linetype = 1),
  axis.text.x=element_text(family="myFont2",angle=0,hjust=0.5,vjust=0,size=7, colour="black"),
  axis.title.x=element_text(family="myFont2", size=8,colour="black",face="bold"),
  axis.text.y=element_text(family="myFont2",size=7,colour="black"),
  axis.title.y=element_text(family="myFont2", size=8, angle=90,colour="black",face="bold"),
  legend.text = element_text(family="myFont2",size=7),
  legend.key.width=unit(0.1,"cm"),
  legend.key.height=unit(0.4,"cm"),
  legend.title=element_text(family="myFont2", size=8),
  panel.background = element_blank(),
  legend.background = element_blank())
 # strip.background =element_blank())
#检查样式是否已生效：x轴文字横向，图例宽度为0.1CM
ggplot(mpg, aes(manufacturer, fill=class))+ geom_bar()+chart_legend


chart_pie<-theme_minimal()+theme_update(
  strip.text=element_text(family="myFont2", size=8) , 
  axis.text.x=element_text(family="myFont2",angle=0,hjust=0.5,vjust=0,size=7, colour="black"),
  axis.title.x=element_text(family="myFont2", size=8,colour="black",face="bold"),
  axis.text.y=element_text(family="myFont2",size=7,colour="black"),
  axis.title.y=element_text(family="myFont2", size=8, angle=90,colour="black",face="bold"),
  legend.text = element_text(family="myFont2",size=7),
  legend.key.width=unit(0.1,"cm"),
  legend.key.height=unit(0.4,"cm"),
  legend.title=element_text(family="myFont2", size=8),
  axis.ticks = element_blank(),
  axis.line= element_blank(),
  panel.background = element_blank(),
  legend.background = element_blank())
# strip.background =element_blank())
#检查样式是否已生效：x轴文字横向，图例宽度为0.1CM
ggplot(mpg, aes(manufacturer, fill=class))+ geom_bar()+chart_pie




chart_linex270<-theme_minimal()+theme_update(
  strip.text=element_text(family="myFont2", size=8),
  axis.line = element_line(size = 0.3, colour = "darkgrey", linetype = 1),
  axis.ticks = element_line(size = 0.3, colour = "darkgrey", linetype = 1),
  axis.text.x=element_text(family="myFont2",angle=270,hjust=0,vjust=0,size=7, colour="black"),
  axis.title.x=element_text(family="myFont2", size=8,colour="black",face="bold"),
  axis.text.y=element_text(family="myFont2",size=7,colour="black"),
  axis.title.y=element_text(family="myFont2", size=8, angle=90,colour="black",face="bold"),
  legend.text = element_text(family="myFont2",size=7),
  legend.key.width=unit(0.5,"cm"),
  legend.key.height=unit(0.4,"cm"),
  legend.title=element_text(family="myFont2", size=8),
  panel.background = element_blank(),
  legend.background = element_blank() , 
  axis.line = element_line(size = 0.3, colour = "darkgrey", linetype = 1))
 # strip.background =element_blank())
#检查样式是否已生效：x轴文字竖向，图例宽度为0.5CM
ggplot(mpg, aes(manufacturer, fill=class))+ geom_bar()+chart_linex270

chart_linex<-theme_minimal()+theme_update(
  strip.text=element_text(family="myFont2", size=8),
  axis.line = element_line(size = 0.3, colour = "darkgrey", linetype = 1),
  axis.ticks = element_line(size = 0.3, colour = "darkgrey", linetype = 1),
  axis.text.x=element_text(family="myFont2",angle=0,hjust=0.5,vjust=0,size=7, colour="black"),
  axis.title.x=element_text(family="myFont2", size=8,colour="black",face="bold"),
  axis.text.y=element_text(family="myFont2",size=7,colour="black"),
  axis.title.y=element_text(family="myFont2", size=8, angle=90,colour="black",face="bold"),
  legend.text = element_text(family="myFont2",size=7),
  legend.key.width=unit(0.5,"cm"),
  legend.key.height=unit(0.4,"cm"),
  legend.title=element_text(family="myFont2", size=8),
  panel.background = element_blank(),
  legend.background = element_blank(), 
  axis.line = element_line(size = 0.3, colour = "darkgrey", linetype = 1))
 # strip.background =element_blank())
#检查样式是否已生效：x轴文字横向，图例宽度为0.5CM
ggplot(mpg, aes(manufacturer, fill=class))+ geom_bar()+chart_linex


