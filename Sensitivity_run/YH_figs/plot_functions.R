plot_contour<-function(x,y,data,var_name,cbar_limit,ii){
    s = expand.grid(x,y) 
    paletteLength <- 50
    myColor <- colorRampPalette(c("blue", "white", "red"))(paletteLength)
    # length(breaks) == length(paletteLength) + 1
    # use floor and ceiling to deal with even/odd length pallettelengths
    data[data<=cbar_limit[1]] = cbar_limit[1]  #make sure min values are also plotted!
    data[data>=cbar_limit[2]] = cbar_limit[2]
#    myBreaks <- c(seq(min(data), 0, length.out=ceiling(paletteLength/2) + 1), 
#                  seq(max(data)/paletteLength, max(data), length.out=floor(paletteLength/2)))
#    myBreaks <- scales::rescale(myBreaks) 
    df = data.frame(s,as.vector(data))
    fs = 18
    colnames(df) = c("vmax","jmax","z")
    v<-ggplot(df,aes(x=vmax,y=jmax,z=z,fill=z))+geom_contour_filled()+ 
       geom_tile()+
       #geom_contour(color = "black",bins = 15)+ 
       #geom_text_contour(stroke = 0.2,min.size = 2)+
       #xlab("vmax")+
       scale_fill_distiller(name=var_name,palette = "Spectral",limits = cbar_limit)+
#       scale_fill_distiller(name=var_name,palette = myColor,limits = cbar_limit)+
#    scale_fill_gradient2(low="blue", mid="white", high="red", midpoint=0,limits = cbar_limit)
#remove grey background
       theme_bw() +
       theme(axis.text   =element_text(size=fs),
             axis.title  =element_text(size=fs,face="bold"),
             legend.text =element_text(size=fs),
             legend.title=element_text(size=fs))
#remove axis labels 
     v <- v + theme(axis.text =element_blank(),
                    axis.ticks=element_blank(),
                    axis.title=element_blank()) 
#only plot legend at one column
#     if(ii!=4){
     v <- v + guides(fill="none", color="none")
#     }
#add a triangle in the center
     v <- v + geom_point(aes(x=1,y=1),shape=24, fill="grey",color="black", size=7)
       scale_fill_distiller(name=var_name,palette = "RdBu",limits = cbar_limit)
     return(v)
}

layers_contour<-function(x,y,var2plot,cbar_limit,pdf_name,no_layers,years){
	var2plot_layers = paste(rep("layer",no_layers),1:no_layers,sep="") 
	plot_list = list()
	for (j in 1:length(var2plot_layers)){
	        varname = var2plot_layers[j]
	        var_diff = var2plot[,,,j] 
		for (i in 1:length(years)){
		    year_i = years[i]
		    fig_i = plot_contour(x,y,var_diff[,,i],varname,cbar_limit)
	            fig_order = i+ (j-1) * length(years)
		    plot_list[[fig_order]] = fig_i 
		}
	}
	pdf(pdf_name,height = 48, width=24)
	grid.arrange(grobs = plot_list,nrow=length(var2plot_layers),ncol=length(years))
	dev.off()
}

barplot<-function(df,pdfname,var_type){
   df <- within(df, CO2 <- factor(CO2,levels=names(sort(table(CO2),decreasing=FALSE))))
   CO2_label = paste(unique(df$CO2),"ppm")
   plot_list = list()
   ynames = colnames(df)[-c(1,2)]#remove year and CO2 
   text_size = 24
 for (i in 1:length(ynames)){
   df_tmp = df[,c(1,2,i+2)]
   print(c(ynames[i],range(df_tmp[,ynames[i]]))) 
   df_with_sd = cbind(df_tmp[1,],sd=NaN) #init the target df 
   for(CO2 in unique(df$CO2)){
      df_tmp2 = df_tmp[df_tmp$CO2==CO2,]
      df_mean = mean(df_tmp2[,3])
      df_sd   = sd(df_tmp2[,3])
      tmp = cbind(df_tmp2[1,c(1,2)],df_mean,df_sd)
      colnames(tmp) = colnames(df_with_sd)
      df_with_sd = rbind(df_with_sd,tmp)
   }
   df_with_sd = df_with_sd[-1,]
   if(var_type==1){ #percentage
     ylimit = c(0.0,10.0)
  #   y_label = expression(paste(Delta,colnames(df_with_sd)[3],"(%)"))
     y_label = bquote(paste(Delta,.(colnames(df_with_sd)[3])," (%)",sep=""))
     if(ynames[i]=="WUE" || ynames[i]=="iWUE"){
        ylimit = c(-5.0,10.0)
     }
   }else if(var_type==2){ #diff-absolute
     ylimit = c(0.0,0.5)
     y_label = bquote(paste(Delta,.(colnames(df_with_sd)[3])," (Mg/ha)",sep=""))
     if(ynames[i]=="LAI"){
       ylimit = c(0,1.0)
       y_label = bquote(paste(Delta,.(colnames(df_with_sd)[3]),sep="")~(m^2/m^2))
     }else if(ynames[i]=='An_DailyMean'|ynames[i]=='An_DailySum'){
       ylimit = c(0,0.5)
       y_label = bquote(Delta*.(colnames(df_with_sd)[3])~(mu*mol~m^-2*s^-1))
#        y_label = expression(paste(colnames(df_with_sd)[3]," (",mu,"mol/",sep=""))
     }else if(ynames[i]=='An_DailyMax'){
       y_label = bquote(Delta*.(colnames(df_with_sd)[3])~(mu*mol~m^-2*s^-1))
       ylimit = c(0,4)
     }else if(ynames[i]=="WUE" || ynames[i]=="iWUE"){
       ##umol CO2 / mmol H2O
#       y_label = bquote(.(colnames(df_with_sd)[3])~(mu*mol~CO[2]/mmol~H[2]*O))
       y_label = bquote(Delta*.(colnames(df_with_sd)[3])~(mu*mol/mmol))
       ylimit = c(-1,1)
     }
   }else if(var_type==3){ #original value
     ylimit = c(0.0,15.0)
     y_label = paste(colnames(df_with_sd)[3]," (Mg/ha)",sep="")
     if(ynames[i]=="LAI"){
       y_label = bquote(paste(.(colnames(df_with_sd)[3]),sep="")~(m^2/m^2))
     }else if(ynames[i]=='An_DailyMean'|ynames[i]=='An_DailySum'){
       ylimit = c(0,10)
       y_label = bquote(.(colnames(df_with_sd)[3])~(mu*mol~m^-2*s^-1))
#        y_label = expression(paste(colnames(df_with_sd)[3]," (",mu,"mol/",sep=""))
     }else if(ynames[i]=='An_DailyMax'){
       y_label = bquote(.(colnames(df_with_sd)[3])~(mu*mol~m^-2*s^-1))
       ylimit = c(0,50)
     }else if(ynames[i]=="WUE" || ynames[i]=="iWUE"){
       ##umol CO2 / mmol H2O
#       y_label = bquote(.(colnames(df_with_sd)[3])~(mu*mol~CO[2]/mmol~H[2]*O))
       y_label = bquote(.(colnames(df_with_sd)[3])~(mu*mol/mmol))
       ylimit = c(0,20)
     }
   } 
#if(ynames[i]=='WUE') print(df_with_sd)
   p <- ggplot(data=df_with_sd, aes_string(x="year", y=ynames[i], fill="CO2")) +
         geom_bar(stat="identity", position=position_dodge())+
         geom_errorbar(aes_string(ymin=paste0(ynames[i],"-sd"), ymax=paste0(ynames[i],"+sd")), width=.2,position=position_dodge(.9))+
         ylab(y_label) + 
#          coord_cartesian(ylim = ylimit,labels = label_number(accuracy = 0.1))+
          scale_y_continuous(limits = ylimit,labels = label_number(accuracy = 0.1))+
#remove grey background 
          theme_bw()+
          theme(axis.text=element_text(size=text_size),
#           axis.title=element_text(size=text_size,face="bold"),
           axis.title=element_text(size=text_size),
           axis.title.x=element_blank(),
           axis.text.x =element_blank(),
           axis.ticks.x=element_blank(),
           legend.position = c(.8, .85), #horizontal, verical
           legend.text     = element_text(size=text_size),
           legend.title    = element_text(size=text_size))
   # Use custom colors
   p <- p + scale_fill_manual(values=c('blue','green','orange','grey'),labels = CO2_label)

   if(i>1) p <- p + guides(fill="none", color="none") #remove legends

   plot_list[[i]] = p
 }
   pdf(pdfname,height = 16, width=24)
   grid.arrange(grobs = plot_list,nrow=2,ncol=3)
   dev.off()
}

barplot_two<-function(df1,df2,pdfname,var_type){
   df1 <- within(df1, CO2 <- factor(CO2,levels=names(sort(table(CO2),decreasing=FALSE))))
   df2 <- within(df2, CO2 <- factor(CO2,levels=names(sort(table(CO2),decreasing=FALSE))))
   CO2_label = paste(unique(df1$CO2),"ppm")
   plot_list = list()
   ynames = colnames(df1)[-c(1,2)]#remove year and CO2 
   text_size = 24
 for (i in 1:length(ynames)){
#get the df_sub1
   df_tmp = df1[,c(1,2,i+2)]
   df_with_sd = cbind(df_tmp[1,],sd=NaN) #init the target df 
   for(CO2 in unique(df1$CO2)){
      df_tmp2 = df_tmp[df_tmp$CO2==CO2,]
      df_mean = mean(df_tmp2[,3])
      df_sd   = sd(df_tmp2[,3])
      tmp = cbind(df_tmp2[1,c(1,2)],df_mean,df_sd)
      colnames(tmp) = colnames(df_with_sd)
      df_with_sd = rbind(df_with_sd,tmp)
   }
   df_with_sd1 = df_with_sd[-1,]
   df_with_sd1 = cbind(df_with_sd1,scenario="CTL")
#get the df_sub2
   df_tmp = df2[,c(1,2,i+2)]
   df_with_sd = cbind(df_tmp[1,],sd=NaN) #init the target df 
   for(CO2 in unique(df2$CO2)){
      df_tmp2 = df_tmp[df_tmp$CO2==CO2,]
      df_mean = mean(df_tmp2[,3])
      df_sd   = sd(df_tmp2[,3])
      tmp = cbind(df_tmp2[1,c(1,2)],df_mean,df_sd)
      colnames(tmp) = colnames(df_with_sd)
      df_with_sd = rbind(df_with_sd,tmp)
   }
   df_with_sd2 = df_with_sd[-1,]
   df_with_sd2 = cbind(df_with_sd2,scenario="EXP")
   df_sd1_sd2 = rbind(df_with_sd1,df_with_sd2)
   if(var_type==1){ #percentage
     ylimit = c(0.0,10.0)
  #   y_label = expression(paste(Delta,colnames(df_with_sd)[3],"(%)"))
     y_label = bquote(paste(Delta,.(colnames(df_with_sd)[3])," (%)",sep=""))
     if(ynames[i]=="WUE" || ynames[i]=="iWUE"){
        ylimit = c(-5.0,10.0)
     }
   }else if(var_type==2){ #diff-absolute
     ylimit = c(0.0,0.5)
     y_label = bquote(paste(Delta,.(colnames(df_with_sd)[3])," (Mg/ha)",sep=""))
     if(ynames[i]=="LAI"){
       ylimit = c(0,1.0)
       y_label = bquote(paste(Delta,.(colnames(df_with_sd)[3]),sep="")~(m^2/m^2))
     }else if(ynames[i]=='An_DailyMean'|ynames[i]=='An_DailySum'){
       ylimit = c(0,0.5)
       y_label = bquote(Delta*.(colnames(df_with_sd)[3])~(mu*mol~m^-2*s^-1))
#        y_label = expression(paste(colnames(df_with_sd)[3]," (",mu,"mol/",sep=""))
     }else if(ynames[i]=='An_DailyMax'){
       y_label = bquote(Delta*.(colnames(df_with_sd)[3])~(mu*mol~m^-2*s^-1))
       ylimit = c(0,4)
     }else if(ynames[i]=="WUE" || ynames[i]=="iWUE"){
       ##umol CO2 / mmol H2O
#       y_label = bquote(.(colnames(df_with_sd)[3])~(mu*mol~CO[2]/mmol~H[2]*O))
       y_label = bquote(Delta*.(colnames(df_with_sd)[3])~(mu*mol/mmol))
       ylimit = c(-1,1)
     }
   }else if(var_type==3){ #original value
     ylimit = c(0.0,15.0)
     y_label = paste(colnames(df_with_sd)[3]," (Mg/ha)",sep="")
     if(ynames[i]=="LAI"){
       y_label = bquote(paste(.(colnames(df_with_sd)[3]),sep="")~(m^2/m^2))
     }else if(ynames[i]=='An_DailyMean'|ynames[i]=='An_DailySum'){
       ylimit = c(0,10)
       y_label = bquote(.(colnames(df_with_sd)[3])~(mu*mol~m^-2*s^-1))
#        y_label = expression(paste(colnames(df_with_sd)[3]," (",mu,"mol/",sep=""))
     }else if(ynames[i]=='An_DailyMax'){
       y_label = bquote(.(colnames(df_with_sd)[3])~(mu*mol~m^-2*s^-1))
       ylimit = c(0,50)
     }else if(ynames[i]=="WUE" || ynames[i]=="iWUE"){
       ##umol CO2 / mmol H2O
#       y_label = bquote(.(colnames(df_with_sd)[3])~(mu*mol~CO[2]/mmol~H[2]*O))
       y_label = bquote(.(colnames(df_with_sd)[3])~(mu*mol/mmol))
       ylimit = c(0,20)
     }
   } 
#if(ynames[i]=='WUE') print(df_with_sd)
   p <- ggplot(data=df_sd1_sd2, aes_string(x="CO2", y=ynames[i], fill="scenario")) +
         geom_bar(stat="identity", position=position_dodge())+
         geom_errorbar(aes_string(ymin=paste0(ynames[i],"-sd"), ymax=paste0(ynames[i],"+sd")), width=.2,position=position_dodge(.9))+
         ylab(y_label) + 
#          coord_cartesian(ylim = ylimit,labels = label_number(accuracy = 0.1))+
          scale_y_continuous(limits = ylimit,labels = label_number(accuracy = 0.1))+
#remove grey background 
          theme_bw()+
          theme(axis.text=element_text(size=text_size),
#           axis.title=element_text(size=text_size,face="bold"),
           axis.title=element_text(size=text_size),
#           axis.title.x=element_blank(),
#           axis.text.x =element_blank(),
#           axis.ticks.x=element_blank(),
           legend.position = c(.8, .85), #horizontal, verical
           legend.text     = element_text(size=text_size),
           legend.title    = element_text(size=text_size))
   # Use custom colors
#   p <- p + scale_fill_manual(values=c('blue','green','orange','grey'),labels = CO2_label)

   if(i>1) p <- p + guides(fill="none", color="none") #remove legends

   plot_list[[i]] = p
 }
   pdf(pdfname,height = 16, width=24)
   grid.arrange(grobs = plot_list,nrow=2,ncol=3)
   dev.off()
}

scatter_plot<-function(CTL,EXP,pdfname){
  df1 <- within(CTL, CO2 <- factor(CO2,levels=names(sort(table(CO2),decreasing=FALSE))))
  df2 <- within(EXP, CO2 <- factor(CO2,levels=names(sort(table(CO2),decreasing=FALSE))))
  CO2_label = paste(unique(df1$CO2),"ppm")
  plot_list = list()
  ynames = colnames(df1)[-c(1,2)]#remove year and CO2 
  text_size = 24
 for (i in 1:length(ynames)){
  df = data.frame(CTL=df1[,ynames[i]],EXP=df2[,ynames[i]],CO2=df1$CO2)
  if(i==1){
    model <- lm(EXP~CTL*CO2-1,data=df[df$CO2==400|df$CO2==1000,])
    print(summary(model))
  }
  ylimit = c(0.0,15.0)
  y_label = paste(ynames[i]," (Mg/ha)",sep="")
  x_label = paste("CTL",y_label)
  y_label = paste("EXP",y_label)
  if(ynames[i]=="LAI"){
       y_label = bquote(EXP~.(ynames[i])~(m^2/m^2))
       x_label = bquote(CTL~.(ynames[i])~(m^2/m^2))
  }else if(ynames[i]=='An_DailyMean'|ynames[i]=='An_DailySum'){
       ylimit = c(0,10)
       y_label = bquote(EXP~.(ynames[i])~(mu*mol~m^-2*s^-1))
       x_label = bquote(CTL~.(ynames[i])~(mu*mol~m^-2*s^-1))
#        y_label = expression(paste(colnames(df_with_sd)[3]," (",mu,"mol/",sep=""))
  }else if(ynames[i]=='An_DailyMax'){
       y_label = bquote(EXP~.(ynames[i])~(mu*mol~m^-2*s^-1))
       x_label = bquote(CTL~.(ynames[i])~(mu*mol~m^-2*s^-1))
       ylimit = c(0,50)
  }else if(ynames[i]=="WUE" || ynames[i]=="iWUE"){
       ##umol CO2 / mmol H2O
#       y_label = bquote(.(colnames(df_with_sd)[3])~(mu*mol~CO[2]/mmol~H[2]*O))
       y_label = bquote(EXP~.(ynames[i])~(mu*mol/mmol))
       x_label = bquote(CTL~.(ynames[i])~(mu*mol/mmol))
       ylimit = c(0,20)
  }
  p<-ggplot(df,aes(x=CTL,y=EXP,color=CO2))+
     geom_point()+
     geom_smooth(method="lm",formula=y~x-1,fill=CO2)+
     xlab(x_label) + 
     ylab(y_label) + 
     theme_bw()+
     theme(axis.text=element_text(size=text_size),
#           axis.title=element_text(size=text_size,face="bold"),
           axis.title=element_text(size=text_size),
#           axis.title.x=element_blank(),
#           axis.text.x =element_blank(),
#           axis.ticks.x=element_blank(),
           legend.position = c(.2, .85), #horizontal, verical
           legend.text     = element_text(size=text_size),
           legend.title    = element_text(size=text_size))
  plot_list[[i]]=p
 }
 pdf(pdfname,height = 16, width=24)
 grid.arrange(grobs = plot_list,nrow=2,ncol=3)
 dev.off()
}

timeseries_plot<-function(CTL,EXP,pdfname,plot_type){
  df1 <- within(CTL, CO2 <- factor(CO2,levels=names(sort(table(CO2),decreasing=FALSE))))
  df2 <- within(EXP, CO2 <- factor(CO2,levels=names(sort(table(CO2),decreasing=FALSE))))
  CO2_label = paste(unique(df1$CO2),"ppm")
  plot_list = list()
  ynames = colnames(df1)[-c(1,2)]#remove year and CO2 
  text_size = 24
 for (i in 1:length(ynames)){
  df = data.frame(Years=df1$year,CTL=df1[,ynames[i]],EXP=df2[,ynames[i]],CO2=df1$CO2)
  ylimit = c(0.0,15.0)
  if(plot_type==1){ #seperately
    y_label = paste(ynames[i]," (Mg/ha)",sep="")
    if(ynames[i]=="LAI"){
         y_label = bquote(.(ynames[i])~(m^2/m^2))
         x_label = bquote(.(ynames[i])~(m^2/m^2))
    }else if(ynames[i]=='An_DailyMean'|ynames[i]=='An_DailySum'){
         ylimit = c(0,10)
         y_label = bquote(.(ynames[i])~(mu*mol~m^-2*s^-1))
         x_label = bquote(.(ynames[i])~(mu*mol~m^-2*s^-1))
#          y_label = expression(paste(colnames(df_with_sd)[3]," (",mu,"mol/",sep=""))
    }else if(ynames[i]=='An_DailyMax'){
         y_label = bquote(.(ynames[i])~(mu*mol~m^-2*s^-1))
         x_label = bquote(.(ynames[i])~(mu*mol~m^-2*s^-1))
         ylimit = c(0,50)
    }else if(ynames[i]=="WUE" || ynames[i]=="iWUE"){
         ##umol CO2 / mmol H2O
#         y_label = bquote(.(colnames(df_with_sd)[3])~(mu*mol~CO[2]/mmol~H[2]*O))
         y_label = bquote(.(ynames[i])~(mu*mol/mmol))
         x_label = bquote(.(ynames[i])~(mu*mol/mmol))
         ylimit = c(0,20)
    }
    p<-ggplot(df)+
       geom_line(aes(x=Years,y=CTL,color=CO2),size=2)+
       geom_point(aes(x=Years,y=CTL,color=CO2))+
       geom_line(aes(x=Years,y=EXP,color=CO2),size=2,linetype = "dashed")+
       geom_point(aes(x=Years,y=EXP,color=CO2))+
       scale_x_continuous(breaks = unique(df$Years),guide = guide_axis(angle = 45))+
       ylab(y_label) + 
       theme_bw()+
       theme(axis.text=element_text(size=text_size),
#             axis.title=element_text(size=text_size,face="bold"),
             axis.title=element_text(size=text_size),
#             axis.title.x=element_blank(),
#             axis.text.x =element_blank(),
#             axis.ticks.x=element_blank(),
             legend.position = c(.2, .85), #horizontal, verical
             legend.text     = element_text(size=text_size),
             legend.title    = element_text(size=text_size))
    plot_list[[i]]=p
  }else if(plot_type==2){
    y_label = bquote(Delta*.(ynames[i])~(Mg/ha))
    if(ynames[i]=="LAI"){
         y_label = bquote(Delta*.(ynames[i])~(m^2/m^2))
         x_label = bquote(Delta*.(ynames[i])~(m^2/m^2))
    }else if(ynames[i]=='An_DailyMean'|ynames[i]=='An_DailySum'){
         ylimit = c(0,10)
         y_label = bquote(Delta*.(ynames[i])~(mu*mol~m^-2*s^-1))
         x_label = bquote(Delta*.(ynames[i])~(mu*mol~m^-2*s^-1))
    }else if(ynames[i]=='An_DailyMax'){
         y_label = bquote(Delta*.(ynames[i])~(mu*mol~m^-2*s^-1))
         x_label = bquote(Delta*.(ynames[i])~(mu*mol~m^-2*s^-1))
         ylimit = c(0,50)
    }else if(ynames[i]=="WUE" || ynames[i]=="iWUE"){
         ##umol CO2 / mmol H2O
         y_label = bquote(Delta*.(ynames[i])~(mu*mol/mmol))
         x_label = bquote(Delta*.(ynames[i])~(mu*mol/mmol))
         ylimit = c(0,20)
    }
    p<-ggplot(df)+
       geom_line(aes(x=Years,y=EXP-CTL,color=CO2),size=2)+
       geom_point(aes(x=Years,y=EXP-CTL,color=CO2))+
       scale_x_continuous(breaks = unique(df$Years),guide = guide_axis(angle = 45))+
       ylab(y_label) + 
       theme_bw()+
       theme(axis.text=element_text(size=text_size),
#             axis.title=element_text(size=text_size,face="bold"),
             axis.title=element_text(size=text_size),
#             axis.title.x=element_blank(),
#             axis.text.x =element_blank(),
#             axis.ticks.x=element_blank(),
             legend.position = c(.2, .85), #horizontal, verical
             legend.text     = element_text(size=text_size),
             legend.title    = element_text(size=text_size))
    plot_list[[i]]=p
  }else if(plot_type==3){
    y_label = bquote(Delta*.(ynames[i])~('%'))
    ylimit = c(-5,10)
    if(ynames[i]=="WUE"){
       ylimit = c(-5,10)
    }
    tag_label = paste0("(",letters[i],")") 
    if(i==1){
       p<-ggplot(df)+
          geom_line(aes(x=Years,y=(EXP-CTL)/CTL*100,color=CO2),size=2)+
          geom_point(aes(x=Years,y=(EXP-CTL)/CTL*100,color=CO2))+
          scale_x_continuous(breaks = unique(df$Years),guide = guide_axis(angle = 45))+
#          scale_y_continuous(limits = ylimit,labels = label_number(accuracy = 1))+
          ylab(y_label) + 
          labs(tag = tag_label)+
          theme_bw()+
          theme(axis.text=element_text(size=text_size),
#                axis.title=element_text(size=text_size,face="bold"),
                axis.title=element_text(size=text_size),
#                axis.title.x=element_blank(),
#                axis.text.x =element_blank(),
#                axis.ticks.x=element_blank(),
                plot.tag = element_text(size=text_size),
                plot.tag.position = c(0.15,0.95),
                legend.position = c(.5, .25), #horizontal, verical
                legend.key.width = unit(2, 'cm'),
                legend.text     = element_text(size=text_size),
                legend.title    = element_text(size=text_size))
    }else if(i==length(ynames)){
       #remove legends for other plots
       p<-ggplot(df)+
          geom_line(aes(x=Years,y=(EXP-CTL)/CTL*100,color=CO2),size=2)+
          geom_point(aes(x=Years,y=(EXP-CTL)/CTL*100,color=CO2))+
          scale_x_continuous(breaks = unique(df$Years),guide = guide_axis(angle = 45))+
          ylab(y_label) + 
          labs(tag = tag_label)+
          theme_bw()+
          theme(axis.text=element_text(size=text_size),
                axis.title=element_text(size=text_size),
                legend.position="none",
                plot.tag = element_text(size=text_size),
                plot.tag.position = c(0.2, 0.95))
    }else{
       #remove legends for other plots
       p<-ggplot(df)+
          geom_line(aes(x=Years,y=(EXP-CTL)/CTL*100,color=CO2),size=2)+
          geom_point(aes(x=Years,y=(EXP-CTL)/CTL*100,color=CO2))+
          scale_x_continuous(breaks = unique(df$Years),guide = guide_axis(angle = 45))+
          ylab(y_label) + 
          labs(tag = tag_label)+
          theme_bw()+
          theme(axis.text=element_text(size=text_size),
                axis.title=element_text(size=text_size),
                legend.position="none",
                plot.tag = element_text(size=text_size),
                plot.tag.position = c(0.15, 0.95))
    }
    plot_list[[i]]=p
  }
 }
 grobs = plot_list
 #make sure X-Axis align
 # grobs<-list()
 # widths<-list()
 # for (i in 1:length(ynames)){
 #     grobs[[i]] <- ggplotGrob(plot_list[[i]])
 #     widths[[i]] <- grobs[[i]]$widths[2:5]
 # }
 # maxwidth <- do.call(grid::unit.pmax, widths)
 # for (i in 1:length(grobs)){
 #    grobs[[i]]$widths[2:5] <- as.list(maxwidth)
 #}
 pdf(pdfname,height = 12, width=24)
 grid.arrange(grobs = grobs,nrow=2,ncol=3)
 dev.off()
}

gradient_desc<-function(x,y,z,optionx){
	#start point
	x0 = 1
	y0 = 1
	# Step size multiplier
	alpha=0.001
	num_iter = 100
        x1 = x0
        y1 = y0
        h = 0.01
        xygrid = expand.grid(x,y)
        xy_trace = c()
	for (i in 1:100) {
            xp = c(x1,x1-h,x1+h)
            yp = c(y1,y1-h,y1+h)
            fxy= interp(xygrid$Var1,xygrid$Var2,z,xp,yp,linear=FALSE)
            dzdx = (fxy$z[3,1]-fxy$z[2,1])/(2*h)
            dzdy = (fxy$z[1,3]-fxy$z[1,2])/(2*h)
            if(optionx==1){  #descent
              x1 = x1 - alpha * dzdx 
              y1 = y1 - alpha * dzdy
            }else if(optionx==2){ #ascent
              x1 = x1 + alpha * dzdx 
              y1 = y1 + alpha * dzdy
            }else{stop("no such option!")}
            if(is.na(x1)| is.na(y1)| x1<min(x) | x1>max(x) | y1<min(y) | y1>max(y)) break
            xy_trace = rbind(xy_trace,c(x1,y1))
	}	
     return(xy_trace)
}

corr_plot <- function(df,pdfname,varnames,weather_varname){
   text_size = 18
   plist = list()
   for (i in 1:length(varnames)){
   yname = varnames[i] 
   p <- ggplot(data=df, aes_string(x=weather_varname, y=yname,label="year")) +
        geom_point()+
        geom_text(vjust = 0, nudge_y = 0.5)+
        geom_smooth(method="lm")+
        theme(axis.text=element_text(size=text_size),
              axis.title=element_text(size=text_size,face="bold"))
   plist[[i]] = p
   }
   pdf(pdfname,height = 8, width=12)
   grid.arrange(grobs = plist,nrow=2,ncol=3)
   dev.off()
}

plot_diurnal<-function(y,years,months_gs,pdfname,v_use,j_use,v_scaler,j_scaler){
#array(NaN,c(length(v_scaler),length(j_scaler),length(years),length(months_gs),24,2))
 v_index = which(abs(v_scaler-v_use)<1e-10)
 j_index = which(abs(j_scaler-j_use)<1e-10)
 y1 = y[v_index,j_index,,,,]

 plot_list = list()
 fig_order = 1
 x_hours = seq(0,23,by=1)
 for (j in 1:length(months_gs)){
 for (i in 1:length(years)){
     y_mean = y1[i,j,,1]
     y_std  = y1[i,j,,2]
     df = data.frame(x_hours,y_mean,y_std)
     p <- ggplot(df,aes(x=x_hours,y=y_mean))+
          geom_line(size=1) +
          geom_point(size=1.5)+
#          geom_errorbar(aes(ymin=y_mean-y_std, ymax=y_mean+y_std), width=.2)+
	  xlab("hours")+ylab("A_net")+
          coord_cartesian(ylim = c(-10,20))+
          theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=16),
               axis.text.y = element_text(face="bold", color="#993333", 
                           size=16),
               axis.title=element_text(size=14))
     plot_list[[fig_order]] = p
     fig_order = fig_order+1
 } 
 } 

     no_rows = length(months_gs)
     pdf(pdfname,height = 36, width=24)
     grid.arrange(grobs = plot_list,nrow=no_rows,ncol=length(years))
     dev.off()
}

profile_plot1<-function(y1,y2,no_layers,years,pdfname,v_use,j_use,v_scaler,j_scaler){
	plot_list = list()
        fig_order = 1
         layers = 1:no_layers
 for (i in 1:length(v_use)){
 for (j in 1:length(j_use)){
                xlabel = paste("Ci(vmax*",v_use[i],",jmax*",j_use[j],")",sep="")
		for (k in 1:length(years)){
                    v_index = which(abs(v_scaler-v_use[i])<1e-10)
                    j_index = which(abs(j_scaler-j_use[j])<1e-10)
                    if(length(v_index)!=1 | length(j_index)!=1) stop("indexing error")
                    y11 = y1[v_index,j_index,k,]
                    y22 = y2[v_index,j_index,k,]
 			df = data.frame(shaded=y11,sunlit=y22,layers)
                        df_molten=melt(df,id.vars="layers")
                     plot_list[[fig_order]] =   ggplot(df_molten,aes(x=value,y=layers,color=variable)) + 
                      geom_line()+
                      geom_point()+
		      xlab(xlabel)
 		     fig_order= fig_order+1
                }
 }
 }
        no_rows = length(v_use)*length(j_use)
	pdf(pdfname,height = 48, width=24)
	grid.arrange(grobs = plot_list,nrow=no_rows,ncol=length(years))
	dev.off()
}

profile_plot2<-function(y,no_layers,years,pdfname,v_use,j_use,v_scaler,j_scaler){

	plot_list = list()
        fig_order = 1
        layers = 1:no_layers
	for (k in 1:length(years)){
        y_sub = (1:no_layers)*NaN  #create a empty column for cbind
        col_names = c()
 	for (i in 1:length(v_use)){
 	for (j in 1:length(j_use)){
             v_index = which(abs(v_scaler-v_use[i])<1e-10)
             j_index = which(abs(j_scaler-j_use[j])<1e-10)
            if(length(v_index)!=1 | length(j_index)!=1) stop("indexing error")
             y_sub = cbind(y_sub,y[v_index,j_index,k,])
             col_names = c(col_names,paste("vmax",v_use[i],"jmax",j_use[j],sep=""))
	}      
	}      
        #after cbind, remove the first column
        y_sub = y_sub[,-1]
        colnames(y_sub) = col_names
 	     df = data.frame(y_sub,layers)
             df_molten=melt(df,id.vars="layers")
            plot_list[[fig_order]] =   ggplot(df_molten,aes(x=value,y=layers,color=variable,shape=variable)) + 
            geom_line()+
            geom_point(alpha=0.5)+
	    xlab("Ci(ppm)")+
           scale_y_continuous(breaks=c(1:no_layers))+
            theme(axis.text.x = element_text(face="bold", color="#993333", 
                           size=14),
                  axis.text.y = element_text(face="bold", color="#993333", 
                           size=14),
                  axis.title=element_text(size=14),
                  legend.position = c(0.8, 0.2),
                  legend.text=element_text(size=14))
 	    fig_order= fig_order+1
	}
	pdf(pdfname,height = 12, width=24)
	grid.arrange(grobs = plot_list,nrow=1,ncol=length(years))
	dev.off()
}
# Define functions to create plots
plot_all_tissues <- function(res, year, biomass, biomass.std) {
  
  r <- reshape2::melt(res[, c("time","Root","Leaf","Stem","Grain")], id.vars="time")
  r.exp <- reshape2::melt(biomass[, c("DOY", "Leaf", "Stem", "Pod")], id.vars = "DOY")
  r.exp.std <- reshape2::melt(biomass.std[, c("DOY", "Leaf", "Stem", "Pod")], id.vars = "DOY")
  r.exp.std$ymin<-r.exp$value-r.exp.std$value
  r.exp.std$ymax<-r.exp$value+r.exp.std$value
  
  # Colorblind friendly color palette (https://personal.sron.nl/~pault/)
  col.palette.muted <- c("#332288", "#117733", "#999933", "#882255")
  
  size.title <- 12
  size.axislabel <-10
  size.axis <- 10
  size.legend <- 12
  
  f <- ggplot() + theme_classic()
  f <- f + geom_point(data=r, aes(x=time,y=value, colour=variable), show.legend = TRUE, size=0.25)
  f <- f + geom_errorbar(data=r.exp.std, aes(x=DOY, ymin=ymin, ymax=ymax), width=3.5, size=.25, show.legend = FALSE)
  f <- f + geom_point(data=r.exp, aes(x=DOY, y=value, fill=variable), shape=22, size=2, show.legend = FALSE, stroke=.5)
  f <- f + labs(title=element_blank(), x=paste0('Day of Year (',year,')'),y='Biomass (Mg / ha)')
  f <- f + coord_cartesian(ylim = c(0,10)) + scale_y_continuous(breaks = seq(0,10,2)) + scale_x_continuous(breaks = seq(150,275,30))
  f <- f + theme(plot.title=element_text(size=size.title, hjust=0.5),
                 axis.text=element_text(size=size.axis),
                 axis.title=element_text(size=size.axislabel),
                 legend.position = c(.15,.85), legend.title = element_blank(),
                 legend.text=element_text(size=size.legend),
                 legend.background = element_rect(fill = "transparent",colour = NA),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(), panel.background = element_rect(fill = "transparent",colour = NA),
                 plot.background = element_rect(fill = "transparent", colour = NA))
  f <- f + guides(colour = guide_legend(override.aes = list(size=2)))
  f <- f + scale_fill_manual(values = col.palette.muted[2:4], guide = "none")
  f <- f + scale_colour_manual(values = col.palette.muted, labels=c('Root','Leaf','Stem','Pod'))
  
  return(f)
}



