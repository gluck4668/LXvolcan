
#' volcano_02
#'
#' @param title_text
#' @param data
#'
#' @return
#' @export
#'
#' @examples
volcano_02 <- function(data,title_text){

data$log2FoldChange <- abs(data$log2FC)

change_n <- table(data$type)
change_nm <-data.frame(change_n)

Down <- c(paste("Down",c(change_nm[1,2])))#Down
No <- c(paste("Not sig",c(change_nm[2,2])))#Not sig
UP <- c(paste("Up",c(change_nm[3,2])))#Up

label01=paste("       *",Down)
label02=paste("          *",No)
label03=paste("*",UP)

label_all <- paste(label01,"\n", label02,"\n",label03)

mytheme3<-theme_bw()+
  theme(text=element_text(family = "sans",colour ="black",face="bold",size =12),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.6,colour = "gray30"),
        axis.ticks.length = unit(1.5,units = "mm"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.3, 5.5, 0.5, 1.0), "cm"))

xytheme3 <-theme(axis.text.x = element_text(face="bold",color="black",size=11,angle =0,hjust=1))+
  theme(axis.text.y = element_text(face="bold",color="black",size=11))+
  theme(legend.text=element_text(face="bold",color="black",size=11),
        legend.position = c(1.2,0.40))

tag_theme3 <- theme(plot.tag = element_text(size =12,colour = "black"),
                    plot.tag.position = c(1.10,0.83))

# ------------------------------
p3 <- ggplot(data,aes(x=log2FC,y=-log10(pvalue)))+
  geom_point(aes(color=-log10(pvalue),size=log2FoldChange))+
  scale_color_gradientn(values=seq(0,1,0.2),
                        colors=c("#39489f","#39bbec","#f9ed36","#f38466","#b81f25"))+
  scale_size_continuous(rang=c(1,3))+
  geom_hline(yintercept = -log10(0.05),linetype="dashed",color="#808080")+
  geom_vline(xintercept = c(-1.2,1.2),linetype="dashed",color="#808080")+
  labs(x="log2 (Fold Change)",y="-log10(p value)",title = title_text,tag =label_all)+
  mytheme3+xytheme3+tag_theme3
}
