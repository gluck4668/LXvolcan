
#' volcano_01
#'
#' @param data
#' @param title_gene
#'
#' @return
#' @export
#'
#' @examples
volcano_01 <- function(data,title_text){

  mytheme<-theme_bw()+
  theme(text=element_text(family = "sans",colour ="black",face="bold",size =12),
        #panel.border = element_rect (linewidth = 0.8,color = "gray30"),
        axis.line = element_blank(),
        axis.ticks = element_line(linewidth = 0.6,colour = "gray30"),
        axis.ticks.length = unit(1.5,units = "mm"))+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5),"cm"))

xytheme <-theme(axis.text.x = element_text(face="bold",color="black",size=11,angle =0,hjust=1))+
  theme(axis.text.y = element_text(face="bold",color="black",size=11))+
  theme(legend.text=element_text(face="bold",color="black",size=11))

vol_01 <- ggplot(data,aes(x=log2FC,y=-log10(pvalue)))+
  geom_point(aes(color=changes))+
  scale_color_manual(values = c("#00bfff","#c0c0c0","#ff4500"))+
  geom_hline(yintercept = -log10(0.05),linetype="dashed",color="#808080")+
  geom_vline(xintercept = c(-1,1),linetype="dashed",color="#808080")+
  labs(x="log2 (Fold Change)",y="-log10(p value)",title = title_text) +
  mytheme+xytheme
}

