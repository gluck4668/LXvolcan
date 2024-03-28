#' LXvolcan
#'
#' @param data_type
#' @param data_file
#' @param FC
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
LXvolcan <- function(data_file,data_type,FC){

packages <- function(){

# 查看已安装的R包
installed_packages <- data.frame(installed.packages())

# 计划要安装的R包
pack <- c("ggplot2","openxlsx","stringr","plyr","psych","dplyr",
          "ggrepel","patchwork","raster","png","janitor","purrr")

# 真正还没安装的R包
not_install <- pack[!pack %in% installed_packages$Package]

# 如果存在未安装的R包，则用sapply()函数批量安装
if (length(not_install)>0){
fun_pack <-function(x){install.packages(x,update = F,ask = F)}
sapply(not_install, fun_pack,simplify = T)}


Bio_packs <- c("clusterProfiler","org.Rn.eg.db","org.Mm.eg.db","org.Hs.eg.db")
no_bio <- Bio_packs[!Bio_packs %in% installed_packages$Package]

if (length(no_bio)>0){
  fun_bio <-function(i){BiocManager::install(i,update = F,ask = F)}
  sapply(no_bio, fun_bio,simplify = T)}

# 批量library
fun_lib <-function(x){library(x,character.only = T)}
sapply(c(pack,Bio_packs),fun_lib,simplify = T)

rm(installed_packages,not_install,pack,fun_lib)

}

packages()


#----- 数据处理---------------

# 根据data_type的类型，进行相应的数据处理

if(tolower(trimws(data_type))=="gene")
  df <- gene_data_processing(data_file)

if(tolower(trimws(data_type))=="protein")
  df <- protein_data_processing(data_file)

if(tolower(trimws(data_type))=="metabolite")
  {data_meta <- read.xlsx(data_file) %>% na.omit()
  df <- meta_data_processing(data_meta)}

if(tolower(trimws(data_type))=="metabolite_raw_data")
  df <- meta_rawdata_processing(data_file)

data <- df$data %>% data.frame()
data <- distinct(data,ID,.keep_all = T) # ID去重

# p值为0，表示其中一个组的表达0，意义不大。建议删除
data <- dplyr::filter(data,pvalue>0)

# 标注down, up 和 not sig
if(is.null(FC)==TRUE)
  {data$type <- ifelse(data$log2FC>0 & data$pvalue<0.05,"Up",
                       ifelse(data$log2FC<0 & data$pvalue<0.05,"Down","Not sig")) } else
                       {data$type <- ifelse(data$log2FC>=log2(FC) & data$pvalue<0.05,"Up",
                                            ifelse(data$log2FC<=-log2(FC) & data$pvalue<0.05,"Down","Not sig"))}

change_n <- table(data$type) %>% data.frame()

Down <- c(paste(change_n[1,1],change_n[1,2]))#Down
No <- c(paste(change_n[2,1],change_n[2,2]))#Not sig
UP <- c(paste(change_n[3,1],change_n[3,2]))#Up

data$changes <- ifelse(data$type=="Up",UP,
                         ifelse(data$type=="Down",Down,No))

data$p_log10 <- (-log10(data$pvalue))

-log10(0.05)

df_order <- data[data[,ncol(data)]>1.30103,] # 筛选出 > -log10(0.05)，即p<0.05

df_order <- df_order[order(-df_order[,ncol(data)]),]

i <- case_when(nrow(df_order)>1000 ~0.04,
                 nrow(df_order)>500 ~0.05,
                 nrow(df_order)>200 ~0.06,
                 nrow(df_order)>100 ~0.07,
                 TRUE ~0.1)

show_n <- round(nrow(df_order)*i,0)

df_n <- case_when(show_n >25 ~25,
                    TRUE ~show_n)

df_new <- df_order[c(1:df_n),]

df_show <- df_new[abs(df_new$log2FC)>=1,]

#----------------------------------------------------

group_name <- df$group

if(tolower(trimws(data_type))=="metabolite_raw_data")
  df_type <- c("metabolite") else
  df_type <- data_type

title_text <- c(paste0("The ",df_type," volcano graphics",
                         " (",group_name[2]," VS ",group_name[1],")"))


group_names <- paste0("(",group_name[2]," VS ",group_name[1],")")

p1 <- volcano_01(data,title_text)

p1

#---------------------

p2 <-  p1+
    geom_label_repel(data=df_show,aes(x=log2FC,y=-log10(pvalue), label=ID),
                     label.size =0.1,size=2, box.padding = 0.4, max.overlaps =show_n)

p2


p3 <- volcano_02(data,title_text)

p3

#########################################################

p4 <- p3+
    geom_label_repel(data=df_show,aes(x=log2FC,y=-log10(pvalue), label=ID),#只显示绝对值log2FC>=1的ID
                     label.size =0.1,size=2, box.padding = 0.4, max.overlaps =show_n)

p4


if(!dir.exists("analysis result"))
    dir.create("analysis result")
#---------------------------------------------------------
df_lst <- data[,c(1:5)]
df_lst <- df_lst[order(df_lst[,5]),]

# 根据FC值（2或NULL）进行相应的数据处理
if(is.null(FC)==TRUE)
  {df_name <- paste0("analysis result/",data_type,
                         " volcano data (P0.05, unlimited FC)_all ",group_names,".xlsx")
   write.xlsx(df_lst,df_name)

   not <- !grepl("not",df_lst$type,ignore.case = TRUE) # 筛选出不是not sig的项，即只有down 和 up的行
   df_dif <- df_lst[not,] %>% na.omit()

   df_dif_name <- paste0("analysis result/",data_type,
                             " volcano data (P0.05, unlimited FC)_diferent ",group_names,".xlsx")
   write.xlsx(df_dif,df_dif_name)

  #---------------------------------------------------
  p1_name <- paste0("analysis result/",data_type,
                    " Volcano graphics 01 (P0.05, unlimited FC) ",group_names,".png")
  p2_name <- paste0("analysis result/",data_type,
                   " Volcano graphics 02 (P0.05, unlimited FC) ",group_names,".png")
  p3_name <- paste0("analysis result/",data_type,
                   " Volcano graphics 03 (P0.05, unlimited FC) ",group_names,".png")
  p4_name <- paste0("analysis result/",data_type,
                   " Volcano graphics 04 (P0.05, unlimited FC) ",group_names,".png")

  ggsave(p1_name,p1, width=1200, height =1000, dpi=180,units = "px")
  ggsave(p2_name,p2, width=1200, height =1000, dpi=180,units = "px")
  ggsave(p3_name,p3, width=1400, height =1000, dpi=180,units = "px")
  ggsave(p4_name,p4, width=1400, height =1000, dpi=180,units = "px")} else

  {FC2_file_name <- paste("analysis result/",data_type,
                          " volcano data (P0.05, FC2)_all ",group_names,".xlsx")
  write.xlsx(df_lst,FC2_file_name)

  not <- !grepl("not",df_lst$type,ignore.case = TRUE) # 筛选出不是not sig的项，即只有down 和 up的行
  df_dif <- df_lst[not,] %>% na.omit()

  FC2_dif_name <- paste0("analysis result/",data_type,
                        " volcano data (P0.05, FC2)_diferent ",group_names,".xlsx")
  write.xlsx(df_dif,FC2_dif_name)

  #---------------------------------------------------
  FC2_p1 <- paste0("analysis result/",data_type,
                   " volcano graphics 01 (P0.05, FC2) ",group_names,".png")
  FC2_p2 <- paste0("analysis result/",data_type,
                   " volcano graphics 02 (P0.05, FC2) ",group_names,".png")
  FC2_p3 <- paste0("analysis result/",data_type,
                   " volcano graphics 03 (P0.05, FC2) ",group_names,".png")
  FC2_p4 <- paste0("analysis result/",data_type,
                   " volcano graphics 04 (P0.05, FC2) ",group_names,".png")

  ggsave(FC2_p1,p1, width=1200, height =1000, dpi=180,units = "px")
  ggsave(FC2_p2,p2, width=1200, height =1000, dpi=180,units = "px")
  ggsave(FC2_p3,p3, width=1400, height =1000, dpi=180,units = "px")
  ggsave(FC2_p4,p4, width=1400, height =1000, dpi=180,units = "px")}

  print("The results can be fond in the folder of <analysis result>")

  na.change <-grepl("NA",data$changes,ignore.case = T) %>% table()
  if("TRUE" %in% names(na.change)){
    print("#------------------------------------------------------------------#")
    print("The data were abnormal, such as all pvalue<0. Please check the data")
    print("#------------------------------------------------------------------#")
    }

  p1

}

