
#' Title
#'
#' @param data_file
#'
#' @return
#' @export
#'
#' @examples

meta_rawdata_processing <- function(df_set){

df <- df_set[-c(1:2),]
names(df) <- df_set[2,]

title_text <- names(df_set)

nor <- grep("Normalised",title_text,ignore.case = TRUE) %>% as.numeric()
raw <- grep("Raw",title_text,ignore.case = TRUE)-1

# 去掉空格方法：
groups <- df_set[1,c(nor:raw)] %>% trimws() %>% as.character() %>% na.omit() # 转换为字符串
c(groups)=="" #查看哪些是空格
!c(groups)=="" #不是空格
group_names <- groups[!c(groups)==""] # 只列出不是空格的字符串

# 可以整合成一条代码
group_names <- df_set[1,c(nor:raw)] %>% trimws() %>% as.character() %>% na.omit() %>% .[!c(.)==""]

group1 <- group_names[1]
group2 <- group_names[2]

#---去掉组名中含有的括号()
ft <- gsub("[()]","",df_set[1,])
gg1 <- gsub("[()]","",group1)
gg2 <- gsub("[()]","",group2)

g1 <- grep(gg1,ft,ignore.case = T)[1] %>% as.numeric()
g2 <- grep(gg2,ft,ignore.case = T)[1] %>% as.numeric()

#------筛选出所需要的列-------------
lst1 <- grep("Accepted",names(df),ignore.case = TRUE)
lst2 <- grep("Fold",names(df),ignore.case = TRUE)[1]
lst3 <- grep("Highest",names(df),ignore.case = TRUE)
lst4 <- grep("Anova",names(df),ignore.case = TRUE)[1]
lst5 <- c(nor:raw)

names(df)[g1:(g2-1)] <- paste0(group1,c(1:(g2-g1)))
names(df)[g2:raw] <- paste0(group2,c(1:(raw-g2+1)))

data_meta <- df[,c(lst1[1],lst2,lst3,lst4,lst5)] %>% data.frame()

result <- meta_data_processing(data_meta)

return(result)

}
