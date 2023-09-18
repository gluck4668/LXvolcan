
#' Title
#'
#' @param data_file
#'
#' @return
#' @export
#'
#' @examples
rawdata <- function(data_file){

library(openxlsx)
library(dplyr)

csv <- read.csv(data_file)
df <- csv[-c(1:2),]
names(df) <- csv[2,]

title_csv <- names(csv)

nor <- grep("normalised",title_csv,ignore.case = TRUE) %>% as.numeric()
raw <- grep("Raw",title_csv,ignore.case = TRUE)-1

# 去掉空格方法：
groups <- csv[1,c(nor:raw)] %>% trimws() %>% as.character() # 转换为字符串
c(groups)=="" #查看哪些是空格
!c(groups)=="" #不是空格
group_names <- groups[!c(groups)==""] # 只列出不是空格的字符串

# 可以整合成一条代码
group_names <- csv[1,c(nor:raw)] %>% trimws() %>% as.character() %>% .[!c(.)==""]

group1 <- group_names[1]
group2 <- group_names[2]

g1 <- grep(group1,csv[1,])[1] %>% as.numeric()
g2 <- grep(group2,csv[1,])[1] %>% as.numeric()


lst1 <- grep("Accepted",names(df),ignore.case = TRUE)[1]
lst2 <- grep("Fold",names(df),ignore.case = TRUE)[1]
lst3 <- grep("Highest",names(df),ignore.case = TRUE)
lst4 <- grep("Anova",names(df),ignore.case = TRUE)[1]
lst5 <- grep("q",names(df),ignore.case = TRUE)[1]
lst6 <- c(nor:raw)

names(df)[g1:(g2-1)] <- paste0(group1,c(1:(g2-g1)))
names(df)[g2:raw] <- paste0(group2,c(1:(raw-g2+1)))

result <- df[,c(lst1,lst2,lst3,lst4,lst5,lst6)]

return(result)

}
