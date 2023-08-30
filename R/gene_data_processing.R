
#' gene_data_processing
#'
#' @param gene_data
#' @param group1
#' @param group2
#'
#' @return
#' @export
#'
#' @examples
gene_data_processing <- function(data_file){

df <- read.xlsx(data_file) %>% na.omit()
colnames(df)[1:4] <- c("ID","log2FC","pvalue","qvalue")

# 去掉表达量为无穷大Infinity的行,
not_inf <- !grepl("inf",df$log2FC,ignore.case = T)
df <- df[not_inf,] %>% data.frame()

# 去掉表达量均为0的行
df$sum <-rowSums(df[,c(5:ncol(df))])
df <- df[df[,ncol(df)]>0,]
df <- df[,-ncol(df)]

df <- df[,c(1:4)]

# 转换为数字格式
df2 <- apply(X = df[,-1],MARGIN = 2,FUN = as.numeric) %>% data.frame()
df <- cbind(df[,1],df2)
colnames(df) <- c("ID","log2FC","pvalue","qvalue")

# 去重
df <- distinct(df, ID, .keep_all = TRUE)

data <- df

}


