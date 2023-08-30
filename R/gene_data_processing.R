
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

# 去掉表达量均为0的行
df$sum <-rowSums(df[,c(5:ncol(df))])
df <- df[df[,ncol(df)]>0,]
df <- df[,-ncol(df)]

df <- df[,c(1:4)]

# 去掉表达量为无穷大的行
for(i in 1:nrow(df)){
  if(substring(tolower(df[i,2]),1,3)=="inf")
    df[i,2]=NA}
rm(i)
df <- na.omit(df)

# 转换为数字格式
df2 <- apply(X = df[,-1],MARGIN = 2,FUN = as.numeric) %>% data.frame()
df <- cbind(df[,1],df2)
colnames(df) <- c("ID","log2FC","pvalue","qvalue")

# 去重
df <- distinct(df, ID, .keep_all = TRUE)

if(is.null(FC)==TRUE)
{df$type <- ifelse(df$log2FC>0 & df$pvalue<0.05,"Up",
                     ifelse(df$log2FC<0 & df$pvalue<0.05,"Down","Not sig")) } else
                     {df$type <- ifelse(df$log2FC>=log2(FC) & df$pvalue<0.05,"Up",
                                          ifelse(df$log2FC<=-log2(FC) & df$pvalue<0.05,"Down","Not sig"))}

change_n <- table(df$type) %>% data.frame()

Down <- c(paste(change_n[1,1],change_n[1,2]))#Down
No <- c(paste(change_n[2,1],change_n[2,2]))#Not sig
UP <- c(paste(change_n[3,1],change_n[3,2]))#Up

df$changes <- ifelse(df$type=="Up",UP,
                       ifelse(df$type=="Down",Down,No))
data <- df

}


