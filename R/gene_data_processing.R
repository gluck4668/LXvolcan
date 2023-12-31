
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

# 抽出组名
group_name <- colnames(df) %>%
  gsub("\\d+$","",.) %>%   # 去掉字符末尾的数字，等同 gsub("\\d+$","", group_gene)
  .[duplicated(.)] %>% # 选出重复项（只有组名才可能重复，因此重复即为组名）
  .[!duplicated(.)] # 去掉重复的字符串，等同 group_gene[!duplicated(group_gene)]

# 判断FC是正还是负
mean1 <- df[,grep(group_name[1],names(df))]
df$mean1 <- rowMeans(mean1)

mean2 <- df[,grep(group_name[2],names(df))]
df$mean2 <- rowMeans(mean2)

n_FC <- grep("FC",names(df))
n1 <- grep("mean1",names(df))
n2 <- grep("mean2",names(df))

df[,n_FC] <- as.numeric(df[,n_FC])

i=1
for (i in 1:nrow(df)){
  df[i,n_FC] <- ifelse(df[i,n2]>df[i,n1],abs(df[i,n_FC]),(0-abs(df[i,n_FC])))
}

df <- df[,c(1:4)]


# 转换为数字格式
df2 <- apply(X = df[,-1],MARGIN = 2,FUN = as.numeric) %>% data.frame()
df <- cbind(df[,1],df2)
colnames(df) <- c("ID","log2FC","pvalue","qvalue")

# 去重
df <- distinct(df, ID, .keep_all = TRUE)

result <- list(group=group_name,data=df)  # 用list包含多个结果
return(result) #输出结果

}


