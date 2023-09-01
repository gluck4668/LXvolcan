
#' meta_data_processing
#'
#' @param data_file
#'
#' @return
#' @export
#'
#' @examples
meta_data_processing <- function(data_file){

  df <- read.xlsx(data_file)
  df <- na.omit(df)
  df <- mutate(df,rowsum=rowSums(df[,c(6:ncol(df))])) # 行之和rowSums()
  df <- dplyr::filter(df,rowsum>0) #去掉表达量为0的行
  colnames(df)[1:5] <- c("ID","FC","High","pvalue","qvalue")

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
  n_high <- grep("High",names(df))
  n1 <- grep("mean1",names(df))
  n2 <- grep("mean2",names(df))

  df[,n_FC] <- as.numeric(df[,n_FC])

  for (i in 1:nrow(df)){
    df[i,n_high] <- ifelse(df[i,n2]>df[i,n1],group_name[2],group_name[1])
  }

# 选出做火山图需要的数据
  meta <- df[,c(1:5)]

  #group_name <- meta$High[!duplicated(meta$High)] # 找出组名

# 把第2列的无穷大数据infinity修改为一个指定数据
  meta[grepl("inf",meta$FC,ignore.case = T),2] <- 1000

  meta$FC <- as.numeric(meta$FC)

  meta$log2FC <- NA

# High项下，如果是group_name[1]，则取1/FC，并计算其log2(1/FC)值；如果为 group_name[2]，则直接计算log2(FC)
  meta[grepl(group_name[1],meta$High,ignore.case = T),6] <- log2(1/meta[grepl(group_name[1],meta$High,ignore.case = T),2])
  meta[grepl(group_name[2],meta$High,ignore.case = T),6] <- log2(meta[grepl(group_name[2],meta$High,ignore.case = T),2])

# 去重，并整理数量结构
  meta <- data.frame(distinct(meta, ID, .keep_all = TRUE))
  meta <- dplyr::select(.data = meta,ID,log2FC,pvalue,qvalue)

  result <- list(group=group_name,data=meta) # 用list包含多个结果
  return(result) # 输出结果


}
