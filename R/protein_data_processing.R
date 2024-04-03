
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
protein_data_processing <- function(data_file){

df0 <- read.xlsx(data_file) %>% na.omit()
names(df0)[1] <- c("prot_id")

#-----------------ID 转换----------------------------------------------

if(any(grepl("ENS",df0[,1],ignore.case = T))) #判断是否是ENS ID, 如果是则转换为UNIPROT ID
  {org.db <-c("org.Hs.eg.db","org.Rn.eg.db","org.Mm.eg.db")  # 物种数据类型
   fun_bitr <-function(i){id_convert <- tryCatch(bitr(geneID = df0$prot_id,   # tryCath（）发生错误时仍可继续运行后续代码
                                                 fromType = "ENSEMBLPROT",toType = c("ENSEMBLPROT","UNIPROT"),OrgDb = i),
                                                 error=function(e){e}  # {e} 不做任何处理。当然也可以运行具体代码，如{print("Come on..")}
                                                 )

                   if(!grepl("error", class(id_convert)) %>% any())
                    prot_list <<- distinct(id_convert,ENSEMBLPROT,.keep_all = T)  # 这里使用了全局变量 <<-
                    }

   sapply(org.db,fun_bitr)
   colnames(prot_list) <-c("prot_id","Uniprot_id")
   df <- inner_join(df0,prot_list,by = "prot_id")
   df[,"prot_id"] <- df[,"Uniprot_id"]
   df <-  df[,-grep("Uniprot_id",names(df))]
} else

df <- df0

#----进行log2-----------------------------------------
if( !any(grepl("log2",names(df),ignore.case = T)) )
  df[,2] <- log2(df[,2])

colnames(df)[1:3] <- c("ID","log2FC","pvalue")

# 去掉表达量为无穷大Infinity的行,
not_inf <- !grepl("inf",df$log2FC,ignore.case = T)
df <- df[not_inf,] %>% data.frame()

# 抽出组名
group_name <- colnames(df) %>%
  gsub("\\d+$","",.) %>%   # 去掉字符末尾的数字，等同 gsub("\\d+$","", group_gene)
  .[duplicated(.)] %>% # 选出重复项（只有组名才可能重复，因此重复即为组名）
  .[!duplicated(.)] # 去掉重复的字符串，等同 group_gene[!duplicated(group_gene)]

group1 <- group_name[1]
group2 <- group_name[2]

group1_col <- grep(group1,names(df))  # group1所在的列
group2_col <- grep(group2,names(df))  # group1所在的列

# 去掉表达量均为0的行
fun_num <- function(i){as.numeric(i)}
df_num <- sapply(df[,c(group1_col,group2_col)],fun_num) %>% data.frame()
df[,c(group1_col,group2_col)] <- df_num

df$type <-"type"

for(i in 1:nrow(df)){
  if(all(df[i,c(group1_col,group2_col)]==0))
    df[i,"type"] <- NA
 }

df <- na.omit(df)
df <- df[,-grep("type",names(df))]

# 判断FC是正还是负
mean1 <- df[,grep(group_name[1],names(df))]
df$mean1 <- rowMeans(mean1)

mean2 <- df[,grep(group_name[2],names(df))]
df$mean2 <- rowMeans(mean2)

n_FC <- grep("FC",names(df))
n1 <- grep("mean1",names(df))
n2 <- grep("mean2",names(df))

df[,n_FC] <- as.numeric(df[,n_FC])

for (i in 1:nrow(df)){
  df[i,n_FC] <- ifelse(df[i,n2]>df[i,n1],abs(df[i,n_FC]),(0-abs(df[i,n_FC])))
}

df <- df[,c(1:3)]

# 转换为数字格式
df2 <- apply(X = df[,-1],MARGIN = 2,FUN = as.numeric) %>% data.frame()
df <- cbind(df[,1],df2)
colnames(df) <- c("ID","log2FC","pvalue")

# 去重
df <- distinct(df, ID, .keep_all = TRUE)

result <- list(group=group_name,data=df)  # 用list包含多个结果
return(result) #输出结果

}


