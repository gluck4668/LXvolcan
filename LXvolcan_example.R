
install.packages("devtools")

library(devtools)

install_github("gluck4668/LXvolcan")

library(LXvolcan)

#--------------------------------
data(gene_data_example)
data(meta_data_example)
data(meta_rawdata)
#--------------------------------

rm(list=ls())

if(!is.null(dev.list()))
  dev.off()

#devtools::load_all()


data_file = "Model vs PNS.csv"

data_file = "model vs pns.xlsx"

data_file = "gene.xlsx"

data_file = "meta_data.xlsx"

data_file = "rawdata.xlsx"

data_file = "rawdata.csv"

data_file="meta_abnormal.xlsx"

FC = NULL #(FC is the Fold Change and should be 2 or NULL )


LXvolcan (data_file,FC)




