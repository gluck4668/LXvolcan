
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

devtools::load_all()

data_file = "gene_data.xlsx"

data_file = "meta_data.xlsx"

data_file = "meta_rawdata.csv"


FC = NULL #(FC is the Fold Change and should be 2 or NULL )

FC = 2

LXvolcan (data_file,FC)




