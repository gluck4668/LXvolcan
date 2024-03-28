
install.packages("devtools")

library(devtools)

install_github("gluck4668/LXvolcan")

library(LXvolcan)

#--------------------------------
data(gene_data_example)
data(protein_data_example)
data(meta_data_example)
data(meta_rawdata)
#--------------------------------

rm(list=ls())

devtools::load_all()

data_file = "gene_data.xlsx"
data_type="gene"

data_file = "protein_data.xlsx"
data_type="protein"

data_file = "meta_data.xlsx"
data_type="metabolite"

data_file = "meta_rawdata.csv"
data_type="metabolite_raw_data"

data_type="protein" # It should be "gene", "protein", "metabolite", or "metabolite_raw_data"

FC = NULL #(FC is the Fold Change and should be 2 or NULL )


LXvolcan (data_file,data_type,FC)






