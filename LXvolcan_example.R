
install.packages("devtools")

library(devtools)

install_github("gluck4668/LXvolcan")

library(LXvolcan)

#--------------------------------
data(gene_data_example)
data(meta_data_example)
#--------------------------------

rm(list=ls())

if(!is.null(dev.list()))
  dev.off()

devtools::load_all()

data_type="gene" # it should be "gene" or "metabolite"

data_file = "gene_data.xlsx"

FC = 2 #(FC is the Fold Change and should be 2 or NULL )


LXvolcan (data_type,data_file,FC)


devtools::document()
devtools::build()
