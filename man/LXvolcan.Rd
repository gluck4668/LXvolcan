\name{LXvolcan}
\alias{LXvolcan}
\title{LXvolcan}
\usage{
LXvolcan (data_file,data_type,FC)
}
\description{
LXvolcan can be used to analyze the differently expressed genes or metabolites.
}
\examples{
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

data_file = "gene_data.xlsx"

data_type="gene" # It should be "gene", "protein", "metabolite", or "metabolite_raw_data"

FC = NULL #(FC is the Fold Change and should be 2 or NULL )

LXvolcan (data_file,data_type,FC)

}
