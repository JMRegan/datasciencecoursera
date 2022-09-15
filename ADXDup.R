ADX_Duplicates <-function(file1, file2) {
  sapply(c('tidyverse', 'data.table'), 
         require, character.only = TRUE)
  ##loads required packages
  ##Downloaded X genes from:
  ##https://www.ncbi.nlm.nih.gov/gene/?term=X%5BCHR%5D+AND+human%5BORGN%5D
  readAD <-as_tibble(read.csv((paste(file1))))
  ##read AD gene file into a tibble
  readX <-as_tibble(read.csv((paste(file2))))
  ##read X gene file into a tibble
  ShareX <- intersect(readAD$GeneID,readX$GeneID)
  ##create vector resulting from intesection of GeneIDs that are shared
  ShareXdf <-data.frame(ShareX)
  ##turn vector into a dataframe
  colnames(ShareXdf) <-c('GeneID')
  ##rename column "x" as GeneID
  head(ShareXdf)
  ##check that our results are as expected
  final <-readX %>% filter(GeneID %in% ShareXdf$GeneID)
  ##go back to list of X genes and filter based on common genes.
  ##this allows us to get column values associated with the shared genes
  head(final)
  ##check that our results are as expected
  write.csv(final, "AD_XGenes.csv")
}
