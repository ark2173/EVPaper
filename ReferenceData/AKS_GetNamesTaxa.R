AKS_GetNamesTaxa <- function (ProteinAccList, directorypath = NULL) 
{
  message("Please wait we are processing your accessions ...")
  pb <- progress::progress_bar$new(total = length(ProteinAccList))
  baseUrl <- "https://rest.uniprot.org/uniprotkb/search?query=accession:"
  columns = "accession,id,gene_names,gene_primary,gene_synonym,gene_oln,gene_orf,organism_name,organism_id,protein_name,xref_proteomes,lineage,virus_hosts"
  ProteinInfoParsed_total = data.frame()
  for (ProteinAcc in ProteinAccList) {
    Request <- tryCatch({
      GET(paste0(baseUrl, ProteinAcc, "&format=tsv"), 
          timeout(7))
    }, error = function(cond) {
      message("Internet connection problem occurs and the function will return the original error")
      message(cond)
    })
    ProteinName_url <- paste0(ProteinAcc, "&format=tsv&fields=", 
                              columns)
    RequestUrl <- paste0(baseUrl, ProteinName_url)
    RequestUrl <- URLencode(RequestUrl)
    if (length(Request) == 0) {
      message("Internet connection problem occurs")
      return()
    }
    if (Request$status_code == 200) {
      ProteinDataTable <- tryCatch(read.csv(RequestUrl, 
                                            header = TRUE, sep = "\t"), error = function(e) NULL)
      if (!is.null(ProteinDataTable)) {
        ProteinDataTable <- ProteinDataTable[1, ]
        ProteinInfoParsed <- as.data.frame(ProteinDataTable, 
                                           row.names = ProteinAcc)
        ProteinInfoParsed_total <- rbind(ProteinInfoParsed_total, 
                                         ProteinInfoParsed)
      }
    }
    else {
      HandleBadRequests(Request$status_code)
    }
    pb$tick()
  }
  if (!is.null(directorypath)) {
    write.csv(ProteinInfoParsed_total, paste0(directorypath, 
                                              "/", "Names & Taxa Information.csv"))
  }
  return(ProteinInfoParsed_total)
}