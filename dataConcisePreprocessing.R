#library for sql based data table
library(data.table)
library(arules)

# Setting working directory
mutwd <- "D:/docs/Spring 2018/CS378/Git378/cs378-msk/Original Data"
setwd(mutwd)
intermediatefilename <- 'mergeddata.csv'
finalfilename <- 'concisedatatable.csv'

# # Reading raw mutation data file
# datamutation <- read.table('data_mutations_uniprot_revised.txt',sep = '\t',fill = TRUE,stringsAsFactors = FALSE,header = TRUE)
# 
# # Converting raw data into data.table type
# datamutation <- data.table(datamutation)
# 
# # Delete certain useless columns with no content
# deletecol <- c()
# colnamemut <- colnames(datamutation)
# for(i in ncol(datamutation):1){
#   if(nrow(datamutation[is.na(get(colnamemut[i]))]) == nrow(datamutation)){
#     deletecol <- c(deletecol,colnamemut[i])
#   }
# }
# print(deletecol)
# datamutation <- datamutation[,(deletecol) := NULL]
# 
# # Append sample information
# # Read Sample Information
# datasampleinfo <- read.table('data_clinical_sample.txt',sep = '\t',fill = TRUE,stringsAsFactors = FALSE,header = TRUE)
# datasampleinfo <- data.table(datasampleinfo)
# 
# #Merging data
# # set the ON clause as keys of the tables:
# setkey(datamutation,Tumor_Sample_Barcode)
# setkey(datasampleinfo,SAMPLE_ID)
# # perform the right outer join using the merge function
# mergeddata <- datamutation[datasampleinfo,nomatch = 0]
# 
# # To case list information folder
# clwd <- paste0(mutwd,'/case_lists')
# setwd(clwd)
# 
# # Get all cancer type information
# cancerlists <- list.files()
# cancerlists <- cancerlists[which(grepl('.txt',cancerlists))]
# IDdf = data.frame(matrix(ncol = 2,nrow =0))
# colnames(IDdf) <- c('Cancer Type','IDs')
# for(i in 1:length(cancerlists)){
#   # Get the file as string
#   tmpstring = readChar(cancerlists[i], file.info(cancerlists[i])$size)
#   # get the substring stating with Cancer type name
#   tmpstring = substr(tmpstring,regexpr('Tumor Type',tmpstring)[1]+12,nchar(tmpstring))
#   # get the substring of cancer type (name end with '\r')
#   tmpcancertype = substr(tmpstring,1,regexpr('\\r',tmpstring)[1]-1)
#   # get the substring of cancer IDs
#   tmpIDstring = substr(tmpstring,regexpr('case_list_ids',tmpstring)[1]+15,nchar(tmpstring))
#   tmpIDvector = strsplit(tmpIDstring,split = '\t')[[1]]
#   # Create data frame
#   tmpIDdf <- data.frame('Cancer Type' = rep(tmpcancertype,length(tmpIDvector)),'IDs' = tmpIDvector)
#   IDdf <- rbind(IDdf,tmpIDdf)
# }
# 
# IDdf$Cancer.Type <- as.character(IDdf$Cancer.Type)
# IDdf$IDs <- as.character(IDdf$IDs)
# IDdf <- data.table(IDdf)
# #View(table(IDdf$IDs)) # Every patientID correspond to one cancer type
# 
# # Back to mutation data
# setwd(mutwd)
# 
# # Enter all the Cancer Type info into the table by patient IDs
# mergeddata$CancerType = rep('',nrow(mergeddata))
# for(i in 1:nrow(mergeddata)){
#   if(i %% 50 == 0){
#     print(i)
#   }
#   mergeddata$CancerType[i] = IDdf[IDs == mergeddata$Tumor_Sample_Barcode[i]]$Cancer.Type
# }
# 
# # Found that one of the patients with Mastoccytosis does not have any mutations
# #mergect <- unique(mergeddata$CancerType)
# #orict <- unique(IDdf$Cancer.Type)
# #for(i in 1:length(orict)){
# #  if(!(orict[i] %in% mergect)){
# #    print(orict[i])
# #  }
# #}
# #mastocytosisptID <- IDdf[Cancer.Type == 'Mastocytosis']
# #print(mergeddata[Tumor_Sample_Barcode == 'P-0003114-T01-IM5'])
# 
# write.csv(mergeddata,file = intermediatefilename,row.names = FALSE)

mergeddatact <- read.csv(intermediatefilename,stringsAsFactors = FALSE, header = TRUE)
mergeddatact <- data.table(mergeddatact)
# Cleaning up data
# 1. Get rid of useless columns: 

#table(mergeddatact$NCBI_Build)
#table(mergeddatact$SAMPLE_COLLECTION_SOURCE) # useless, does not make biological sense to use it as category
#table(mergeddatact$SAMPLE_CLASS) # All tumors
## sample informations
#table(mergeddatact$SAMPLE_COVERAGE) 
#table(mergeddatact$SPECIMEN_PRESERVATION_TYPE)
#table(mergeddatact$SAMPLE_TYPE)
#table(mergeddatact$ONCOTREE_CODE) # identified cancer type code
#table(mergeddatact$TUMOR_PURITY)
#table(mergeddatact$DNA_INPUT)
#table(mergeddatact$SPECIMEN_TYPE)
#table(mergeddatact$MATCHED_STATUS)
#table(mergeddatact$Hotspot) # all 0s
mergeddatact <- mergeddatact[,c('NCBI_Build','SAMPLE_COLLECTION_SOURCE','SAMPLE_CLASS','SAMPLE_COVERAGE','SPECIMEN_PRESERVATION_TYPE','SAMPLE_TYPE', 'ONCOTREE_CODE','TUMOR_PURITY','DNA_INPUT','SPECIMEN_TYPE','MATCHED_STATUS','Hotspot') := NULL]
# 2. get rid of columns that are always the same: c('HGVSp_Short',)

# Reference_Allele == Tumor_Seq_Allele1
# length(which(mergeddatact$Reference_Allele == mergeddatact$Tumor_Seq_Allele1)) == nrow(mergeddatact) # if the two are equal in all columns
# MIGHT HAVE HOMOZYGOUS MUTATIONS SO DO NOT DELETE

# mergeddatact$HGVSp and mergeddatact$HGVSp_Short are the same meaning

# Some transcript ID does not correspond to HGV information
#identified = TRUE
#for(i in 1:nrow(mergeddatact)){
#  if(!grepl(pattern = mergeddatact$Transcript_ID[i],x = mergeddatact$HGVSc[i])){
#    print(i)
#    identified = FALSE
#    print('Transcript ID not found in HGVSc')
#    break
#  }
#}
mergeddatact <- mergeddatact[,c('HGVSp') := NULL]
# 3. getting rid of patient ID information
mergeddatact <- mergeddatact[,c('PATIENT_ID') := NULL]

# Process to 341 gene panel genes
gene341string <- readChar('data_gene_panel_impact341.txt', file.info('data_gene_panel_impact341.txt')$size)
gene341string <- substr(gene341string,regexpr('gene_list:\t',gene341string)[1]+11,nchar(gene341string))
gene341vector <- strsplit(gene341string,'\t')[[1]]

# get rid of genes not in the 341 panel
mergeddatact <- mergeddatact[-which(!(mergeddatact$Hugo_Symbol %in% gene341vector)),]

# First column is dicrete 341 levels (corresponding genes)
# Second column is discrete 23 levels (1-22, x)
# NO CLEANING NEEDED

# Third column
# > summary(mergeddatact$Start_Position)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 218480  22160680  49424970  68105287 110434513 244006429 
# Fourth column
# > summary(mergeddatact$End_Position)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 218480  22160680  49424970  68105289 110434513 244006429 
# NO CLEANING NEEDED

# Fifth + sixth column
# > table(mergeddatact$Strand)
# + 
#   25 72917 
# CLEANING NEEDED: mutation calls without strand/consequences/(HGVSc, HGVSp_Short,Transcript_ID) are the same
# Possible bad reads, going to delete by row indices.
which(mergeddatact$Strand == '')
which(mergeddatact$Consequence == '')
intersect(which(mergeddatact$HGVSc == ''),intersect(which(mergeddatact$HGVSp_Short == '' ),which(mergeddatact$Transcript_ID == '' )))
mergeddatact <- mergeddatact[-which(mergeddatact$Strand == '')]

# Seventh + eighth column
# > table(mergeddatact$Variant_Classification)
# 3Flank                   3UTR 
# 34                      1 
# 5Flank                   5UTR 
# 1366                     40 
# Frame_Shift_Del        Frame_Shift_Ins 
# 5924                   2365 
# In_Frame_Del           In_Frame_Ins 
# 1265                    266 
# Intron      Missense_Mutation 
# 57                  51345 
# Nonsense_Mutation       Nonstop_Mutation 
# 7476                     41 
# Silent          Splice_Region 
# 13                     1
# Splice_Site Translation_Start_Site 
# 2619                     92 
# > table(mergeddatact$Variant_Type)
# 
# DEL   DNP   INS   ONP   SNP 
# 7715   969  2755   132 61346 
# NO CLEANING NEEDED

# Ninth, tenth, eleventh column
# which(names(table(mergeddatact$Reference_Allele) ) == '')
# which(names(table(mergeddatact$Reference_Allele) ) == 'A')
# which(names(table(mergeddatact$Tumor_Seq_Allele1) ) == '')
# which(names(table(mergeddatact$Tumor_Seq_Allele1) ) == 'A')
# which(names(table(mergeddatact$Tumor_Seq_Allele2) ) == '')
# which(names(table(mergeddatact$Tumor_Seq_Allele2) ) == 'A')
# NO CLEANING NEEDED

# Twelfth is tumor ID

# Thirteenth and fourteenth column
which(!is.numeric(mergeddatact$t_ref_count))
which(!is.numeric(mergeddatact$t_alt_count))

# Fifteenth and sixteenth column needs no editing, but fill empty cell in with '-'
#mergeddatact[which(mergeddatact$HGVSc == '')] <- "-"
#mergeddatact[which(mergeddatact$HGVSp_Short == '')] <- "-"

# Seventeenth column needs no editing
# Eighteenth column needs to be deleted (refseq < ensembl ID (Transcript_ID))
# mergeddatact <- mergeddatact[,RefSeq := NULL]

# Nineteenth column

mergeddatact <- mergeddatact[,c(9,10,11,13,14,15,16,18,19,20,21) := NULL]

# Decide how many categories to discretize for continuous variables
categorynum <- 10
quantilevector <- as.character(c(1:categorynum))
mergeddatact$Start_Position <- discretize(mergeddatact$Start_Position,categories = categorynum,
                                          labels = quantilevector)
mergeddatact$End_Position <- discretize(mergeddatact$End_Position,categories = categorynum,
                                        labels = quantilevector)




# Making the final data table
# list of genes appeared in the results
uniquegenelist <- unique(mergeddatact$Hugo_Symbol)
# list of tumor ID appearring in the results
uniquetumorIDlist <- unique(mergeddatact$Tumor_Sample_Barcode)
# number of columns and rows
colnum <- (ncol(mergeddatact)-2)*length(uniquegenelist)+2
rownum <- length(uniquetumorIDlist)
# make the column names
tmpcolnames <- colnames(mergeddatact)
delindices <- which(tmpcolnames %in% c('Tumor_Sample_Barcode','CancerType'))
tmpcolnames <- tmpcolnames[-delindices]
finaldtcolnames <- rep('',0)
for (i in 1:length(uniquegenelist)) {
  finaldtcolnames <- c(finaldtcolnames,paste0(uniquegenelist[i],'-',tmpcolnames))
}
finaldtcolnames <- c(finaldtcolnames,'Sample_Barcode','Cancer Type')
# Make final dt
finaldt <- data.frame(matrix(ncol = colnum,nrow = rownum))
colnames(finaldt) <- finaldtcolnames
finaldt$Sample_Barcode <- uniquetumorIDlist

for(i in 1:nrow(finaldt)){
  tmpsampledt <- mergeddatact[Tumor_Sample_Barcode == finaldt$Sample_Barcode[i]]
  finaldt$`Cancer Type`[i] = tmpsampledt$CancerType[1]
  for (j in 1:nrow(tmpsampledt)){
    tmpmutvector <- as.character(tmpsampledt[j,])[-delindices]
    indicies = (which(uniquegenelist == tmpmutvector[1])-1)*length(tmpmutvector)
    finaldt[i,(indicies+1):(indicies+length(tmpmutvector))] <- tmpmutvector
  }
}

write.csv(finaldt,file = finalfilename,row.names = FALSE)

