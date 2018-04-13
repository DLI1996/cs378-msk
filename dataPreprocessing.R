#library for sql based data table
library(data.table)

# Setting working directory
setwd("D:/docs/Spring 2018/CS378/Git378/cs378-msk/Original Data")

# Reading raw mutation data file
datamutation <- read.table('data_mutations_uniprot_revised.txt',sep = '\t',fill = TRUE,stringsAsFactors = FALSE,header = TRUE)

# Converting raw data into data.table type
#datam  utation <- data.table(datamutation)

# Change column names
colnames(datamutation) <- as.character(unlist(datamutation[1,]))
# Delete first row
datamutation <- datamutation[-1,]

# Delete certain useless columns with no content
deletecol <- c()
colnamemut <- colnames(datamutation)
for(i in ncol(datamutation):1){
  if(nrow(datamutation[get(colnamemut[i]) == ""]) == nrow(datamutation)){
    deletecol <- c(deletecol,colnamemut[i])
  }
}
datamutation <- datamutation[,(deletecol) := NULL]

tmp <- datamutation[1:(nrow(datamutation)-1),]

# Append sample information
# Read Sample Information
datasampleinfo <- read.table('data_clinical_sample.txt',sep = '\t',fill = TRUE)
datasampleinfo <- data.table(datasampleinfo)
# Change column names
colnames(datasampleinfo) <- as.character(unlist(datasampleinfo[1,]))
# Delete first row
datasampleinfo <- datasampleinfo[-1,]

#Merging data
# set the ON clause as keys of the tables:
setkey(datamutation,Tumor_Sample_Barcode)
setkey(datasampleinfo,SAMPLE_ID)
# perform the join using the merge function
#Result <- merge(datamutation,datasampleinfo, all.x=TRUE)

mergeddata <- datamutation[datasampleinfo,nomatch = 0]


grep('\n','5Flank\tSNP\tG\tG\tA\t\t\tP-0012381-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t166\t92\t\t\t\t\t\tENST00000310581\tNM_198253.2\t\t\t0\nFGF4\t\t\tGRCh37\t11\t69589783\t69589783\t+\tmissense_variant\tMissense_Mutation\tSNP\tC\tC\tT\t\t\tP-0012381-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t81\t93\t\t\tENST00000168712.1:c.70G>A\tp.Ala24Thr\tp.A24T\tENST00000168712\tNM_002007.2\t24\tGcg/Acg\t0\nAPC\t\t\tGRCh37\t5\t112175211\t112175211\t+\tmissense_variant\tMissense_Mutation\tSNP\tT\tT\tA\t\t\tP-0012381-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t409\t166\t\t\tENST00000257430.4:c.3920T>A\tp.Ile1307Lys\tp.I1307K\tENST00000257430\tNM_000038.5\t1307\taTa/aAa\t0\nTSC1\t\t\tGRCh37\t9\t135786451\t135786451\t+\tmissense_variant\tMissense_Mutation\tSNP\tG\tG\tT\t\t\tP-0012381-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t426\t371\t\t\tENST00000298552.3:c.1079C>A\tp.Thr360Asn\tp.T360N\tENST00000298552\tNM_001162426.1\t360\taCt/aAt\t0\nPOLE\t\t\tGRCh37\t12\t133244133\t133244133\t+\tmissense_variant\tMissense_Mutation\tSNP\tG\tG\tA\t\t\tP-0012381-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t280\t259\t\t\tENST00000320574.5:c.2275C>T\tp.Arg759Cys\tp.R759C\tENST00000320574\tNM_006231.2\t759\tCgt/Tgt\t0\nPLK2\t\t\tGRCh37\t5\t57751900\t57751900\t+\tmissense_variant\tMissense_Mutation\tSNP\tC\tC\tG\t\t\tP-0012381-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t1096\t542\t\t\tENST00000274289.3:c.1337G>C\tp.Arg446Pro\tp.R446P\tENST00000274289\tNM_006622.3\t446\tcGg/cCg\t0\nCIC\t\t\tGRCh37\t19\t42797116\t42797116\t+\tmissense_variant\tMissense_Mutation\tSNP\tA\tA\tG\t\t\tP-0012381-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t328\t300\t\t\tENST00000575354.2:c.3478A>G\tp.Met1160Val\tp.M1160V\tENST00000575354\tNM_015125.3\t1160\tAtg/Gtg\t0\nEP300\t\t\tGRCh37\t22\t41551083\t41551083\t+\tmissense_variant\tMissense_Mutation\tSNP\tG\tG\tA\t\t\tP-0012381-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t554\t492\t\t\tENST00000263253.7:c.3227G>A\tp.Arg1076His\tp.R1076H\tENST00000263253\tNM_001429.3\t1076\tcGt/cAt\t0\nNCOA3\t\t\tGRCh37\t20\t46279816\t46279836\t+\tinframe_deletion\tIn_Frame_Del\tDEL\tCAGCAGCAGCAGCAGCAGCAA\tCAGCAGCAGCAGCAGCAGCAA\t-\t\t\tP-0012381-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t275\t93\t\t\tENST00000371998.3:c.3762_3782delACAGCAGCAGCAGCAGCAGCA\tp.Gln1270_Gln1276del\tp.Q1270_Q1276del\tENST00000371998\t\t1248\tCAGCAGCAGCAGCAGCAGCAA/-\t0\nB2M\t\t\tGRCh37\t15\t45003764\t45003764\t+\tstop_gained\tNonsense_Mutation\tSNP\tT\tT\tG\t\t\tP-0012524-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t414\t263\t\t\tENST00000558401.1:c.20T>G\tp.Leu7Ter\tp.L7*\tENST00000558401\tNM_004048.2\t7\ttTa/tGa\t0\nCREBBP\t\t\tGRCh37\t16\t3786704\t3786704\t+\tmissense_variant\tMissense_Mutation\tSNP\tA\tA\tT\t\t\tP-0012524-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t1030\t478\t\t\tENST00000262367.5:c.4507T>A\tp.Tyr1503Asn\tp.Y1503N\tENST00000262367\tNM_004380.2\t1503\tTac/Aac\t0\nTNFAIP3\t\t\tGRCh37\t6\t138192661\t138192661\t+\tsplice_donor_variant\tSplice_Site\tSNP\tT\tT\tG\t\t\tP-0012524-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t572\t106\t\t\tENST00000237289.4:c.295+2T>G\t\tp.X99_splice\tENST00000237289\tNM_001270507.1\t\t\t0\nCHEK1\t\t\tGRCh37\t11\t125523718\t125523718\t+\tmissense_variant\tMissense_Mutation\tSNP\tA\tA\tG\t\t\tP-0012524-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t568\t394\t\t\tENST00000428830.2:c.1311A>G\tp.Ile437Met\tp.I437M\tENST00000428830\tNM_001114121.2\t437\tatA/atG\t0\nPALB2\t\t\tGRCh37\t16\t23646306\t23646306\t+\tmissense_variant\tMissense_Mutation\tSNP\tT\tT\tC\t\t\tP-0012524-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t1022\t423\t\t\tENST00000261584.4:c.1561A>G\tp.Thr521Ala\tp.T521A\tENST00000261584\tNM_024675.3\t521\tAcc/Gcc\t0\nANKRD11\t\t\tGRCh37\t16\t89350849\t89350849\t+\tmissense_variant\tMissense_Mutation\tSNP\tT\tT\tA\t\t\tP-0012524-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t1508\t630\t\t\tENST00000301030.4:c.2101A>T\tp.Ser701Cys\tp.S701C\tENST00000301030\tNM_001256183.1\t701\tAgc/Tgc\t0\nBCL2\t\t\tGRCh37\t18\t60985767\t60985767\t+\tmissense_variant\tMissense_Mutation\tSNP\tC\tC\tT\t\t\tP-0012524-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t429\t169\t\t\tENST00000333681.4:c.133G>A\tp.Ala45Thr\tp.A45T\tENST00000333681\t\t45\tGca/Aca\t0\nBCL2\t\t\tGRCh37\t18\t60985808\t60985808\t+\tmissense_variant\tMissense_Mutation\tSNP\tT\tT\tC\t\t\tP-0012524-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t595\t233\t\t\tENST00000333681.4:c.92A>G\tp.Asp31Gly\tp.D31G\tENST00000333681\t\t31\tgAt/gGt\t0\nTNFRSF14\t\t\tGRCh37\t1\t2489268\t2489269\t+\tframeshift_variant\tFrame_Shift_Ins\tINS\t-\t-\tTCCA\t\t\tP-0012524-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t447\t267\t\t\tENST00000355716.4:c.174_177dupTCCA\tp.Gly60SerfsTer18\tp.G60Sfs*18\tENST00000355716\tNM_003820.2\t58\tagt/agTCCAt\t0\nTNFAIP3\t\t\tGRCh37\t6\t138196871\t138196872\t+\tframeshift_variant\tFrame_Shift_Del\tDEL\tAC\tAC\t-\t\t\tP-0012524-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t868\t204\t\t\tENST00000237289.4:c.537_538delAC\tp.Pro180HisfsTer73\tp.P180Hfs*73\tENST00000237289\tNM_001270507.1\t178\tgAC/g\t0\nMLL2\t\t\tGRCh37\t12\t49440414\t49440415\t+\tframeshift_variant\tFrame_Shift_Ins\tINS\t-\t-\tG\t\t\tP-0012524-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t596\t245\t\t\tENST00000301067.7:c.4395dupC\tp.Lys1466GlnfsTer25\tp.K1466Qfs*25\tENST00000301067\tNM_003482.3\t1465\t-/C\t0\nCDKN2A\t\t\tGRCh37\t9\t21971170\t21971170\t+\tmissense_variant\tMissense_Mutation\tSNP\tA\tA\tT\t\t\tP-0005919-T01-IM5\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t153\t59\t\t\tENST00000304494.5:c.188T>A\tp.Leu63Gln\tp.L63Q\tENST00000304494\tNM_000077.4\t63\tcTg/cAg\t0\n')

View(head(mergeddata))
View(head(datamutation))
