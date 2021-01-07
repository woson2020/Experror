##TG:TDH3-GFP; AG:AGP1-GFP; TU:TDH3-URA3; AU:AGP1-URA3.
1) TG_PacBio_genotype_barcode_calling.py, TU_PacBio_genotype_barcode_calling.py,  AG_PacBio_genotype_barcode_calling.py and 
 AU_PacBio_genotype_barcode_calling.py are used to call barcode and its corresponding genotype according to PacBio sequencing data. 
For TG and TU library, we have sequenced three PacBio SMRT cells (PacBio sequel I) , each cells must run with relevant script and then combine 
those three cells' result to generate a summary file. However, for AU and AG library,we adopted PacBio sequel II which has a higher throughput
than  PacBio sequel I (about 8-fold). In this situation, AU and AG are combined into one cell for sequencing, sequencing mapping was used to 
discriminate them.

#######
Ref_GFP.fasta and Ref_URA3.fasta are the wildtype sequence of GFP and URA3, respectively, which are necessary for TG_PacBio_genotype_barcode_calling.py, 
TU_PacBio_genotype_barcode_calling.py, AG_PacBio_genotype_barcode_calling.py and AU_PacBio_genotype_barcode_calling.py to call barcode and its corresponding genotype.

2) GFP_filter_paired_with_multiple.py (suitable for both TG and AG) and URA3_filter_paired_with_multiple.py (suitable for both TU and AU) are used to
filter genotype when a barcode corresponds to multiple genotypes.
The file from 1) is the input of this script, the output is  genotype and all its unique barcode.

3)TG_TU_illumina_barcode_calling.py (suitable for both TG and TU) and AG_AU_illumina_barcode_calling.py (suitable for both AG and AU) are used to 
call barcode according to Illumina sequencing data (from competitive culture).
Through this script, the frequency of various barcode was got and then use to calculate fitness.

