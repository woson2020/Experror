#-*-coding:utf-8 -*-
#2020-12-03
#Pacbio_sequence analysis , find illumina_sequencing umi
from sys import argv
import regex
input1,input2,output1=argv[1:]
out1=open(output1,"w")
#barcode(20bp)+index(5bp),actually,only the barcode(20bp) is useful in subsequent analysis
f_re="(?e)(?P<p1>TCGCTCTTATTGACCACACC){e<=1}(?P<umi_f>[ATCG]{25})(?P<p2>GCTTCGGCAGCACATATACT){e<=1}"
r_re="(?e)(?P<p3>AGTATATGTGCTGCCGAAGC){e<=1}(?P<umi_r>[ATCG]{25})(?P<p4>GGTGTGGTCAATAAGAGCGA){e<=1}"


mud_dict={"A":"T","T":"A","C":"G","G":"C","N":"N"}
reverComple=lambda s: "".join([mud_dict[c] for c in s])[::-1]


with open(input1,"r") as r1,open(input2,"r") as r2:
    for i,j in enumerate(zip(r1,r2)):      
        if i % 4==1 :
            j0=j[0].strip()
            j1=j[1].strip()
            f_pre=regex.search(f_re,j0,flags=0)
            r_pre=regex.search(r_re,j1,flags=0)
            if f_pre is not None and r_pre is not None:
                f=f_pre.groupdict()["umi_f"]
                r=reverComple(r_pre.groupdict()["umi_r"])
                if f==r:#forward strand=reverse strand
                    out1.write(f[:20]+"\n")
out1.close()

