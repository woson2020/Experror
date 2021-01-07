#-*-coding:utf-8 -*-
from sys import argv
import os
import re
import multiprocessing
"""
Since the the data from PacBio sequel II is so big, it is difficult to build 
a subreads-dict within memory,so we adopted a strategy that build the generator of big file to split positive
strand and negative strand.
"""
raw_bam,logfile,threads,ref_file=argv[1:]

os.system("blasr --bam --out %s.blasr.bam %s %s"%(raw_bam[:-4],raw_bam,ref_file))#mapping to the wild-type of gfp
os.system("samtools view -h -o %s.blasr.sam %s.blasr.bam"%(raw_bam[:-4],raw_bam[:-4]))#tranform bam file to sam file 
os.system("samtools view -h -o %s.sam %s"%(raw_bam[:-4],raw_bam))

p_file=open("%s.p.sam"%(raw_bam[:-4]),"w")#positive strand
n_file=open("%s.n.sam"%(raw_bam[:-4]),"w")#negative strand
log_file=open(logfile,"w")

with open("%s.sam"%(raw_bam[:-4]),"r") as  sam_file:#get the header
    for hed in sam_file:
        if hed[0]!="m":
            p_file.write(hed)
            n_file.write(hed)
        else:
            break
def file(input_bam):#build generator of raw subreads-bam file
    with open(input_bam,"r") as bam:
        bam_dict={}
        count=0
        zmv1=""
        while True:
            try:
                l_1=next(bam).strip()
                if l_1[0]=="m":
                    hole_all=l_1.strip().split()[0]#name
                    hole_only=hole_all.split("/")[1]#ZMW hole
                    if count>0 and hole_only!=zmv1:
                        yield bam_dict, zmv1
                        bam_dict={}
                        bam_dict[hole_all]=l_1
                        zmv1=hole_only
                    else:
                        bam_dict[hole_all]=l_1
                        zmv1=hole_only
                        count+=1
            except StopIteration:
                yield bam_dict, zmv1
                print("Bam file is end")
                break
def b_file(input_blasr):#build generator of raw subreads-bam file
    with open(input_blasr,"r") as bam:
        bam_dict={}
        count=0
        zmv1=""
        while True:
            try:
                l_1=next(bam).strip()
                if l_1[0]=="m":
                    hole_all=l_1.split()[0]#name
                    hole_only=hole_all.split("/")[1]#hole
                    if count>0 and hole_only!=zmv1:
                        yield bam_dict, zmv1
                        bam_dict={}
                        bam_dict[hole_all]=l_1.split()[1]
                        zmv1=hole_only
                    else:
                        bam_dict[hole_all]=l_1.split()[1]
                        zmv1=hole_only
                        count+=1
            except StopIteration:
                yield bam_dict, zmv1
                print("Blasr-bam file is end")
                break

mud_dict={"A":"T","T":"A","C":"G","G":"C"}
Reverse_complemrnt= lambda x:"".join([mud_dict[i] for i in x][::-1])

bam_file=file("%s.sam"%(raw_bam[:-4]))
blasr_file=b_file("%s.blasr.sam"%(raw_bam[:-4]))
c=0
while True:
    try:
        bam_subdict,zmv1=next(bam_file)
        blasr_subdict,zmv2=next(blasr_file)
        c+=1
        while zmv1!=zmv2:
            log_file.write(">"+zmv1+"\n")
            for ii in bam_subdict.keys():
                log_file.write(bam_subdict[ii]+"\n")
            bam_subdict,zmv1=next(bam_file)
        if zmv1==zmv2:
            for i in blasr_subdict:
                if blasr_subdict[i]=="16":
                    n_file.write(bam_subdict[i]+"\n")
                elif blasr_subdict[i]=="0":
                    p_file.write(bam_subdict[i]+"\n")
        else:
            print("error")
    except StopIteration:
        print("bam is end",zmv1,zmv2)
        break

p_file.close()
n_file.close()
log_file.close()

def run_ccs(name):
    os.system("samtools view -bS %s.sam -o %s.bam"%(name[:-4],name[:-4]))
    #plz pay attention to the number of thread
    os.system("ccs --min-length 900 --max-length 1600 -j %s --min-passes 5  %s.bam %s_ccs.bam"%(threads,name[:-4],name[:-4]))
    os.system("samtools view -h -o %s_ccs.sam %s_ccs.bam"%(name[:-4],name[:-4]))

#run ccs of positive strand set and negative strand set
p_n=[raw_bam[:-4]+".p.sam",raw_bam[:-4]+".n.sam"]
multiple_p=multiprocessing.Pool(2)
for file in p_n:
    ress=multiple_p.apply_async(run_ccs,args=(file,))
     
print('Waiting for all subprocesses done...')
multiple_p.close()
multiple_p.join()
print('All subprocesses done.')

#remove intermediate file
os.system("echo "" > *%s.n.bam|rm *%s.n.bam"%(raw_bam[:-4]),raw_bam[:-4])
os.system("echo "" > *%s.p.bam|rm *%s.p.bam"%(raw_bam[:-4],raw_bam[:-4]))
os.system("echo "" > *%s.n.sam|rm *%s.n.sam"%(raw_bam[:-4],raw_bam[:-4]))
os.system("echo "" > *%s.p.sam|rm *%s.p.sam"%(raw_bam[:-4],raw_bam[:-4]))

geno_pattern=re.compile("GACAAAGATAGCTTCGCACA\w{804}ATAAGCGAATTTCTTATGAT")
umi_pattern=re.compile("TCGCTCTTATTGACCACACC\w{20}GCTTCGGCAGCACATATACT")
outfile=open("%s.final_result"%(raw_bam[:-4]),"w")
with open("%s.n_ccs.sam"%(raw_bam[:-4]),"r") as inputr:#nagetive strand ccs result
    for nega in inputr:    
        if nega[0]=="m":
            nega_line_list=nega.strip().split()
            nega_seq=nega_line_list[9]#the result sequence after ccs 
            zmw_nega=nega_line_list[0].split("/")[1]#the number of ZMW
            nega_reverse=Reverse_complemrnt(nega_seq)
            geno2=re.search(geno_pattern,nega_reverse,flags=0)
            umi2=re.search(umi_pattern,nega_reverse,flags=0)
            if geno2 is not None and umi2 is not None:
                geno3 = geno2.group()[20:824]
                umi3 = umi2.group()[20:40]
                outfile.write(">"+str(zmw_nega)+"n"+"\n"+"n_"+geno3+" "+umi3+"\n")
with open("%s.p_ccs.sam"%(raw_bam[:-4]),"r") as inputf:#positive strand ccs result      
    for posi in inputf:
        if posi[0]=="m":
            posi_line_list=posi.strip().split()
            posi_seq=posi_line_list[9]
            zmw_posi=posi_line_list[0].split("/")[1]
            geno=re.search(geno_pattern,posi_seq,flags=0)
            umi=re.search(umi_pattern,posi_seq,flags=0)
            if geno is not None and umi is not None:
                geno1 = geno.group()[20:824]
                umi1 = umi.group()[20:40]
                outfile.write(">"+str(zmw_posi)+"p"+"\n"+"p_"+geno1+" "+umi1+"\n")
                

os.system("rm *%s.*_ccs.bam.pbi"%(raw_bam[:-4]))
os.system("rm *%s.*_ccs.sam"%(raw_bam[:-4]))
outfile.close()