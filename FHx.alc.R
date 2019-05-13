rm(list=ls())

source("fcn.R")

############################################################
#main
############################################################
require(openxlsx)
library(stringr)
dat.source=1; #0:data dump ; 1:nda package; both data source should land to the same outcomes


if(dat.source==0){
	year.month = ""
	my.path="~/Desktop/ABCD/Downloads/";
	my.MH.sheet.name="ABCD Release 2_DataSharing2018_Mental Health"

	#P_Family History Assessment Parent(survey): only happens at baseline

	sheet.name.HMH=loadWorkbook(paste0(my.path,year.month,my.MH.sheet.name,".xlsx"))

	#which sheet has "P Family History"?
	indx.Fhx=which(names(sheet.name.HMH)=="P Family History")


	dat = read.xlsx(paste0(my.path,year.month,my.MH.sheet.name,".xlsx"), sheet = indx.Fhx)
	dat.bl = dat[dat$redcap_event_name == "baseline_year_1_arm_1",]
	rm.indx=which(duplicated(colnames(dat.bl)))
	dat.bl=dat.bl[,-rm.indx]
	rm.indx=which(grepl("fhx_ss_",colnames(dat.bl))) #rm some old variables
	dat.bl=dat.bl[,-rm.indx]
	var.names=colnames(dat.bl)
	
	dic = read.xlsx(paste0(my.path,year.month,my.MH.sheet.name,".xlsx"), sheet = 1) #dictionary
	#dic.Fhx=dic[which(dic[,"field_name"]%in%colnames(dat)),]

	}else{

	############################################################
	fhx = read.csv(file = "~/Desktop/ABCD/analysis-nda18/notebooks/general/data/ABCDstudyNDA/fhxp102.txt", sep = '\t',header = TRUE, 	row.names=NULL, comment.char = "", quote="", check.names=FALSE)
	fhx = as.data.frame(sapply(fhx, function(x) gsub("\"", "", x)))
    names(fhx) = as.list(sapply(names(fhx), function(x) gsub("\"", "", x)))
    
	fhx.1=fhx[-1,]


	fhx = read.csv(file = "~/Desktop/ABCD/analysis-nda18/notebooks/general/data/ABCDstudyNDA/fhxp201.txt", sep = '\t',header = TRUE, 	row.names=NULL, comment.char = "", quote="", check.names=FALSE)
	fhx = as.data.frame(sapply(fhx, function(x) gsub("\"", "", x)))
    names(fhx) = as.list(sapply(names(fhx), function(x) gsub("\"", "", x)))
    
	fhx.2=fhx[-1,-c(1,2,3,4,6,7,8,9,10)]   

	dat.bl=merge(fhx.1,fhx.2,by="src_subject_id")
	var.names=colnames(dat.bl)

}
############################################################




threshold=1;
prob_cate="alc"
prob_cate_dic="alcohol"
############################################################
##For parents and grandparents
############################################################
shared.var="famhx_1"
master.var="famhx_4_p";
Input.vars.all<-c(shared.var,master.var)
p.gp.sub.var.header.vec=c("famhx4a_p","famhx4b_p","famhx_4c_p","famhx_4d_p","famhx_4e_p","famhx_4f_p")
p.gp.cate.vec=c("fath","patgf","patgm","moth","matgf","matgm")
num.sub.var=7


for(j in 1:length(p.gp.cate.vec)){
	dat.obj=parent.grandparent.fcn(dat.bl,shared.var,master.var,p.gp.sub.var.header.vec[j],p.gp.cate.vec[j], threshold,num.sub.var,prob_cate,dat.source=dat.source)
	dat.bl=dat.obj$my.dat
	Input.vars.all=c(Input.vars.all,dat.obj$Input.vars)
}

####################################################################################################
##For uncles and aunts, including 5 pat uncles, 5 pat aunts, 5 mat uncles and 5 mat aunts, total 20;
####################################################################################################
master.var="famhx_4_p";
num.sub.var=7;
num.sub.var.2=5;
sub.var.vec=c(rep("famhx_2a",num.sub.var.2),rep("famhx_2b",num.sub.var.2),rep("famhx_1a",num.sub.var.2),rep("famhx_1b",num.sub.var.2))


ua.sub.var.header.vec=paste0(c("q4g_pat_uncle1_","q4g_pat_uncle2_","q4g_pat_uncle3_","q4g_pat_uncle4_","q4g_pat_uncle5_",
"q4h_pat_aunt1_","q4h_pat_aunt2_","q4h_pat_aunt3_","q4h_pat_aunt4_","q4h_pat_aunt5_",
"q4i_mat_uncle1_","q4i_mat_uncle2_","q4i_mat_uncle3_","q4i_mat_uncle4_","q4i_mat_uncle5_",
"q4j_mat_aunt1_","q4j_mat_aunt2_","q4j_mat_aunt3_","q4j_mat_aunt4_","q4j_mat_aunt5_"),prob_cate)




ua.cate.vec=c("patunc1","patunc2","patunc3","patunc4","patunc5","patant1","patant2","patant3","patant4","patant5","matunc1","matunc2","matunc3","matunc4","matunc5","matant1","matant2","matant3","matant4","matant5")

for(j in 1:length(ua.cate.vec)){

	dat.obj=uncle.aunt.sib.fcn(dat.bl,shared.var,master.var,ua.sub.var.header.vec[j],sub.var.vec[j],ua.cate.vec[j], threshold, num.sub.var,prob_cate,dat.source=dat.source)
	dat.bl=dat.obj$my.dat
	Input.vars.all=c(Input.vars.all,dat.obj$Input.vars)
	
}



################################################################################
##For siblings: 5 full younger; 5 full older; 5 half younger; 5 half older; total:20
################################################################################
master.var="famhx_4_p";
num.sub.var=7;
num.sub.var.2=5;
sub.var.vec=c(rep("fhx_3f_younger",num.sub.var.2),rep("fhx_3f_older",num.sub.var.2),rep("fhx_3h_younger",num.sub.var.2),rep("fhx_3h_older",num.sub.var.2))

sib.sub.var.header.vec=paste0(c("q4k_full_sib_young1_","q4k_full_sib_young2_","q4k_full_sib_young3_","q4k_full_sib_young4_","q4k_full_sib_young5_","q4l_full_sib_old1_","q4l_full_sib_old2_","q4l_full_sib_old3_","q4l_full_sib_old4_","q4l_full_sib_old5_",
"q4m_half_sib_young1_","q4m_half_sib_young2_","q4m_half_sib_young3_","q4m_half_sib_young4_","q4m_half_sib_young5_",
"q4n_half_sib_old1_","q4n_half_sib_old2_","q4n_half_sib_old3_","q4n_half_sib_old4_","q4n_half_sib_old5_"),prob_cate)


sib.cate.vec=c("fulsiby1","fulsiby2","fulsiby3","fulsiby4","fulsiby5","fulsibo1","fulsibo2","fulsibo3","fulsibo4","fulsibo5",
"hlfsiby1","hlfsiby2","hlfsiby3","hlfsiby4","hlfsiby5","hlfsibo1","hlfsibo2","hlfsibo3","hlfsibo4","hlfsibo5")

for(j in 1:length(sib.cate.vec)){
	print(j)
	dat.obj=uncle.aunt.sib.fcn(dat.bl,shared.var,master.var,sib.sub.var.header.vec[j],sub.var.vec[j],sib.cate.vec[j], threshold, num.sub.var,prob_cate,dat.source=dat.source)
	dat.bl=dat.obj$my.dat
	Input.vars.all=c(Input.vars.all,dat.obj$Input.vars)
}

Input.vars.all=unique(Input.vars.all);
############################################################
#Either parent problem? dichotomous 
############################################################
#0:No; 1:Yes
dat.bl[,paste0("momdad_",prob_cate)]=NA;
dat.bl[which(dat.bl[,paste0("moth_prob_",prob_cate)]==0 & dat.bl[,paste0("fath_prob_",prob_cate)]==0),paste0("momdad_",prob_cate)]=0;
dat.bl[which(dat.bl[,paste0("moth_prob_",prob_cate)]==1 | dat.bl[,paste0("fath_prob_",prob_cate)]==1),paste0("momdad_",prob_cate)]=1
#Input.vars.all=c(Input.vars.all,paste0("fhx_ss_momdad_",prob_cate))

table(dat.bl[,paste0("momdad_",prob_cate)],useNA="ifany")
table(dat.bl[,paste0("moth_prob_",prob_cate)],useNA="ifany")
table(dat.bl[,paste0("fath_prob_",prob_cate)],useNA="ifany")

############################################################
#Overal parental problem
#0:none; 1:dad only; 2:mom only; 3:both;
#-1:dad missing; -2:mom missing
############################################################
dat.bl[,paste0("parent_",prob_cate)]=NA;
dat.bl[,paste0("parent_",prob_cate)][which(dat.bl[,paste0("moth_prob_",prob_cate)]==0 & dat.bl[,paste0("fath_prob_",prob_cate)]==0)]=0
dat.bl[,paste0("parent_",prob_cate)][which(dat.bl[,paste0("moth_prob_",prob_cate)]==0 & dat.bl[,paste0("fath_prob_",prob_cate)]==1)]=1
dat.bl[,paste0("parent_",prob_cate)][which(dat.bl[,paste0("moth_prob_",prob_cate)]==1 & dat.bl[,paste0("fath_prob_",prob_cate)]==0)]=2
dat.bl[,paste0("parent_",prob_cate)][which(dat.bl[,paste0("moth_prob_",prob_cate)]==1 & dat.bl[,paste0("fath_prob_",prob_cate)]==1)]=3


dat.bl[,paste0("parent_",prob_cate)][which(is.na(dat.bl[,paste0("moth_prob_",prob_cate)]) & dat.bl[,paste0("fath_prob_",prob_cate)]==1)]=-2;
dat.bl[,paste0("parent_",prob_cate)][which(dat.bl[,paste0("moth_prob_",prob_cate)]==1 & is.na(dat.bl[,paste0("fath_prob_",prob_cate)]))]=-1




table(dat.bl[,paste0("parent_",prob_cate)],useNA="ifany")


new.dic.vars=colnames(dat.bl)[which(!colnames(dat.bl)%in%var.names)]

########################################################################################################################
ss.dic=make.dic(paste0("fhx_ss_",new.dic.vars),prob_cate,prob_cate_dic)
save(file=paste0("fhx_ss_",prob_cate,"_dic.RData"),ss.dic)

if(dat.source==0){
	dat.bl.ss=dat.bl[,c("id_redcap","redcap_event_name",new.dic.vars)]
	colnames(dat.bl.ss)[-c(1,2)]=paste0("fhx_ss_",new.dic.vars)
	save(file=paste0("fhx_ss_dump_",prob_cate,".RData"),dat.bl.ss)
}else{
	dat.bl.ss=dat.bl[,c("src_subject_id","eventname",new.dic.vars)]
	colnames(dat.bl.ss)[-c(1,2)]=paste0("fhx_ss_",new.dic.vars)
	save(file=paste0("fhx_ss_nda_",prob_cate,".RData"),dat.bl.ss)
}



for(i in 3:dim(dat.bl.ss)[2]){
	print("############################")
	print(table(dat.bl.ss[,i],useNA="ifany",dnn=colnames(dat.bl.ss)[i]))
}


