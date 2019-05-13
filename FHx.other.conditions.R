rm(list=ls())
setwd("~/Desktop/ABCD/FamHx/R.code/")
source("fcn.R")
############################################################
#main
############################################################
require(openxlsx)
library(stringr)
############################################################
dat.source=1; #0:data dump; 1:nda package; both source should land to the same output


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
	var.names=colnames(dat.bl)
	rm.indx=which(duplicated(var.names))
	dat.bl=dat.bl[,-rm.indx]
	############################################################
	indx=which(colnames(dat.bl)=="q8i_pat_uncle2_visions") #typo in variable name
	if(length(indx)>0)
		colnames(dat.bl)[indx]="q8i_mat_uncle2_visions"
	############################################################
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
    
	fhx.2=fhx[-1,-c(1,2,3,4,6,7,8,9)]   

	dat.bl=merge(fhx.1,fhx.2,by="src_subject_id")
	###############################################################
	indx=which(colnames(dat.bl)=="q8i_pat_uncle2_visions") #typo in variable name
	if(length(indx)>0)
		colnames(dat.bl)[indx]="q8i_mat_uncle2_visions"		
	###############################################################	
	#for "professional", ndar changed the variable name to "_prof" instead of "_professional" for all siblings;
	sib.chg.var=colnames(dat.bl)[which(grepl("q11k|q11l|q11m|q11n",colnames(dat.bl)))]
	sib.chg.var=sib.chg.var[!grepl("same",sib.chg.var)]
	indx=which(colnames(dat.bl)%in%sib.chg.var)
	colnames(dat.bl)[indx]=gsub("_prof","_professional",sib.chg.var)
	###############################################################
	#for "hospitalized", ndar changed the variable name to "_hosp" instead of "_hospitalized" for all siblings;
	sib.chg.var=colnames(dat.bl)[which(grepl("q12k|q12l|q12m|q12n",colnames(dat.bl)))]
	sib.chg.var=sib.chg.var[!grepl("same",sib.chg.var)]
	indx=which(colnames(dat.bl)%in%sib.chg.var)
	colnames(dat.bl)[indx]=gsub("_hosp","_hospitalized",sib.chg.var)
	###############################################################
	var.names=colnames(dat.bl)

}
orig.var.names=var.names
########################################################################################################################
prob_cate_vec=c("depression","mania","visions","trouble","nerves","professional","hospitalized","suicide")
prob_cate_dic_vec=c("depression","mania","visions of others spying/plotting","trouble holds job/fights/police","nerves/nervous breakdown","to professional due to emotional/mental","hospitalized due to emotional/mental","suicide")

p.gp.sub.var.header.vec_list=list()
p.gp.sub.var.header.vec_list[[1]]=c(paste0(c("fam_history_q6a_","fam_history_q6b_","fam_history_q6c_","fam_history_q6d_","fam_history_q6e_","fam_history_q6f_"),prob_cate_vec[1]),"fam_history_6_yes_no")

p.gp.sub.var.header.vec_list[[2]]=c(paste0(c("fam_history_q7a_","fam_history_q7b_","fam_history_q7c_","fam_history_q7d_","fam_history_q7e_","fam_history_q7f_"),prob_cate_vec[2]),"fam_history_7_yes_no")

p.gp.sub.var.header.vec_list[[3]]=c(paste0(c("fam_history_q8a_","fam_history_q8b_","fam_history_q8c_","fam_history_q8d_","fam_history_q8e_","fam_history_q8f_"),prob_cate_vec[3]),"fam_history_8_yes_no")

p.gp.sub.var.header.vec_list[[4]]=c(paste0(c("fam_history_q9a_","fam_history_q9b_","fam_history_q9c_","fam_history_q9d_","fam_history_q9e_","fam_history_q9f_"),prob_cate_vec[4]),"fam_history_9_yes_no")

p.gp.sub.var.header.vec_list[[5]]=c(paste0(c("fam_history_q10a_","fam_history_q10b_","fam_history_q10c_","fam_history_q10d_","fam_history_q10e_","fam_history_q10f_"),prob_cate_vec[5]),"fam_history_10_yes_no")

p.gp.sub.var.header.vec_list[[6]]=c(paste0(c("fam_history_q11a_","fam_history_q11b_","fam_history_q11c_","fam_history_q11d_","fam_history_q11e_","fam_history_q11f_"),prob_cate_vec[6]),"fam_history_11_yes_no")

p.gp.sub.var.header.vec_list[[7]]=c(paste0(c("fam_history_q12a_","fam_history_q12b_","fam_history_q12c_","fam_history_q12d_","fam_history_q12e_","fam_history_q12f_"),prob_cate_vec[7]),"fam_history_12_yes_no")

p.gp.sub.var.header.vec_list[[8]]=c(paste0(c("fam_history_q13a_","fam_history_q13b_","fam_history_q13c_","fam_history_q13d_","fam_history_q13e_","fam_history_q13f_"),prob_cate_vec[8]),"fam_history_13_yes_no")
############################################################

ua.sub.var.header.vec_list=list()
ua.sub.var.header.vec_list[[1]]=c(paste0(c("q6g_pat_uncle1_", "q6g_pat_uncle2_","q6g_pat_uncle3_","q6g_pat_uncle4_","q6g_pat_uncle5_","q6h_pat_aunt1_","q6h_pat_aunt2_","q6h_pat_aunt3_","q6h_pat_aunt4_","q6h_pat_aunt5_","q6i_mat_uncle1_","q6i_mat_uncle2_","q6i_mat_uncle3_","q6i_mat_uncle4_","q6i_mat_uncle5_","q6j_mat_aunt1_","q6j_mat_aunt2_","q6j_mat_aunt3_","q6j_mat_aunt4_","q6j_mat_aunt5_"),prob_cate_vec[1]),"fam_history_6_yes_no")

ua.sub.var.header.vec_list[[2]]=c(paste0(c("q7g_pat_uncle1_", "q7g_pat_uncle2_","q7g_pat_uncle3_","q7g_pat_uncle4_","q7g_pat_uncle5_","q7h_pat_aunt1_","q7h_pat_aunt2_","q7h_pat_aunt3_","q7h_pat_aunt4_","q7h_pat_aunt5_","q7i_mat_uncle1_","q7i_mat_uncle2_","q7i_mat_uncle3_","q7i_mat_uncle4_","q7i_mat_uncle5_","q7j_mat_aunt1_","q7j_mat_aunt2_","q7j_mat_aunt3_","q7j_mat_aunt4_","q7j_mat_aunt5_"),prob_cate_vec[2]),"fam_history_7_yes_no")

ua.sub.var.header.vec_list[[3]]=c(paste0(c("q8g_pat_uncle1_", "q8g_pat_uncle2_","q8g_pat_uncle3_","q8g_pat_uncle4_","q8g_pat_uncle5_","q8h_pat_aunt1_","q8h_pat_aunt2_","q8h_pat_aunt3_","q8h_pat_aunt4_","q8h_pat_aunt5_","q8i_mat_uncle1_","q8i_mat_uncle2_","q8i_mat_uncle3_","q8i_mat_uncle4_","q8i_mat_uncle5_","q8j_mat_aunt1_","q8j_mat_aunt2_","q8j_mat_aunt3_","q8j_mat_aunt4_","q8j_mat_aunt5_"),prob_cate_vec[3]),"fam_history_8_yes_no")

ua.sub.var.header.vec_list[[4]]=c(paste0(c("q9g_pat_uncle1_", "q9g_pat_uncle2_","q9g_pat_uncle3_","q9g_pat_uncle4_","q9g_pat_uncle5_","q9h_pat_aunt1_","q9h_pat_aunt2_","q9h_pat_aunt3_","q9h_pat_aunt4_","q9h_pat_aunt5_","q9i_mat_uncle1_","q9i_mat_uncle2_","q9i_mat_uncle3_","q9i_mat_uncle4_","q9i_mat_uncle5_","q9j_mat_aunt1_","q9j_mat_aunt2_","q9j_mat_aunt3_","q9j_mat_aunt4_","q9j_mat_aunt5_"),prob_cate_vec[4]),"fam_history_9_yes_no")

ua.sub.var.header.vec_list[[5]]=c(paste0(c("q10g_pat_uncle1_", "q10g_pat_uncle2_","q10g_pat_uncle3_","q10g_pat_uncle4_","q10g_pat_uncle5_","q10h_pat_aunt1_","q10h_pat_aunt2_","q10h_pat_aunt3_","q10h_pat_aunt4_","q10h_pat_aunt5_","q10i_mat_uncle1_","q10i_mat_uncle2_","q10i_mat_uncle3_","q10i_mat_uncle4_","q10i_mat_uncle5_","q10j_mat_aunt1_","q10j_mat_aunt2_","q10j_mat_aunt3_","q10j_mat_aunt4_","q10j_mat_aunt5_"),prob_cate_vec[5]),"fam_history_10_yes_no")

ua.sub.var.header.vec_list[[6]]=c(paste0(c("q11g_pat_uncle1_", "q11g_pat_uncle2_","q11g_pat_uncle3_","q11g_pat_uncle4_","q11g_pat_uncle5_","q11h_pat_aunt1_","q11h_pat_aunt2_","q11h_pat_aunt3_","q11h_pat_aunt4_","q11h_pat_aunt5_","q11i_mat_uncle1_","q11i_mat_uncle2_","q11i_mat_uncle3_","q11i_mat_uncle4_","q11i_mat_uncle5_","q11j_mat_aunt1_","q11j_mat_aunt2_","q11j_mat_aunt3_","q11j_mat_aunt4_","q11j_mat_aunt5_"),prob_cate_vec[6]),"fam_history_11_yes_no")

ua.sub.var.header.vec_list[[7]]=c(paste0(c("q12g_pat_uncle1_", "q12g_pat_uncle2_","q12g_pat_uncle3_","q12g_pat_uncle4_","q12g_pat_uncle5_","q12h_pat_aunt1_","q12h_pat_aunt2_","q12h_pat_aunt3_","q12h_pat_aunt4_","q12h_pat_aunt5_","q12i_mat_uncle1_","q12i_mat_uncle2_","q12i_mat_uncle3_","q12i_mat_uncle4_","q12i_mat_uncle5_","q12j_mat_aunt1_","q12j_mat_aunt2_","q12j_mat_aunt3_","q12j_mat_aunt4_","q12j_mat_aunt5_"),prob_cate_vec[7]),"fam_history_12_yes_no")

ua.sub.var.header.vec_list[[8]]=c(paste0(c("q13g_pat_uncle1_", "q13g_pat_uncle2_","q13g_pat_uncle3_","q13g_pat_uncle4_","q13g_pat_uncle5_","q13h_pat_aunt1_","q13h_pat_aunt2_","q13h_pat_aunt3_","q13h_pat_aunt4_","q13h_pat_aunt5_","q13i_mat_uncle1_","q13i_mat_uncle2_","q13i_mat_uncle3_","q13i_mat_uncle4_","q13i_mat_uncle5_","q13j_mat_aunt1_","q13j_mat_aunt2_","q13j_mat_aunt3_","q13j_mat_aunt4_","q13j_mat_aunt5_"),prob_cate_vec[8]),"fam_history_13_yes_no")

############################################################
sib.sub.var.header.vec_list=list()
sib.sub.var.header.vec_list[[1]]=c(paste0(c("q6k_full_sib_young1_","q6k_full_sib_young2_","q6k_full_sib_young3_","q6k_full_sib_young4_","q6k_full_sib_young5_","q6l_full_sib_old1_","q6l_full_sib_old2_","q6l_full_sib_old3_","q6l_full_sib_old4_","q6l_full_sib_old5_",
"q6m_half_sib_young1_","q6m_half_sib_young2_","q6m_half_sib_young3_","q6m_half_sib_young4_","q6m_half_sib_young5_",
"q6n_half_sib_old1_","q6n_half_sib_old2_","q6n_half_sib_old3_","q6n_half_sib_old4_","q6n_half_sib_old5_"),prob_cate_vec[1]),"fam_history_6_yes_no")


sib.sub.var.header.vec_list[[2]]=c(paste0(c("q7k_full_sib_young1_","q7k_full_sib_young2_","q7k_full_sib_young3_","q7k_full_sib_young4_","q7k_full_sib_young5_","q7l_full_sib_old1_","q7l_full_sib_old2_","q7l_full_sib_old3_","q7l_full_sib_old4_","q7l_full_sib_old5_",
"q7m_half_sib_young1_","q7m_half_sib_young2_","q7m_half_sib_young3_","q7m_half_sib_young4_","q7m_half_sib_young5_",
"q7n_half_sib_old1_","q7n_half_sib_old2_","q7n_half_sib_old3_","q7n_half_sib_old4_","q7n_half_sib_old5_"),prob_cate_vec[2]),"fam_history_7_yes_no")


sib.sub.var.header.vec_list[[3]]=c(paste0(c("q8k_full_sib_young1_","q8k_full_sib_young2_","q8k_full_sib_young3_","q8k_full_sib_young4_","q8k_full_sib_young5_","q8l_full_sib_old1_","q8l_full_sib_old2_","q8l_full_sib_old3_","q8l_full_sib_old4_","q8l_full_sib_old5_",
"q8m_half_sib_young1_","q8m_half_sib_young2_","q8m_half_sib_young3_","q8m_half_sib_young4_","q8m_half_sib_young5_",
"q8n_half_sib_old1_","q8n_half_sib_old2_","q8n_half_sib_old3_","q8n_half_sib_old4_","q8n_half_sib_old5_"),prob_cate_vec[3]),"fam_history_8_yes_no")

sib.sub.var.header.vec_list[[4]]=c(paste0(c("q9k_full_sib_young1_","q9k_full_sib_young2_","q9k_full_sib_young3_","q9k_full_sib_young4_","q9k_full_sib_young5_","q9l_full_sib_old1_","q9l_full_sib_old2_","q9l_full_sib_old3_","q9l_full_sib_old4_","q9l_full_sib_old5_",
"q9m_half_sib_young1_","q9m_half_sib_young2_","q9m_half_sib_young3_","q9m_half_sib_young4_","q9m_half_sib_young5_",
"q9n_half_sib_old1_","q9n_half_sib_old2_","q9n_half_sib_old3_","q9n_half_sib_old4_","q9n_half_sib_old5_"),prob_cate_vec[4]),"fam_history_9_yes_no")

sib.sub.var.header.vec_list[[5]]=c(paste0(c("q10k_full_sib_young1_","q10k_full_sib_young2_","q10k_full_sib_young3_","q10k_full_sib_young4_","q10k_full_sib_young5_","q10l_full_sib_old1_","q10l_full_sib_old2_","q10l_full_sib_old3_","q10l_full_sib_old4_","q10l_full_sib_old5_",
"q10m_half_sib_young1_","q10m_half_sib_young2_","q10m_half_sib_young3_","q10m_half_sib_young4_","q10m_half_sib_young5_",
"q10n_half_sib_old1_","q10n_half_sib_old2_","q10n_half_sib_old3_","q10n_half_sib_old4_","q10n_half_sib_old5_"),prob_cate_vec[5]),"fam_history_10_yes_no")


sib.sub.var.header.vec_list[[6]]=c(paste0(c("q11k_full_sib_young1_","q11k_full_sib_young2_","q11k_full_sib_young3_","q11k_full_sib_young4_","q11k_full_sib_young5_","q11l_full_sib_old1_","q11l_full_sib_old2_","q11l_full_sib_old3_","q11l_full_sib_old4_","q11l_full_sib_old5_",
"q11m_half_sib_young1_","q11m_half_sib_young2_","q11m_half_sib_young3_","q11m_half_sib_young4_","q11m_half_sib_young5_",
"q11n_half_sib_old1_","q11n_half_sib_old2_","q11n_half_sib_old3_","q11n_half_sib_old4_","q11n_half_sib_old5_"),prob_cate_vec[6]),"fam_history_11_yes_no")


sib.sub.var.header.vec_list[[7]]=c(paste0(c("q12k_full_sib_young1_","q12k_full_sib_young2_","q12k_full_sib_young3_","q12k_full_sib_young4_","q12k_full_sib_young5_","q12l_full_sib_old1_","q12l_full_sib_old2_","q12l_full_sib_old3_","q12l_full_sib_old4_","q12l_full_sib_old5_",
"q12m_half_sib_young1_","q12m_half_sib_young2_","q12m_half_sib_young3_","q12m_half_sib_young4_","q12m_half_sib_young5_",
"q12n_half_sib_old1_","q12n_half_sib_old2_","q12n_half_sib_old3_","q12n_half_sib_old4_","q12n_half_sib_old5_"),prob_cate_vec[7]),"fam_history_12_yes_no")

sib.sub.var.header.vec_list[[8]]=c(paste0(c("q13k_full_sib_young1_","q13k_full_sib_young2_","q13k_full_sib_young3_","q13k_full_sib_young4_","q13k_full_sib_young5_","q13l_full_sib_old1_","q13l_full_sib_old2_","q13l_full_sib_old3_","q13l_full_sib_old4_","q13l_full_sib_old5_",
"q13m_half_sib_young1_","q13m_half_sib_young2_","q13m_half_sib_young3_","q13m_half_sib_young4_","q13m_half_sib_young5_",
"q13n_half_sib_old1_","q13n_half_sib_old2_","q13n_half_sib_old3_","q13n_half_sib_old4_","q13n_half_sib_old5_"),prob_cate_vec[8]),"fam_history_13_yes_no")
############################################################


master.var="famhx_1";
Input.vars.all_list=list();

#suppose there is already a folder named "other.cond.output" and there are sub-folders inside of it named as prob_cate_vec;
my.output.path.all=paste0("~/Desktop/ABCD/FamHx/R.code/other.cond.output/")


var.names=orig.var.names
for (k in 1:length(prob_cate_vec)){
	
	prob_cate=prob_cate_vec[k]
	prob_cate_dic=prob_cate_dic_vec[k]
	my.output.path=paste0(my.output.path.all,prob_cate,"/")
	print("######################################")
	print(prob_cate)
	print("######################################")
	############################################################
	##For parents and grandparents
	############################################################

	Input.vars.all<-c(master.var)
	p.gp.sub.var.header.vec=p.gp.sub.var.header.vec_list[[k]]
	p.gp.cate.vec=c("fath","patgf","patgm","moth","matgf","matgm")


	dat.obj=other.cond.fcn(dat.bl,master.var,p.gp.sub.var.header.vec,p.gp.cate.vec,prob_cate,rel.cate=0,dat.source=dat.source)
	dat.bl=dat.obj$my.dat
	Input.vars.all=c(Input.vars.all,dat.obj$Input.vars)

	############################################################
	##For uncles and aunts
	############################################################
	ua.sub.var.header.vec=ua.sub.var.header.vec_list[[k]]
			ua.cate.vec=c("patunc1","patunc2","patunc3","patunc4","patunc5","patant1","patant2","patant3","patant4","patant5","matunc1","matunc2","matunc3","matunc4","matunc5","matant1","matant2","matant3","matant4","matant5")
num.sub.var.2=5;

	##2a:patunc, 2b:pataun, 1a:matunc, 1b:mataun
	sub.var.vec=c(rep("famhx_2a",num.sub.var.2),rep("famhx_2b",num.sub.var.2),rep("famhx_1a",num.sub.var.2),rep("famhx_1b",num.sub.var.2))

	dat.obj=other.cond.fcn(dat.bl,master.var,ua.sub.var.header.vec,ua.cate.vec,prob_cate,rel.cate=1,sub.var.2=sub.var.vec,dat.source=dat.source)
	dat.bl=dat.obj$my.dat
	Input.vars.all=c(Input.vars.all,dat.obj$Input.vars)
	
	################################################################################
	##For siblings: 5 full younger; 5 full older; 5 half younger; 5 half older; total:20
	################################################################################
	
	num.sub.var.2=5;
	sub.var.vec=c(rep("fhx_3f_younger",num.sub.var.2),rep("fhx_3f_older",num.sub.var.2),rep("fhx_3h_younger",num.sub.var.2),rep("fhx_3h_older",num.sub.var.2))

	sib.sub.var.header.vec=sib.sub.var.header.vec_list[[k]]
	
	sib.cate.vec=c("fulsiby1","fulsiby2","fulsiby3","fulsiby4","fulsiby5","fulsibo1","fulsibo2","fulsibo3","fulsibo4","fulsibo5",
"hlfsiby1","hlfsiby2","hlfsiby3","hlfsiby4","hlfsiby5","hlfsibo1","hlfsibo2","hlfsibo3","hlfsibo4","hlfsibo5")


	dat.obj=other.cond.fcn(dat.bl,master.var,sib.sub.var.header.vec,sib.cate.vec,prob_cate,rel.cate=1,sub.var.2=sub.var.vec,dat.source=dat.source)
	dat.bl=dat.obj$my.dat
	Input.vars.all=c(Input.vars.all,dat.obj$Input.vars)


	############################################################
	#Either parent problem? dichotomous 
	############################################################
	#0:No; 1:Yes
	dat.bl[,paste0("fhx_ss_momdad_",prob_cate)]=NA;
	dat.bl[which(dat.bl[,paste0("fhx_ss_moth_prob_",prob_cate)]==0 & dat.bl[,paste0("fhx_ss_fath_prob_",prob_cate)]==0),paste0("fhx_ss_momdad_",prob_cate)]=0;
	dat.bl[which(dat.bl[,paste0("fhx_ss_moth_prob_",prob_cate)]==1 | dat.bl[,paste0("fhx_ss_fath_prob_",prob_cate)]==1),paste0("fhx_ss_momdad_",prob_cate)]=1

	#Input.vars.all=c(Input.vars.all,paste0("fhx_ss_momdad_",prob_cate))
	
	# cat("\n###########################\n")
	# print(table(dat.bl[,paste0("fhx_ss_momdad_",prob_cate)],useNA="ifany",dnn=paste0("fhx_ss_momdad_",prob_cate)))


	############################################################
	#Overal parental problem
	#0:none; 1:dad only; 2:mom only; 3:both;
	#-1:dad missing;-2:mom missing
	############################################################
	dat.bl[,paste0("fhx_ss_parent_",prob_cate)]=NA;
	dat.bl[,paste0("fhx_ss_parent_",prob_cate)][which(dat.bl[,paste0("fhx_ss_moth_prob_",prob_cate)]==0 & dat.bl[,paste0("fhx_ss_fath_prob_",prob_cate)]==0)]=0
	dat.bl[,paste0("fhx_ss_parent_",prob_cate)][which(dat.bl[,paste0("fhx_ss_moth_prob_",prob_cate)]==0 & dat.bl[,paste0("fhx_ss_fath_prob_",prob_cate)]==1)]=1
	dat.bl[,paste0("fhx_ss_parent_",prob_cate)][which(dat.bl[,paste0("fhx_ss_moth_prob_",prob_cate)]==1 & dat.bl[,paste0("fhx_ss_fath_prob_",prob_cate)]==0)]=2
	dat.bl[,paste0("fhx_ss_parent_",prob_cate)][which(dat.bl[,paste0("fhx_ss_moth_prob_",prob_cate)]==1 & dat.bl[,paste0("fhx_ss_fath_prob_",prob_cate)]==1)]=3



	dat.bl[,paste0("fhx_ss_parent_",prob_cate)][which(dat.bl[,paste0("fhx_ss_moth_prob_",prob_cate)]==1 & is.na(dat.bl[,paste0("fhx_ss_fath_prob_",prob_cate)]))]=-1
	dat.bl[,paste0("fhx_ss_parent_",prob_cate)][which(dat.bl[,paste0("fhx_ss_fath_prob_",prob_cate)]==1 & is.na(dat.bl[,paste0("fhx_ss_moth_prob_",prob_cate)]))]=-2
	#Input.vars.all=c(Input.vars.all,paste0("fhx_ss_parent_",prob_cate))
	Input.vars.all_list[[k]]=unique(Input.vars.all);
	
	# cat("\n###########################\n")
	# print(table(dat.bl[,paste0("fhx_ss_parent_",prob_cate)],useNA="ifany",dnn=paste0("fhx_ss_parent_",prob_cate)))

	##################################################################
	new.dic.vars=colnames(dat.bl)[which(!colnames(dat.bl)%in%var.names)]
	
	my.txt=ifelse(dat.source==0,"dump","nda")
	my.tot=dim(dat.bl)[1]
	
	sink(paste0(my.output.path,my.txt,".",my.tot,".",prob_cate,".freq.txt"))
	
	for(i in 1:length(new.dic.vars)){
		cat("############################\n")
		print(table(dat.bl[,new.dic.vars[i]],useNA="ifany",dnn=new.dic.vars[i]))
		cat("\n")
	}
	sink()
	
	
	var.names=c(var.names,new.dic.vars)
	###################################################################
	#dictionary: 
	ss.dic=make.dic(new.dic.vars,prob_cate,prob_cate_dic)
	save(file=paste0(my.output.path,"fhx_ss_",prob_cate,"_dic.RData"),ss.dic)

	if(dat.source==0){
		dat.bl.ss=dat.bl[,c("id_redcap","redcap_event_name",new.dic.vars)]
		save(file=paste0(my.output.path,"fhx_ss_dump_",prob_cate,".RData"),dat.bl.ss)
	}else{
		dat.bl.ss=dat.bl[,c("src_subject_id","eventname",new.dic.vars)]
		save(file=paste0(my.output.path,"fhx_ss_nda_",prob_cate,".RData"),dat.bl.ss)
	}


	if(k==length(prob_cate_vec)){
		if(dat.source==0){
			save(file=paste0(my.output.path.all,"dat.bl.dump.oth.conds.RData"),dat.bl,Input.vars.all_list)
		
			
		}else{
			save(file=paste0(my.output.path.all,"dat.bl.nda.oth.conds.RData"),dat.bl,Input.vars.all_list)
			
		}
		
	}
}	


