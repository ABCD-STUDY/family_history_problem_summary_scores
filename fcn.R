###########################################################
#function
###########################################################
uncle.aunt.sib.fcn=function(my.dat,shared.var,master.var,sub.var.header,sub.var.2, cate, threshold=1,num.sub.var,prob_cate,dat.source=1){
	prob_no=rep(NA,dim(my.dat)[1]);
	prob=prob_no;
	
	sub.var=NULL;
	for(i in 1:num.sub.var){
		sub.var[i]=paste0(sub.var.header,"___",i);
		
	}
	
	if(dat.source==0){
		
		my.dat[,c(shared.var,master.var,sub.var)]=sapply(my.dat[,c(shared.var,master.var,sub.var)],as.numeric)
		my.dat[,paste0(sub.var.header,"___999")]=as.numeric(my.dat[,paste0(sub.var.header,"___999")])
		
		
	}else{
		
		tmp=sapply(my.dat[,c(shared.var,master.var,sub.var)],as.character)
		my.dat[,c(shared.var,master.var,sub.var)]=sapply(tmp,as.numeric)
		
				
		 my.dat[,paste0(sub.var.header,"___999")]=as.character(my.dat[,paste0(sub.var.header,"___999")])
		 my.dat[,paste0(sub.var.header,"___999")]=as.numeric(my.dat[,paste0(sub.var.header,"___999")])
		 
		
	}
	
	prob_no=rowSums(my.dat[,sub.var],na.rm=T) #use na.rm=T, all NAs are added up to 0;
	
	#reset;
	all.nas=apply(my.dat[,sub.var],1, function(x) length(which(is.na(x))))
	prob_no[which(all.nas==num.sub.var)]=NA;
	
	indx=which(prob_no >= threshold)
	if(length(indx)>0){
		prob[indx]=1
		prob[-indx]=0
	}else{
		prob=rep(0,dim(my.dat)[1]);
		prob[which(is.na(prob_no))]=NA;
	}
	
	#reset some situations
	indx=which(my.dat[,master.var] ==0)
	prob[indx]=0
	prob_no[indx]=0;
	
	indx=which(my.dat[,paste0(sub.var.header,"___999")]==1 & prob_no==0)
	prob[indx]=NA;
	prob_no[indx]=NA;
	
	########additional section different from parents/grandparents
	#depends on which uncle/aunt/sibling, it is slightly different;
	which.one=as.numeric(str_extract(cate, "\\d"))
	my.include.list=c(seq(0,which.one-1),NA)
	
		
	if(dat.source==0){
		my.dat[,paste0(sub.var.2,"_p")]=as.numeric(my.dat[,paste0(sub.var.2,"_p")])
		#should use _dk_p always
		my.dat[,paste0(sub.var.2,"_dk_p")]=as.numeric(my.dat[,paste0(sub.var.2,"_dk_p")])
				
	}else{
		my.dat[,paste0(sub.var.2,"_p")]=as.numeric(as.character(my.dat[,paste0(sub.var.2,"_p")]))
		#should use _dk_p always
		my.dat[,paste0(sub.var.2,"_dk_p")]=as.numeric(as.character(my.dat[,paste0(sub.var.2,"_dk_p")]))
		
		#some values of nda data were reset from data dump, in order to get the same _ss_ as data dump:
		indx=which(is.na(as.numeric(as.character(my.dat[,paste0(sub.var.header,"___0")]))) & is.na(prob) & my.dat[,master.var]==1)
		if(length(indx)>0)	
			prob[indx]=0;
			prob_no[indx]=0					
	}	
		
		
		
	indx=which(my.dat[,paste0(sub.var.2,"_p")]%in%my.include.list | my.dat[,paste0(sub.var.2,"_dk_p")]==999)

	prob[indx]=NA;
	prob_no[indx]=NA
	####################################################
	
	indx=which(my.dat[,master.var]%in%c(999,7,NA))
	prob[indx]=NA;
	prob_no[indx]=NA
	####################################################
	#add shared.var:famhx_1
	indx=which(my.dat[,shared.var]==0)
	prob[indx]=NA;
	prob_no[indx]=NA
	####################################################
	
	my.dat[,paste0(cate,"_prob_",prob_cate)]=prob;
	print("###########################")
	print(paste0(cate,"_prob_",prob_cate));
	print(table(my.dat[,paste0(cate,"_prob_",prob_cate)],useNA="ifany"))

	#return(my.dat)
	return(list(my.dat = my.dat, Input.vars=c(sub.var,paste0(sub.var.header,"___999"),paste0(sub.var.2,"_dk_p"))))
	
}

parent.grandparent.fcn=function(my.dat,shared.var,master.var,sub.var.header,cate, threshold=1,num.sub.var,prob_cate,dat.source=1){
	prob_no=rep(NA,dim(my.dat)[1]);
	prob=prob_no;
	
	sub.var=NULL;
	
	for(i in 1:num.sub.var){
		sub.var[i]=paste0(sub.var.header,"___",i);
	}
	
	
	if(dat.source==0){
		
		my.dat[,c(shared.var,master.var,sub.var)]=sapply(my.dat[,c(shared.var,master.var,sub.var)],as.numeric)
	
		
	}else{
		
		tmp=sapply(my.dat[,c(shared.var,master.var,sub.var)],as.character)
		my.dat[,c(shared.var,master.var,sub.var)]=sapply(tmp,as.numeric)
		
		
	}
	
	prob_no=rowSums(my.dat[,sub.var],na.rm=T) #use na.rm=T, all NAs are added up to 0;
	
	#reset;
	all.nas=apply(my.dat[,sub.var],1, function(x) length(which(is.na(x))))
	prob_no[which(all.nas==num.sub.var)]=NA;

	#print(table(prob_no,useNA="ifany"))
	
	indx=which(prob_no >= threshold)
	if(length(indx)>0){
		prob[indx]=1
		prob[-indx]=0
	}else{
		prob=rep(0,dim(my.dat)[1]);
		prob[which(is.na(prob_no))]=NA;
	}

	
	#reset some situations
	indx=which(my.dat[,master.var] ==0)
	prob[indx]=0
	prob_no[indx]=0;
	
	indx=which(my.dat[,paste0(sub.var.header,"___999")]==1 & prob_no==0)
	prob[indx]=NA;
	prob_no[indx]=NA;
	
	if(dat.source==1){
		#some values of nda data were reset from data dump, in order to get the same _ss_ as data dump:
		indx=which(is.na(as.numeric(as.character(my.dat[,paste0(sub.var.header,"___0")]))) & is.na(prob) & my.dat[,master.var]==1)
		if(length(indx)>0)	
			prob[indx]=0;
			prob_no[indx]=0	
	}	
	
	indx=which(my.dat[,master.var]%in%c(999,7,NA))
	prob[indx]=NA;
	prob_no[indx]=NA
	
	####################################################
	#add shared.var:famhx_1
	indx=which(my.dat[,shared.var]==0)
	prob[indx]=NA;
	prob_no[indx]=NA
	
	my.dat[,paste0(cate,"_prob_",prob_cate)]=prob;
	print("###########################")
	print(paste0(cate,"_prob_",prob_cate));
	print(table(my.dat[,paste0(cate,"_prob_",prob_cate)],useNA="ifany"))

	 
	return(list(my.dat = my.dat, Input.vars=c(sub.var,paste0(sub.var.header,"___999"))))
	                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  }


make.dic=function(new.dic.vars,prob_cate,pro_cate_dic){
	field_name=new.dic.vars
	field_choice=rep(NA,length(field_name));

	indx=which(grepl(paste0("_prob_",prob_cate),new.dic.vars))
	field_choice[indx]="0, no problem endorsed | 1, problem endorsed"

	indx=which(grepl("momdad_",new.dic.vars))
	field_choice[indx]="0, no | 1, yes"

	indx=which(grepl("parent_",new.dic.vars))
	field_choice[indx]="0, none | 1, father only | 2, mother only | 3, both | -1, father missing (mother positive) | -2, mother missing (father positive)"

	field_label=paste0(c("father","paternal grandfather","paternal grandmother","mother","maternal grandfather","maternal grandmother","paternal uncle 1", "paternal uncle 2" ,"paternal uncle 3", "paternal uncle 4" ,"paternal uncle 5" ,"paternal aunt 1", "paternal aunt 2", "paternal aunt 3", "paternal aunt 4", "paternal aunt 5", "maternal uncle 1" ,"maternal uncle 2", "maternal uncle 3", "maternal uncle 4", "maternal uncle 5" ,"maternal aunt 1", "maternal aunt 2", "maternal aunt 3", "maternal aunt 4","maternal aunt 5", "full sibling younger 1", "full sibling younger 2" ,"full sibling younger 3" ,"full sibling younger 4" ,"full sibling younger 5" ,"full sibling older 1" ,"full sibling older 2", "full sibling older 3", "full sibling older 4", "full sibling older 5" ,"half sibling younger 1" ,"half sibling younger 2" ,"half sibling younger 3" ,"half sibling younger 4" ,"half sibling younger 5" ,"half sibling older 1", "half sibling older 2", "half sibling older 3" ,"half sibling older 4" ,"half sibling older 5","Either parents with", "Overall parents")," ", prob_cate_dic," problem");



	dic=cbind(field_name,field_label,field_choice)
	
	return(dic)
}

#function
other.cond.fcn=function(my.dat,master.var,sub.var.header,cate,prob_cate,rel.cate,sub.var.2=NULL,dat.source=1){
	# rel.cate=0:parents and grandparents
	# rel.cate=1:uncle/aunt;
	
	#dat.source=0:data dump;
	#dat.source=1:nda package txt file
	if(dat.source==0){
		
		my.dat[,c(master.var,sub.var.header)]=sapply(my.dat[,c(master.var,sub.var.header)],as.numeric)
		
	}else{
		
		tmp=sapply(my.dat[,c(master.var,sub.var.header)],as.character)
		my.dat[,c(master.var,sub.var.header)]=sapply(tmp,as.numeric)
		
	}
	
	
	sub.var=sub.var.header[-length(sub.var.header)]; 
	sub.yes.no=sub.var.header[length(sub.var.header)]
	
	return.vars=sub.var.header;
	for(i in 1:length(sub.var)){
		prob=rep(NA,dim(my.dat)[1]);
		indx.1=which(my.dat[,sub.yes.no]==1 & my.dat[,sub.var[i]]==1)
		prob[indx.1]=1;
		
		indx.0=which(my.dat[,sub.yes.no]==1 & my.dat[,sub.var[i]]==0)
		prob[indx.0]=0
		
		prob[which(my.dat[,sub.yes.no]==0)]=0;
		
		prob[which(my.dat[,master.var]==0 | my.dat[,sub.yes.no]%in%c(7,999) | my.dat[,sub.var[i]]== 999)]=NA;
	
	
		if(rel.cate==1){ #uncle/aunts/sib
			########additional section different from parents/grandparents
			#depends on which uncle/aunt/sibling, it is slightly different;
			which.one=as.numeric(str_extract(cate[i], "\\d"))
			my.include.list=c(seq(0,which.one-1),NA)
			if(dat.source==0){
				my.dat[,paste0(sub.var.2[i],"_p")]=as.numeric(my.dat[,paste0(sub.var.2[i],"_p")])
				#should use _dk_p always
				my.dat[,paste0(sub.var.2[i],"_dk_p")]=as.numeric(my.dat[,paste0(sub.var.2[i],"_dk_p")])
				
			}else{
				my.dat[,paste0(sub.var.2[i],"_p")]=as.numeric(as.character(my.dat[,paste0(sub.var.2[i],"_p")]))
				
				my.dat[,paste0(sub.var.2[i],"_dk_p")]=as.numeric(as.character(my.dat[,paste0(sub.var.2[i],"_dk_p")]))
				
				
			}
			indx=which(my.dat[,paste0(sub.var.2[i],"_p")]%in%my.include.list | my.dat[,paste0(sub.var.2[i],"_dk_p")]==999)
			return.vars=c(return.vars,paste0(sub.var.2[i],"_p"),paste0(sub.var.2[i],"_dk_p"))


			prob[indx]=NA
		}
		
		my.dat[,paste0("fhx_ss_",cate[i],"_prob_",prob_cate)]=prob;
		

	}
	
	return(list(my.dat = my.dat, Input.vars=unique(return.vars)))
	
}

