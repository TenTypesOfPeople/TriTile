for(pack in c("rSymPy","MASS")){
  if(!pack %in% installed.packages())
    install.packages(pack)
}

#For any dimension greater than 1:

#generate a regular n-simplex with the center as one of its vertices:
n.simplex<-function(n){
	a<-matrix(0,nrow=n,ncol=n)
	a[1,1]<-1 #The second point is (1,0,.....,0), wlog
	for(i in 1:(n-1)){
		for(j in 1:i){
			if(j<i){
				a[i+1,j]<-a[i,j]
			}else{
				a[i+1,j]<-a[i,j]/(i+1)
				a[i+1,j+1]<-a[i,j]*sqrt(((i+1)^2-1)/((i+1)^2))
			}
		}
	}
	a
}

#fractions(n.simplex(dim)^2) #so as to not lose any accuracy when solving
#Use above in circeq

#generate set of spherical equations based on coordinates
circeq<-function(eq.signs,vec){ #using radius 1
	if(nrow(vec>1)){
		eqs<-vector(length=nrow(vec))
		for(i in 1:length(eqs)){
			eqs[i]<-paste("Eq(",paste("(x",1:length(eqs),eq.signs[i,],"(",vec[i,],")**(1/2))**2",sep="",collapse="+"),",1)",sep="")
		}
		return(gsub("\\(0\\)\\*\\*\\(1/2\\)","0",paste(eqs,sep="",collapse=",")))
	}else{
		return(gsub("\\(0\\)\\*\\*\\(1/2\\)","0",paste("Eq(",paste("(x",1:length(vec),eq.signs(vec),"(",vec,")**(1/2))**2",sep="",collapse="+"),",1)",sep="")))
	}
}
#edit so that + or - changes depending on coordinate

#template to paste an equation?
circeq(signs(rbind(c(0,0,0),n.simplex(3)[1:2,])),fractions(rbind(c(0,0,0),n.simplex(3)[1:2,]^2)))

#generate n variables
genvars<-function(n){
	for(i in 1:n){
		sympy(paste("var('x",i,"',real=True)",sep=""))
	}
}

#Find if a given solution is overlapping with any other points
acceptable<-function(vec,mat){
	is.okay<-TRUE
	for(i in 1:nrow(mat)){
		if((sum(vec - mat[i,])^2)<1){
			is.okay<-FALSE
		} 
	}
	return(is.okay)
}

#paste n variables
pastevars<-function(n){paste("[",paste("x",1:n,sep="",collapse=","),"]",sep="")}

#Solve multiple equations at once- first n-1 rows of the n simplex with origin
solve.multi<-function(i){sympy(paste("solve([",circeq(fractions(rbind(rep(0,i),n.simplex(i)[1:(i-1),]^2))),"],",pastevars(i),")",sep=""))}

#To get sign of original matrix since we're squaring the terms
signs<-function(x){ifelse(x<0,"+","-")} #flipped since it's of the form (x-a)^2

#Distance between two points p1 and p2 using sqrt(sum(p1^2 - p2^2)))
#^2 is only required if the values aren't yet squared.

#replace py with r exponent notation in string
replace.exp<-function(x){gsub("\\*\\*","\\^",x)}

#pull the solutions out of sympy output
extract.sols<-function(sols){regmatches(sols, gregexpr("\\(([^()]|(?R))*\\)", sols, perl=T))[[1]]}

#Convert a given solution string into a vector to bind to matrix
convert.sol<-function(x){eval(parse(
						text=paste(
							"c",replace.exp(x),
								ifelse(grepl("\\^\\([[:digit:]]+/[[:digit:]]+\\)$",replace.exp(x)),")",""),
							sep="")
					))}

find.kn<-function(dim){

	num.pts<-dim # num of vertices not at the center
	genvars(dim)
	no.more<-FALSE
	ctr<-0
	points<-n.simplex(dim)
	while(!no.more){
		sols<-extract.sols(sympy(paste("solve([",circeq(signs(rbind(rep(0,dim),n.simplex(dim)[(1+ctr):(dim+ctr-1),])),fractions(rbind(rep(0,dim),n.simplex(dim)[(1+ctr):(dim+ctr-1),]^2))),"],",pastevars(dim),")",sep="")))
		#A fun property here is we know there's only 2 solutions
		sols.1<-convert.sol(sols[1])
		sols.2<-convert.sol(sols[2])
		flag.1<-((!any(duplicated(rbind(sols.1,points)))) & acceptable(sols.1,points))
		flag.2<-((!any(duplicated(rbind(sols.2,points)))) & acceptable(sols.2,points))
		if(flag.1){
			points<-rbind(points,sols.1)
			num.pts<-num.pts+1	
		}
		if(flag.2){
			points<-rbind(points,sols.2)
			num.pts<-num.pts+1	
		}
		if((!flag.1)&(!flag.2)){
			no.more<-TRUE
		}else{ctr<-ctr+1}
	}
	cat("The kissing number for dimension ",dim," is ",num.pts,'\n')
	return(points)
}

library(rSymPy)
library(MASS)

print("Enter # of dimensions:")
dim<-scan(n=1)
center<-1:dim*0 #input number of dimensions from user to create zero vector of that length
#Initialize variables based on number of dimensions
base.points<-n.simplex(dim)
base.squares<-fractions(base.points^2)
for(i in 1:dim){sympy(paste("var('x",i,"')"))}
while(!no.more){
	
}


no.more<-FALSE #once no more eligible lattices can be found, terminate
ctr<-1

#Can find if a new point is overlapping based on whether dist output is <1



cat("The kissing number for ",dim," dimensions is ",find.kn(dim))


######################## Playground

#paste a list of x variables
#paste("[",paste("x",1:x,sep="",collapse=","),"]",sep="")
#paste("[",paste("x",1:x,sep="",collapse=","),"]",sep="")
#get solutions out of those damn brackets
#regmatches(sols, gregexpr("(?=\\().*?(?<=\\))", sols, perl=T))[[1]]
#Then of course use eval parse on each value 
#To append a solution to 
#rbind(cbind(n.simplex(k),rep(0,2)),b)
#To find whether a solution already exists in the given set of points
#any(duplicated(rbind(b,n.simplex(4))))
#Solve equations using n.simplex and sympy
#sympy(paste("solve([",circeq(fractions(n.simplex(2)^2)),"],[",paste("x",1:2,sep="",collapse=","),"])",sep=""))
#to sub out any (0)**(1/2) terms
#gsub("\\(0\\)\\*\\*\\(1/2\\)","0",
#use any(duplicated()) on above to determine first if the solution is a duplicate
