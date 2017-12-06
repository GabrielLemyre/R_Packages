chr <- function(n) { rawToChar(as.raw(n)) }

Copie.Presse.Papier <- function(string) {
	os <- Sys.info()[['sysname']]
	if (os == "Windows") { # Si systeme dexploitation windows
		return(utils::writeClipboard(string))
	} else if (os == "Darwin") { # Si systeme dexploitation iOS
		Mac.Copie.Presse.Papier <- function(string){
			presse.papier <- pipe("pbcopy", "w")
			cat(string, file = presse.papier, sep = "\n")
			close(presse.papier)	# Fermer lobjet presse-papier
		}
		return(Mac.Copie.Presse.Papier(string))
	}
}

Make.LaTeX.Table = function(R.Matrix.Object,
							caption=TRUE,
                            title = "", 
						    Col.Titles = NULL, 
    						Row.Titles = NULL,
							n.dec = 3,
							type = "Table",
							Row.Pos="c",
							Cross.Lines=FALSE,
							print.Cons=TRUE,
							copy.CB=TRUE,...){ # Ecrit le code LaTeX dun environnement Tabular a partir dune matrice R
	
	n.col = ncol(R.Matrix.Object)
	n.Row = nrow(R.Matrix.Object)
	
	Row.Titles.Ind = FALSE
	Col.Titles.Ind = FALSE
	if(!is.null(Row.Titles)){Row.Titles.Ind = TRUE}
	if(!is.null(Col.Titles)){Col.Titles.Ind = TRUE}
	
	if(Row.Titles.Ind | Cross.Lines  & !(n.Row==1)){
	  pos.col = "r |"
	  nb.col.pos = n.col
	  if(Cross.Lines & !Row.Titles.Ind){
	  	nb.col.pos = nb.col.pos - 1
	  }
	}else{
	  pos.col=""
	  nb.col.pos = n.col
	}
	
	pos.col = paste(pos.col," ",chr(42),nb.col.pos,"{",Row.Pos,"} ",sep="")
	
	str = paste("% ------------------------ \n", 
				chr(92),"begin{minipage}{",
				chr(92),"linewidth} \n",
				chr(92),"centering",
				if(caption){paste(chr(92),"captionof{table}{",title,"} \n",sep="")},
				chr(92),"label{",title,"} \n",
				chr(92),"begin{tabular}[t]{",pos.col,"} \n",sep="")
	
	Print.Nth.Element = function(Element,String,Col,last=FALSE){
		if (is.numeric(Element)){
			String=paste(String," $",round(Element,n.dec),"$ ",if(!last){chr(38)}else{""},sep="")
		}else{
			String=paste(String," ",Element," ",if(!last){chr(38)}else{""},sep="")
		}
		Print.Nth.Element = String # Returns the input string modified
	}
	
	if (Col.Titles.Ind){
	  print(Col.Titles.Ind)
		lenoff = length(Col.Titles)
		if (Row.Titles.Ind){
			if (length(Col.Titles) == n.col){
				Col.Titles = append(Col.Titles,"",after=0)
			}
		}
		for (i in 1:(lenoff-1)){
			str = paste(str,Col.Titles[i],chr(38))
		}
		str = paste(str," ",Col.Titles[lenoff]," ", chr(92),chr(92),"\n ", chr(92),"hline \n", sep="")
	}
	
	for (i in 1:n.Row){
		if (!is.null(Row.Titles[i])){ # Adds the Row titles if there are any
			str = paste(str,Row.Titles[i],chr(38)) # Adds the ith one
		}
	
		if (n.col!=1){ # If there is more than one Column
			for (k in 1:n.col){ # Does all the Columns and doesnt add & on the last one
				str = Print.Nth.Element(R.Matrix.Object[i,k],str,k,last=isTRUE(k==n.col))
			}
		} else {
			str = Print.Nth.Element(R.Matrix.Object[i,n.col],str,n.col,last=TRUE) # If one Column only, does that one only
		}
		str=paste(str," ",chr(92),chr(92),sep="") # Adds the double backslash at the end of the line
		if(!(i==n.Row)){str=paste(str,"\n",sep="")} # If it is the last line, it wont add line jump
		if (i==1){ # If the first line was already printed
		  if (Cross.Lines & !Col.Titles.Ind & !(n.Row==1)){ # If the line is to be printed but no col.titles giving expressively
		    str = paste(str, chr(92),"hline \n", sep="")
		  }
		}
	}
	
	
	
	# Fin de lexecution et retour du resultat
	if(copy.CB){
		Copie.Presse.Papier(paste(str,"\n",
										chr(92),"end{tabular} \n",
										chr(92),"end{minipage}","\n ~",chr(92),chr(92),
										"\n % ------------------------",sep="")) # Copie le string concatene au presse-papier
	}
	
	if(print.Cons){
		str=cat(paste(str,"\n",
							chr(92),"end{tabular} \n",
							chr(92),"end{minipage}","\n ~",chr(92),chr(92),
							"\n % ------------------------",sep="")) # Limprime aussi dans la console R
	}
}




# Explication imprime dans la console
cat("------------------------------------------------------------------------------------------------------------- \n")
cat("This package was created by [ Gabriel Lemyre ] in Montreal, Qc, Canada \n")
cat(paste("Last updated on the {",format(Sys.Date(), format="%B %d, %Y"),"} \n",sep=""))
cat("------------------------------------------------------------------------------------------------------------- \n")
cat(paste("For the code to run in LaTeX, your TeX editor requires the package ",chr(92),"usepackage{caption} if caption=TRUE \n",sep=""))
cat("------------------------------------------------------------------------------------------------------------- \n")
cat("Prints the LaTeX code for a tabular environnement from an R matrix object \n",
	"[ Make.LaTeX.Table ] \n",
    "R.Matrix.Object,    [matrix]  The object to be printed in LaTeX format", 
	"\n caption=TRUE,       [boolean] Should a caption be printed",
    "\n title='',           [string]  That title of the tabular environnement",
    "\n Col.Titles=NULL,    [vector]  The titles of the columns, must be at least the same lenght as ncol(R.Matrix.Object)-1 and at most ncol(R.Matrix.Object)",
    "\n Row.Titles=NULL,    [vector]  The titles of the rows, must be exactly the same length as nrow(R.Matrix.Object)",
    "\n n.dec = 3,          [scalar]  The number of decimals to round the numeric values in the table",
    "\n type='Table',       [string]  The type to table to be created, 'ConfInt' refers to confidence intervals, R.Matrix.Object must have exactly 2 columns",
    "\n Row.Pos='c',        [string]  The position of the columns (Not including the title of rows which are right-justified)",
	"\n Cross.Lines=FALSE,  [boolean] Should lines be printed under first row and first column",
	"\n print.Cons=TRUE,    [boolean] Should the output be printed in the console",
	"\n copy.CB=TRUE        [boolean] Should the output be copied to the users clipboard",
    "\n")
cat("--------------------------------------------------- \n")
cat("Note that if the matrix has only one line, the horizontale line won't appear even if Cross.Lines=TRUE \n")
cat("------------------------------------------------------------------------------------------------------------- \n")
cat("The printed result is also copied to the users clipboard. \n")
cat("------------------------------------------------------------------------------------------------------------- \n")
cat("For help, contact gabriel @ lemyre . org. \n")
cat("------------------------------------------------------------------------------------------------------------- \n \n")
cat("Both the following INPUTS \n")
cat('% ------------------------ \n',
	'Matrix.Object = matrix(c(seq(1,6)),ncol=3, byrow=TRUE) \n',
	'col.Titles = c("","Column title 1","Column title 2","Column title 3") \n',
	'line.Titles = c("Row title 1","Row title 2") \n',
	'\n',
	'Make.LaTeX.Table(Matrix.Object, title="Table Title", Col.Titles=col.Titles, Row.Titles=line.Titles) # Creation de la table LaTeX \n',
	'% ------------------------ \n')
cat('AND \n')
cat('% ------------------------ \n',
	'Matrix.Object = matrix(c(seq(1,6)),ncol=3, byrow=TRUE) \n',
	'col.Titles = c("","Column title 1","Column title 2","Column title 3") \n',
	'line.Titles = c("Row title 1","Row title 2") \n',
	'\n',
	'Matrix.Object.2 = rbind(col.Titles,cbind(line.Titles,Matrix.Object)) \n',
	'Make.LaTeX.Table(Matrix.Object.2, title="Table Title", Col.Titles=col.Titles, Row.Titles=line.Titles) # Creation de la table LaTeX \n',
	'% ------------------------ \n')
cat("------------------------------------------------------------------------------------------------------------- \n")
cat("gives the following OUTPUT \n")
cat(paste("% ------------------------ \n", 
	chr(92),"begin{minipage}{",
	chr(92),"linewidth} \n",
	chr(92),"centering",
	chr(92),"captionof{table}{Table Title} \n",
	chr(92),"label{Table Title} \n",
	chr(92),"begin{tabular}[t]{r | *3{c} } \n",
    chr(38)," Column title 1 ",chr(38)," Column title 2 ",chr(38)," Column title 3 ",chr(92),chr(92)," \n",
	chr(92),"hline \n",
 			"Row title 1 ",chr(38)," $1$ ",chr(38)," $2$ ",chr(38)," $3$  ",chr(92),chr(92)," \n",
 			"Row title 2 ",chr(38)," $4$ ",chr(38)," $5$ ",chr(38)," $6$  ",chr(92),chr(92)," \n",
	chr(92),"end{tabular} \n",
	chr(92),"end{minipage} \n",
 			"~",chr(92),chr(92)," \n",
 			"% ------------------------ \n",sep=""))
	
	


# ---------------------------------------------------------------------------------------------------------------------------------------------

Make.LaTeX.IntConf = function(x,title=''){ # Ecrit le code LaTeX dun environnement Tabular pour intervalles de confiance a partir dune matrice R
	
	str = paste("% ------------------------ \n",
				chr(92),"begin{minipage}{",
				chr(92),"linewidth} \n",
				chr(92),"centering",
				chr(92),"captionof{table}{",title,"} \n",
				chr(92),"label{tab:title} \n",
				chr(92),"begin{tabular}[t]{rl}",sep="")
	
	for (i in 1:nrow(x)){
		str=paste(str,"\n $",
				  chr(92),"left",chr(91),
				  round(x[i,1],3),
				  chr(92),"right.,$ ",chr(38)," $",
				  chr(92),"left.",
				  round(x[i,2],3),
				  chr(92),"right",chr(93),"$ ",
				  chr(92),chr(92),sep="")
	}
	Copie.Presse.Papier(paste(str,"\n",chr(92),"end{tabular} \n",chr(92),"end{minipage}","\n % ------------------------ \n",sep="")) # Copie le string concatene au presse-papier
	str=cat(paste(str,"\n",chr(92),"end{tabular} \n",chr(92),"end{minipage}","\n % ------------------------ \n",sep="")) # Limprime aussi dans la console R
}