
# GBH Functions
# Hugo Andres Dorado
# Dummy Process

dummyProcess <- function(dataset,response){
 
    dummyMat <- data.frame(dataset[sapply(dataset, is.factor)],dataset[response])
    
    modDummy    <- dummyVars(formula(paste (response, "~ .")),data = dummyMat)
    
    procesMatrix <- as.data.frame(predict(modDummy,dummyMat))
    
    dummyNams <- strsplit(names(procesMatrix),"[.]")
    
    
    namVar <- sapply(dummyNams,function(x){x[1]})
    
    namInd <- sapply(dummyNams,function(x){x[2]})
    
    namVarInd <- lapply(split(namInd,namVar),paste,collapse = ',')
    
    namFinal <- paste(names(namVarInd),unlist(namVarInd),sep="_")
    
    idenxList <- split(1:ncol(procesMatrix),namVar)
    
    lisMatrix <- lapply(idenxList,function(x){
        u <- procesMatrix[,x]
        data.frame(apply(u,1,paste,collapse = ","))
    }
    )
    
    lisMatrix <- do.call(cbind,lisMatrix)
    
    names(lisMatrix) <- namFinal

    numDataSet <- dataset[,sapply(dataset, is.numeric)]
    
    matOptim =data.frame(numDataSet,lisMatrix )
    
    namsMatOp <- names(matOptim)
    
    matOptim <- data.frame(matOptim[,-which(namsMatOp==response) ],
                           matOptim[which(namsMatOp==response)])
    
    
    matTrain <- data.frame(numDataSet,procesMatrix)
    
    namsMatTr <- names(matTrain)
    
    matTrain <- data.frame(matTrain[-which(namsMatTr==response) ],
                           matTrain[which(namsMatTr==response) ])
    
    list(matOptim = matOptim  ,matTrain = matTrain )
    
}


# Dummy reverse

desconvMat <- function(matrixDummy,namDummyVars)
{
    newNames <- lapply(strsplit(namDummyVars,"_"),function(x){paste(x[1],unlist(strsplit(x[2],'[.]')),sep=".")})
    
    matConv <- 
        do.call(cbind,
                lapply(seq(length(namDummyVars)),function(x){
                    
                    mainNam<- namDummyVars[x]
                    
                    dummMar <- matrixDummy[mainNam]
                    
                    ncols <- (nchar(as.character(dummMar[1,]))+1)/2
                    
                    df_conv <- data.frame(matrix(as.numeric(unlist(strsplit(paste(matrixDummy[,mainNam],collapse = ","),',') )),nrow =nrow(dummMar) ,
                                                 ncol =ncols , byrow = T))
                    
                    names(df_conv) <- newNames[[x]]
                    df_conv
                    
                })
        )
    
    data.frame(matConv , matrixDummy[!(names(matrixDummy) %in%namDummyVars) ])
    
}

# Extract ranges

extract_ranges <- function(matrix,scale){
  
  seq_var <- 1:length(scale)
  nam_var <- names(matrix)
  
  ranges <- 
  lapply(seq_var,function(w){
    
    var <-  matrix[w]
    scl <-  scale[w]
    
      if(scl=='Nominal'){
        
        as.character(unique(var)[,1])
      }else if(scl=='Discrete'|scl=='Continuos'){
        
        c(max=max(var),min=min(var))
          
        }
    
  })
  names(ranges) <- nam_var
  ranges
}

# Random inicialization variable

randomIni <- function(rang,scale){
    
    switch(scale,
           Continuos   = runif(1,min = rang[2], max = rang[1]),
           Discrete  = sample(rang[2]:rang[1],1 ),
           Nominal = {
             factor(sample(rang,1),levels = rang)
                    }
           )
}

# Random inicialization population

randomGen <- function(ranges , scales ,n){
    
    matGen <- 
        do.call(rbind,lapply(1:n,function(u){
        sol <- do.call(data.frame,lapply(seq(length(scales)),
                                   function(x){randomIni(ranges[[x]],scales[x])}
                                   )  )
        names(sol) <- paste0("x",1:length(sol))
        sol        
        })
        )
    
    
    names(matGen) <- names(ranges)
    
    matGen
}

# Improvise Fun

improviseFun <- function(hm,hmcr,bestHarmony,par,ranges,scales){ # Incluir Harmony memory
    # Recibir harmony memory
    
    hmss <- hm[,-ncol(hm)]
    namsHarmony <- names(hmss) 
    
    # Hacer este proceso para una sola harmonia
    # Cada columna puede ser modificada, por harmonias distintas o mejor solucion, o aleatoria
    
    #unlist( sapply(seq(length(harmony)-1),function(x){

    solution <-
    do.call(data.frame,
        lapply( 1:ncol(hmss) ,  function(w){
       
            if(runif(1) <= hmcr){ # Memory consideration
            
            xi <- hmss[sample(1:nrow(hmss),1),w] #---Escoger una al azar y escoger subx (Escoger al azar un armonia)
            
                if(runif(1)<=par){ # Pitch adjustment
                
                    xi <-  bestHarmony[w]
                }
           
            }else{xi <- randomIni(ranges[[w]],scales[w] )} # Random selection
            
            xi
            
            }
        )
    )
    names(solution) <- namsHarmony
    solution
}

computeLeves <- function(varc){
    len <- (nchar(as.character(varc))+1)/2
    vect0 <- array(0,len)
    sapply(1:length(vect0),function(w){
        vect0[w] <- 1
        paste(vect0,collapse = ',')
    })
}

# como hacemos la optimizacion de parametros?
# Revisar remplazo
# Agregar grafiquita de mejora

# par(.30,.35,.40)
# hmcr(.85,.90,.95)
# hms(5,10,15)
# numIprov(maximo tiempo ejecucion) por definir

# GHS FUN training

GHS_MM <- function(hms,hmcr,par,maxNumImp,fitnessfun,model_train,fixedValues=NULL,ranges,scales,namFile = "opt_ghs.txt",ghrp=T){
    
    cat("# GHS Optimization\n",file=namFile,append = F)
    cat("# Hugo Andres Dorado, Reserach at CIAT and master student Unicauca\n",file=namFile,append = T)
    cat("\n",file=namFile,append = T)
    
    hm <- randomGen(ranges,scales,hms) # Generate Harmony memory randomly 
    

    hm <- data.frame(hm,fitness = fitnessfun( model = model_train,inputs =  hm,
                                              fixedValues = fixedValues))
    
    hm <- hm[order(hm$fitness,decreasing = T),]
    
    gr <- data.frame(iter = 1:hms,hm )
    
    bestHarmony <- hm[1,]
    worstHarmony <- hm[hms,]
    
    write.table(as.data.frame(rbind(names(gr))),append=T, file=namFile,sep='\t',row.names=F,col.names = F)
    write.table(gr,append=T, file=namFile,sep='\t',row.names=F,col.names = F)
    
    bestSol <- sort(hm$fitness,decreasing = F)
    if(ghrp){
        plot(1:hms,sort(hm$fitness,decreasing = F),type="l")
        
    }
    # for(i in (hms+1):12){
    # HS Initialization 
    for(i in (hms+1):maxNumImp){

         # aplicar para toda la pob y obtener una sola solucion
      
        newHM   <- improviseFun(hm,hmcr,bestHarmony,par,ranges,scales) 
        
        fitness <- fitnessfun( model_train, newHM,fixedValues = fixedValues) # Debe ser un solo registro
        
        # Buscar el peor, 
        # si newHM.fiteness > peor.fitness -> Remplazar en hm al peor por newHM
        # Y ordenar la memoria
        
        newHM <-  data.frame(newHM,fitness=fitness)
        
        if( newHM$fitness > worstHarmony$fitness){
            
            hm[hms,] <- newHM
            
            hm <- hm[order(hm$fitness,decreasing = T),]
            
            bestHarmony <- hm[1,]
            worstHarmony <- hm[hms,]
        }
        
        bestSol[i] <- bestHarmony$fitness
        if(ghrp){
            
            plot(1:i,c(bestSol ),type="l",xlab = 'iteration' ,ylab = 'aptitude')
        }
        
        write.table(data.frame(iter = i, bestHarmony ),file=namFile,append=T, sep='\t',row.names=F, col.names=F)
    }
    list(bestSol=bestSol,bestHarmony=bestHarmony)
}

# Fitness function by default

fitnessfun <- function(model,inputs,fixedValues = NULL){

  cualt_vars <- names(inputs)[sapply(inputs,is.factor)]
  train_ds <- model$trainingData
  
  for(vars in cualt_vars){
    inputs[,vars] <- factor(inputs[,vars],levels = levels(train_ds[,vars]))
  }
  
    as.numeric(
        predict( model,if(is.null(fixedValues)){
            inputs
        }else{
          
          
          
            data.frame(inputs,fixedValues,row.names = 1:nrow(inputs))
        } 
        )
    )
}
