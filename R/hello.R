# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
processCommand <- function(cid3){
  if(cid3$results$resultType=="error"){
    stop(cid3$results$cause)
  }else if(cid3$results$resultType=="image"){
    system(sprintf("dbfs cp dbfs:/FileStore%s %s/", cid3$results$fileName,
                   tempdir()))
    f <- sprintf("%s/%s",tempdir(),basename(cid3$results$fileName))
    out <-sprintf("%s\n", cid3$results$data)
    rr <-  function(){
      x <- (out)
      g <- (f)
      return(function(){
        require(png)
        img <- readPNG(g)
        grid::grid.newpage()
        grid::grid.raster(img)
        cat(x)
      })}()
    assign("dbxShow",value =rr(),envir=.GlobalEnv)
  }else if(cid3$results$resultType=="text"){
    out <-sprintf("%s\n", cid3$results$data)
    rr <-  function(){
      x <- (out)
      return(function(){
        cat(x)
      })}()
    assign("dbxShow",value =rr(),envir=.GlobalEnv)
  }
  rstudioapi::sendToConsole("dbxShow()", execute=TRUE)
}

sendSelection <- function(){
  dctx <- rstudioapi::getActiveDocumentContext()
  content <- dctx$contents
  sel <- rstudioapi::primary_selection(dctx)
  cursorAtRow <- sel$range$start[[1]]

  ## We now move down to find next line with ```
  whereisEnd <- NA
  current <- cursorAtRow
  while(TRUE && current <= length(content)){
     if(grepl("^```$",content[current])){
       whereisEnd <- current-1
       break
     }else current <- current+1
  }
  if(is.na(whereisEnd)) stop("Could not find location of end of chunk(i.e. ```)")
  ## where does chunk start?
  whereisStart <- NA
  current <- cursorAtRow
  while(TRUE && current >=1){
    if(grepl("^```\\{",content[current])){
      whereisStart <- current+1
      break
    }else current <- current-1
  }
  if(is.na(whereisStart)) stop("Could not find location of beginning of chunk(i.e. ```{)")

  ## Now Get Language
  languageLine <- content[whereisStart-1]
  type1 <-gregexpr("```\\{[[:blank:]]*?(?<first>[[:alnum:]]+),*[[:blank:]]+",languageLine,perl=TRUE)
  lang <- NA
  if(type1[[1]]==-1){
    ## user didn't give ```{lang ...}
    type1 <-gregexpr("```\\{[[:blank:]]*?(?<first>[[:alnum:]]+),*\\}",languageLine,perl=TRUE)
    if(type1[[1]]==-1){
      stop(sprintf("No language found .... in %s",languageLine))
    }
  }
  s <- attr(type1[[1]],"capture.start")[,'first']
  e <-  s+attr(type1[[1]],"capture.length")[,'first']-1
  lang = substr(languageLine,s,e)
  if(exists("dbxShow",envir=.GlobalEnv))
    rm("dbxShow",envir=.GlobalEnv)
  ## If this pydbx
  if(lang == 'pydbx'){
    if(is.null(getOption("dbpycontext"))){
      r <- dbxCtxMake()
      while(TRUE){
        ctxStats <- dbxCtxStatus(r)
        if(isContextRunning(ctxStats)) break
      }
      options(dbpycontext=r)
    }
    ctx <- getOption("dbpycontext")
    f <- paste( content[whereisStart:whereisEnd], collapse='\n')
    cid3 <- dbxRunCommand(f,ctx=ctx,language='python',wait=3)
    processCommand(cid3)
  }else if(lang=='rdbx'){
    if(is.null(getOption("dbRcontext"))){
      r <- dbxCtxMake(language='r')
      while(TRUE){
        ctxStats <- dbxCtxStatus(r)
        if(isContextRunning(ctxStats)) break
      }
      options(dbRcontext=r)
    }
    ctx <- getOption("dbRcontext")
    f <- paste( content[whereisStart:whereisEnd], collapse='\n')
    cid3 <- dbxRunCommand(f,ctx=ctx,language='r',wait=3)
    processCommand(cid3)

  }else if(lang=='r'){
    f <- paste( content[whereisStart:whereisEnd], collapse='\n')
    rstudioapi::sendToConsole(f)
  }


  ## otherwise do nothing. We expect the author to use a different key sequence.
  ## This is not the most elegant but a chunk has many ways to be run - Stan, R,
  ## Local Python with many options. Too many edge cases. I could handle the R
  ## bit ...


}
