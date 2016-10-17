regexMerge = function(regexList, exact = FALSE){
  assertthat::assert_that(is.logical(exact))
  exact = as.character(exact)
  out = switch (exact,
                'FALSE' = {paste0('(',paste0(regexList,collapse=')|('),')')},
                'TRUE' =  {paste0('(\\Q',paste0(regexList,collapse='\\E)|(\\Q'),'\\E)')}
  )
  out = paste0('(',out,')')
}