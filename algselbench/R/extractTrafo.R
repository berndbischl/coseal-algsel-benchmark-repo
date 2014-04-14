extractTrafo = function(trafo) {
  trafo.string = deparse(trafo)
  if (trafo.string[1] == "function (x) ") {
    trafo.string = trafo.string[2]
  } else if (grepl(".Primitive", trafo.string)) {
    trafo.string = sprintf("%s(x)", strsplit(deparse(trafo), "\"")[[1]][2])
  }  
  return(trafo.string)
}
