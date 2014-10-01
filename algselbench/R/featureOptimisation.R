optimise = function(bitstring, ldf) {
    sel = (charToRaw(bitstring) == charToRaw("1"))
    ldf$features = ldf$features[sel]
    model = regression(makeLearner("regr.randomForest"), ldf)
    score = mean(parscores(ldf, model))
    return(score)
}
