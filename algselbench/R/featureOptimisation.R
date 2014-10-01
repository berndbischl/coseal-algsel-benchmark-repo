optimise = function(bitstring, ldf) {
    sel = (charToRaw(bitstring) == charToRaw("1"))
    ldf$features = ldf$features[sel]
    folds = cvFolds(ldf)
    model = regression(makeLearner("regr.randomForest"), folds)
    score = mean(parscores(folds, model))
    return(score)
}
