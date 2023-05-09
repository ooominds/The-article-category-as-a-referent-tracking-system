
require(randomForest) # tested versions are: 4.6-14 and 4.7-1.1
require(party)        # tested versions are: 1.3.10 and 1.3.13

load('forestdat.rda')

# set seed for the pseudo-random number generator to get comparable results
# note that results will be identical only if you run all the steps
set.seed(1828)

#####################
### All variables ###
#####################

output.forest.corr <- randomForest(article ~
    number + count + phrase + elaboration + sr + hk + corpus,
    data=forestdat)
print(output.forest.corr)
# Call:
#  randomForest(formula = article ~ number + count + phrase + elaboration +      sr + hk + corpus, data = forestdat) 
#                Type of random forest: classification
#                      Number of trees: 500
# No. of variables tried at each split: 2
# 
#         OOB estimate of  error rate: 8.77%
# Confusion matrix:
#       0   a the class.error
# 0   398  42  63  0.20874751
# a    22 700   3  0.03448276
# the  45  18 909  0.06481481

print(importance(output.forest.corr,type=2)) 
#             MeanDecreaseGini
# number             160.52619
# count              104.79478
# phrase              11.99925
# elaboration         19.17432
# sr                 132.09558
# hk                 520.72642
# corpus              14.51855

# tree
articleclassifier.corr <- ctree(article ~
    number + count + phrase + elaboration + sr + hk + corpus,
    data=forestdat)
articletree <- plot(articleclassifier.corr)
table(predict(articleclassifier.corr), forestdat$article)
  #       0   a the
  # 0   428  26  59
  # a    41 697  18
  # the  34   2 895

#############################
### Excluding set phrases ###
#############################

data_nosetphr <- forestdat[which(forestdat$phrase=='no'),]

output.forest.corr.nosetphr <- randomForest(article ~
    number + count + elaboration + sr + hk + corpus,
    data=data_nosetphr)
print(output.forest.corr.nosetphr)
# Call:
#  randomForest(formula = article ~ number + count + elaboration +      sr + hk + corpus, data = data_nosetphr) 
#                Type of random forest: classification
#                      Number of trees: 500
# No. of variables tried at each split: 2
# 
#         OOB estimate of  error rate: 5.45%
# Confusion matrix:
#       0   a the class.error
# 0   354  14  29  0.10831234
# a    12 571   2  0.02393162
# the  36   4 759  0.05006258

print(importance(output.forest.corr.nosetphr,type=2))
#             MeanDecreaseGini
# number            158.025404
# count              94.060572
# elaboration        14.818542
# sr                121.177680
# hk                443.815725
# corpus              9.811223

# tree
articleclassifier.corr.nosetphr <- ctree(article ~
    number + count + elaboration + sr + hk + corpus,
    data=data_nosetphr)
articletree <- plot(articleclassifier.corr.nosetphr)
table(predict(articleclassifier.corr.nosetphr), data_nosetphr$article)
  #       0   a the
  # 0   363  14  38
  # a    14 569   4
  # the  20   2 757

###########################################
### Excluding set phrases and no number ###
###         >>> FINAL MODEL <<<         ###
###########################################

data_nonothing <- forestdat[which(forestdat$phrase=='no' & forestdat$number!='no'),]

output.forest.corr.nonothing <- randomForest(article ~
    number + count + elaboration + sr + hk + corpus,
    importance=TRUE, # to get the variable importance
    data=data_nonothing)
print(output.forest.corr.nonothing)
# Call:
#  randomForest(formula = article ~ number + count + elaboration +      sr + hk + corpus, data = data_nonothing, importance = TRUE) 
#                Type of random forest: classification
#                      Number of trees: 500
# No. of variables tried at each split: 2
# 
#         OOB estimate of  error rate: 5.43%
# Confusion matrix:
#       0   a the class.error
# 0   347  14  28  0.10796915
# a    11 571   2  0.02226027
# the  37   4 754  0.05157233

print(importance(output.forest.corr.nonothing,type=2))
#             MeanDecreaseGini
# number            159.014326
# count              89.345417
# elaboration        14.022149
# sr                122.484131
# hk                435.724246
# corpus              8.838762

# Plots: Gini and Decrease in Accuracy
varImpPlot(output.forest.corr.nonothing) 

# get Mean Decrease in Accuracy (in decreasing order)
impTable = importance(output.forest.corr.nonothing)
impTable = data.frame(
    names = rownames(impTable),
    values = impTable[,'MeanDecreaseAccuracy'],
    row.names=NULL
)
impTable = impTable[order(impTable[,2], decreasing=TRUE),]
impTable
#         names   values
# 5          hk 81.77669
# 1      number 65.90465
# 2       count 51.46218
# 4          sr 25.98047
# 6      corpus 11.17880
# 3 elaboration 10.60218

# tree
articleclassifier.corr.nonothing <- ctree(article ~
    number + count + elaboration + sr + hk + corpus,
    data=data_nonothing)
articletree <- plot(articleclassifier.corr.nonothing)
table(predict(articleclassifier.corr.nonothing), data_nonothing$article)
  #       0   a the
  # 0   356  13  42
  # a    14 569   4
  # the  19   2 749

# complete importance table
# importance(output.forest.corr.nonothing)


