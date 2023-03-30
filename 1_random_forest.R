
require(randomForest)
require(party)

# suppress significant stars
options(show.signif.stars=FALSE)

# set seed for the pseudo-random number generator to get comparable results
# note that results might vary insignificantly due to random sampling
set.seed(1245)

# load dataset
load('forestdat.rda')

#####################
### Random Forest ###
#####################

output.forest <- randomForest(article ~
    number + count + elaboration + sr + hk + corpus,
    importance=TRUE,
    data=forestdat)
print(output.forest)
# Call:
#  randomForest(formula = article ~ number + count + elaboration + sr + hk + corpus, data = forestdat, importance = TRUE) 
#                Type of random forest: classification
#                      Number of trees: 500
# No. of variables tried at each split: 2
# 
#         OOB estimate of  error rate: 5.37%
# Confusion matrix:
#       0   a the class.error
# 0   345  14  30  0.11311054
# a    11 571   2  0.02226027
# the  34   4 757  0.04779874

print(importance(output.forest, type=2))
#             MeanDecreaseGini
# number            156.547238
# count              89.511474
# elaboration        14.068683
# sr                116.711512
# hk                441.871327
# corpus              9.662347

# generating the three and the confusion matrix
articleclassifier <- ctree(article ~
    number + count + elaboration + sr + hk + corpus,
    data=forestdat)
table(predict(articleclassifier), forestdat$article)
  #       0   a the
  # 0   356  13  42
  # a    14 569   4
  # the  19   2 749

# # Tree plot
# # (uncomment the next line if you want it)
# articletree <- plot(articleclassifier)

# getting the Mean Decrease in Accuracy
impTable = importance(output.forest)
impTable = data.frame(
    names = rownames(impTable),
    values = impTable[,'MeanDecreaseAccuracy'],
    row.names=NULL
)
impTable = impTable[order(impTable[,2], decreasing=TRUE),]
impTable
#         names   values
# 5          hk 86.89078
# 1      number 62.51398
# 2       count 50.45750
# 4          sr 25.88932
# 6      corpus 10.86416
# 3 elaboration 10.00557

# # Accuracy and Gini Index plots
# # (uncomment the next line if you want them)
# varImpPlot(output.forest)



