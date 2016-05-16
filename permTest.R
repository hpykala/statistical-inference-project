##Function to perform two sided permutation test to test if the mean of variable y is different
##between groups g1 and g2
## y: vector containing samples for the variable
## group: vector containing group values for y
## g1: value of the first group
## g2: value of the second group
permTest <- function(y, group, g1, g2, n = 10000) {
    
    # test validity of parameters
    if(length(y) != length(group)) {
        message("ERROR: y and group have different lengths")
        return()
    }
    
    if(!any(group == g1)) {
        message("no samples in g1")
        return()
        
    }
    
    if(!any(group == g2)) {
        message("no samples in g2")
        return()
        
    }
    
    #subset only the groups being tested
    gsub <- group[group %in% c(g1,g2)]
    ysub <- y[group %in% c(g1,g2)]

    # test statistic is the difference of the mean of y in groups g1 and g2
    testStat <- function(var,gr) {mean(var[gr==g2]) - mean(var[gr == g1])}
    
    #original permutation
    origStat <- testStat(ysub,gsub)
    
    #test statistic for n permutations
    permStats <- sapply(1:n, function(i) testStat(ysub, sample(gsub)))
    
    #p value for two sided test is calculated as the percentage of permutations 
    #having more extreme value than the original regardless of the sign
    p <- mean(abs(permStats) > abs(origStat))
    
    # return p-value, test statistics of permutations and the original test statistic
    list(p = p, permStats = permStats, origStat = origStat)
}