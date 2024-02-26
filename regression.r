options(pkg.build_extra_flags = "missing")
devtools::load_all()

set.seed(10)
trueModel <- function(X1, X2, X3) 3.2 * X1 + 2 * X2 + X3 + 0.05*X1*X2 + 0.04*X1*X2*X3
sim <- function(N){
    x1 <- runif(N,.5,15)
    x2 <- rnorm(N, mean=3.5-0.03*x1, sd = 0.93)
    x3 <- runif(N, min = x1, max = (-1)^(x1 < 0) * x1 + 3)
    y <- trueModel(x1, x2, x3) + rnorm(N, sd = 3)
    data.frame(y = y, x1, x2, x3)
}

# RPF
trainDat <- sim(1000)

plot(trainDat$x3, trainDat$y)
plot(trainDat$x3, trainDat$x1)

rpfit <- rpf(y ~ x1 + x2 + x3, data = trainDat,
             ntrees = 50,
             max_interaction = 3,
             t_try = 0.5,
             split_try = 5,
             splits = 100, CV = F, parallel = F, loss="L2")
print(rpfit)

testDat <- sim(10000)
predsTest <- predict(rpfit, testDat)
predsTrain <- predict(rpfit, trainDat)

plot(predsTest$.pred, testDat$y)
plot(predsTrain$.pred, trainDat$y)
