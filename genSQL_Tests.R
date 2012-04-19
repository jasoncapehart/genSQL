#--------------------
# genSQL Test
# TODO: Make this into a real set of tests with the testhat package
#

#----------------
# Test Functions
#---------------
genSQL(lm1)
genSQL(glm1)
genSQL(tree)



#------------
# Model and Data Generation for Test Functions
#-------------

# Data
#--------
x1 <- runif(50, min = -10, max = 10)
x2 <- runif(50, min = -10, max = 10)
y <- x1 + x2 + rnorm(50, mean=0, sd = 3)
y2 <- rbinom(50, size = 1, prob = 0.5)

df <- data.frame(x1, x2, y)
df2 <- data.frame(x1, x2, y2)

x3 <- c(rep("Really Long Variable Name Here", times = 40), rep("Shorter Different Variable Name", times = 10))
x3 <- factor(x3)
y3 <- factor(c(rep(0, times = 40), rep(1, times = 10)))
df3 <- data.frame(x1, x3, y3)


# LM
#------------
lm1 <- lm(y~., data=df)

summary(lm1)$coefficients

coeff <- summary(lm1)$coefficients[, "Estimate"]
var.names <- rownames(summary(lm1)$coefficients)

sql.var <- paste(coeff[-1], "*" ,var.names[-1], "+", collapse = " ")
sql.statement <- paste("SELECT", sql.var, coeff[1])

# GLM
#--------------
glm1 <- glm(y2~x1*x2, data=df2, family = "binomial")
summary(glm1)$coefficients

coeff <- summary(glm1)$coefficients[, "Estimate"]
var.names <- rownames(summary(glm1)$coefficients)

sql.var <- paste(coeff[-1], "*" ,var.names[-1], "+", collapse = " ")
sql.statement <- paste("SELECT 1 / (1 + exp(", sql.var, coeff[1], ")")


# Decision Tree
#---------------
library(rpart)

# 2 parts
# 1) Parse out variable name, sign, variable value, and output value
# 2) Build the tree

tree <- rpart(y2 ~., df2)
tree2 <- rpart(y3~., data = df3, method="class")