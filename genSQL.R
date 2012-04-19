#-----------
# SQL Translation
#------------


#--------------------
# genSQL Function
#     The genSQL function currently works for the following model types
#              * Any lm model with interactions
#              * Any binomial logistic regression model with interactions
#              * Any rpart model
# Input: model - an object of type lm(), glm(), or rpart()
#        digits - the number of digits to round to, defaults to machine's floating point precision
#        outcome.name - the name for the decision tree prediction, defaults to NULL
# Output: An ANSI SQL calculation for the model
# Calls: lm2sql(), glm2sql(), tree2sql()
#------------------

genSQL <- function(model, digits = .Machine$sizeof.longdouble, outcome.name = NULL) {
  model.class <- class(model)[1]
  # Error handling
  if (any(model.class == c("glm", "rpart", "lm")) == FALSE) {
    warning(model.class, " is not a supported model type")
    stop()
  }
  
  # Model function selection
    if (model.class == "lm") {
        sql.statement <- lm2sql(lm.model = model, digits)
    } else if (model.class == "glm") {
        sql.statement <- glm2sql(glm.model = model, digits)
    } else if (model.class == "rpart") {
        sql.statement <- tree2sql(dtree=model, outcome.name)
    }
  
  return(sql.statement)
}


#-----------------
# lm2sql
# Input: lm.model - a model of class "lm"
#        digits - the number of digits to round to
# Output: an SQL description of the model
#----------------

lm2sql <- function(lm.model, digits) {
  # Get the coefficients
  coeff <- summary(lm.model)$coefficients[, "Estimate"]
  coeff <- round(coeff, digits)
  # Store the variable names
  var.names <- rownames(summary(lm.model)$coefficients)
  # Concatenate the coefficients and variable names
  sql.var <- paste(coeff[-1], "*" ,var.names[-1], "+", collapse = " ")
  # Concatentate the SQL syntax and add the intercept
  sql.statement <- paste("SELECT", sql.var, coeff[1])
  # Apply any interaction correction
  sql.statement <- interaction.correction(sql.statement)
  
  return(sql.statement)
}

#-------------
# glm2sql
# Input: glm.model - a model of class "glm"
#        digits - the number of digits to round to
# Output: an SQL description of the model
# Notes: only supports binomial logistic regression
#----------------

glm2sql <- function(glm.model, digits) {
  # Error checking
  if (glm.model$family$family != "binomial") {
    warning("Only binomial logistic regression supported")
    stop()
  }
  # Get the coefficients
  coeff <- summary(glm.model)$coefficients[, "Estimate"]
  coeff <- round(coeff, digits)
  # Store the variable names
  var.names <- rownames(summary(glm.model)$coefficients)
  # Concatenate the coefficients and variable names
  sql.var <- paste(coeff[-1], "*" ,var.names[-1], "+", collapse = " ")
  # Concatenate the SQL syntax and add the intercept
  sql.statement <- paste("SELECT 1 / (1 + exp(", sql.var, coeff[1], "))")
  # Apply any interaction correction
  sql.statement <- interaction.correction(sql.statement)
  
  return(sql.statement)
}

#----------
# interaction.check
#   Checks to see if any of the specified model terms are interactions
# Input: sql.statement -
# Output: a sql statement with the interactions correctly specified
#------------
interaction.correction <- function(sql.statement) {
 corrected.statement <- gsub(pattern=":", replacement="*", x=sql.statement) 
 return(corrected.statement)
}

#---------------------------------
# Tree Parse Function
# Input: dtree - a decision tree from rpart()
# Output: the parsed rule set
#-----------------------------------

tree.parse <- function(dtree) {
  # Grab the rule stack
  rule.stack <- capture.output(dtree)
  # Remove the data to the right of the rule
  match.right <- regexpr(pattern="[[:digit:]]+[[:space:]]{2}[[:digit:]].*", text=rule.stack)
  sub.result <- unlist(regmatches(x = rule.stack, m=match.right, invert = TRUE))
  # Remove the rule number and spaces to the left of the rule
  match.left <- regexpr(pattern="[[:space:]]*[[:digit:]]+)[[:space:]]", text = sub.result)
  final.result <- unlist(regmatches(x = sub.result, m=match.left, invert=TRUE))
  final.result <- final.result[final.result != ""][-1:-4]
  
  return(final.result)
}

#--------------
# Node Prediction Function
# Input: dtree - a decision tree from rpart()
# Output: a vector of the probabilities/predictions for each node
#-------------

node.pred <- function(dtree) {
  pred.df <- NA
  
  if (dtree$method == "anova") { pred <- dtree$frame$yval
    } else if (dtree$method == "class") { pred <- dtree$frame$yval2[, 4] }
  
  leaf <- ifelse(dtree$frame$var == "<leaf>", yes=1, no = 0)
  
  pred.df <- data.frame("node" = rownames(dtree$frame), "pred" = pred, "leaf" = leaf)
  
  return(pred.df)
}

#--------------------
# Tree Stack Function
# Input: dtree - a decision tree from rpart()
# Output: a rule stack data frame
# Required: node.pred(), tree.parse()
#------------------

tree.stack <- function(dtree) {
  df <- node.pred(dtree)
  node.rule <- c("root", tree.parse(dtree))
  df <- data.frame(df, "rule" = node.rule)
  
  return(df)
}

#------------------
# Tree to SQL Translation Function
# Input: dtree - a decision tree from rpart()
#        outcome.name - the name for the predicted value
# Output: An ANSI SQL description of the model
# Required: tree.stack(), node.pred(), tree.parse()
#----------------

tree2sql <- function(dtree, outcome.name) {
  
  # Initialize
  stack <- tree.stack(dtree)
  sql.statement <- NULL
  curr.state <- 0
  branch.depth <- 0
  
  # Loop through each rule in the stack
  for (i in 2:dim(stack)[1]) {
    # Setup
    #-----------
    # Current and Next State
    if (i == 2) { curr.state <- 0
      } else { curr.state <- stack[i-1, "leaf"]}
    next.state <- stack[i, "leaf"]
    # Current Rule
    rule.i <- stack[i, "rule"]
    # Tab number
    tabs <- paste(rep("\t", times=branch.depth), collapse='')
    
    # Logic: Translates stack -> tree
    #-------------
    # Node -> Node
    if (curr.state == 0 & next.state == 0) {
      # Write statement
      statement.i <- paste("\n", tabs, "CASE WHEN", rule.i, "THEN")
      sql.statement <- paste(sql.statement, statement.i)
      # Increment branch depth
      branch.depth <- branch.depth + 1
      next()
    }
    # Node -> Leaf Transition
    if (curr.state == 0 & next.state == 1) {
      # Write statement
      pred.i <- stack[i, "pred"]
      statement.i <- paste("\n", tabs, "CASE WHEN", rule.i, "THEN", pred.i)
      sql.statement <- paste(sql.statement, statement.i)
      # Increment branch depth
      branch.depth <- branch.depth + 1
      next()
    }           
    # Leaf -> Node Transition
    if (curr.state == 1 & next.state == 0) {
      # Write statement
      statement.i <- paste("\n", tabs, "WHEN", rule.i, "THEN")
      sql.statement <- paste(sql.statement, statement.i)
      next()
    }
    # Leaf -> Leaf Transition
    if (curr.state == 1 & next.state == 1) {
      # Write statement
      pred.i <- stack[i, "pred"]
      statement.i <- paste("\n", tabs, "WHEN", rule.i, "THEN", pred.i)
      sql.statement <- paste(sql.statement, statement.i)
      # Close 1 open CASE statement
      sql.statement <- paste(sql.statement, "\n", tabs, "END")
      # Decrement the branch.depth
      branch.depth <- branch.depth - 1
      next()
    }
  }
  # END any hanging CASE clauses
  termination <- paste(rep("\nEND", times = branch.depth), collapse='')
  # Throw on the outcomes name
  sql.statement <- paste("SELECT", sql.statement, termination, "AS", outcome.name)
  return(sql.statement)
}

