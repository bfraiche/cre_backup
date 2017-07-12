(a <- list(aa=1, bb=2, cc=3))

# Create a 2-row, 3-column matrix with named headings
data <- c(1, 2, 3, 4, 5, 6)
headings <- list(NULL, c("a","b","c"))
m <- matrix(data, nrow=2, ncol=3, byrow=TRUE, dimnames=headings)

# if then else
a <- 66
if (a > 55) {
  print("a is more than 55")
} else {
  print("A is less than or equal to 55")
}

# for loop
mylist <- c(55, 66, 77, 88, 99)
for (value in mylist) {
  print(value)
}

# while loop
a <- 100
while (a < 500) {
  a <- a + 100
}

# the caret package is very useful in machine learning
install.packages("caret")
library(caret)
library(help="caret")

# Assignment. R uses the arrow operator (<-) for assignment, not a single equals (=).
# Case Sensitive. The R language is case sensitive, meaning that C() and c() are two different function calls.
# Help. You can help on any operator or function using the help() function or the ? operator and help with packages using the double question mark operator (??).
# How To Quit. You can exit the R interactive environment by calling the q() function.
# Documentation. R installs with a lot of useful documentation. You can review it in the browser by typing: help.start()
