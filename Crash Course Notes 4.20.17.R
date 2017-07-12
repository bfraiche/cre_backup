# a DATA.FRAME is like a matrix, but accomodates fields (columns) with different data types
(df <- data.frame(name = c('Matt','Kate','Jacquie','Simon','Nita'), age = c(35,29,32,35,39)))

# examine their internal stucture
str(df)

# data interrogation with square brackets
df[1,]     #row
df[2:3,]   #rows
df[,1]     #column
df[2,1]    #value
# interrogate data.frames by field name using the '$' operator. the result is a simple vector
df$name
df$name[2]

# data.frame and matrix objects can have field (column) and record (row) names
dimnames(df)
colnames(df)
row.names(df)

names(df) <- c('person','years') # names can be reassigned

# check dimensions of vector/matrix/array/data.frame objects
length(vec)
dim(df)
dim(arry)
nrow(df)
ncol(df)

# aggregate() is a powerful function for summarising categorical data
aggregate(InsectSprays$count, by=list(InsectSprays$spray), FUN=mean)
sumInsects <- aggregate(InsectSprays$count, by=list(InsectSprays$spray), FUN=sum)
names(sumInsects) <- c('group', 'sum')
print(sumInsects)

sumWB <- aggregate(warpbreaks$breaks, by=list(warpbreaks$wool, warpbreaks$tension), FUN=mean)

# subset/apply filter to a data.frame
warpbreaks[warpbreaks$wool=='A',]                 # by 1 condition
warpbreaks[warpbreaks$tension %in% c('L','M') & warpbreaks$wool=='A',]   # multiple conditions

# handling 'NA' values
(x = 1:5)
x[8] = 8
x[3] = NA
x[na.omit(x)]
x[!is.na(x)]

# adding entries is possible (if a bit tricky)
(newrow <- data.frame(breaks=42, wool='B', tension='M'))
(warpbreaks <- rbind(warpbreaks, newrow))

# reorder a dataframe with 'order'
df[order(df$years),]

# useful basic math functions
seq(-2, 2, by=.2)                 # sequence of equal difference
seq(length=10, from=-5, by=.2)    # with range defined by vector length
rnorm(20, mean = 0, sd = 1)       # random normal distribution
runif(20, min=0, max=100)         # array of random numbers
sample(0:100, 20, replace=TRUE)   # array of random integers
table(warpbreaks[,2:3])           # array summary stats (powerful summary tool)

abs(-5)                                # magnitude
sd(rnorm(10))                          # standard deviation
4^2                                    # square
sqrt(16)                               # square root
5%%3                                   # modulo (remainder after subtraction of any multiple)
6%%2                                   
for(i in 1:100) if(i%%20==0) print(i)  # useful for running an operation every n'th cycle

# Importing and exporting data using comma-separated file
write.csv(df, 'example.csv')       # save to csv file
read.csv('example.csv')