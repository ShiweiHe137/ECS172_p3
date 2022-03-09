library(rectools)
library(devtools)
library(softImpute)
library(recosystem)
library(stringr)
library(regtools)
library(qeML)
bookRatings <- fread("BX-Book-Ratings.csv")
bookBX <- fread("BX-Books.csv")
userBX <- fread("BX-Users.csv")

bookRatings <- data.frame(bookRatings[sample(nrow(bookRatings), 30000), ])

index_book <- data.frame(match(bookRatings$ISBN,
                               bookBX$ISBN, 
                               nomatch = NA_integer_, 
                               incomparables = NULL))
for(i in 1:nrow(index_book)){
  bookRatings[i,4] <- bookBX[i,4]
}

userBX$Location <- word(userBX$Location,-1)
userBX <- userBX[userBX$Age !="NULL",]
index_user <- data.frame(match(bookRatings$User.ID, 
                               userBX$`User-ID`, 
                               nomatch = NA_integer_, 
                               incomparables = NULL))
for(i in 1:nrow(index_user)){
  bookRatings[i,5] <- userBX[i,2]
  bookRatings[i,6] <- userBX[i,3]
}


new <- data.frame(NULL)
temp <- data.frame(unique(bookRatings$ISBN))

index<-data.frame(match(temp[,1],bookRatings$ISBN, nomatch = NA_integer_, incomparables = NULL))
index<- data.frame(index[sample(nrow(index), 10000), ])

for(i in index[,1]){
  new <- rbind(new,bookRatings[i,])
}
i = 0
for(i in 1:nrow(new)){
  new[i,2] = i
}

#using matrix factorization to train on bookRatings
set.seed(123)
r = Reco()
split1<- sample(c(rep(0, 0.8 * nrow(new)), rep(1, 0.2 * nrow(new))))
bookRatings_train_set <- new[split1 == 0, ]
bookRatings_test_set <- new[split1== 1, ] 
bookRatings_test_set <- bookRatings_test_set[,-6]
bookRatings_test_set <- bookRatings_test_set[,-5]
bookRatings_test_set <- bookRatings_test_set[,-4]
bookRatings_test_set <- bookRatings_test_set[,-3]
bookRatings_test_set$ISBN <- as.numeric(bookRatings_test_set$ISBN)


# remove rating = 0, reduce file size
#bookRatings_train_set <- bookRatings_train_set[bookRatings_train_set$'Book-Rating'!=0,]
#bookRatings_train_set$ISBN <- gsub("X",0,bookRatings_train_set$ISBN)

bookRatings_train_set$ISBN <- as.numeric(bookRatings_train_set$ISBN)

# save to disk
write.table(bookRatings_train_set, "train.txt", row.names = FALSE, col.names = FALSE)
# call data_file to load train.dat from disk for train
bookRating_train = data_file("train.txt")
#bookRatings_train_sample <- bookRatings_train_sample[!is.na(as.numeric(as.character(bookRatings_train_sample[,2]))),]
# train a model
opts = r$tune(bookRating_train, opts = list(dim = c(10, 20, 30), lrate = c(0.1, 0.2),
                                            costp_l1 = 0, costq_l1 = 0,
                                            nthread = 1, niter = 10))
print(opts$min)
print(opts$res)

r$train(bookRating_train, opts = c(opts$min, nthread = 1, niter = 10))

write.table(bookRatings_test_set, "test.txt", row.names = FALSE, col.names = FALSE)
bookRating_test = data_file("test.txt")
pred_rvec = r$predict(bookRating_test,out_memory())

