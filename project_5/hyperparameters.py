#@author
#Wolf
#project 5, machine learning

#this file is designed to store all the hyperparameters
#that have been used in the project

source_db_train = "train.csv"
#source_db_train = "partition.csv"
source_db_test = "test.csv"
db_train = "db_train.csv"
db_test = "db_test.csv"
submission = "submission.csv"
num_classes = 9


#logreg
limit_iterations = 10000
learning_rate_logreg = 1e-6
eps = 1e-4
