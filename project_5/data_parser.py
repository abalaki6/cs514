#@author
#Wolf
#project 5, machine learning


import hyperparameters as hp
import numpy as np

def parse():

    print("Parsing")
    #prepeare the train dataset
    source_traing = open(hp.source_db_train, "r").read().split("\n")
    destination = open(hp.db_train,"w+")
    matrix = [i.split(",") for i in source_traing]
    matrix = matrix[1:]
    for i in range(len(matrix)-1):
        matrix[i][-1] = matrix[i][-1][6]
        destination.write(",".join(matrix[i]) + "\n")

    matrix[:-1][-1] = matrix[:-1][-1][6]
    destination.write(",".join(matrix[-1]))
    destination.close()
    #end of prepearing train dataset
    #prepeare the test dataset
    matrix = [i.split(",") for i in open(hp.source_db_test,"r").read().split("\n")]
    matrix = matrix[1:]
    destination = open(hp.db_test,"w+")
    for i in range(len(matrix) - 1):
        destination.write(",".join(matrix[i]) + ",N/A\n")
    destination.write(",".join(matrix[-1]) + ",N/A")
    destination.close()

    print("Saved")

#this method is designed to return matrix of features from the source
def get_features(source):
    return np.array([i.split(",")[1:-1] for i in open(source,"r").read().split("\n")][:-1],dtype=int)

#this method is designed to return matrix of labels from the source
def get_labels(source):
    labels = [i.split(",")[-1] for i in open(source,"r").read().split("\n")][:-1]
    data = np.zeros((len(labels),hp.num_classes))
    for i in range(len(labels)):
        data[i][int(labels[i])-1] = 1
    return data


#this method return couple of [X,Y] of input output tensors from source
def get_XY(source):
    return [get_features(source),get_labels(source)]


def save_answer(answer):
    file = open(hp.submission,"w")
    file.write("id,Class_1,Class_2,Class_3,Class_4,Class_5,Class_6,Class_7,Class_8,Class_9\n")

    print(len(answer))
    np.savetxt(hp.submission,
               np.insert(answer,0,(np.arange(len(answer),dtype=int)+1),axis=1)
               ,delimiter=",")
