#@author
#Wolf
#project 5, machine learning


import data_parser as p
import logistic_regression as lr

def train(model):
    p.parse()

    if model == "logistic regression":
        answer = lr.pure_logreg()
        p.save_answer(answer)
    

train("logistic regression")
