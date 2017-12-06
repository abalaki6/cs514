#@author
#Wolf
#project 5, machine learning

import hyperparameters as hp
import data_parser as p
import numpy as np

def pure_logreg():
    [X,Y] = p.get_XY(hp.db_train)
    X_test = p.get_features(hp.db_test)

    model = fit(X,Y, hp.limit_iterations, hp.learning_rate_logreg, hp.eps)
    return predict(model, X_test)
#pure_logreg()



def fit(X,Y, limit, n, eps):
    print("Number of iterations: " + str(limit))
    sizeX = X.shape[1]
    sizeY = Y.shape[1]
    w = np.zeros((sizeX,sizeY))
    grad = np.zeros((sizeX,sizeY))

    count = 0
    absgrad = eps+1
    while absgrad > eps:
        prediction = predict(w,X)
          
        for j in range(len(w)):
                grad[j] = np.sum([ (y * x[j] - x[j] * s) for x,y,s in zip(X,Y, prediction)],axis=0)
        w = w + n * grad
        count += 1

        
        if count > limit:
            break

        #if count % 100 == 0:
        print("At iteration: " + str(count))

        absgrad = np.sum(np.square(grad))
    return w

def predict(model,X):
    power = np.matmul(X,model)
    power = power - np.amax(power,axis=1)[:,None]
    exp = np.exp(power)
    prediction = exp/np.sum(exp,axis=1)[:,None]
    return prediction
