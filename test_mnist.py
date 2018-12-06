from keras.datasets import mnist

(X_train, y_train), (X_test, y_test) = mnist.load_data()

X_train = X_train.reshape(-1, 28 * 28) # 784
X_test = X_test.reshape(-1, 28 * 28)
X_train /= 255
X_test /= 255

X_train = X_train[:100]
y_train = y_train[:100]

import pyNN.nest as pynn
import volrpynn as v

pynn.setup()

p1 = pynn.Population(784, pynn.IF_cond_exp())
p2 = pynn.Population(100, pynn.IF_cond_exp())
p3 = pynn.Population(10, pynn.IF_cond_exp())
l1 = v.Dense(pynn, p1, p2)
l2 = v.Dense(pynn, p2, p3)
m = v.Model(pynn, l1, l2)
optimiser = v.GradientDescentOptimiser(v.spike_softmax, 0.1)
_, e = optimiser.train(m, X_train, y_train, v.sum_squared_error, v.relu_derived)
print(e)
