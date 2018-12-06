import pyNN.nest as pynn
import pynn_utils as pu
import pynn_model as pm

import json
import numpy
import sys

p1 = pynn.Population(1,pynn.IF_cond_exp(**pu.clean_parameters({"tau_syn_I":5,"tau_refrac":0,"v_thresh":-50,"v_rest":-65,"tau_syn_E":5,"v_reset":-65,"tau_m":20,"e_rev_I":-70,"type":"IFCondExp","i_offset":0,"cm":1,"e_rev_E":0})))
p3 = pynn.Population(1,pynn.IF_cond_exp(**pu.clean_parameters({"tau_syn_I":5,"tau_refrac":0,"v_thresh":-50,"v_rest":-65,"tau_syn_E":5,"v_reset":-65,"tau_m":20,"e_rev_I":-70,"type":"IFCondExp","i_offset":0,"cm":1,"e_rev_E":0})))
proj0 = pynn.Projection(p1, p3, connector = pynn.AllToAllConnector())
node1 = pm.LearningNode(p1)
node3 = pm.LearningNode(p3)
proj0.set(weight=numpy.random.normal(1.0, 1.0))
node1.connect_to(node3, proj0)

model = pm.Model(node1,node3,[p1,p3],pynn)

if __name__ == '__main__':
    [[x_train, y_train], [x_test, y_test]] = json.load(sys.stdin)
    
    model.train(x_train, y_train)
    #model.test(x_test, y_test)

