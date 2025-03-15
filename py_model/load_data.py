import numpy as np
import pandas as pd
import pickle
import os
from sklearn.model_selection import train_test_split

def load_data():
    try: 
        model_data = pd.read_csv('model/model_ready_data.csv')
    except FileNotFoundError:
        raise FileNotFoundError("cannot find data csv file for model")
    
    edge_list = pd.read_csv('model/cluster_edge_list.csv')

    return model_data, edge_list