#DEGnext
#Copyright Tulika Kakati 2020
#Distributed under GPL v3 open source license
#This code may be used to test the DEGnextModel.pth on new datasets.

############################################
import torch
import torch.nn as nn
import numpy as np
import matplotlib.pyplot as plt
from torch.utils.data import DataLoader
from torch.autograd import Variable
from sklearn.metrics import confusion_matrix
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.preprocessing import label_binarize
from sklearn.metrics import roc_curve, auc
import CancerDataset_global_pooled_multi_datasets_all
from sklearn.metrics import matthews_corrcoef
import DatasetTransform_global_pooled_all
from sklearn.neighbors import KNeighborsClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import matthews_corrcoef
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score, precision_score, recall_score, f1_score
import xgboost as xgb
from sklearn.svm import SVC
from sklearn import svm
from sklearn import metrics
import pickle
import pandas
import warnings
import random
warnings.simplefilter(action='ignore', category=FutureWarning)

#Input dataset
Te_CancerProjects=["LP"]
#parameters
learning_rate=1e-4
np.random.seed(0)
torch.manual_seed(0)
loss_func = nn.BCEWithLogitsLoss()
performance_by_dataset=[]
auc_file=[]
##CNN Architecture
class MyNetwork(nn.Module):
    def __init__(self):
        super(MyNetwork,self).__init__()
        self.ConvLayer1 = nn.Sequential(
            nn.Conv2d(1, 6, kernel_size=(3,3), stride=1,padding=1),
            nn.Conv2d(6, 12, kernel_size=(3,3), stride=1,padding=1),
            nn.ReLU()
            )
        self.ConvLayer2 = nn.Sequential(
            nn.Conv2d(12, 24, kernel_size=(3,3), stride=1,padding=1),
            nn.Conv2d(24, 48, kernel_size=(3,3), stride=1,padding=1),
            nn.ReLU()
            )
        self.ConvLayer3 = nn.Sequential(
            nn.Conv2d(48, 64, kernel_size=(3,3), stride=1,padding=1),
            nn.Conv2d(64, 102, kernel_size=(3,3), stride=1,padding=1),
            nn.ReLU()
            )
        self.ConvLayer4 = nn.Sequential(
            nn.Conv2d(102, 164, kernel_size=(3,3), stride=1,padding=1),
            nn.Conv2d(164, 256, kernel_size=(3,3), stride=1,padding=1),
            nn.MaxPool2d(2),
            nn.ReLU()
            )
        self.global_pool = nn.AdaptiveMaxPool2d((1,1))
        self.Lin1 = nn.Sequential(
                nn.Linear(256,120),
                nn.ReLU()
            )
        self.Lin2 = nn.Sequential(
                nn.Linear(120,64),
                nn.ReLU()
            )
        self.Lin3 = nn.Sequential(
                nn.Linear(64,32),
                nn.ReLU()
                    )
        self.Lin4 = nn.Sequential(
                nn.Linear(32,8),
                nn.ReLU()
                    )
        self.Lin5 = nn.Sequential(
                nn.Linear(8,2),
                nn.ReLU()
                    )
        self.func=nn.Softmax(dim=1)
    def forward(self,x):
        x = self.ConvLayer1(x)
        x = self.ConvLayer2(x)
        x = self.ConvLayer3(x)
        x = self.ConvLayer4(x)
        x = self.global_pool(x)
        x = x.view(x.size(0), -1)
        x = self.Lin1(x)
        x = self.Lin2(x)
        x = self.Lin3(x)
        x = self.Lin4(x)
        x = self.Lin5(x)
        output = self.func(x)
        return output


def calculate_accuracy_from_cm(Y_t,y_hat_class):
    cm_accuracy=accuracy_score(Y_t, y_hat_class,normalize=True)
    cm_recall=recall_score(Y_t, y_hat_class, average='weighted')
    precision=precision_score(Y_t, y_hat_class, average='weighted')
    cm_f_measure=f1_score(Y_t, y_hat_class, average='weighted')
    cm_mcc=matthews_corrcoef(Y_t,y_hat_class)
    return cm_accuracy*100, cm_recall*100, precision*100, cm_f_measure,cm_mcc


@torch.no_grad()
def bio_test(model):
    test_loss = []
    test_accuracy = []
    test_recall = []
    test_precision = []
    test_fmeasure = []
    test_MCC=[]
    cum_accuracy=0
    cum_precision=0
    cum_MCC=0
    labels_predict=np.array([])
    prob_predict=np.array([])
    for data in test_loader:
        index, img, y_img = data
        img = Variable(img)
        y_img = y_img.view(y_img.size(0), -1)
        y_img = Variable(y_img)
        X_t = torch.FloatTensor(img)
        y_hat= model(X_t)
        Y_t = torch.from_numpy(y_img.numpy())
        _, y_hat_class = torch.max(y_hat, 1)
        # loss = loss_func(y_hat,Y_t)
        # test_loss.append(loss.item())
        # y_hat_class = np.where(y_hat<0.5, 0, 1)
        labels_predict=np.append(labels_predict,y_hat_class)

    return labels_predict
        # prob_predict=np.append(prob_predict,y_hat)
        # cm_accuracy, cm_recall, precision, fmeasure,mcc =calculate_accuracy_from_cm(y_img.numpy(),y_hat_class)
        # test_accuracy.append(cm_accuracy)
        # test_recall.append(cm_recall)
        # test_precision.append(precision)
        # test_fmeasure.append(fmeasure)
        # test_MCC.append(mcc)
    # cum_accuracy=sum(test_accuracy)/len(test_accuracy)
    # cum_recall=sum(test_recall)/len(test_recall)
    # cum_precision=sum(test_precision)/len(test_precision)
    # cum_fmeasure=sum(test_fmeasure)/len(test_fmeasure)
    # cum_MCC=sum(test_MCC)/len(test_MCC)
    # return cum_accuracy, cum_recall,cum_precision,cum_fmeasure,cum_MCC

##Network initialize

# model = MyNetwork()
# optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate, betas=(0.9, 0.999), eps=1e-08, weight_decay=0, amsgrad=False)
performance_by_dataset=[]
testing_accuracy = []
testing_recall = []
testing_precision = []
testing_fmeasure = []
testing_mcc=[]
testing_loss=[]

model = MyNetwork()
MyModelName='DEGnextModel_count_6.pth'
# model.Lin5=nn.Linear(8,1)
# model.func=nn.Identity()
model.load_state_dict(torch.load(MyModelName))
model.eval()
print('Model loaded...')

File1 = 'datasets/MESC/MESC.csv'
File2 = 'datasets/MESC/MESC_cell_label.csv'
xy_file = pandas.read_csv(File1, index_col=0, header=0)
xy = xy_file.values.astype('float32')
gene_name = list(xy_file.index)
cell_label = pandas.read_csv(File2).values[:, 1:].astype('int')

File = '/Users/guoyin/Desktop/desk/wmwa/DEGnext-main/datasets/tung/counts.csv'
xy = pandas.read_csv(File).values[:, 1:].astype('float32')
File = '/Users/guoyin/Desktop/desk/wmwa/DEGnext-main/datasets/tung/golden.csv'
xy_label = pandas.read_csv(File).values[:, 1:].astype('float32')
xy_label[np.where(xy_label == 0)] = 2
xy_label[np.where(xy_label == 1)] = 0
xy_label[np.where(xy_label == 2)] = 1
# File = '/Users/guoyin/Desktop/github/Real_data/LP/DEGnext_code/LP_cell_label.csv'
cell_label = np.vstack((np.ones((288,1)), 2*np.ones((288,1)))).astype('float32')

ntimes = 100
n1 = [5, 5, 5, 5, 10, 15, 20, 25]
n2 = [5, 10, 15, 20, 10, 15, 20, 25]

tensor_shape = (ntimes, len(n1), len(xy_label))
predict_label = np.zeros(tensor_shape)

for i in range(ntimes):
    for j in range(6,len(n1)):

        casesamples = list(np.where(cell_label == 1)[0])
        contsamples = list(np.where(cell_label == 2)[0])

        case_selection = random.sample(casesamples, n1[j])
        cont_selection = random.sample(contsamples, n2[j])

        data = np.hstack((xy[:, case_selection], xy[:, cont_selection]))
        # label = np.vstack((cell_label[case_selection], cell_label[cont_selection]))

        test_dataset = CancerDataset_global_pooled_multi_datasets_all.MyDataset(input=data, gene_label=xy_label, case='LP',
                                                                                transform=DatasetTransform_global_pooled_all.Compose([DatasetTransform_global_pooled_all.ArrayToTensor()]))
        batch_size = 256
        test_loader = DataLoader(dataset=test_dataset, batch_size=batch_size, shuffle=False, drop_last=False)
        samples = test_dataset.len
        predict_label[i, j, :] = bio_test(model)


File='Bio_test_data_accuracy_selected_datasets.txt'
np.savetxt(File, performance_by_dataset, fmt='%s')
print("Testing ended....")
File = 'X_data_test_predicted_labels.txt'
xy_predicted_labels = np.loadtxt(File,delimiter=',', dtype=np.float32)
xy_predicted_labels=np.where(xy_predicted_labels=="1.","UR", xy_predicted_labels)
xy_predicted_labels=np.where(xy_predicted_labels=="1.0","--UR", xy_predicted_labels)
xy_predicted_labels=np.where(xy_predicted_labels=="0.0","--DR", xy_predicted_labels)

