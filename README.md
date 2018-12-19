# DataScience
The Data Science projects that I have worked on in the past 6-8 months. Code is available in both R and Python.
************************************************************************************************************************************

Intelligent Medical Call Records Classification
------------------------------------------------
To segregate the call records as per the call details (text), and  classify them into 6 categories- Appointments, Cancellation, Lab Queries, Medical Refills, Insurance Related and General Doctor Advice
•	Data cleansing & Text Pre-Processing
o	Data Cleanup is a crucial step that determines the accuracy 
o	Processing the call details to remove the redundant & common information and retain the discrete information.
•	Building ML Models; 5 Models have been built
o	Multi-layered Perceptron – 6 hidden layers were used
o	GloVe word embeddings with GRU – Pre-trained GloVe word vectors were used with one GRU hidden layer
o	GloVe word embeddings with 1D CNN – Pre-trained GloVe word vectors were used with one convolutional and 1 fully connected layer
o	LSTM with word embeddings –One Embedding layer (100 dimensions) and one LSTM layer
o	GRU with word embeddings – One Embedding layer (100 dimensions) and one GRU layer.
To avoid over-fitting, batch normalization and drop outs were used.
************************************************************************************************************************************

Comparative Study of OpenCV, Traditional ML and DNNs for image classification
------------------------------------------------------------------------------
CIFAR data set was used and images were classified using OpenCV, statistical models and CNNs
•	Data cleansing & Pre-Processing
o	Pre-trained Haar Cascade Classifiers are used to classify images 
o	Convert the images into HDR (high dynamic range) and then perform feature extraction using some filters (from skimage library)
o	Feature extraction techniques – HOG(histogram of oriented gradients), Gabor Filtering, spatial binning, Histogram of color intensities,  blurring are used to extract features from images to be able to apply traditional ML algorithms
o	CNNs with transfer learning is used
o	Data Augmentation have been used to improve accuracy
•	Building ML Models; 4 Models have been built
o	SVM – linear kernel is used
o	XGBoost – gblinear booster is used
o	Transfer learning with VGG16, NASNetMobile, ResNet50 were used.
************************************************************************************************************************************

Predicting the potential maintenance opportunity of Machines
-------------------------------------------------------------
To predict the major possible future events/ action points for each MachineID ie., like any “ComponentRepair”(major), “ComponentReplacement”, “NoIssue” (Minor error or normal status) , in the next one month
•	Data cleansing & Pre-Processing
o	New statistical features were created for Sensor data for lag periods of 1 week, 2 weeks, 3 weeks and 4 weeks
o	New features were created from machine error and repair logs
o	Total of 51 features were engineered from the data
o	Only 7% of the data was for machines that had the component replaced, so oversampling  and cost sensitive learning techniques were used to cater to the unbalanced class distribution
o	F1 score of the minority class was used as an model evaluation measure
o	Top 10 data patterns were extracted from the engineered features using Decision Tree algorithm
o	PCA and Auto encoders were used to extract more features from the engineered ones
•	Building ML Models; 3 Models have been built
o	SVM – linear kernel with hyper-parameter tuning and specified class weights
o	Decision Tree – entropy loss function with specified class weights
o	Naaive Bayes – Gaussian Naaive Bayes for multi-class classification was used.
************************************************************************************************************************************

Predicting Bankruptcy of a Company 
-----------------------------------
To predict the bankruptcy of a company based on the parameters (Financial, but masked) provided. There was similar kind of parameters (data) for companies which have survived and companies which have been bankrupt. 
•	Data cleansing & Pre-Processing: 
o	All the financial attributes provided were ratios
o	20% of the data had NULL values in most of the columns
o	One-hot encoding was done to capture the NULL value patterns for each attribute column
o	Sum of the NULL values for each company was included as an additional attribute
o	New features were created by multiplying some of the existing attributes
o	Only 5% of the data was for companies that actually went bankrupt, so oversampling  and cost sensitive learning techniques were used to cater to the unbalanced class distribution
o	F1 score was used as an model evaluation measure
•	Building ML Models; 3 Models have been built
o	PCA and logistic regression – PCA and then top 20 features were used
o	Random Forest – 100 trees were used and model was tuned for optimum no. of features
o	XGBoost – Top 20 importance attributes were used.
************************************************************************************************************************************

Predicting Overtime for an Employee
------------------------------------
To predict the overtime for an employee in a project so that the company can balance the resource allocation effectively
•	Data cleansing & Pre-Processing: 
o	New features were created from the data to capture the employees’ job profile eg. Work experience, years in current role or project, overtime probability, etc.
o	Outliers were identified using box plots and capped
o	Correlation terms were multiplied
o	Accuracy and Recall scores were used for model evaluation
•	Building ML Models; 3 Models have been built
o	Clustering and Random Forest – Data was divided into 3 clusters and then used random forest 
o	XGBoost – top 30 attributes were used and XGBoost model was tuned with optimum hyper parameters
o	Stacked Ensemble of kNN, random forest and XGBoost  – 3 weak learners were stacked to come up with a strong classifier.
************************************************************************************************************************************
