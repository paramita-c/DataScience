In this notebook, I have used Titanic dataset to build models and interpret them using LIME. I have also done some data pre-processing and feature engineering on the dataset to come up with 5 new features. 
For LIME interpretation you can refer: https://github.com/marcotcr/lime 
Since this is tabular data I have used LimeTabularExplainer. For Text and Images there is LimeTextExplainer and LimeImagesExplainer available.
I tried linear (logistic regression), non-linear(decision trees) and ensemble models(random forest and gradient boosting) to understand the predictions through LIME for Titanic dataset. One consistent thing that I have noticed here is that if the 'Fareperpeson' is high, the person is most likely to Survive (prediction is 1). 
In addition if the passenger is a woman or child (Women_Children is 1) then most likely they survived too. This makes sense as women/children and/or elite class travelling  were the ones that were saved on priority and hence they survived.


