#Anomaly detection example script with DBSCAN model#

import numpy as np
from sklearn.cluster import DBSCAN
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt

# Generate random data for demonstration
np.random.seed(0)
X = np.random.randn(100, 2) * 5  # Generate 100 data points in 2D space

# Add some anomalies
anomalies = np.array([[20, 20], [25, 25]])
X = np.vstack([X, anomalies])

# Scale the data
X = StandardScaler().fit_transform(X)

# Fit DBSCAN model
db = DBSCAN(eps=0.5, min_samples=5)
y_pred = db.fit_predict(X)

# Separate anomalies (label -1) from the clustered data
anomaly_mask = y_pred == -1
clustered_data = X[~anomaly_mask]
anomalies = X[anomaly_mask]

# Plot the results
plt.scatter(clustered_data[:, 0], clustered_data[:, 1], c='blue', label='Clustered Data')
plt.scatter(anomalies[:, 0], anomalies[:, 1], c='red', label='Anomalies')
plt.xlabel('X1')
plt.ylabel('X2')
plt.legend()
plt.show()
