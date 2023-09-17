#Basic SARIMA model in Python using generated data#
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.tsa.statespace.sarimax import SARIMAX

# Generate synthetic time series data for tech stock prices
np.random.seed(0)
index = pd.date_range(start='1/1/2015', periods=100, freq='D')
data = np.random.randint(500, 1500, size=len(index)) + np.linspace(0, 1000, len(index))
ts = pd.Series(data, index=index)

# Plot the synthetic time series data
plt.figure(figsize=(12, 6))
plt.plot(ts)
plt.title('Synthetic Stock Prices')
plt.xlabel('Date')
plt.ylabel('Stock Price')
plt.show()

# Define the SARIMA model
order = (1, 1, 1)  # (p, d, q)
seasonal_order = (1, 1, 1, 12)  # (P, D, Q, s)

# Fit the SARIMA model
model = SARIMAX(ts, order=order, seasonal_order=seasonal_order)
results = model.fit()

# Forecasting
forecast_steps = 12  # Number of steps to forecast
forecast = results.forecast(steps=forecast_steps)

# Plot the original time series and forecasted values
plt.figure(figsize=(12, 6))
plt.plot(ts, label='Original Stock Prices')
plt.plot(pd.date_range(start=index[-1], periods=forecast_steps, freq='D'), forecast,
         color='green', label='Forecasted Stock Prices')
plt.xlabel('Date')
plt.ylabel('Stock Price')
plt.legend()
plt.show()
