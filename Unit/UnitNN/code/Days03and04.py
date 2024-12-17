import sys
sys.path.append('./code')
from data_preprocessing import preprocess_data
from model_utils import create_cnn
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report
import numpy as np

# Preprocess data
X, y = preprocess_data("data/primary_data.csv")
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Reshape data for Conv1D
X_train_cnn = np.expand_dims(X_train, axis=2)
X_test_cnn = np.expand_dims(X_test, axis=2)

# Create CNN model
print("\nTraining Convolutional Neural Network...")
cnn_model = create_cnn(input_shape=(X_train_cnn.shape[1], X_train_cnn.shape[2]))
cnn_model.compile(optimizer="adam", loss="binary_crossentropy", metrics=["accuracy"])

# Train the model
history = cnn_model.fit(X_train_cnn, y_train, validation_split=0.2, epochs=20, batch_size=32, verbose=1)

# Evaluate the model
loss, accuracy = cnn_model.evaluate(X_test_cnn, y_test, verbose=0)
print(f"Test Accuracy (CNN): {accuracy:.4f}")

# Generate classification report
y_pred_probs = cnn_model.predict(X_test_cnn)
y_pred = (y_pred_probs > 0.5).astype("int32")
print("\nClassification Report (CNN):")
print(classification_report(y_test, y_pred))

import sys

sys.path.append('./code')

from data_preprocessing import preprocess_data
from model_utils import create_cnn
from sklearn.model_selection import train_test_split
import numpy as np
from tensorflow.keras.optimizers import Adam, RMSprop, SGD
import matplotlib.pyplot as plt

# Preprocess data
X, y = preprocess_data("data/primary_data.csv")
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Reshape data for CNN
X_train_cnn = np.expand_dims(X_train, axis=2)
X_test_cnn = np.expand_dims(X_test, axis=2)

# Optimizers to test
optimizers = {
    "Adam": Adam(),
    "RMSprop": RMSprop(),
    "SGD": SGD(learning_rate=0.01, momentum=0.9)
}

results = {}
history_dict = {}

# Train CNNs with different optimizers
for opt_name, opt in optimizers.items():
    print(f"\nTraining CNN with {opt_name} optimizer...")
    model = create_cnn(input_shape=(X_train_cnn.shape[1], X_train_cnn.shape[2]))
    model.compile(optimizer=opt, loss="binary_crossentropy", metrics=["accuracy"])

    history = model.fit(X_train_cnn, y_train, validation_split=0.2, epochs=20, batch_size=32, verbose=0)
    loss, accuracy = model.evaluate(X_test_cnn, y_test, verbose=0)
    print(f"{opt_name} - Test Accuracy: {accuracy:.4f}")

    results[opt_name] = accuracy
    history_dict[opt_name] = history

# Plot results
plt.figure()
for opt_name, history in history_dict.items():
    plt.plot(history.history["val_accuracy"], label=f"{opt_name} (Validation)")
plt.title("Validation Accuracy Across Optimizers")
plt.xlabel("Epochs")
plt.ylabel("Accuracy")
plt.legend()
plt.show()

# Print best optimizer
best_optimizer = max(results, key=results.get)
print(f"\nBest Optimizer: {best_optimizer} with accuracy: {results[best_optimizer]:.4f}")
