import sys
sys.path.append('./code')
from data_preprocessing import preprocess_data
from tensorflow.keras.applications import VGG16
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Dropout, GlobalAveragePooling2D
from tensorflow.keras.optimizers import Adam
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report
import numpy as np
import cv2

# Reshape data to images
def reshape_to_images(X, image_size=(64, 64)):
    reshaped_data = []
    for row in X.values:
        row = np.pad(row, (0, image_size[0] * image_size[1] - len(row)), 'constant')
        image = np.reshape(row, image_size)
        image = cv2.resize(image, image_size)
        reshaped_data.append(image)
    return np.array(reshaped_data)

# Preprocess data
X, y = preprocess_data("data/primary_data.csv")
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Reshape data
X_train_images = reshape_to_images(X_train) / 255.0
X_test_images = reshape_to_images(X_test) / 255.0
X_train_images = np.repeat(X_train_images[..., np.newaxis], 3, axis=-1)
X_test_images = np.repeat(X_test_images[..., np.newaxis], 3, axis=-1)

# Load VGG16 base model
base_model = VGG16(weights="imagenet", include_top=False, input_shape=(64, 64, 3))
for layer in base_model.layers:
    layer.trainable = False

# Add custom layers
model = Sequential([
    base_model,
    GlobalAveragePooling2D(),
    Dense(128, activation="relu"),
    Dropout(0.3),
    Dense(1, activation="sigmoid")
])

# Compile and train the model
model.compile(optimizer=Adam(learning_rate=0.0001), loss="binary_crossentropy", metrics=["accuracy"])
print("\nTraining Transfer Learning Model (VGG16)...")
history = model.fit(X_train_images, y_train, validation_split=0.2, epochs=10, batch_size=32, verbose=1)

# Evaluate the model
loss, accuracy = model.evaluate(X_test_images, y_test, verbose=0)
print(f"Test Accuracy (Transfer Learning): {accuracy:.4f}")

# Classification report
y_pred_probs = model.predict(X_test_images)
y_pred = (y_pred_probs > 0.5).astype("int32")
print("\nClassification Report (Transfer Learning):")
print(classification_report(y_test, y_pred))
