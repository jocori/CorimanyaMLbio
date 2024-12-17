import sys
sys.path.append('./utils')
from data_preprocessing import preprocess_data
from model_utils import create_dense_nn
from sklearn.model_selection import train_test_split
from sklearn.metrics import classification_report

X, y = preprocess_data("data/primary_data.csv")
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

model = create_dense_nn(input_dim=X_train.shape[1])
model.compile(optimizer="adam", loss="binary_crossentropy", metrics=["accuracy"])
history = model.fit(X_train, y_train, validation_split=0.2, epochs=30, batch_size=32, verbose=1)

loss, accuracy = model.evaluate(X_test, y_test, verbose=0)
print(f"Test Accuracy: {accuracy:.4f}")
# Train two alternative NNs with different architectures
model1, acc1 = train_and_evaluate_model(X_train, y_train, X_test, y_test,
                                        layers=[128, 64], dropout_rates=[0.4, 0.3],
                                        model_name="Model 1: Larger Network")

model2, acc2 = train_and_evaluate_model(X_train, y_train, X_test, y_test,
                                        layers=[32, 16], dropout_rates=[0.2, 0.1],
                                        model_name="Model 2: Smaller Network")
# Compare accuracies
print(f"Model 1 Accuracy: {acc1:.4f}")
print(f"Model 2 Accuracy: {acc2:.4f}")

# Select the best-performing model
best_model = model1 if acc1 > acc2 else model2
print("Best model selected!")

# Analyze failures
print("\nAnalyzing Model Failures...")

# Predict probabilities on the test set
y_pred_probs = best_model.predict(X_test)
y_pred = (y_pred_probs > 0.5).astype("int32")

# Identify misclassified samples
misclassified_indices = np.where(y_pred.flatten() != y_test.values)[0]

# Display a few failure cases
for i in range(min(5, len(misclassified_indices))):
    idx = misclassified_indices[i]
    print(f"\nSample {i + 1}:")
    print(f"True Class: {'Edible' if y_test.values[idx] == 1 else 'Poisonous'}")
    print(f"Predicted Probability: {y_pred_probs[idx][0]:.4f}")
    print(f"Predicted Class: {'Edible' if y_pred[idx][0] == 1 else 'Poisonous'}")
    print(X_test.iloc[idx])

# Print classification report for a detailed breakdown
print("\nClassification Report:")
print(classification_report(y_test, y_pred))
