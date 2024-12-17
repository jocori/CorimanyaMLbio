from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Dropout, Conv1D, MaxPooling1D, Flatten, GlobalAveragePooling2D

def create_dense_nn(input_dim, layers=[64, 32], dropout_rates=[0.3, 0.2]):
    """Create a fully connected neural network."""
    model = Sequential()
    for i, (units, rate) in enumerate(zip(layers, dropout_rates)):
        if i == 0:
            model.add(Dense(units, activation="relu", input_dim=input_dim))
        else:
            model.add(Dense(units, activation="relu"))
        model.add(Dropout(rate))
    model.add(Dense(1, activation="sigmoid"))
    return model

def create_cnn(input_shape):
    """Create a 1D CNN."""
    model = Sequential([
        Conv1D(32, kernel_size=3, activation="relu", input_shape=input_shape),
        MaxPooling1D(pool_size=2),
        Conv1D(64, kernel_size=3, activation="relu"),
        MaxPooling1D(pool_size=2),
        Flatten(),
        Dense(64, activation="relu"),
        Dropout(0.3),
        Dense(1, activation="sigmoid")
    ])
    return model

def create_transfer_model(base_model):
    """Create a transfer learning model."""
    model = Sequential([
        base_model,
        GlobalAveragePooling2D(),
        Dense(128, activation="relu"),
        Dropout(0.3),
        Dense(1, activation="sigmoid")
    ])
    return model
