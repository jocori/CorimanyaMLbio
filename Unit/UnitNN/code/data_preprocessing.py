import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler

def clean_range(column):
    """Clean and preprocess ranges like '[10, 20]' → mean value."""
    extracted_ranges = column.str.extract(r"\[(\d+),\s*(\d+)\]", expand=True)
    extracted_ranges = extracted_ranges.apply(pd.to_numeric, errors="coerce")
    return extracted_ranges.mean(axis=1).fillna(0)

def preprocess_data(file_path):
    """Load, clean, preprocess, and return feature-target split."""
    data = pd.read_csv(file_path, delimiter=",")

    # Clean specific range columns
    data["cap_diameter"] = clean_range(data["cap_diameter"])
    data["stem_height"] = clean_range(data["stem_height"])
    data["stem_width"] = clean_range(data["stem_width"])

    # Fill missing categorical values with 'unknown'
    categorical_columns = ["cap_shape", "Cap_surface", "cap_color", "does_bruise_or_bleed",
                           "gill_attachment", "gill_spacing", "gill_color", "stem_root",
                           "stem_surface", "stem_color", "veil_type", "veil_color",
                           "has_ring", "ring_type", "Spore_print_color", "habitat", "season"]
    for col in categorical_columns:
        data[col] = data[col].fillna("unknown")

    # One-hot encode categorical columns
    data = pd.get_dummies(data, columns=categorical_columns, drop_first=True)

    # Encode target column
    data["class"] = data["class"].apply(lambda x: 1 if x == "e" else 0)

    # Drop non-relevant columns
    X = data.drop(columns=["class", "family", "name"])
    y = data["class"]

    # Convert to numeric and fill NaNs
    X = X.apply(pd.to_numeric, errors="coerce").fillna(X.mean())

    # Normalize features
    scaler = StandardScaler()
    X = pd.DataFrame(scaler.fit_transform(X), columns=X.columns)

    return X, y
