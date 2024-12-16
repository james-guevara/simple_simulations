import numpy as np

# Initialize smaller weights and biases
np.random.seed(42)  # For reproducibility
weights1 = np.random.randn(3, 2)  # Input (3) -> Hidden layer (2)
bias1 = np.zeros(2)
weights2 = np.random.randn(2, 1)  # Hidden layer (2) -> Output (1)
bias2 = np.zeros(1)

def relu(x):
    return np.maximum(0, x)

def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def generator_forward(noise):
    hidden = relu(np.dot(noise, weights1) + bias1)  # Hidden layer
    output = sigmoid(np.dot(hidden, weights2) + bias2)  # Output layer
    return output

# Generate a small example with random noise
noise = np.random.randn(1, 3)  # A single noise vector (3 values)
fake_data = generator_forward(noise)

noise, fake_data

# Initialize weights and biases for the Discriminator
weights1_disc = np.random.randn(1, 2)  # Input (1) -> Hidden layer (2)
bias1_disc = np.zeros(2)
weights2_disc = np.random.randn(2, 1)  # Hidden layer (2) -> Output (1)
bias2_disc = np.zeros(1)

def discriminator_forward(data):
    """Perform a forward pass through the Discriminator."""
    hidden = relu(np.dot(data, weights1_disc) + bias1_disc)  # Hidden layer
    output = sigmoid(np.dot(hidden, weights2_disc) + bias2_disc)  # Output layer
    return output

# Test the Discriminator with a small input
fake_data_example = np.array([[0.5]])  # Example fake data (from the Generator)
discriminator_output = discriminator_forward(fake_data_example)

fake_data_example, discriminator_output


