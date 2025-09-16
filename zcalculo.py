import numpy as np
import matplotlib.pyplot as plt

# Define the functions to compare
def f1(x): return x
def f2(x): return x**2
def f3(x): return x**3
def f4(x): return x**4
def f5(x): return x**5

# Create a dictionary of functions
functions = {
    "f(x) = x": f1,
    "f(x) = x^2": f2,
    "f(x) = x^3": f3,
    "f(x) = x^4": f4,
    "f(x) = x^5": f5 }

# Discretize the interval [0, 1]
N = 1000
x = np.linspace(1e-6, 1, N)  # avoid x=0 to prevent division by zero
h = x[1] - x[0]

# Function to approximate derivative using finite differences
def approximate_derivative(f, x, h):
    df = np.zeros_like(x)
    df[:-1] = (f(x[1:]) - f(x[:-1])) / h
    df[-1] = df[-2]  # repeat last value
    return df

# Function to compute time integral
def compute_time(f, x, h):
    fx = f(x)
    dfx = approximate_derivative(f, x, h)
    integrand = np.sqrt(1 + dfx**2) / np.sqrt(fx)
    T = np.trapz(integrand, dx=h)
    return T, integrand

# Store results
results = {}

# Compute and plot
plt.figure(figsize=(10, 6))
for label, f in functions.items():
    T, integrand = compute_time(f, x, h)
    results[label] = T
    plt.plot(x, integrand, label=f"{label}, T ≈ {T:.3f}")

# Plot settings
plt.title("Integrando del tiempo para diferentes funciones f(x)")
plt.xlabel("x")
plt.ylabel("Integrando")
plt.yscale("log")
plt.ylim(1e0, 1e4)  # opcional: escala log para ver todo
plt.grid(True)
plt.legend()
plt.tight_layout()
plt.show()

# Print summary of times
print("Tiempos aproximados de caída (en unidades relativas):")
for label, T in results.items():
    print(f"{label:30s}  T ≈ {T:.5f}")

