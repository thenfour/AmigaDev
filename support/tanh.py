import numpy as np
import sys

def generate_tanh_table(num_samples, min_value, max_value, input_start, input_end):
    """
    Generate a hyperbolic tangent (tanh) waveform table based on the given parameters.

    Parameters:
    num_samples (int): The total number of samples in the table.
    min_value (int): The minimum value for the output.
    max_value (int): The maximum value for the output.
    input_start (float): The start value for the input range.
    input_end (float): The end value for the input range.

    Returns:
    list: A list of tanh wave values.
    """
    # Generate the tanh values
    tanh_values = np.tanh(np.linspace(input_start, input_end, num_samples))

    # Scale the values to fit the specified range and then convert to integers
    tanh_values_scaled = (((tanh_values + 1) / 2) * (max_value - min_value) + min_value).round().astype(int)

    return tanh_values_scaled

def main():
    if len(sys.argv) != 6:
        print("Usage: python tanh.py <num_samples> <min_value> <max_value> <input_start> <input_end>")
        return

    num_samples = int(sys.argv[1])
    min_value = int(sys.argv[2])
    max_value = int(sys.argv[3])
    input_start = float(sys.argv[4])
    input_end = float(sys.argv[5])

    tanh_table = generate_tanh_table(num_samples, min_value, max_value, input_start, input_end)

    # Print the generated table with commas
    for value in tanh_table[:-1]:
        print(f"{value}, ", end="")
    print(tanh_table[-1])  # Print the last value without a trailing comma

if __name__ == "__main__":
    main()
