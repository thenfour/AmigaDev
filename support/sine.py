# Python script to be saved as "sine.py"
# This script generates a sine waveform table based on command line arguments, with separate scaling for positive and negative poles.
import numpy as np
import sys

def generate_sine_wave_table(period, num_samples, min_value, max_value):
    """
    Generate a sine waveform table based on the given parameters.

    Parameters:
    period (int): The period of the sine wave (number of samples per cycle).
    num_samples (int): The total number of samples in the table.
    min_value (int): The minimum value for the negative pole.
    max_value (int): The maximum value for the positive pole.

    Returns:
    list: A list of sine wave values.
    """
    # Generate the sine wave values
    sine_values = np.sin(np.linspace(0, 2 * np.pi * num_samples / period, num_samples, endpoint=False))

    # Scale the values separately for positive and negative poles, then convert to integers
    sine_values_scaled = np.where(sine_values > 0, sine_values * max_value, sine_values * -min_value).round().astype(int)

    return sine_values_scaled

def main():
    if len(sys.argv) != 5:
        print("Usage: python sine.py <period> <num_samples> <min_value> <max_value>")
        return

    period = int(sys.argv[1])
    num_samples = int(sys.argv[2])
    min_value = int(sys.argv[3])
    max_value = int(sys.argv[4])

    sine_wave_table = generate_sine_wave_table(period, num_samples, min_value, max_value)

    # Print the generated table with commas
    for value in sine_wave_table[:-1]:
        print(f"{value}, ", end="")
    print(sine_wave_table[-1])  # Print the last value without a trailing comma

if __name__ == "__main__":
    main()
