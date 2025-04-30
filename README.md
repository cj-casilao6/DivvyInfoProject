# Overview
This F# application analyzes and visualizes real-world Divvy bike ride data from Chicago (commuting service in Chicago using bikes). It calculates descriptive statistics and outputs a histogram to provide insight into rider demographics and usage trends.
Thiis project only uses pure functional programming - no mutable variables, loops, or arrays.

# Features / Analysis
After the user inputs a valid CSV filename, the program performs the following analyses:
- Total number of trips
- Number and percentage of riders by gender
- Average age of riders
- Ride duration distribution (0 - 30 mins, 30 - 60 mins, 60 - 120 mins, > 2 hours)
- Histogram of start hours (riders are grouped by start hour and displayed using asterisks, scaled by number of rides (1 star = 100 trips)

<img width="380" alt="Screenshot 2025-04-30 at 12 03 42 PM" src="https://github.com/user-attachments/assets/792dcb34-dddf-4aba-83b6-f9b456749e7d" />
