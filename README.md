This project aimed to overhaul the current diagnostics system that Linear Diagnostics currently has. I created this app in R Shiny in order for it to be used as a website, so it can be used remotely from anywhere and allows the option for live updates. 
The UI:
There are three main sections for the user to input data.
  1. Sample Name
  2. CT Value Threshold
  3. The Fluorescence Series

-----------------------------------------------------------------

Since there are three/four separate data series for each sample, there are four text boxes for the input of the data. Not all of these need to be filled in order to produce results.

Once data is input, the table will show a result. The Max_Difference_Time will show the CT Value (Renamed from CT Value in order to provide clarity).
If this Max_Difference_Time value is obscure compared to the other series' readings then it is clear that the data is anomalous.

The Result column will show the diagnostic result of the sample. Realistically they will all be displaying the same value. If the CT Value is below the CT Threshold then the result will be positive, and vice versa for negative.

The Sample will display the Sample name that was input by the user.

Additionally, The fluorescence series will be displaying in a line graph format above the table. This will allow the user to visually analyse the data to check for any major errors in the inputted data. 
The graph allows the visualisation of multiple samples. As this gets cluttered with many samples, I have provided the option for sampels to be shown in the graph or not with a checkbox. 

To add another sample, the user will need to click the Add Sample button at the end of the text boxes.

-----------------------------------------------------------------
