# SurveyTools

`surveytools` is a new tidyverse compiant package for survey analysis, comprising of miscellaneous functions used often for survey analysis at [Dalberg](https://dalberg.com/). We have used these tools internally to analyse multiple surveys and dataframes, as well as use as a base to build additional tools and GUI over. Please reach out to us for questions regarding use and example code. 

## Functions

We provide two key functions that can be used for survey analysis:

1. Summariser:

`summariser_base` forms the work horse of the package. It takes in the following arguments:

<table>
    <tr>
        <th>Parameter</th>
        <th>Value</th>
    </tr>
    <tr>
        <td>Data</td>
        <td> <b>(Required)</b> Data to be summarised </td>
    </tr>
    <tr>
        <td>Variable</td>
        <td> <b>(Required)</b> Name of atleast one variable to be summarised by. Does not support vectors of values, but supports all tidyverse select formats </td>
    </tr>
    <tr>
        <td>Grouping variables</td>
        <td>Name, or tidyverse selection, of any variable to group the results by. Supports multiple groups concurrently</td>
    </tr>
    <tr>
        <td>Stat</td>
        <td>One of "mean", "median", or "total". Defines the stat to be summarised over</td>
    </tr>
    <tr>
        <td>Survey Design</td>
        <td><b>Required for analysis of surveys.</b> Provides the structure of the survey (i.e. pps, etc.) Please see the `survey` package for full list of available options that can be passed in the list  </td>
    </tr>
    <tr>
        <td>Compare significance to</td>
        <td>Changes the default total against which all significance resuts are tested. Please provide in a "group::response" structure</td>
    </tr>
    <tr>
        <td>Default level of significance</td>
        <td>0.95. Can be modified</td>
    </tr>
</table>

The function is pipping compliant -> the output of the function is always a dataframe.

2. Crosstabs:

`cross_tab` is our second primary function. It allows you to convert the tidy data recieved from the `summarise_base` function, into a more commonly discernable dataframe