The aim of the input tab is to allow you to upload your data to the app and visualise the structure of the index.

#### What to do

**1. Select your country**

The first thing to do is to select the country that you have data for from the dropdown menu. If your country is not available in the list, please contact us (*contact email to be added*) and we will do our best to add it. As an advanced option you may also upload your own geometry file: this an advanced option - see the [online documentation](https://unhcr-guatemala.github.io/A2SIT/book/user_geometry.html).

**2. Download template**

Having selected the country, click "download template". This downloads an Excel template with the Admin 2 codes for your country automatically filled in. Follow the instructions in the spreadsheet and fill in your data. Please read the instructions carefully because the data needs to be in the correct format to be successfully read into the app.

We strongly recommend downloading the [example data set for Guatemala](https://github.com/UNHCR-Guatemala/A2SIT/raw/main/inst/A2SIT_data_input_template_GTM.xlsx) which demonstrates how the template should be compiled.

**3. Upload data**

Once you have filled in the template with your data, use the "Browse" button under "Upload data" to browse to the location of your input spreadsheet. Then click "Load".

#### Successful upload

If the data upload was successful, you will get a notification (if not, see below for some help).

In the **Index Framework** box you will see a sunburst plot of the index structure. This shows the grouping of indicators at each level up to the index. Click on sections of this plot to drill down to particular groups. Like most plots in the app, hovering over the plot will show a camera icon, which lets you download the plot as a png file.

If any data cleaning operations were performed by the app during the data import, the **Messages** box will report them there. For example, if you have indicators with no data, these will be removed and reported. Likewise, if any aggregation groups have been defined with no indicators in them, these will be reported in the messages box.

#### Unsuccessful upload

If your data is not successfully imported, you will receive a notification. The A2SIT app tries to detect the reason that your data wasn't able to be imported and give a helpful message: these will be reported in the **Messages* box. Common reasons that you can't import your data are:

* You have selected a country that is not the same as the data you are uploading.
* Including text in your data: data *must only be numbers*.
* Changing the format of the template, e.g. by adding rows/columns or changing header names.
* Indicator codes include duplicates or break the app rules: e.g. they cannot be numerical or start with a number

If the error is not obvious, please:

* Check the [A2SIT documentation](https://unhcr-guatemala.github.io/A2SIT/book/data_input.html)
* Compare your spreadsheet with the [example data set](https://github.com/UNHCR-Guatemala/A2SIT/raw/main/inst/A2SIT_data_input_template_GTM.xlsx)

If you are still having trouble or spot a bug, please contact us (*email to add*).
