# Instructions

## Submission

On clicking the exam link a new repository has been created for you in the exam classroom. You should now clone this repository into a new RStudio project then **commit and push** to GitHub as usual. 

GitHub takes a snapshot of your **local git** repository at the deadline so **commit and push often**, as you should do in all spatial data science projects. The deadline is based on your last commit, but we expect the repository to be pushed to GitHub very soon after. 

## Before you begin: 

* Go to the top of the `exam_response.Rmd` RMarkdown document and edit the name and student number fields (at the top of the `.Rmd`).

* Complete the originality declaration

## Task

You have six hours to complete this open book exam. You must select and undertake **only one** of the research questions below. Links to the data for each question have been provided and you are free to source additional data for extension analysis, but everything you need is provided.

* You must write your response in the `exam_response.Rmd`, under the originality declaration.

* You may use any resource to assist you but the work must be your own and you are required to sign a originality statement within the exam. 

* Questions about the exam must be asked on the open Slack GIS channel. 

* You can use RStudio visual markdown editor if you wish.

* If you copy a section of code from an online source please provide a relevant link or acknowledgment.

Marks are awarded as per the marking scheme. It's important to note there is no 'right' answer, even if your findings are inconclusive or not as expected, you are awarded marks for how you approach the problem.  

## Within your work you must:

* Provide an initial project scope in bullet point form. Your project scope should include:

    * If you intend to propose a variation of the original question (e.g. selecting a specific year of data to analyse), this must be based on appropriate reasoning and clearly stated.
  * A brief evaluation of your main research dataset(s) as well as an assessment of any data processing tasks that will be required or additional data that might be required to complete your analysis.
  * A brief explanation of the data wangling and analysis you intend to undertake, prior to starting the analysis. This may include research questions or hypotheses you identify as relevant. 
  * You may also wish to identify any constraints (around the data you have been instructed to analyse) or obvious omissions from the set task that could limit what will be produced in this short project. These could relate to spatial or temporal limitations in the dataset, what you decide is reasonable to analyse or anything else that is relevant. 

* Produce a well commented and fully explained RMarkdown document that attempts to answer the research question.

* Create at least one graphical output and at least one mapped output.

* Critically reflect on the results you have produced. 

## Tips:

* In the time you have, prioritise good solid analysis over innovative analysis that uses advanced techniques.

* Structure your RMarkdown document with titles and subtitles. 

* Comment and explain your working throughout.

* State assumptions and describe limitations.

* In most questions some administrative boundary data has been provided, use this to assist guiding recommendations and outputs.

* Provide critical commentary about the data you are using and the analysis you are undertaking throughout.

* Plan your time. We suggest 1 hour for data exploration, 2-3 hours for analysis, 1 hour for visualisations, 1 hour for interpretation and reflection. 

# Practice Question

## New York Evictions

New York City wish to conduct a study that aims to prevent people being evicted through understand possible related factors.You have been enlisted as a consultant and tasked to conduct an analysis of their data from 2020.

You should use appropriate data processing and analysis methods to produce an overview report which summarises the patterns revealed in the data in this year. It is expected that at least some of the methods you use will relate to the spatial dimensions of the data.

Your report should include a brief introduction including relevant contextual information at the beginning and a critical review of your findings at the end. You must include at least one map. 

### Data

* List of evictions - https://data.cityofnewyork.us/City-Government/Evictions/6z8x-wfk4
* New York City community districts - https://data.cityofnewyork.us/City-Government/Community-Districts/yfnk-k7r4

## The impacts of redlining on social equity  

Starting in the 1930s the American Home Owners' Loan Corporation (HOLC) was tasked with refinancing mortgages on properties to prevent missed payments. To do this the HOLC assessed the "credit-worthiness" of neighbourhoods, dividing them largely on race into grades between A, considered the "best", and D considered "hazardous", drawn around in red, hence the term "redlining". Other services such as healthcare and infrastructure investments were decided in similar way. 

The Civil Rights Act of 1968 (the Fair Housing Act) made it unlawful to discriminate housing or financing in this manner. However, the redlining legacy has left severe social equity issues across cities, such as access to outdoor space, clean air and trees. 

Vegetation has a large impact on local temperature, with heat being the leading cause of weather related deaths in the USA. As a result you have been enlisted as a consultant and tasked to conduct an analysis exploring temperature differences across the HOLC "graded neighbourhoods" in Los Angeles.

You should use appropriate data processing and analysis methods to produce an overview report which summarises the patterns revealed in the data. It is expected that at least some of the methods you use will relate to the spatial dimensions of the data.

Your report should include a brief introduction including relevant contextual information at the beginning and a critical review of your findings at the end. You must include at least one map.

### Data

* Los Angeles redlining shapefile - https://dsl.richmond.edu/panorama/redlining/#loc=3/41.245/-105.469&text=downloads
* May 2021 Landsat 8 image (Landsat Collection 2 Level-1) covering the study area with minimal could cover - https://earthexplorer.usgs.gov/
* Los Angeles census tracts (outline of which is the city of Los Angeles) - https://data.lacounty.gov/Geospatial/Census-Tracts-2010/ay2y-b9rg

## Graffiti mitigation 

In 2004 the city of San Francisco stopped cleaning graffiti on private property passing the responsibility to property owners who must act within 30 days or face a $500 fine. A local group of business owners are getting frustrated by the cost of continually removing graffiti. 

The local business owners have enlisted you as a consultant and tasked you to conduct an analysis of possible contributing factors to graffiti occurrence so they can present a case to city legislators for pro-active measures. 

You should use appropriate data processing and analysis methods to produce an overview report which summarises the patterns revealed in the data. It is expected that at least some of the methods you use will relate to the spatial dimensions of the data.

Your report should include a brief introduction including relevant contextual information at the beginning and a critical review of your findings at the end. You must include at least one map.

### Data

* American Community Household Survey: https://data.census.gov/cedsci/advanced?g=0400000US06%241400000

Go to advanced search > Geography > Tract > California > All Census Tracts within California > search > all tables > then you can filter on a keyword and download tables.

From the American Community Household Survey you might consider using:
  * Household data (census tract)
  * Education data (census tract)
  * Household income data (numbers on households in each category) (census tract)
  * Population age data (census tract)
  * Household income in dollars (census tract)
  * Poverty status (census tract)
  * Private Health insurance coverage (census tract)
  
* Census tract spatial data - https://data.sfgov.org/Geographic-Locations-and-Boundaries/Census-2010-Tracts-for-San-Francisco/rarb-5ahf
* Graffiti points - https://data.sfgov.org/City-Infrastructure/Graffiti/vg6y-3pcr 
