## Below are images of the application with example inputs and outputs.

### Projects Table Tab
##### This is the main tab. It shows project specifics such as the Title and Creator information. The IDCol represents the number used to tether the SQL tables in the backend. The left panel has a Reset button used to clear the page of input data and a multiselection input used for specifying wafer data points.
![Projects Table](https://github.com/TDobs287/Internship-Project/assets/113118701/7d54893c-d7b6-4529-aa97-afa37cd0e1bb)
--------------------------------------------------------------------------------------------------------------------


### Data Entry Tab
##### This tab is used to input and store info related to wafer test runs, the number of wafers per test, and wafer affiliated data using data tables. The Project ID and Test Run ID are shown upon creation of a project and test run, respectively, as a backend tether to the database via primary and foreign keys.
![Data Entry 1](https://github.com/TDobs287/Internship-Project/assets/113118701/efa9c394-bdcf-4fc6-9104-61eee83c4190)


![Data Entry 2](https://github.com/TDobs287/Internship-Project/assets/113118701/2658d174-17e4-4351-b6e4-711da4119303)
--------------------------------------------------------------------------------------------------------------------


### Load Project Tab
##### This tab loads from the database any previously saved project or test runs. You may put in a project or test run ID to load that specific information. The ProjectID can be found in the main tab by searching for the Title of a project. upon loading a project, the list of affiliated test runs and their IDs are given.
![Load Project](https://github.com/TDobs287/Internship-Project/assets/113118701/c4c64d62-492a-4d4b-bbcf-96c5f57ac8e9)


![Load Project 2](https://github.com/TDobs287/Internship-Project/assets/113118701/f0db9dbc-7d23-4ec9-b4a6-a9333bb8b0c8)
--------------------------------------------------------------------------------------------------------------------


### File Input Tab
##### This tab allows the user to upload the wafer testing data docs from the fab into the application, where the data is manipulated then represented below and on the following Plot Results tab. This data may be saved to specific wafer test runs in a project as the wafer's data points.
![File Input](https://github.com/TDobs287/Internship-Project/assets/113118701/1370acdd-4982-4e59-9f5a-a56544c7bff7)
--------------------------------------------------------------------------------------------------------------------


### Plot Results Tab
##### This tab showcases the graphical representation of the wafer data points uploaded in the previous tab for the users to analyze and interpret. This information and graph may be saved to the database. The graph is also updated in real time when selecting different Site options on the left panel.
![Plot Results 1](https://github.com/TDobs287/Internship-Project/assets/113118701/c438b292-0056-4b98-a1d9-11f7a1fb4fa6)
