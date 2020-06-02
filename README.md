# DEQAir
CV
Develop functions to clean and visualize air quality data

The expected flow is as follows:
<ul>
<li> Download hourly data from AirData  (2010-2019) into the data folder (using the download_airdata_hourly function, in the file with that name). </li>
<li> Download and save the 2020 data from the AQI website or EnVista database as a csv. Right now this is manual.
•	Create a dataframe of hourly data for each station. The get_station_data function returns a dataframe with all the years of data for the pollutant requested. It reads the feather file for Oregon sites for that pollutant. The feather files are created in the download step </li>
<li> Combine the AirData data with the ENvista data. This is somewhat manual, as I have been getting the files in different formats each time </li>
<li> Use this dataframe which has current data to create daily and weekly summaries; the summaries are written out to the summaries folder. The functions are in  create_summaries.R</li>
<li> Use the daily & weekly summaries to create graphs; graphs are written out to the graphs folder (which must exist, there is no check). Currently there are 5 types of graphs that can be created: a point, line and tile graph based on daily averages; a line graph based on weekday aligned averages, and a boxplot based on weekly averages. THe graph functions are in make_graphs.R </li>
<li> Once the summaries are created, anyone can make the graphs using the functions in make_graphs.R, or create their own </li>
<li> An example of automation (not including AirData download) is in daily_graphs_automation.R. <li>

