# HomeGrow
Homegrow is a hydroponics system utilizing IBM's AI and Cloud technology alongside a raspberry pi and sensors to automate the farming process. 

The product is designed to be space-efficient, water-efficient, and easy to use for farms in LEDC's without reliable access to water.

## Table of contents
1. [Overview of Design](#Overview)
    1. [Bill of Materials](#Bom)
2. [Physical Build](#Build)
3. [Hardware](#Hardware)
4. [Database](#Database)
    1. [Mnesia](#Mnesia)
    2. [Cloudant](#Cloudant)
5. [User Interface](#UI)
    1. [Importing the Flow](#Flow)
    2. [Farm Status Displays](#Displays)
    3. [Notifications](#Notifications)
    4. [Chat](#Chat)
    5. [Custom Types](#Custom)
    6. [Connection Status](#Connection)
    7. [CSS](#CSS)
6. [Voice Assistant](#Assistant)
7. [Integration](#Integration)
    1. [Database <-> Pi](#D2P)
    2. [Database <-> Node-red](#D2N)
    3. [UI <-> Voice Assistant](#U2V)
8. [Sustainability and Ethical Report](#Sustainability)
9. [Future Work](#Future)
10. [Meetings Minutes](#Meeting)

## Overview of Design <a id="Overview"></a>
<br/>
<p align="center">
    <img src="https://github.com/bjt19/HomeGrow/blob/main/pictures/overall_design.PNG">
</p>

### Bill of Materials <a id="Bom"></a>
| No.  | Description | Price per Unit (£) | Quantity | Total Price (£) |
| ------------- | ------------- | ------------- | ------------- | ------------- |
| 1  | Black Downpipe 2.5m 68mm | 6.81 | 2 | 13.62 |
| 2  | Half Round 92.5 Degree Offset Bend | 3.26 | 8 | 26.08 |
| 3 | Round Pipe Branch 68mm | 6.70 | 5 | 33.50 |
| 4 | SEN0244 Gravity Analog TDS Sensor | 11.56 | 1 | 11.56 |
| 5 | Water Detection Sensor | 1.91 | 1 | 1.91 |
| 6 | SI1145 Light Sensor | 9.74 | 1 | 9.74 |
| 7 | LED Lights | 9.12 | 4 | 36.48 |
| 8 | Immersible Water Pump | 9.01 | 1 | 9.01 |
| 9 | Peristaltic Pump | 24.43 | 1 | 24.43 |
| 10 | Reservoir | 31.72 | 1 | 31.72 |
| 11 | Raspberry Pi Zero | 14 | 1 | 14 |
| 12 | Rockwool | 4.95 | 1 | 4.95 |
| 13 | Seeds | 1.50 | 1 | 1.50 |
| 14 | Analogue-to-Digital Converter| 14.10 | 1 | 14.10 |
| | | | | 232.60 |

## Physical Build <a id="Build"></a>
<p align="center">
    <img src="https://github.com/bjt19/HomeGrow/blob/main/pictures/image016.jpg"> <br>
    Picture of the Farm <br>
    <img src="https://github.com/bjt19/HomeGrow/blob/main/pictures/image018.png"> 
    <img src="https://github.com/bjt19/HomeGrow/blob/main/pictures/image020.png"> <br>
    &emsp;Schematic for Physical System of the Farm &emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;3D-Printed Plant Holder <br>
    <img src="https://github.com/bjt19/HomeGrow/blob/main/pictures/image022.png"> 
    <img src="https://github.com/bjt19/HomeGrow/blob/main/pictures/image024.png"><br>
    3D-Printed Pipe Stopper&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;3D-Printed Water Level Barrier <br><br>
</p>
1.	Measure the overlap distance between the 90° joints & UPVC pipes and tee joints & UPVC pipes <br>
2.	Drill 4 rectangular holes with the size of 5cm x 5.1cm on each of the 0.5m UPVC pipes <br>
3.	Measure the diameter of 90° joint <br>
4.	Drill the hole with a diameter of 68mm on the water reservoir (the diameter of 90° joint) <br>
5.	Connect the joints with UPVC pipes according to the given configuration <br>
6.	3D-print the plant holders as given in the figure and install on rectangular holes <br>
7.	3D-print the pipe stopper given in the figure and install on the free side of UPVC pipes at the top <br>
8.	3D-print the water level barrier to distribute the water flow evenly. Install this barrier at the joints on the top branch of the system. (The two tee joints and single 90° joint) <br>
9.	Install the submersible water pump inside the water reservoir. Open two little holes on top of the reservoir lid for this, one for the pump hose and the other for power cable of the motor. Connect the power cable to automation system and hose to the 3D-printed pipe stopper on the free side UPVC pipes at the top. <br>
10.	Install the water level sensor at the top the reservoir with electronics placed outside for connection to the main system. <br>
11.	Install the peristaltic pump with one hose inside the nutrient reservoir and the other inside the water reservoir. For this open another small hole on top of the water reservoir specifically for this hose. <br>
12.	Install the dissolved solid sensor inside the nutrient reservoir. Connect this to the outside electronics sharing the same hole as water sensor power cable. <br>
13.	Before starting the system, do a test run and check for leakages. If found, hot glue the affected area. (This mostly happens at joint merging points) <br>
14.	Fill your water & nutrient reservoirs, and plant your seeds in the 3D-printed plant holders <br>
15.	You are now ready to experiment with hydroponics. Good luck! <br>


## Hardware <a id="Hardware"></a>
circuit diagram <br/>
pi,sensors,pumps,leds

## Database <a id="Database"></a>
2 databases are used, an Mnesia database and a Cloudant database.

### Mnesia <a id="Mnesia"></a>
example text

### Cloudant <a id="Cloudant"></a>
Cloudant is a non-relational, JSON document database used to store the node-red flows and settings.  

## User Interface <a id="UI"></a>
Node-red is used to create a webpage as the user interface. It is a flow-based programming tool built on Node.js, developed by IBM. The node-red app is created and deployed using Cloud Foundry on IBM Cloud.<br/>

The full documentation on node-red can be found [here](https://nodered.org/docs/).<br/>

### Importing the Flow <a id="Flow"></a>
The flows for the project can be found [here](https://github.com/bjt19/HomeGrow/blob/main/node-red.json), to be imported after the app is created.

A node-red app can be created following [this guide](https://developer.ibm.com/tutorials/how-to-create-a-node-red-starter-application/) and changing the package.json file in its gitlab source to include the required libraries as shown below:

```javascript
{
    "name": "node-red-app", 
    "version": "1.1.3", 
    "dependencies": { 
        "node-red-dashboard": "3.1.7", 
        "node-red-node-ui-microphone": "0.3.x", 
        "node-red-contrib-mic": "0.0.1", 
        "node-red-node-random": "0.4.0", 
        "node-red-contrib-scx-ibmiotapp": "0.0.49", 
        "node-red-contrib-cloudantplus": "2.0.5",
        "node-red-contrib-postgresql": "0.10.1", 
        "node-red-contrib-httpauth": "1.0.12", 
        "node-red-node-email": "1.15.1", 
        "node-red-contrib-chat": "1.0.0", 
        "@ibm-cloud/cloudant": "^0.0.25", 
        "bcrypt": "^5.0.1",
        "body-parser": "1.x", 
        "express": "4.x", 
        "http-shutdown": "1.2.2", 
        "ibm-cloud-env": "^0", 
        "node-red": "^2.2.2",
        "node-red-contrib-ibm-db2": "0.x", 
        "node-red-node-cf-cloudant": "0.x", 
        "node-red-node-openwhisk": "0.x", 
        "node-red-node-watson": "0.x", 
        "node-red-nodes-cf-sqldb-dashdb": "0.x" 
    }, 
    "scripts": { 
        "start": "node --max-old-space-size=160 index.js --settings ./bluemix-settings.js -v" <br/>
    }, 
    "engines": { 
        "node": "14.x" 
    } 
} 
``` 

After deploying the flows, the /red in the link can be replaced with /ui to access the dashboard, giving the following display:

<p align="center">
<img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/ui_home.jpg" width="180" height="395">  <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/ui_notifs.jpg" width="180" height="395"> <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/ui_custom.jpg" width="180" height="395">  <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/ui_chat.jpg" width="180" height="395"> <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/ui_settings.jpg" width="180" height="395">
</p>

### Farm Status Displays <a id="Displays"></a>
<p align="center">
    <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/data_display.PNG">
</p>

The above flow creates the displays for the current water level, light intensity, nutrient level and duration of light of the farm. There are "inject" and "random" nodes which are used as placeholders for the database during testing, and link nodes which create the connection to the http request flow to get data from the database.

### Notifications <a id="Notifications"></a>
<p align="center">
    <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/notifications.PNG">
</p>

The above flow adds notifications to the previous farm status nodes, using the javascript in the "function" nodes alongside the "switch" nodes to create the notification logic, resulting in pop-up notifications and a notification log as shown in the ui images above. The light intensity does not have notifications because light intensity control has not been implemented in the raspberry pi, and the duration of light notification logic is different, because it checks every 24hours if the necessary light has been given to the plant rather than notifying the current condition of the farm.

### Chat <a id="Chat"></a>
<p align="center">
    <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/chat_inputs.PNG">
</p>

The above flow creates text and microphone inputs which are then sent to the voice assistant to get a response, and also creates a log of previous messages as shown in the ui image above. 

<p align="center">
    <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/settings.PNG">
</p>

The above flow creates a button input for enabling or disabling the speaker when using the chat.

### Custom Types <a id="Custom"></a>
<p align="center">
    <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/custom_types.PNG">
</p>


The above flow contains a "plant selection" list where users can choose one of the preset plant types and hence the farms optimal values. These values are then displayed on the Home page as text outputs alongside the farm status. There are also 2 "custom plant settings" forms which users can use to add custom plant types and their optimal values. After adding custom plants types, they are saved as flow variables, added to the plant list, and displayed in the custom tab where they can also be deleted. The flow also contains some "ui control" nodes which are used to hide or show the forms so the dashboard is less clutered.

### Connection Status <a id="Connection"></a>
<p align="center">
    <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/connection_status.PNG">
</p>

The above flow is used to check the connection status to the database and farm, if data hasnt been received in 30 minutes, a disconnected notification is triggered, if data is received after, the status is changed to connected.

### CSS <a id="CSS"></a>
```
<style>
    .blue {
        background-color: #7FFFD4 !important;
        color: black !important;
    }
    .green {
        background-color: #7FFF00 !important;
        color: black !important;
    }
    .yellow {
        background-color: #FFD700 !important;
        color: black !important;
    }
    .white{
        background-color: #F8F8FF !important;
        color: black !important;
    }
    .crimson{
        background-color: #DC143C !important;
        color: black !important;
    }
    .assist_msg{
        background-color: #ADD8E6 !important;
        color: black !important;
    }
    .usr_msg{
        background-color: #ADD8E6 !important;
        color: black !important;
    }
    .text_border{
        border-top: 2px green !important;
    }
    .welcome{
        color: #228B22 !important;
        font-size: 150% !important;
        font-family: "Times New Roman" !important;
    }

</style>
```

A html template node is created with the above CSS code which is applied to the different displays to improve the dashboard.

## Voice Assistant <a id="Assistant"></a>

<p align="center">
    <img src="https://github.com/bjt19/HomeGrow/blob/main/pictures/voice_recognition.png">
</p>

IBM's Speech-to-Text, Text-to-Speech and Watson Assistant are used to build the voice recognition system. 

### Speech-to-Text <a id="Speech-to-Text"></a>
IBM Speech-to-Text is an API cloud service that utilises AI-powered speech recognition and transcription to convert speech to text. The language chosen is English but it is also possible to choose other languages such as Arabic, French and Korean. 

### Text-to-Speech <a id="Text-to-Speech"></a>
IBM Text-to-Speech is an API cloud service that allows you to convert text to speech using natural-sounding voices. Male or female voices can be chosen.

### Watson Assistant <a id="Watson Assistant"></a>
IBM Watson Assistant uses artificial intelligence to formulate fast, appropriate and accurate responses across any application. By detecting the user's intents, the virtual agent can carry out actions that can be set within the workspace such as responding using dialog that was preset. The main intents for this system are water, light, nutrients and condition as the main application for thie voice recognition feature is for the user to understand the condition of the plants. Possible user examples are added for each intent.

<p align="center">
    <img src="https://github.com/bjt19/HomeGrow/blob/main/pictures/intents.png">
</p>

In the Node-RED flow, user intents are extracted from the Watson Assistant. Based on the intent, the relevant sensor value is extracted from the database and compared against the optimal threshold. For instance, if the user wants to check the light level of the LED lights, the intent 'light' will be detected by Watson Assistant and hence the latest light sensor value is retrieved from the database and checked against the optimal light level needed by the particular type of plant. An appropriate response is given depending on whether there is enough light.

## Integration <a id="Integration"></a>

### Database <-> Pi <a id="D2P"></a>

### Database <-> Node-red <a id="D2P"></a>
<p align="center">
    <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/database_connections.PNG">
</p>

Http request...


### UI <-> Voice Assistant <a id="U2V"></a>
<p align="center">
    <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/watson_assitant.PNG">
</p>

Nodes for IBMs cloud services are available on node-red, the service credentials are entered into the nodes which are then used to pass the user chat inputs to the speech-to-text and watson assitant to decipher the message's intent, which then replies the user with the farm conditions comparing the data from the database and the optimal values, the reply is in the form of text and also through the speaker using speech-to-text. 

## Sustainability and Ethical Report <a id="Sustainability"></a>
HomeGrow integrates AI and IoT technology into a traditional vertical hydroponics farming system. Our product is intended to meet the needs of those living in LEDCs without reliable access to drinking water. 

Our product provides the following solutions for sustainability.
First, we designed a system that recycles water. Water is pumped through the water pump from the reservoir into the pipes. The water then flows through the plant holders back into the reservoir. Since water access is not guaranteed in LEDCs, reducing the usage is water benefits the farmers greatly.

Second, our system uses a special growing material, rockwool, rather than soil to maximize water savings. Furthermore, our self-controlled design makes it possible for a minimum number of farmers to be responsible for the plants. They will only need to top up water and nutrients into the reservoir and nutrients container. 
On the other hand, to increase crop yield, we use special fertilizer to make plants grow four times faster. In addition, we use a vertical design to save space in order to plant more plants in a limited area. A vertical hydroponics design also helps to reduce the chances of plant diseases, thus increasing the rate of plants surviving to harvest.

By increasing crop yield, the government will not only be able to solve the problem of food shortage but will also be able to sell crops to other countries in order to make money and improve their economic conditions. A country can use the money earned by selling crops to develop its economy. The reduced amount of water and space needed also indicates farmers will have reduced infrastructure and maintenance costs.

The vertical farm is constructed from inexpensive UPVC pipes and electronics. Compared to other current hydroponic systems for sale, our product can be up to two times cheaper, making it more accessible. Furthermore, more pipes can be added easily to upscale the farm. As a result, the cost for a large-scale farm is very reasonable and within the reach of government officials in LEDCs.


## Future Work <a id="Future"></a>
1. Larger frames to support more plants
2. Advanced voice recognition system
3. Integration with weather API
4. Image recognition for plant diseases
5. Sourcing alternatives to UPVC pipes to have a biodegradable system

## Meeting Minutes <a id="Meeting"></a>

![image](https://user-images.githubusercontent.com/59923913/176884025-70e04a98-7b56-4861-aafc-54c4658fb033.png)

![image](https://user-images.githubusercontent.com/59923913/176894984-427b2cf7-e787-4396-80d2-43693faab744.png)

![image](https://user-images.githubusercontent.com/59923913/176884393-16f42c55-adcd-44ba-a32a-05227f7de6f0.png)

![image](https://user-images.githubusercontent.com/59923913/176884489-8219b357-0181-487b-99b4-e1d15d2fbde7.png)

![image](https://user-images.githubusercontent.com/59923913/176884607-014992ad-e6e5-4769-82b3-b464b473f886.png)

![image](https://user-images.githubusercontent.com/59923913/176884676-050faba5-6bde-495c-9689-a82f35e25cba.png)

![image](https://user-images.githubusercontent.com/59923913/176884729-55d30d0c-831e-4737-ab97-a176b356ed46.png)






