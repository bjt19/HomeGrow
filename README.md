# HomeGrow
Homegrow is a hydroponics system utilizing IBM's AI and Cloud technology alongside a raspberry pi and sensors to automate the farming process. 

The product is designed to be space-efficient, water-efficient, and easy to use for farms in LEDC's without reliable access to water.

## Table of contents
1. [Overview of Design](#Overview)
    1. [Bill of Materials](#Bom)
    2. [Software Utilized](#Software)
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
table of bill of materials.

### Software Utilized <a id="Software"></a>
Node-red: <br/>
description. <br/>

Cloudant: <br/>
description <br/>

IBM Watson Assistant <br/>
description <br/>

## Physical Build <a id="Build"></a>
picture of farm <br/>
3d printed stuff, pipes, anything to do with positioning.

## Hardware <a id="Hardware"></a>
circuit diagram <br/>
pi,sensors,pumps,leds

## Database <a id="Database"></a>
2 database are used,  Mnesia database for the data, cloudant stores the nodered flow/watson stuff.

### Mnesia <a id="Mnesia"></a>
example text

### Cloudant <a id="Cloudant"></a>
example text

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
<img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/ui_home.jpg" width="400" height="790">  <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/ui_notifs.jpg" width="400" height="790">
<img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/ui_custom.jpg" width="400" height="790">  <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/ui_chat.jpg" width="400" height="790">
    <img src="https://github.com/bjt19/HomeGrow/blob/Benjamin/pictures/ui_settings.jpg" width="400" height="790">
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

The above flow is used to check the connection status to the database and farm, if data hasnt been received in 30 minutes, a disconnected notification is triggered, if data is received after, the status is changed to connetced.

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
example text

## Future Work <a id="Future"></a>
1. Larger frames to supoprt more plants
2. Advanced voice recognition system
3. Integration with weather API
4. Image recognition for plant diseases

## Meeting Minutes <a id="Meeting"></a>







