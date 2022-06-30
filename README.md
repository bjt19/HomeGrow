# HomeGrow
An AI Hydroponics Farm    ---  fill in specification


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
    1. [Farm Status Displays](#Displays)
    2. [Notifications](#Notifications)
    3. [Chat](#Chat)
    4. [CSS](#CSS)
6. [Voice Assistant](#Assistant)
7. [Integration](#Integration)
8. [Sustainability and Ethical Report](#Sustainability)
8. [Future Work](#Future)
8. [Meetings Minutes](#Meeting)

## Overview of Design <a id="Overview"></a>
picture of overall design as done in poster <br/>
![alt text](https://github.com/bjt19/HomeGrow/blob/main/pictures/example.PNG?raw=true)

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
uses node red, javascript,html <br/>
import [node-red](https://github.com/bjt19/HomeGrow/blob/main/node-red.json) node-red<br/>

<code>
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
        "start": "node --max-old-space-size=160 index.js --settings ./bluemix-settings.js -v"
    },
    "engines": {
        "node": "14.x"
    }
}
<code>

### Farm Status Displays <a id="Displays"></a>
example text

### Notifications <a id="Notifications"></a>
example text

### Chat <a id="Chat"></a>
example text

### CSS <a id="CSS"></a>
example text

## Voice Assistant <a id="Assistant"></a>
watson assistant, speech to text,etc

## Integration <a id="Integration"></a>
MQTT: <br/>
pi <-> database <br/>

Node-red: <br/>
ai -> ui <br/>

HTTP: <br/>
ui + ai <-> database <br/>

## Sustainability and Ethical Report <a id="Sustainability"></a>
example text

## Future Work <a id="Future"></a>
example text

## Meeting Minutes <a id="Meeting"></a>
-table of meetings?






