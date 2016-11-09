# AUTHOR: Kelsey D'Souza
# COMS4111 Project 1-3 kmg2183 and kad2185

# Infectious Disease Database

## How to Use

Visit our server at: http://kad2185.ngrok.io/

I am currently running the app through ngrok tunneling to my machine server.
I had trouble setting up Azure (added the inbound security rule and followed
the other instructions, but could not connect.) The app still accesses the 
w4111 database under our kmg2183 username.

Because of the ngrok/laptop setup, the latency might increase slightly.


## About the files

#### server.py
This is the main server file. It uses Flask as instructed, constructs
query strings incorporating each of our entities and relations, and retrieves
selected columns from each query.

I've created genericized functions to query any table by any number of field 
inputs:
  SELECT <columns specified> FROM <table> WHERE <field specs> 

I've created other functions for SELECT from joined tables, and a custom query
for finding the five closest patients to a medic for their ToDo list.

#### cloud_connect.py
This python script uses sqlalchemy to create an engine connected to the w4111
azure-based database.


#### Client-side code
- Main.elm, Menu.elm
I constructed the client using Elm (compiles to HTML + Javascript). 
It tracks user input and sends POST requests to the Flask server using JSON.
The responses from flask come back as JSON, and I decode these to render the
resulting tables.



## Functionality


** Note you must re-input each value into the fields,
   the field inputs do not clear on their own.

- Login with PID or MID
- Hospitals
- Diseases
- Patients
- Me

### Hospitals
View all hospital information. Search by any field combination.

Clicking a row in the hospital table displays the diseases treated at that 
hospital in the labeled table below.

### Diseases
View all diseases we track. Search by any field combination.

Clicking a disease displays the symptoms and descriptions associated with 
that disease in the table below.


### Patients
View all patient records. Search by any field combination.
Clicking a patient displays their list of contacts that patient might have 
infected, if any. (Try out the first patient, pid=1600000 Gretchen Britt)


### Me 
If we've successfully found your PID in our database, the Me page displays
a patient view with: 
  - your patient record 
  - your assigned doctor
  - your symptoms, if any
  - the diseases you've been diagnosed with, if any

If we've found your MID, the Me page displays a medic view with:
  - your medic record
  - your table of patients
  - a ToDo list of the five closest patients for you to check on that day



### Map
Implemented, but glitchy and markers do not show up due to API key issues.
This map will display hospital, patient, and medic locations.
Key functionality comes from a 'contact tracing' option, where specifying a 
patient displays the contacts traced to that patient.
Specifying a disease will show the geographical spread of that disease.


Thank you!