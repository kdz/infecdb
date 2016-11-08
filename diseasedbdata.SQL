--
-- PostgreSQL database dump
--

-- Dumped from database version 9.3.11
-- Dumped by pg_dump version 9.5.4

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: kmg2183; Type: SCHEMA; Schema: -; Owner: kmg2183
--

CREATE SCHEMA kmg2183;


ALTER SCHEMA kmg2183 OWNER TO kmg2183;

SET search_path = kmg2183, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: admitted_to; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE admitted_to (
    pid integer NOT NULL,
    hospital_name character varying(60) NOT NULL,
    admitted_on date
);


ALTER TABLE admitted_to OWNER TO kmg2183;

--
-- Name: checks_on; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE checks_on (
    pid bigint NOT NULL,
    mid bigint NOT NULL,
    checked_on timestamp without time zone
);


ALTER TABLE checks_on OWNER TO kmg2183;

--
-- Name: contacted; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE contacted (
    infected_pid bigint NOT NULL,
    exposed_pid bigint NOT NULL,
    contact_date timestamp without time zone,
    CONSTRAINT contacted_check CHECK ((exposed_pid <> infected_pid))
);


ALTER TABLE contacted OWNER TO kmg2183;

--
-- Name: disease; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE disease (
    virus_name text NOT NULL,
    incubation bigint,
    duration bigint
);


ALTER TABLE disease OWNER TO kmg2183;

--
-- Name: exhibits; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE exhibits (
    pid integer NOT NULL,
    symptom_name character varying(20) NOT NULL,
    since date
);


ALTER TABLE exhibits OWNER TO kmg2183;

--
-- Name: has; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE has (
    pid integer NOT NULL,
    virus_name character varying(20) NOT NULL,
    since date
);


ALTER TABLE has OWNER TO kmg2183;

--
-- Name: hospital; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE hospital (
    hospital_name text NOT NULL,
    number_of_beds bigint,
    latitude character varying(20),
    longitude character varying(20)
);


ALTER TABLE hospital OWNER TO kmg2183;

--
-- Name: location; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE location (
    latitude text,
    longitude text,
    state text,
    country text,
    postal_code text
);


ALTER TABLE location OWNER TO kmg2183;

--
-- Name: medic; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE medic (
    mid bigint NOT NULL,
    name text,
    phone_number text,
    latitude character varying(20),
    longitude character varying(20),
    hospital_name text
);


ALTER TABLE medic OWNER TO kmg2183;

--
-- Name: patient; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE patient (
    pid bigint NOT NULL,
    forename text,
    surname text,
    status text,
    phone_number text,
    dob text,
    latitude character varying(20),
    longitude character varying(20)
);


ALTER TABLE patient OWNER TO kmg2183;

--
-- Name: produces; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE produces (
    virus_name character varying(20) NOT NULL,
    symptom_name character varying(35) NOT NULL
);


ALTER TABLE produces OWNER TO kmg2183;

--
-- Name: symptom; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE symptom (
    symptom_name text NOT NULL,
    description text
);


ALTER TABLE symptom OWNER TO kmg2183;

--
-- Name: treatment_for; Type: TABLE; Schema: kmg2183; Owner: kmg2183
--

CREATE TABLE treatment_for (
    hospital_name character varying(60) NOT NULL,
    virus_name character varying(20) NOT NULL
);


ALTER TABLE treatment_for OWNER TO kmg2183;

--
-- Data for Name: admitted_to; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY admitted_to (pid, hospital_name, admitted_on) FROM stdin;
1600019	Lenox Hill Hospital	2016-09-24
1600023	New York University Langone Medical Center	2016-10-03
1600024	New York University Langone Medical Center	2016-09-26
1600029	New York University Langone Medical Center	2016-08-23
1600031	New York-Presbyterian, Lower Manhattan Hospital	2016-10-04
1600033	New York-Presbyterian, Lower Manhattan Hospital	2016-10-07
1600038	Mount Sinai St. Lukes	2016-08-31
1600037	Mount Sinai St. Lukes	2016-10-03
1600025	Mount Sinai Beth Israel Medical Center	2016-08-23
1600026	Mount Sinai Beth Israel Medical Center	2016-08-23
1600027	Mount Sinai Beth Israel Medical Center	2016-08-26
1600036	ArchCare at Terence Cardinal Cooke Health Care Center	2016-09-23
\.


--
-- Data for Name: checks_on; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY checks_on (pid, mid, checked_on) FROM stdin;
1600000	9004	2016-01-01 00:00:00
1600001	9003	2016-01-02 00:00:00
1600002	9011	2016-01-03 00:00:00
1600003	9011	2016-01-04 00:00:00
1600004	9019	2016-01-05 00:00:00
1600005	9019	2016-01-06 00:00:00
1600006	9011	2016-01-07 00:00:00
1600007	9019	2016-01-08 00:00:00
1600008	9009	2016-01-09 00:00:00
1600009	9013	2016-01-10 00:00:00
1600010	9000	2016-01-11 00:00:00
1600011	9010	2016-01-12 00:00:00
1600012	9002	2016-01-13 00:00:00
1600013	9017	2016-01-14 00:00:00
1600014	9015	2016-01-15 00:00:00
1600015	9004	2016-01-16 00:00:00
1600016	9015	2016-01-17 00:00:00
1600017	9002	2016-01-18 00:00:00
1600018	9009	2016-01-19 00:00:00
1600019	9010	2016-01-20 00:00:00
1600020	9006	2016-01-21 00:00:00
1600021	9003	2016-01-22 00:00:00
1600022	9003	2016-01-23 00:00:00
1600023	9015	2016-01-24 00:00:00
1600024	9007	2016-01-25 00:00:00
1600025	9003	2016-01-26 00:00:00
1600026	9019	2016-01-27 00:00:00
1600027	9015	2016-01-28 00:00:00
1600028	9014	2016-01-29 00:00:00
1600029	9003	2016-01-30 00:00:00
1600030	9011	2016-01-31 00:00:00
1600031	9001	2016-02-01 00:00:00
1600032	9012	2016-02-02 00:00:00
1600033	9012	2016-02-03 00:00:00
1600034	9008	2016-02-04 00:00:00
1600035	9017	2016-02-05 00:00:00
1600036	9017	2016-02-06 00:00:00
1600037	9011	2016-02-07 00:00:00
1600038	9016	2016-02-08 00:00:00
1600039	9018	2016-02-09 00:00:00
1600040	9003	2016-02-10 00:00:00
1600041	9011	2016-02-11 00:00:00
1600042	9017	2016-02-12 00:00:00
1600043	9010	2016-02-13 00:00:00
1600044	9000	2016-02-14 00:00:00
1600045	9010	2016-02-15 00:00:00
1600046	9003	2016-02-16 00:00:00
1600047	9000	2016-02-17 00:00:00
1600048	9002	2016-02-18 00:00:00
1600049	9005	2016-02-19 00:00:00
\.


--
-- Data for Name: contacted; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY contacted (infected_pid, exposed_pid, contact_date) FROM stdin;
1600033	1600000	2016-01-05 00:00:00
1600006	1600011	2016-01-06 00:00:00
1600004	1600036	2016-01-07 00:00:00
1600041	1600020	2016-01-08 00:00:00
1600003	1600047	2016-01-09 00:00:00
1600019	1600023	2016-01-10 00:00:00
1600034	1600024	2016-01-11 00:00:00
1600046	1600043	2016-01-12 00:00:00
1600009	1600040	2016-01-13 00:00:00
1600035	1600048	2016-01-14 00:00:00
1600016	1600034	2016-01-15 00:00:00
1600042	1600033	2016-01-16 00:00:00
1600030	1600012	2016-01-17 00:00:00
1600000	1600018	2016-01-18 00:00:00
1600015	1600032	2016-01-19 00:00:00
\.


--
-- Data for Name: disease; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY disease (virus_name, incubation, duration) FROM stdin;
Influenza A, B	3	7
Ebola	2	20
Zika	3	14
Tuberculosis	14	188
Hepatitis C	14	84
Dengue	4	12
\.


--
-- Data for Name: exhibits; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY exhibits (pid, symptom_name, since) FROM stdin;
1600020	Muscle Pain	2016-05-29
1600020	Rash	2016-08-24
1600021	Cough	2009-02-02
1600021	Headache	2016-09-04
1600022	Bleeding	2012-06-12
1600023	Swollen Lymph	2015-10-05
1600024	Fever	2015-11-03
1600025	Bleeding	2016-03-15
1600025	Cough	2016-07-28
1600025	Fever	2015-09-15
1600026	Swollen Lymph	2016-10-02
1600027	Chest Pain	2014-04-11
1600027	Fever	2016-10-01
1600028	Headache	2016-08-22
1600029	Cough	2013-09-12
1600030	Fever	2015-08-23
1600030	Nausea/Vomiting	2016-09-14
1600031	Fever	2016-07-12
1600032	Headache	2016-06-19
1600033	Fever	2015-04-12
1600034	Fever	2015-06-13
1600034	Loss of Appetite	2016-08-22
1600034	Muscle Pain	2016-09-02
1600035	Fever	2016-05-13
1600036	Headache	2016-08-26
1600037	Headache	2016-04-18
1600038	Nausea/Vomiting	2016-04-20
1600038	Rash	2016-08-14
1600038	Fever	2016-08-15
1600000	Fever	2016-09-23
1600001	Rash	2016-08-24
1600010	Headache	2015-07-30
\.


--
-- Data for Name: has; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY has (pid, virus_name, since) FROM stdin;
1600019	Ebola	2016-01-07
1600020	Influenza A, B	2016-05-29
1600020	Zika	2016-08-24
1600021	Tuberculosis	2009-02-02
1600021	Influenza A, B	2016-09-04
1600022	Hepatitis C	2012-06-12
1600023	Influenza A, B	2015-10-05
1600024	Dengue	2015-11-03
1600025	Hepatitis C	2016-03-15
1600025	Influenza A, B	2016-07-28
1600025	Ebola	2015-09-15
1600026	Influenza A, B	2016-10-02
1600027	Ebola	2014-04-11
1600027	Influenza A, B	2016-10-01
1600028	Zika	2016-08-22
1600029	Tuberculosis	2013-09-12
1600030	Tuberculosis	2015-08-23
1600030	Influenza A, B	2016-09-14
1600031	Hepatitis C	2016-07-12
1600032	Hepatitis C	2016-06-19
1600033	Hepatitis C	2015-04-12
1600034	Ebola	2015-06-13
1600034	Influenza A, B	2016-08-22
1600034	Zika	2016-09-02
1600035	Tuberculosis	2016-05-13
1600036	Influenza A, B	2016-08-26
1600037	Dengue	2016-04-18
1600038	Dengue	2016-04-20
1600038	Influenza A, B	2016-08-14
1600038	Zika	2016-08-15
\.


--
-- Data for Name: hospital; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY hospital (hospital_name, number_of_beds, latitude, longitude) FROM stdin;
ArchCare at Terence Cardinal Cooke Health Care Center	609	40.793575	-73.950564
Lenox Hill Hospital	423	40.773881	-73.960774
Mount Sinai Beth Israel Medical Center	934	40.732649	-73.981616
New York University Langone Medical Center	668	40.74207	-73.974335
NewYork-Presbyterian / Weill Cornell Medical Center	862	40.764396	-73.95393
New York-Presbyterian, Lower Manhattan Hospital	132	40.710359	-74.005103
Mount Sinai St. Lukes	804	40.805624	-73.96129
\.


--
-- Data for Name: location; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY location (latitude, longitude, state, country, postal_code) FROM stdin;
40.7621946470024	-73.9158139268308	New York	United States	11103
40.8809208014735	-73.934825302796	New York	United States	10034
40.7786391171289	-74.0033845457243	New Jersey	United States	07093
40.7375281498566	-73.9831808645207	New York	United States	10010
40.8941204975109	-74.0131434672884	New Jersey	United States	07666
40.7692284371383	-73.8951024823047	New York	United States	11370
40.7197496523443	-73.9470991823105	New York	United States	11222
40.7538806266415	-73.936677349082	New York	United States	11101
40.905702887494	-73.9191865784042	New York	United States	10471
40.8026299727318	-73.9659152845006	New York	United States	10025
40.9018047657534	-73.8927334162669	New York	United States	10471
40.82748343683	-73.9466470833779	New York	United States	10031
40.8519939136149	-73.9809999002086	New Jersey	United States	07024
40.736551685825	-73.900976836731	New York	United States	11377
40.8902748580474	-73.9444997170313	New Jersey	United States	07632
40.9079589679671	-73.8934853971009	New York	United States	10471
40.8076206722038	-73.9215701675032	New York	United States	10454
40.824246660306	-73.9374565822777	New York	United States	10039
40.8186259608747	-74.0173004872424	New Jersey	United States	07047
40.8808442481714	-73.9232007484599	New York	United States	10463
40.7269654873088	-74.0115109997388	New York	United States	10014
40.7732599797828	-74.0106606434282	New Jersey	United States	07086
40.776929844385	-74.0095762509483	New Jersey	United States	07086
40.8334741369493	-73.9603368707496	New Jersey	United States	07020
40.7698569567045	-73.9641158019791	New York	United States	10021
40.8500051283088	-73.9991868085513	New Jersey	United States	07650
40.8176029230192	-73.9129612319265	New York	United States	10455
40.8002492342963	-73.9401124751493	New York	United States	10035
40.7885848300397	-73.951116096346	New York	United States	10029
40.816062220368	-73.9765819597368	New Jersey	United States	07020
40.7859128677944	-73.9431555938006	New York	United States	10029
40.7785218005788	-73.928818230316	New York	United States	11102
40.7829130863011	-73.8952726742563	New York	United States	11105
40.7103956469171	-73.8925927288487	New York	United States	11381
40.8245061783622	-73.9747125485388	New Jersey	United States	07020
40.7718927411903	-73.9736010486256	New York	United States	10019
40.90376797606	-73.9144745614057	New York	United States	10471
40.8231667906615	-73.9317451264382	New York	United States	10451
40.8747401539683	-73.9316577774148	New York	United States	10034
40.9034196649919	-73.9715910981076	New Jersey	United States	07631
40.788990188597	-73.9908095106311	New York	United States	10024
40.8292787459061	-73.9312078944619	New York	United States	10452
40.8153258644717	-74.0003787809357	New Jersey	United States	07022
40.7209457623467	-73.993663639536	New York	United States	10002
40.7785573857602	-73.9709220639429	New York	United States	10024
40.7111198455585	-73.9372564355841	New York	United States	11206
40.8898893960422	-73.9772202951869	New Jersey	United States	07631
40.9078391936336	-73.9940789495964	New Jersey	United States	07666
40.8460645535238	-73.9196909769264	New York	United States	10452
40.8538980835982	-73.9496958232339	New Jersey	United States	\N
40.793575	-73.950564	New York	United States	10029
40.773881	-73.960774	New York	United States	10075
40.732649	-73.981616	New York	United States	10003
40.74207	-73.974335	New York	United States	10016
40.764396	-73.95393	New York	United States	10065
40.710359	-74.005103	New York	United States	10038
40.805624	-73.96129	New York	United States	10025
40.9100399094638	-74.0030762959925	New Jersey	United States	07666
40.8556972045501	-73.9364807991913	New York	United States	10040
40.7212028805819	-73.922920002259	New York	United States	11378
40.9025276118096	-73.9032729736986	New York	United States	10471
40.8498987606257	-74.0103551522156	New Jersey	United States	07650
40.8642135062323	-74.008922561897	New Jersey	United States	07666
40.7240070834463	-74.0010297174226	New York	United States	10012
40.8372093195412	-73.937061591655	New York	United States	10032
40.7478466918431	-74.0080926370089	New York	United States	10011
40.7119454874278	-73.963090946232	New York	United States	11249
40.7712945085013	-73.9346164340699	New York	United States	11102
40.8050315950155	-73.9693591004487	New York	United States	10025
40.7918825183499	-73.9620358853984	New York	United States	10025
40.9018131491635	-73.9894540260136	New Jersey	United States	07631
40.8362539368823	-73.9800400261777	New Jersey	United States	07024
40.8573925088866	-73.9408970099548	New York	United States	\N
40.9020324844707	-73.9026635723485	New York	United States	10471
40.7998879832362	-73.994452739062	New Jersey	United States	07047
40.7625289834073	-73.9845666756635	New York	United States	10019
40.9063844132778	-73.9458014449663	New Jersey	United States	07670
\.


--
-- Data for Name: medic; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY medic (mid, name, phone_number, latitude, longitude, hospital_name) FROM stdin;
9000	George Robinson	1-212-191-8335	40.9100399094638	-74.0030762959925	New York-Presbyterian, Lower Manhattan Hospital
9001	Zenia Hunt	1-212-778-1847	40.8556972045501	-73.9364807991913	New York-Presbyterian, Lower Manhattan Hospital
9002	August Chambers	1-212-361-3670	40.7212028805819	-73.922920002259	New York University Langone Medical Center
9003	Jorden Barron	1-212-141-4426	40.9025276118096	-73.9032729736986	New York University Langone Medical Center
9005	Willow Finley	1-212-291-0212	40.8642135062323	-74.008922561897	NewYork-Presbyterian / Weill Cornell Medical Center
9006	Quon Webster	1-212-724-1917	40.7240070834463	-74.0010297174226	New York-Presbyterian, Lower Manhattan Hospital
9007	Violet Vang	1-212-403-3329	40.8372093195412	-73.937061591655	ArchCare at Terence Cardinal Cooke Health Care Center
9008	Aretha Clay	1-212-296-4446	40.7478466918431	-74.0080926370089	Lenox Hill Hospital
9009	Jarrod Duke	1-212-474-2466	40.7119454874278	-73.963090946232	New York University Langone Medical Center
9011	Marshall Prince	1-212-250-7639	40.8050315950155	-73.9693591004487	Mount Sinai Beth Israel Medical Center
9012	Aline Ortiz	1-212-998-1205	40.7918825183499	-73.9620358853984	New York University Langone Medical Center
9014	Ferris Richards	1-212-372-9509	40.8362539368823	-73.9800400261777	New York University Langone Medical Center
9015	Lacota Fulton	1-212-434-5990	40.8573925088866	-73.9408970099548	NewYork-Presbyterian / Weill Cornell Medical Center
9016	Owen Hill	1-212-773-4699	40.9020324844707	-73.9026635723485	Lenox Hill Hospital
9017	Seth Pacheco	1-212-806-8596	40.7998879832362	-73.994452739062	Mount Sinai Beth Israel Medical Center
9018	Alexis Leach	1-212-198-3928	40.7625289834073	-73.9845666756635	Mount Sinai Beth Israel Medical Center
9019	Elvis Bird	1-212-474-2737	40.9063844132778	-73.9458014449663	Lenox Hill Hospital
9004	Wynne Mcgee	1-212-139-0471	40.8498987606257	-74.0103551522156	Mount Sinai St. Lukes
9010	Breanna Boone	1-212-282-1166	40.7712945085013	-73.9346164340699	Mount Sinai St. Lukes
9013	Damon Alvarado	1-212-388-7122	40.9018131491635	-73.9894540260136	Mount Sinai St. Lukes
\.


--
-- Data for Name: patient; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY patient (pid, forename, surname, status, phone_number, dob, latitude, longitude) FROM stdin;
1600000	Gretchen	Britt	suspected	1-212-784-5630	06/20/1947	40.7621946470024	-73.9158139268308
1600001	Jordan	Romero	suspected	1-212-790-9339	03/15/1963	40.8809208014735	-73.934825302796
1600002	Emi	Valenzuela	suspected	1-212-883-6463	06/03/1985	40.7786391171289	-74.0033845457243
1600003	Cynthia	Dickson	suspected	1-212-787-4025	08/20/1971	40.7375281498566	-73.9831808645207
1600004	Davis	Key	suspected	1-212-939-1799	06/17/1934	40.8941204975109	-74.0131434672884
1600005	Basil	Mercer	suspected	1-212-682-0009	05/26/1953	40.7692284371383	-73.8951024823047
1600006	Beau	Lopez	suspected	1-212-871-7002	02/25/1972	40.7197496523443	-73.9470991823105
1600007	Dieter	Mcintyre	suspected	1-212-420-8323	06/23/1929	40.7538806266415	-73.936677349082
1600008	Kyla	Day	suspected	1-212-523-7644	01/18/1945	40.905702887494	-73.9191865784042
1600009	Judith	Pittman	suspected	1-212-279-5919	02/25/1952	40.8026299727318	-73.9659152845006
1600010	Idola	Hanson	suspected	1-212-722-5036	07/20/2008	40.9018047657534	-73.8927334162669
1600011	Sonya	Sweet	suspected	1-212-672-6691	12/20/1978	40.82748343683	-73.9466470833779
1600012	Chandler	Mitchell	suspected	1-212-495-7000	05/10/1988	40.8519939136149	-73.9809999002086
1600013	Dara	George	suspected	1-212-682-5194	08/23/1982	40.736551685825	-73.900976836731
1600014	Cherokee	Shields	suspected	1-212-650-1553	08/03/1964	40.8902748580474	-73.9444997170313
1600015	Lester	Barron	suspected	1-212-317-8503	04/27/1920	40.9079589679671	-73.8934853971009
1600016	Maggie	Richard	suspected	1-212-526-4412	05/22/1923	40.8076206722038	-73.9215701675032
1600017	Giselle	Stephenson	suspected	1-212-939-4626	12/29/1950	40.824246660306	-73.9374565822777
1600018	Tanek	Simmons	suspected	1-212-961-9260	04/28/2015	40.8186259608747	-74.0173004872424
1600019	Cheryl	Mcleod	confirmed	1-212-378-9940	01/28/1937	40.8808442481714	-73.9232007484599
1600020	Adara	Meyers	confirmed	1-212-357-7837	04/14/1998	40.7269654873088	-74.0115109997388
1600021	Bernard	Atkins	confirmed	1-212-957-6918	07/21/1934	40.7732599797828	-74.0106606434282
1600022	Maia	Walton	confirmed	1-212-547-0335	07/03/1994	40.776929844385	-74.0095762509483
1600023	Cecilia	Whitney	confirmed	1-212-331-2394	10/29/1968	40.8334741369493	-73.9603368707496
1600024	Tatiana	Ball	confirmed	1-212-354-0467	04/20/2005	40.7698569567045	-73.9641158019791
1600025	MacKenzie	Wood	confirmed	1-212-450-5247	03/20/1995	40.8500051283088	-73.9991868085513
1600026	Norman	Wilder	confirmed	1-212-579-0580	07/27/1997	40.8176029230192	-73.9129612319265
1600027	Tatyana	Phelps	confirmed	1-212-809-9546	03/23/1979	40.8002492342963	-73.9401124751493
1600028	Linus	Richardson	confirmed	1-212-868-7579	08/10/2007	40.7885848300397	-73.951116096346
1600029	Camille	Acevedo	confirmed	1-212-876-7644	06/24/2009	40.816062220368	-73.9765819597368
1600030	Kerry	Blackburn	confirmed	1-212-896-6360	12/06/2001	40.7859128677944	-73.9431555938006
1600031	Lacey	Morgan	confirmed	1-212-658-9214	09/14/1945	40.7785218005788	-73.928818230316
1600032	Maisie	Roth	confirmed	1-212-215-8244	07/30/2002	40.7829130863011	-73.8952726742563
1600033	Colby	Jordan	confirmed	1-212-428-0823	09/27/1917	40.7103956469171	-73.8925927288487
1600034	Gavin	Hall	confirmed	1-212-863-8230	01/03/2014	40.8245061783622	-73.9747125485388
1600035	Rose	Valdez	confirmed	1-212-865-7462	09/24/2001	40.7718927411903	-73.9736010486256
1600036	Buffy	Kim	confirmed	1-212-745-3967	04/16/1975	40.90376797606	-73.9144745614057
1600037	Signe	Owen	confirmed	1-212-988-8287	03/25/1956	40.8231667906615	-73.9317451264382
1600038	Asher	Lester	confirmed	1-212-260-7655	12/16/1971	40.8747401539683	-73.9316577774148
1600039	Xanthus	Bray	contacted	1-212-637-9589	08/03/1933	40.9034196649919	-73.9715910981076
1600040	Mariko	Osborn	contacted	1-212-568-3914	11/07/1936	40.788990188597	-73.9908095106311
1600041	Xander	Puckett	contacted	1-212-628-7067	06/19/2000	40.8292787459061	-73.9312078944619
1600042	Jin	Stone	contacted	1-212-692-5308	05/13/1928	40.8153258644717	-74.0003787809357
1600043	Imogene	Faulkner	contacted	1-212-386-1197	04/21/2000	40.7209457623467	-73.993663639536
1600044	Iris	Gamble	contacted	1-212-864-0298	05/28/1920	40.7785573857602	-73.9709220639429
1600045	Yvonne	Hatfield	contacted	1-212-554-0425	03/12/1924	40.7111198455585	-73.9372564355841
1600046	Savannah	Cote	contacted	1-212-926-3131	10/15/1989	40.8898893960422	-73.9772202951869
1600047	Amery	Cote	contacted	1-212-248-3259	07/06/1990	40.9078391936336	-73.9940789495964
1600048	Kristen	Frost	contacted	1-212-434-7825	05/09/1922	40.8460645535238	-73.9196909769264
1600049	Finn	Cline	contacted	1-212-443-6471	07/21/1925	40.8538980835982	-73.9496958232339
\.


--
-- Data for Name: produces; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY produces (virus_name, symptom_name) FROM stdin;
Influenza A, B	Fever
Influenza A, B	Joint pain
Influenza A, B	Flushing/Redness
Influenza A, B	Headache
Influenza A, B	Sore throat
Influenza A, B	Cough
Influenza A, B	Nausea/Vomiting
Influenza A, B	Swollen Lymph
Ebola	Fever
Ebola	Joint pain
Ebola	Headache
Ebola	Sore throat
Ebola	Nausea/Vomiting
Ebola	Rash
Ebola	Diarrhea
Ebola	Chest Pain
Ebola	Bleeding
Ebola	Abdominal Pain
Zika	Rash
Zika	Fever
Zika	Joint pain
Zika	Conjunctivitis (redness of eyes)
Zika	Muscle Pain
Tuberculosis	Fever
Tuberculosis	Cough
Tuberculosis	Chest Pain
Tuberculosis	Bleeding
Tuberculosis	Muscle Pain
Tuberculosis	Loss of Appetite
Hepatitis C	Fever
Hepatitis C	Nausea/Vomiting
Hepatitis C	Bleeding
Hepatitis C	Muscle Pain
Hepatitis C	Loss of Appetite
Hepatitis C	Abdominal Pain
Dengue	Fever
Dengue	Joint pain
Dengue	Nausea/Vomiting
Dengue	Rash
Dengue	Bleeding
Dengue	Muscle Pain
Dengue	Abdominal Pain
Dengue	Swollen Lymph
\.


--
-- Data for Name: symptom; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY symptom (symptom_name, description) FROM stdin;
Fever	An abnormally high body temperature (greater than 98.6 degrees F), usually accompanied by shivering, headache, and in severe cases, delirium.
Joint pain	Discomfort, pain, or inflammation in any part of the joint, including tendon, bone, ligament, cartilage, and muscle.
Flushing/Redness	An involuntary, temporary reddening of the skin, usually of the face, but sometimes of the neck or chest. Occurs from the dilation of blood vessels beneath the skin.
Headache	Symptom of pain anywhere in the region of the head or neck. This pain can have a dull or sharp sensation. 
Sore throat	Pain swallowing, coughing, aching in throat. Is often caused by bacterial or viral infection. 
Cough	Vigorous explulsion of air through the mouth. Seek medical attention if persistent, or cough up bloody/discolored mucus.
Nausea/Vomiting	Nausea is the sensation of feeling like you will vomit or faint. Vomiting is the expelling of undigested food from the stomach through the mouth.
Rash	Change of the skin that affects color, apperance, and/or texture. May be localized or affect all skin
Diarrhea	A condition in which feces are discharged from the bowels frequently and in a liquid form.
Chest Pain	Dull or sharp pain in the torso region; is often described as a burning or stabbing sensation. Can radiate from chest to other regions of body, such as arms and neck. 
Bleeding	Loss of blood from the body as a result of injury or illness. Can occur in any part of the body, both external and internal.
Conjunctivitis (redness of eyes)	Inflammation or infection of the outer membrane of the eyeball and the inner eyelid. Symptoms include redness, itching, and tearing of the eyes. It can also lead to discharge or crusting around the eyes.
Muscle Pain	Commonly related to overuse, tension, or injury from exercise, phsycially demanding work, or illness. Most commonly localized, but can expereince myalgias throughout the entire body. 
Loss of Appetite	A reduced desire to eat results in decreased appetite. Other related symptoms can inlcude unintentional weight loss and not feeling hungry.
Abdominal Pain	Pain that occurs between the chest and pelvic regions. This type of pain can be dull, sharp, crampy, achy, constant or intermittent. Often caused by disease or inflammation of organs. 
Swollen Lymph	Enlarged and/or tender lymph nodes under jawline, in groin, or under armpit. These are a sign of bacterial or viral infection. 
\.


--
-- Data for Name: treatment_for; Type: TABLE DATA; Schema: kmg2183; Owner: kmg2183
--

COPY treatment_for (hospital_name, virus_name) FROM stdin;
ArchCare at Terence Cardinal Cooke Health Care Center	Influenza A, B
ArchCare at Terence Cardinal Cooke Health Care Center	Zika
ArchCare at Terence Cardinal Cooke Health Care Center	Tuberculosis
ArchCare at Terence Cardinal Cooke Health Care Center	Hepatitis C
Lenox Hill Hospital	Influenza A, B
Lenox Hill Hospital	Ebola
Mount Sinai Beth Israel Medical Center	Influenza A, B
Mount Sinai Beth Israel Medical Center	Ebola
Mount Sinai Beth Israel Medical Center	Zika
Mount Sinai Beth Israel Medical Center	Dengue
New York University Langone Medical Center	Influenza A, B
New York University Langone Medical Center	Ebola
New York University Langone Medical Center	Zika
New York University Langone Medical Center	Tuberculosis
New York University Langone Medical Center	Hepatitis C
New York University Langone Medical Center	Dengue
NewYork-Presbyterian / Weill Cornell Medical Center	Influenza A, B
NewYork-Presbyterian / Weill Cornell Medical Center	Zika
NewYork-Presbyterian / Weill Cornell Medical Center	Tuberculosis
NewYork-Presbyterian / Weill Cornell Medical Center	Hepatitis C
New York-Presbyterian, Lower Manhattan Hospital	Influenza A, B
New York-Presbyterian, Lower Manhattan Hospital	Ebola
New York-Presbyterian, Lower Manhattan Hospital	Dengue
Mount Sinai St. Lukes	Influenza A, B
Mount Sinai St. Lukes	Tuberculosis
\.


--
-- Name: admitted_to_pkey; Type: CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY admitted_to
    ADD CONSTRAINT admitted_to_pkey PRIMARY KEY (pid, hospital_name);


--
-- Name: checks_on_pkey; Type: CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY checks_on
    ADD CONSTRAINT checks_on_pkey PRIMARY KEY (pid, mid);


--
-- Name: contacted_pkey; Type: CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY contacted
    ADD CONSTRAINT contacted_pkey PRIMARY KEY (infected_pid, exposed_pid);


--
-- Name: disease_pkey; Type: CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY disease
    ADD CONSTRAINT disease_pkey PRIMARY KEY (virus_name);


--
-- Name: exhibits_pkey; Type: CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY exhibits
    ADD CONSTRAINT exhibits_pkey PRIMARY KEY (pid, symptom_name);


--
-- Name: has_pkey; Type: CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY has
    ADD CONSTRAINT has_pkey PRIMARY KEY (pid, virus_name);


--
-- Name: hospital_pkey; Type: CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY hospital
    ADD CONSTRAINT hospital_pkey PRIMARY KEY (hospital_name);


--
-- Name: medic_pkey; Type: CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY medic
    ADD CONSTRAINT medic_pkey PRIMARY KEY (mid);


--
-- Name: patient_pkey; Type: CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY patient
    ADD CONSTRAINT patient_pkey PRIMARY KEY (pid);


--
-- Name: produces_pkey; Type: CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY produces
    ADD CONSTRAINT produces_pkey PRIMARY KEY (virus_name, symptom_name);


--
-- Name: symptom_pkey; Type: CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY symptom
    ADD CONSTRAINT symptom_pkey PRIMARY KEY (symptom_name);


--
-- Name: treatment_for_pkey; Type: CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY treatment_for
    ADD CONSTRAINT treatment_for_pkey PRIMARY KEY (hospital_name, virus_name);


--
-- Name: admitted_to_hospital_name_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY admitted_to
    ADD CONSTRAINT admitted_to_hospital_name_fkey FOREIGN KEY (hospital_name) REFERENCES hospital(hospital_name) ON UPDATE CASCADE;


--
-- Name: admitted_to_pid_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY admitted_to
    ADD CONSTRAINT admitted_to_pid_fkey FOREIGN KEY (pid) REFERENCES patient(pid) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: checks_on_mid_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY checks_on
    ADD CONSTRAINT checks_on_mid_fkey FOREIGN KEY (mid) REFERENCES medic(mid) ON UPDATE CASCADE;


--
-- Name: checks_on_pid_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY checks_on
    ADD CONSTRAINT checks_on_pid_fkey FOREIGN KEY (pid) REFERENCES patient(pid) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contacted_exposed_pid_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY contacted
    ADD CONSTRAINT contacted_exposed_pid_fkey FOREIGN KEY (exposed_pid) REFERENCES patient(pid) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: contacted_infected_pid_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY contacted
    ADD CONSTRAINT contacted_infected_pid_fkey FOREIGN KEY (infected_pid) REFERENCES patient(pid) ON UPDATE CASCADE;


--
-- Name: exhibits_pid_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY exhibits
    ADD CONSTRAINT exhibits_pid_fkey FOREIGN KEY (pid) REFERENCES patient(pid) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: exhibits_symptom_name_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY exhibits
    ADD CONSTRAINT exhibits_symptom_name_fkey FOREIGN KEY (symptom_name) REFERENCES symptom(symptom_name) ON UPDATE CASCADE;


--
-- Name: has_pid_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY has
    ADD CONSTRAINT has_pid_fkey FOREIGN KEY (pid) REFERENCES patient(pid) ON UPDATE CASCADE;


--
-- Name: has_virus_name_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY has
    ADD CONSTRAINT has_virus_name_fkey FOREIGN KEY (virus_name) REFERENCES disease(virus_name) ON UPDATE CASCADE;


--
-- Name: produces_symptom_name_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY produces
    ADD CONSTRAINT produces_symptom_name_fkey FOREIGN KEY (symptom_name) REFERENCES symptom(symptom_name) ON UPDATE CASCADE;


--
-- Name: produces_virus_name_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY produces
    ADD CONSTRAINT produces_virus_name_fkey FOREIGN KEY (virus_name) REFERENCES disease(virus_name) ON UPDATE CASCADE;


--
-- Name: treatment_for_hospital_name_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY treatment_for
    ADD CONSTRAINT treatment_for_hospital_name_fkey FOREIGN KEY (hospital_name) REFERENCES hospital(hospital_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: treatment_for_virus_name_fkey; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY treatment_for
    ADD CONSTRAINT treatment_for_virus_name_fkey FOREIGN KEY (virus_name) REFERENCES disease(virus_name) ON UPDATE CASCADE ON DELETE CASCADE;


--
-- Name: works_at; Type: FK CONSTRAINT; Schema: kmg2183; Owner: kmg2183
--

ALTER TABLE ONLY medic
    ADD CONSTRAINT works_at FOREIGN KEY (hospital_name) REFERENCES hospital(hospital_name) ON UPDATE CASCADE;


--
-- Name: kmg2183; Type: ACL; Schema: -; Owner: kmg2183
--

REVOKE ALL ON SCHEMA kmg2183 FROM PUBLIC;
REVOKE ALL ON SCHEMA kmg2183 FROM kmg2183;
GRANT ALL ON SCHEMA kmg2183 TO kmg2183;


--
-- PostgreSQL database dump complete
--

