CREATE TABLE admitted_to
(
    pid INTEGER NOT NULL,
    hospital_name VARCHAR(60) NOT NULL,
    admitted_on DATE,
    CONSTRAINT admitted_to_pkey PRIMARY KEY (pid, hospital_name),
    CONSTRAINT admitted_to_pid_fkey FOREIGN KEY (pid) REFERENCES patient (pid),
    CONSTRAINT admitted_to_hospital_name_fkey FOREIGN KEY (hospital_name) REFERENCES hospital (hospital_name)
);
CREATE TABLE checks_on
(
    pid BIGINT NOT NULL,
    mid BIGINT NOT NULL,
    checked_on TIMESTAMP,
    CONSTRAINT checks_on_pkey PRIMARY KEY (pid, mid),
    CONSTRAINT checks_on_pid_fkey FOREIGN KEY (pid) REFERENCES patient (pid),
    CONSTRAINT checks_on_mid_fkey FOREIGN KEY (mid) REFERENCES medic (mid)
);
CREATE TABLE contacted
(
    infected_pid BIGINT NOT NULL,
    exposed_pid BIGINT NOT NULL,
    contact_date TIMESTAMP,
    CONSTRAINT contacted_pkey PRIMARY KEY (infected_pid, exposed_pid),
    CONSTRAINT contacted_infected_pid_fkey FOREIGN KEY (infected_pid) REFERENCES patient (pid),
    CONSTRAINT contacted_exposed_pid_fkey FOREIGN KEY (exposed_pid) REFERENCES patient (pid)
);
CREATE TABLE disease
(
    virus_name TEXT PRIMARY KEY NOT NULL,
    incubation BIGINT,
    duration BIGINT
);
CREATE TABLE exhibits
(
    pid INTEGER NOT NULL,
    symptom_name VARCHAR(20) NOT NULL,
    since DATE,
    CONSTRAINT exhibits_pkey PRIMARY KEY (pid, symptom_name),
    CONSTRAINT exhibits_pid_fkey FOREIGN KEY (pid) REFERENCES patient (pid),
    CONSTRAINT exhibits_symptom_name_fkey FOREIGN KEY (symptom_name) REFERENCES symptom (symptom_name)
);
CREATE TABLE has
(
    pid INTEGER NOT NULL,
    virus_name VARCHAR(20) NOT NULL,
    since DATE,
    CONSTRAINT has_pkey PRIMARY KEY (pid, virus_name),
    CONSTRAINT has_pid_fkey FOREIGN KEY (pid) REFERENCES patient (pid),
    CONSTRAINT has_virus_name_fkey FOREIGN KEY (virus_name) REFERENCES disease (virus_name)
);
CREATE TABLE hospital
(
    hospital_name TEXT PRIMARY KEY NOT NULL,
    number_of_beds BIGINT,
    latitude VARCHAR(20),
    longitude VARCHAR(20)
);
CREATE TABLE location
(
    latitude TEXT,
    longitude TEXT,
    state TEXT,
    country TEXT,
    postal_code TEXT
);
CREATE TABLE medic
(
    mid BIGINT PRIMARY KEY NOT NULL,
    name TEXT,
    phone_number TEXT,
    latitude VARCHAR(20),
    longitude VARCHAR(20),
    hospital_name TEXT,
    CONSTRAINT works_at FOREIGN KEY (hospital_name) REFERENCES hospital (hospital_name)
);
CREATE TABLE patient
(
    pid BIGINT PRIMARY KEY NOT NULL,
    forename TEXT,
    surname TEXT,
    status TEXT,
    phone_number TEXT,
    dob TEXT,
    latitude VARCHAR(20),
    longitude VARCHAR(20)
);
CREATE TABLE produces
(
    virus_name VARCHAR(20) NOT NULL,
    symptom_name VARCHAR(35) NOT NULL,
    CONSTRAINT produces_pkey PRIMARY KEY (virus_name, symptom_name),
    CONSTRAINT produces_virus_name_fkey FOREIGN KEY (virus_name) REFERENCES disease (virus_name),
    CONSTRAINT produces_symptom_name_fkey FOREIGN KEY (symptom_name) REFERENCES symptom (symptom_name)
);
CREATE TABLE symptom
(
    symptom_name TEXT PRIMARY KEY NOT NULL,
    description TEXT
);
CREATE TABLE treatment_for
(
    hospital_name VARCHAR(60) NOT NULL,
    virus_name VARCHAR(20) NOT NULL,
    CONSTRAINT treatment_for_pkey PRIMARY KEY (hospital_name, virus_name),
    CONSTRAINT treatment_for_hospital_name_fkey FOREIGN KEY (hospital_name) REFERENCES hospital (hospital_name),
    CONSTRAINT treatment_for_virus_name_fkey FOREIGN KEY (virus_name) REFERENCES disease (virus_name)
);
CREATE TABLE pg_aggregate
(
    aggfnoid REGPROC NOT NULL,
    aggtransfn REGPROC NOT NULL,
    aggfinalfn REGPROC NOT NULL,
    aggsortop OID NOT NULL,
    aggtranstype OID NOT NULL,
    agginitval TEXT
);
CREATE UNIQUE INDEX pg_aggregate_fnoid_index ON pg_aggregate (aggfnoid);
CREATE TABLE pg_am
(
    amname NAME NOT NULL,
    amstrategies SMALLINT NOT NULL,
    amsupport SMALLINT NOT NULL,
    amcanorder BOOLEAN NOT NULL,
    amcanorderbyop BOOLEAN NOT NULL,
    amcanbackward BOOLEAN NOT NULL,
    amcanunique BOOLEAN NOT NULL,
    amcanmulticol BOOLEAN NOT NULL,
    amoptionalkey BOOLEAN NOT NULL,
    amsearcharray BOOLEAN NOT NULL,
    amsearchnulls BOOLEAN NOT NULL,
    amstorage BOOLEAN NOT NULL,
    amclusterable BOOLEAN NOT NULL,
    ampredlocks BOOLEAN NOT NULL,
    amkeytype OID NOT NULL,
    aminsert REGPROC NOT NULL,
    ambeginscan REGPROC NOT NULL,
    amgettuple REGPROC NOT NULL,
    amgetbitmap REGPROC NOT NULL,
    amrescan REGPROC NOT NULL,
    amendscan REGPROC NOT NULL,
    ammarkpos REGPROC NOT NULL,
    amrestrpos REGPROC NOT NULL,
    ambuild REGPROC NOT NULL,
    ambuildempty REGPROC NOT NULL,
    ambulkdelete REGPROC NOT NULL,
    amvacuumcleanup REGPROC NOT NULL,
    amcanreturn REGPROC NOT NULL,
    amcostestimate REGPROC NOT NULL,
    amoptions REGPROC NOT NULL
);
CREATE UNIQUE INDEX pg_am_name_index ON pg_am (amname);
CREATE UNIQUE INDEX pg_am_oid_index ON pg_am (oid);
CREATE TABLE pg_amop
(
    amopfamily OID NOT NULL,
    amoplefttype OID NOT NULL,
    amoprighttype OID NOT NULL,
    amopstrategy SMALLINT NOT NULL,
    amoppurpose "char" NOT NULL,
    amopopr OID NOT NULL,
    amopmethod OID NOT NULL,
    amopsortfamily OID NOT NULL
);
CREATE UNIQUE INDEX pg_amop_fam_strat_index ON pg_amop (amopfamily, amoplefttype, amoprighttype, amopstrategy);
CREATE UNIQUE INDEX pg_amop_opr_fam_index ON pg_amop (amopopr, amoppurpose, amopfamily);
CREATE UNIQUE INDEX pg_amop_oid_index ON pg_amop (oid);
CREATE TABLE pg_amproc
(
    amprocfamily OID NOT NULL,
    amproclefttype OID NOT NULL,
    amprocrighttype OID NOT NULL,
    amprocnum SMALLINT NOT NULL,
    amproc REGPROC NOT NULL
);
CREATE UNIQUE INDEX pg_amproc_fam_proc_index ON pg_amproc (amprocfamily, amproclefttype, amprocrighttype, amprocnum);
CREATE UNIQUE INDEX pg_amproc_oid_index ON pg_amproc (oid);
CREATE TABLE pg_attrdef
(
    adrelid OID NOT NULL,
    adnum SMALLINT NOT NULL,
    adbin PG_NODE_TREE,
    adsrc TEXT
);
CREATE UNIQUE INDEX pg_attrdef_adrelid_adnum_index ON pg_attrdef (adrelid, adnum);
CREATE UNIQUE INDEX pg_attrdef_oid_index ON pg_attrdef (oid);
CREATE TABLE pg_attribute
(
    attrelid OID NOT NULL,
    attname NAME NOT NULL,
    atttypid OID NOT NULL,
    attstattarget INTEGER NOT NULL,
    attlen SMALLINT NOT NULL,
    attnum SMALLINT NOT NULL,
    attndims INTEGER NOT NULL,
    attcacheoff INTEGER NOT NULL,
    atttypmod INTEGER NOT NULL,
    attbyval BOOLEAN NOT NULL,
    attstorage "char" NOT NULL,
    attalign "char" NOT NULL,
    attnotnull BOOLEAN NOT NULL,
    atthasdef BOOLEAN NOT NULL,
    attisdropped BOOLEAN NOT NULL,
    attislocal BOOLEAN NOT NULL,
    attinhcount INTEGER NOT NULL,
    attcollation OID NOT NULL,
    attacl ACLITEM[],
    attoptions TEXT[],
    attfdwoptions TEXT[]
);
CREATE UNIQUE INDEX pg_attribute_relid_attnam_index ON pg_attribute (attrelid, attname);
CREATE UNIQUE INDEX pg_attribute_relid_attnum_index ON pg_attribute (attrelid, attnum);
CREATE TABLE pg_auth_members
(
    roleid OID NOT NULL,
    member OID NOT NULL,
    grantor OID NOT NULL,
    admin_option BOOLEAN NOT NULL
);
CREATE UNIQUE INDEX pg_auth_members_role_member_index ON pg_auth_members (roleid, member);
CREATE UNIQUE INDEX pg_auth_members_member_role_index ON pg_auth_members (member, roleid);
CREATE TABLE pg_authid
(
    rolname NAME NOT NULL,
    rolsuper BOOLEAN NOT NULL,
    rolinherit BOOLEAN NOT NULL,
    rolcreaterole BOOLEAN NOT NULL,
    rolcreatedb BOOLEAN NOT NULL,
    rolcatupdate BOOLEAN NOT NULL,
    rolcanlogin BOOLEAN NOT NULL,
    rolreplication BOOLEAN NOT NULL,
    rolconnlimit INTEGER NOT NULL,
    rolpassword TEXT,
    rolvaliduntil TIMESTAMP WITH TIME ZONE
);
CREATE UNIQUE INDEX pg_authid_rolname_index ON pg_authid (rolname);
CREATE UNIQUE INDEX pg_authid_oid_index ON pg_authid (oid);
CREATE TABLE pg_cast
(
    castsource OID NOT NULL,
    casttarget OID NOT NULL,
    castfunc OID NOT NULL,
    castcontext "char" NOT NULL,
    castmethod "char" NOT NULL
);
CREATE UNIQUE INDEX pg_cast_source_target_index ON pg_cast (castsource, casttarget);
CREATE UNIQUE INDEX pg_cast_oid_index ON pg_cast (oid);
CREATE TABLE pg_class
(
    relname NAME NOT NULL,
    relnamespace OID NOT NULL,
    reltype OID NOT NULL,
    reloftype OID NOT NULL,
    relowner OID NOT NULL,
    relam OID NOT NULL,
    relfilenode OID NOT NULL,
    reltablespace OID NOT NULL,
    relpages INTEGER NOT NULL,
    reltuples REAL NOT NULL,
    relallvisible INTEGER NOT NULL,
    reltoastrelid OID NOT NULL,
    reltoastidxid OID NOT NULL,
    relhasindex BOOLEAN NOT NULL,
    relisshared BOOLEAN NOT NULL,
    relpersistence "char" NOT NULL,
    relkind "char" NOT NULL,
    relnatts SMALLINT NOT NULL,
    relchecks SMALLINT NOT NULL,
    relhasoids BOOLEAN NOT NULL,
    relhaspkey BOOLEAN NOT NULL,
    relhasrules BOOLEAN NOT NULL,
    relhastriggers BOOLEAN NOT NULL,
    relhassubclass BOOLEAN NOT NULL,
    relispopulated BOOLEAN NOT NULL,
    relfrozenxid XID NOT NULL,
    relminmxid XID NOT NULL,
    relacl ACLITEM[],
    reloptions TEXT[]
);
CREATE UNIQUE INDEX pg_class_relname_nsp_index ON pg_class (relname, relnamespace);
CREATE UNIQUE INDEX pg_class_oid_index ON pg_class (oid);
CREATE TABLE pg_collation
(
    collname NAME NOT NULL,
    collnamespace OID NOT NULL,
    collowner OID NOT NULL,
    collencoding INTEGER NOT NULL,
    collcollate NAME NOT NULL,
    collctype NAME NOT NULL
);
CREATE UNIQUE INDEX pg_collation_name_enc_nsp_index ON pg_collation (collname, collencoding, collnamespace);
CREATE UNIQUE INDEX pg_collation_oid_index ON pg_collation (oid);
CREATE TABLE pg_constraint
(
    conname NAME NOT NULL,
    connamespace OID NOT NULL,
    contype "char" NOT NULL,
    condeferrable BOOLEAN NOT NULL,
    condeferred BOOLEAN NOT NULL,
    convalidated BOOLEAN NOT NULL,
    conrelid OID NOT NULL,
    contypid OID NOT NULL,
    conindid OID NOT NULL,
    confrelid OID NOT NULL,
    confupdtype "char" NOT NULL,
    confdeltype "char" NOT NULL,
    confmatchtype "char" NOT NULL,
    conislocal BOOLEAN NOT NULL,
    coninhcount INTEGER NOT NULL,
    connoinherit BOOLEAN NOT NULL,
    conkey SMALLINT[],
    confkey SMALLINT[],
    conpfeqop OID[],
    conppeqop OID[],
    conffeqop OID[],
    conexclop OID[],
    conbin PG_NODE_TREE,
    consrc TEXT
);
CREATE UNIQUE INDEX pg_constraint_oid_index ON pg_constraint (oid);
CREATE INDEX pg_constraint_conname_nsp_index ON pg_constraint (conname, connamespace);
CREATE INDEX pg_constraint_conrelid_index ON pg_constraint (conrelid);
CREATE INDEX pg_constraint_contypid_index ON pg_constraint (contypid);
CREATE TABLE pg_conversion
(
    conname NAME NOT NULL,
    connamespace OID NOT NULL,
    conowner OID NOT NULL,
    conforencoding INTEGER NOT NULL,
    contoencoding INTEGER NOT NULL,
    conproc REGPROC NOT NULL,
    condefault BOOLEAN NOT NULL
);
CREATE UNIQUE INDEX pg_conversion_name_nsp_index ON pg_conversion (conname, connamespace);
CREATE UNIQUE INDEX pg_conversion_default_index ON pg_conversion (connamespace, conforencoding, contoencoding, oid);
CREATE UNIQUE INDEX pg_conversion_oid_index ON pg_conversion (oid);
CREATE TABLE pg_database
(
    datname NAME NOT NULL,
    datdba OID NOT NULL,
    encoding INTEGER NOT NULL,
    datcollate NAME NOT NULL,
    datctype NAME NOT NULL,
    datistemplate BOOLEAN NOT NULL,
    datallowconn BOOLEAN NOT NULL,
    datconnlimit INTEGER NOT NULL,
    datlastsysoid OID NOT NULL,
    datfrozenxid XID NOT NULL,
    datminmxid XID NOT NULL,
    dattablespace OID NOT NULL,
    datacl ACLITEM[]
);
CREATE UNIQUE INDEX pg_database_datname_index ON pg_database (datname);
CREATE UNIQUE INDEX pg_database_oid_index ON pg_database (oid);
CREATE TABLE pg_db_role_setting
(
    setdatabase OID NOT NULL,
    setrole OID NOT NULL,
    setconfig TEXT[]
);
CREATE UNIQUE INDEX pg_db_role_setting_databaseid_rol_index ON pg_db_role_setting (setdatabase, setrole);
CREATE TABLE pg_default_acl
(
    defaclrole OID NOT NULL,
    defaclnamespace OID NOT NULL,
    defaclobjtype "char" NOT NULL,
    defaclacl ACLITEM[]
);
CREATE UNIQUE INDEX pg_default_acl_role_nsp_obj_index ON pg_default_acl (defaclrole, defaclnamespace, defaclobjtype);
CREATE UNIQUE INDEX pg_default_acl_oid_index ON pg_default_acl (oid);
CREATE TABLE pg_depend
(
    classid OID NOT NULL,
    objid OID NOT NULL,
    objsubid INTEGER NOT NULL,
    refclassid OID NOT NULL,
    refobjid OID NOT NULL,
    refobjsubid INTEGER NOT NULL,
    deptype "char" NOT NULL
);
CREATE INDEX pg_depend_depender_index ON pg_depend (classid, objid, objsubid);
CREATE INDEX pg_depend_reference_index ON pg_depend (refclassid, refobjid, refobjsubid);
CREATE TABLE pg_description
(
    objoid OID NOT NULL,
    classoid OID NOT NULL,
    objsubid INTEGER NOT NULL,
    description TEXT
);
CREATE UNIQUE INDEX pg_description_o_c_o_index ON pg_description (objoid, classoid, objsubid);
CREATE TABLE pg_enum
(
    enumtypid OID NOT NULL,
    enumsortorder REAL NOT NULL,
    enumlabel NAME NOT NULL
);
CREATE UNIQUE INDEX pg_enum_typid_sortorder_index ON pg_enum (enumtypid, enumsortorder);
CREATE UNIQUE INDEX pg_enum_typid_label_index ON pg_enum (enumtypid, enumlabel);
CREATE UNIQUE INDEX pg_enum_oid_index ON pg_enum (oid);
CREATE TABLE pg_event_trigger
(
    evtname NAME NOT NULL,
    evtevent NAME NOT NULL,
    evtowner OID NOT NULL,
    evtfoid OID NOT NULL,
    evtenabled "char" NOT NULL,
    evttags TEXT[]
);
CREATE UNIQUE INDEX pg_event_trigger_evtname_index ON pg_event_trigger (evtname);
CREATE UNIQUE INDEX pg_event_trigger_oid_index ON pg_event_trigger (oid);
CREATE TABLE pg_extension
(
    extname NAME NOT NULL,
    extowner OID NOT NULL,
    extnamespace OID NOT NULL,
    extrelocatable BOOLEAN NOT NULL,
    extversion TEXT,
    extconfig OID[],
    extcondition TEXT[]
);
CREATE UNIQUE INDEX pg_extension_name_index ON pg_extension (extname);
CREATE UNIQUE INDEX pg_extension_oid_index ON pg_extension (oid);
CREATE TABLE pg_foreign_data_wrapper
(
    fdwname NAME NOT NULL,
    fdwowner OID NOT NULL,
    fdwhandler OID NOT NULL,
    fdwvalidator OID NOT NULL,
    fdwacl ACLITEM[],
    fdwoptions TEXT[]
);
CREATE UNIQUE INDEX pg_foreign_data_wrapper_name_index ON pg_foreign_data_wrapper (fdwname);
CREATE UNIQUE INDEX pg_foreign_data_wrapper_oid_index ON pg_foreign_data_wrapper (oid);
CREATE TABLE pg_foreign_server
(
    srvname NAME NOT NULL,
    srvowner OID NOT NULL,
    srvfdw OID NOT NULL,
    srvtype TEXT,
    srvversion TEXT,
    srvacl ACLITEM[],
    srvoptions TEXT[]
);
CREATE UNIQUE INDEX pg_foreign_server_name_index ON pg_foreign_server (srvname);
CREATE UNIQUE INDEX pg_foreign_server_oid_index ON pg_foreign_server (oid);
CREATE TABLE pg_foreign_table
(
    ftrelid OID NOT NULL,
    ftserver OID NOT NULL,
    ftoptions TEXT[]
);
CREATE UNIQUE INDEX pg_foreign_table_relid_index ON pg_foreign_table (ftrelid);
CREATE TABLE pg_index
(
    indexrelid OID NOT NULL,
    indrelid OID NOT NULL,
    indnatts SMALLINT NOT NULL,
    indisunique BOOLEAN NOT NULL,
    indisprimary BOOLEAN NOT NULL,
    indisexclusion BOOLEAN NOT NULL,
    indimmediate BOOLEAN NOT NULL,
    indisclustered BOOLEAN NOT NULL,
    indisvalid BOOLEAN NOT NULL,
    indcheckxmin BOOLEAN NOT NULL,
    indisready BOOLEAN NOT NULL,
    indislive BOOLEAN NOT NULL,
    indkey INT2VECTOR NOT NULL,
    indcollation OIDVECTOR NOT NULL,
    indclass OIDVECTOR NOT NULL,
    indoption INT2VECTOR NOT NULL,
    indexprs PG_NODE_TREE,
    indpred PG_NODE_TREE
);
CREATE UNIQUE INDEX pg_index_indexrelid_index ON pg_index (indexrelid);
CREATE INDEX pg_index_indrelid_index ON pg_index (indrelid);
CREATE TABLE pg_inherits
(
    inhrelid OID NOT NULL,
    inhparent OID NOT NULL,
    inhseqno INTEGER NOT NULL
);
CREATE UNIQUE INDEX pg_inherits_relid_seqno_index ON pg_inherits (inhrelid, inhseqno);
CREATE INDEX pg_inherits_parent_index ON pg_inherits (inhparent);
CREATE TABLE pg_language
(
    lanname NAME NOT NULL,
    lanowner OID NOT NULL,
    lanispl BOOLEAN NOT NULL,
    lanpltrusted BOOLEAN NOT NULL,
    lanplcallfoid OID NOT NULL,
    laninline OID NOT NULL,
    lanvalidator OID NOT NULL,
    lanacl ACLITEM[]
);
CREATE UNIQUE INDEX pg_language_name_index ON pg_language (lanname);
CREATE UNIQUE INDEX pg_language_oid_index ON pg_language (oid);
CREATE TABLE pg_largeobject
(
    loid OID NOT NULL,
    pageno INTEGER NOT NULL,
    data BYTEA
);
CREATE UNIQUE INDEX pg_largeobject_loid_pn_index ON pg_largeobject (loid, pageno);
CREATE TABLE pg_largeobject_metadata
(
    lomowner OID NOT NULL,
    lomacl ACLITEM[]
);
CREATE UNIQUE INDEX pg_largeobject_metadata_oid_index ON pg_largeobject_metadata (oid);
CREATE TABLE pg_namespace
(
    nspname NAME NOT NULL,
    nspowner OID NOT NULL,
    nspacl ACLITEM[]
);
CREATE UNIQUE INDEX pg_namespace_nspname_index ON pg_namespace (nspname);
CREATE UNIQUE INDEX pg_namespace_oid_index ON pg_namespace (oid);
CREATE TABLE pg_opclass
(
    opcmethod OID NOT NULL,
    opcname NAME NOT NULL,
    opcnamespace OID NOT NULL,
    opcowner OID NOT NULL,
    opcfamily OID NOT NULL,
    opcintype OID NOT NULL,
    opcdefault BOOLEAN NOT NULL,
    opckeytype OID NOT NULL
);
CREATE UNIQUE INDEX pg_opclass_am_name_nsp_index ON pg_opclass (opcmethod, opcname, opcnamespace);
CREATE UNIQUE INDEX pg_opclass_oid_index ON pg_opclass (oid);
CREATE TABLE pg_operator
(
    oprname NAME NOT NULL,
    oprnamespace OID NOT NULL,
    oprowner OID NOT NULL,
    oprkind "char" NOT NULL,
    oprcanmerge BOOLEAN NOT NULL,
    oprcanhash BOOLEAN NOT NULL,
    oprleft OID NOT NULL,
    oprright OID NOT NULL,
    oprresult OID NOT NULL,
    oprcom OID NOT NULL,
    oprnegate OID NOT NULL,
    oprcode REGPROC NOT NULL,
    oprrest REGPROC NOT NULL,
    oprjoin REGPROC NOT NULL
);
CREATE UNIQUE INDEX pg_operator_oprname_l_r_n_index ON pg_operator (oprname, oprleft, oprright, oprnamespace);
CREATE UNIQUE INDEX pg_operator_oid_index ON pg_operator (oid);
CREATE TABLE pg_opfamily
(
    opfmethod OID NOT NULL,
    opfname NAME NOT NULL,
    opfnamespace OID NOT NULL,
    opfowner OID NOT NULL
);
CREATE UNIQUE INDEX pg_opfamily_am_name_nsp_index ON pg_opfamily (opfmethod, opfname, opfnamespace);
CREATE UNIQUE INDEX pg_opfamily_oid_index ON pg_opfamily (oid);
CREATE TABLE pg_pltemplate
(
    tmplname NAME NOT NULL,
    tmpltrusted BOOLEAN NOT NULL,
    tmpldbacreate BOOLEAN NOT NULL,
    tmplhandler TEXT,
    tmplinline TEXT,
    tmplvalidator TEXT,
    tmpllibrary TEXT,
    tmplacl ACLITEM[]
);
CREATE UNIQUE INDEX pg_pltemplate_name_index ON pg_pltemplate (tmplname);
CREATE TABLE pg_proc
(
    proname NAME NOT NULL,
    pronamespace OID NOT NULL,
    proowner OID NOT NULL,
    prolang OID NOT NULL,
    procost REAL NOT NULL,
    prorows REAL NOT NULL,
    provariadic OID NOT NULL,
    protransform REGPROC NOT NULL,
    proisagg BOOLEAN NOT NULL,
    proiswindow BOOLEAN NOT NULL,
    prosecdef BOOLEAN NOT NULL,
    proleakproof BOOLEAN NOT NULL,
    proisstrict BOOLEAN NOT NULL,
    proretset BOOLEAN NOT NULL,
    provolatile "char" NOT NULL,
    pronargs SMALLINT NOT NULL,
    pronargdefaults SMALLINT NOT NULL,
    prorettype OID NOT NULL,
    proargtypes OIDVECTOR NOT NULL,
    proallargtypes OID[],
    proargmodes "CHAR"[],
    proargnames TEXT[],
    proargdefaults PG_NODE_TREE,
    prosrc TEXT,
    probin TEXT,
    proconfig TEXT[],
    proacl ACLITEM[]
);
CREATE UNIQUE INDEX pg_proc_proname_args_nsp_index ON pg_proc (proname, proargtypes, pronamespace);
CREATE UNIQUE INDEX pg_proc_oid_index ON pg_proc (oid);
CREATE TABLE pg_range
(
    rngtypid OID NOT NULL,
    rngsubtype OID NOT NULL,
    rngcollation OID NOT NULL,
    rngsubopc OID NOT NULL,
    rngcanonical REGPROC NOT NULL,
    rngsubdiff REGPROC NOT NULL
);
CREATE UNIQUE INDEX pg_range_rngtypid_index ON pg_range (rngtypid);
CREATE TABLE pg_rewrite
(
    rulename NAME NOT NULL,
    ev_class OID NOT NULL,
    ev_attr SMALLINT NOT NULL,
    ev_type "char" NOT NULL,
    ev_enabled "char" NOT NULL,
    is_instead BOOLEAN NOT NULL,
    ev_qual PG_NODE_TREE,
    ev_action PG_NODE_TREE
);
CREATE UNIQUE INDEX pg_rewrite_rel_rulename_index ON pg_rewrite (ev_class, rulename);
CREATE UNIQUE INDEX pg_rewrite_oid_index ON pg_rewrite (oid);
CREATE TABLE pg_seclabel
(
    objoid OID NOT NULL,
    classoid OID NOT NULL,
    objsubid INTEGER NOT NULL,
    provider TEXT,
    label TEXT
);
CREATE UNIQUE INDEX pg_seclabel_object_index ON pg_seclabel (objoid, classoid, objsubid, provider);
CREATE TABLE pg_shdepend
(
    dbid OID NOT NULL,
    classid OID NOT NULL,
    objid OID NOT NULL,
    objsubid INTEGER NOT NULL,
    refclassid OID NOT NULL,
    refobjid OID NOT NULL,
    deptype "char" NOT NULL
);
CREATE INDEX pg_shdepend_depender_index ON pg_shdepend (dbid, classid, objid, objsubid);
CREATE INDEX pg_shdepend_reference_index ON pg_shdepend (refclassid, refobjid);
CREATE TABLE pg_shdescription
(
    objoid OID NOT NULL,
    classoid OID NOT NULL,
    description TEXT
);
CREATE UNIQUE INDEX pg_shdescription_o_c_index ON pg_shdescription (objoid, classoid);
CREATE TABLE pg_shseclabel
(
    objoid OID NOT NULL,
    classoid OID NOT NULL,
    provider TEXT,
    label TEXT
);
CREATE UNIQUE INDEX pg_shseclabel_object_index ON pg_shseclabel (objoid, classoid, provider);
CREATE TABLE pg_statistic
(
    starelid OID NOT NULL,
    staattnum SMALLINT NOT NULL,
    stainherit BOOLEAN NOT NULL,
    stanullfrac REAL NOT NULL,
    stawidth INTEGER NOT NULL,
    stadistinct REAL NOT NULL,
    stakind1 SMALLINT NOT NULL,
    stakind2 SMALLINT NOT NULL,
    stakind3 SMALLINT NOT NULL,
    stakind4 SMALLINT NOT NULL,
    stakind5 SMALLINT NOT NULL,
    staop1 OID NOT NULL,
    staop2 OID NOT NULL,
    staop3 OID NOT NULL,
    staop4 OID NOT NULL,
    staop5 OID NOT NULL,
    stanumbers1 REAL[],
    stanumbers2 REAL[],
    stanumbers3 REAL[],
    stanumbers4 REAL[],
    stanumbers5 REAL[],
    stavalues1 ANYARRAY,
    stavalues2 ANYARRAY,
    stavalues3 ANYARRAY,
    stavalues4 ANYARRAY,
    stavalues5 ANYARRAY
);
CREATE UNIQUE INDEX pg_statistic_relid_att_inh_index ON pg_statistic (starelid, staattnum, stainherit);
CREATE TABLE pg_tablespace
(
    spcname NAME NOT NULL,
    spcowner OID NOT NULL,
    spcacl ACLITEM[],
    spcoptions TEXT[]
);
CREATE UNIQUE INDEX pg_tablespace_spcname_index ON pg_tablespace (spcname);
CREATE UNIQUE INDEX pg_tablespace_oid_index ON pg_tablespace (oid);
CREATE TABLE pg_trigger
(
    tgrelid OID NOT NULL,
    tgname NAME NOT NULL,
    tgfoid OID NOT NULL,
    tgtype SMALLINT NOT NULL,
    tgenabled "char" NOT NULL,
    tgisinternal BOOLEAN NOT NULL,
    tgconstrrelid OID NOT NULL,
    tgconstrindid OID NOT NULL,
    tgconstraint OID NOT NULL,
    tgdeferrable BOOLEAN NOT NULL,
    tginitdeferred BOOLEAN NOT NULL,
    tgnargs SMALLINT NOT NULL,
    tgattr INT2VECTOR NOT NULL,
    tgargs BYTEA,
    tgqual PG_NODE_TREE
);
CREATE UNIQUE INDEX pg_trigger_tgrelid_tgname_index ON pg_trigger (tgrelid, tgname);
CREATE UNIQUE INDEX pg_trigger_oid_index ON pg_trigger (oid);
CREATE INDEX pg_trigger_tgconstraint_index ON pg_trigger (tgconstraint);
CREATE TABLE pg_ts_config
(
    cfgname NAME NOT NULL,
    cfgnamespace OID NOT NULL,
    cfgowner OID NOT NULL,
    cfgparser OID NOT NULL
);
CREATE UNIQUE INDEX pg_ts_config_cfgname_index ON pg_ts_config (cfgname, cfgnamespace);
CREATE UNIQUE INDEX pg_ts_config_oid_index ON pg_ts_config (oid);
CREATE TABLE pg_ts_config_map
(
    mapcfg OID NOT NULL,
    maptokentype INTEGER NOT NULL,
    mapseqno INTEGER NOT NULL,
    mapdict OID NOT NULL
);
CREATE UNIQUE INDEX pg_ts_config_map_index ON pg_ts_config_map (mapcfg, maptokentype, mapseqno);
CREATE TABLE pg_ts_dict
(
    dictname NAME NOT NULL,
    dictnamespace OID NOT NULL,
    dictowner OID NOT NULL,
    dicttemplate OID NOT NULL,
    dictinitoption TEXT
);
CREATE UNIQUE INDEX pg_ts_dict_dictname_index ON pg_ts_dict (dictname, dictnamespace);
CREATE UNIQUE INDEX pg_ts_dict_oid_index ON pg_ts_dict (oid);
CREATE TABLE pg_ts_parser
(
    prsname NAME NOT NULL,
    prsnamespace OID NOT NULL,
    prsstart REGPROC NOT NULL,
    prstoken REGPROC NOT NULL,
    prsend REGPROC NOT NULL,
    prsheadline REGPROC NOT NULL,
    prslextype REGPROC NOT NULL
);
CREATE UNIQUE INDEX pg_ts_parser_prsname_index ON pg_ts_parser (prsname, prsnamespace);
CREATE UNIQUE INDEX pg_ts_parser_oid_index ON pg_ts_parser (oid);
CREATE TABLE pg_ts_template
(
    tmplname NAME NOT NULL,
    tmplnamespace OID NOT NULL,
    tmplinit REGPROC NOT NULL,
    tmpllexize REGPROC NOT NULL
);
CREATE UNIQUE INDEX pg_ts_template_tmplname_index ON pg_ts_template (tmplname, tmplnamespace);
CREATE UNIQUE INDEX pg_ts_template_oid_index ON pg_ts_template (oid);
CREATE TABLE pg_type
(
    typname NAME NOT NULL,
    typnamespace OID NOT NULL,
    typowner OID NOT NULL,
    typlen SMALLINT NOT NULL,
    typbyval BOOLEAN NOT NULL,
    typtype "char" NOT NULL,
    typcategory "char" NOT NULL,
    typispreferred BOOLEAN NOT NULL,
    typisdefined BOOLEAN NOT NULL,
    typdelim "char" NOT NULL,
    typrelid OID NOT NULL,
    typelem OID NOT NULL,
    typarray OID NOT NULL,
    typinput REGPROC NOT NULL,
    typoutput REGPROC NOT NULL,
    typreceive REGPROC NOT NULL,
    typsend REGPROC NOT NULL,
    typmodin REGPROC NOT NULL,
    typmodout REGPROC NOT NULL,
    typanalyze REGPROC NOT NULL,
    typalign "char" NOT NULL,
    typstorage "char" NOT NULL,
    typnotnull BOOLEAN NOT NULL,
    typbasetype OID NOT NULL,
    typtypmod INTEGER NOT NULL,
    typndims INTEGER NOT NULL,
    typcollation OID NOT NULL,
    typdefaultbin PG_NODE_TREE,
    typdefault TEXT,
    typacl ACLITEM[]
);
CREATE UNIQUE INDEX pg_type_typname_nsp_index ON pg_type (typname, typnamespace);
CREATE UNIQUE INDEX pg_type_oid_index ON pg_type (oid);
CREATE TABLE pg_user_mapping
(
    umuser OID NOT NULL,
    umserver OID NOT NULL,
    umoptions TEXT[]
);
CREATE UNIQUE INDEX pg_user_mapping_user_server_index ON pg_user_mapping (umuser, umserver);
CREATE UNIQUE INDEX pg_user_mapping_oid_index ON pg_user_mapping (oid);
CREATE TABLE pg_available_extension_versions
(
    name NAME,
    version TEXT,
    installed BOOLEAN,
    superuser BOOLEAN,
    relocatable BOOLEAN,
    schema NAME,
    requires NAME[],
    comment TEXT
);
CREATE TABLE pg_available_extensions
(
    name NAME,
    default_version TEXT,
    installed_version TEXT,
    comment TEXT
);
CREATE TABLE pg_cursors
(
    name TEXT,
    statement TEXT,
    is_holdable BOOLEAN,
    is_binary BOOLEAN,
    is_scrollable BOOLEAN,
    creation_time TIMESTAMP WITH TIME ZONE
);
CREATE TABLE pg_group
(
    groname NAME,
    grosysid OID,
    grolist OID[]
);
CREATE TABLE pg_indexes
(
    schemaname NAME,
    tablename NAME,
    indexname NAME,
    tablespace NAME,
    indexdef TEXT
);
CREATE TABLE pg_locks
(
    locktype TEXT,
    database OID,
    relation OID,
    page INTEGER,
    tuple SMALLINT,
    virtualxid TEXT,
    transactionid XID,
    classid OID,
    objid OID,
    objsubid SMALLINT,
    virtualtransaction TEXT,
    pid INTEGER,
    mode TEXT,
    granted BOOLEAN,
    fastpath BOOLEAN
);
CREATE TABLE pg_matviews
(
    schemaname NAME,
    matviewname NAME,
    matviewowner NAME,
    tablespace NAME,
    hasindexes BOOLEAN,
    ispopulated BOOLEAN,
    definition TEXT
);
CREATE TABLE pg_prepared_statements
(
    name TEXT,
    statement TEXT,
    prepare_time TIMESTAMP WITH TIME ZONE,
    parameter_types REGTYPE[],
    from_sql BOOLEAN
);
CREATE TABLE pg_prepared_xacts
(
    transaction XID,
    gid TEXT,
    prepared TIMESTAMP WITH TIME ZONE,
    owner NAME,
    database NAME
);
CREATE TABLE pg_roles
(
    rolname NAME,
    rolsuper BOOLEAN,
    rolinherit BOOLEAN,
    rolcreaterole BOOLEAN,
    rolcreatedb BOOLEAN,
    rolcatupdate BOOLEAN,
    rolcanlogin BOOLEAN,
    rolreplication BOOLEAN,
    rolconnlimit INTEGER,
    rolpassword TEXT,
    rolvaliduntil TIMESTAMP WITH TIME ZONE,
    rolconfig TEXT[],
    oid OID
);
CREATE TABLE pg_rules
(
    schemaname NAME,
    tablename NAME,
    rulename NAME,
    definition TEXT
);
CREATE TABLE pg_seclabels
(
    objoid OID,
    classoid OID,
    objsubid INTEGER,
    objtype TEXT,
    objnamespace OID,
    objname TEXT,
    provider TEXT,
    label TEXT
);
CREATE TABLE pg_settings
(
    name TEXT,
    setting TEXT,
    unit TEXT,
    category TEXT,
    short_desc TEXT,
    extra_desc TEXT,
    context TEXT,
    vartype TEXT,
    source TEXT,
    min_val TEXT,
    max_val TEXT,
    enumvals TEXT[],
    boot_val TEXT,
    reset_val TEXT,
    sourcefile TEXT,
    sourceline INTEGER
);
CREATE TABLE pg_shadow
(
    usename NAME,
    usesysid OID,
    usecreatedb BOOLEAN,
    usesuper BOOLEAN,
    usecatupd BOOLEAN,
    userepl BOOLEAN,
    passwd TEXT,
    valuntil ABSTIME,
    useconfig TEXT[]
);
CREATE TABLE pg_stat_activity
(
    datid OID,
    datname NAME,
    pid INTEGER,
    usesysid OID,
    usename NAME,
    application_name TEXT,
    client_addr INET,
    client_hostname TEXT,
    client_port INTEGER,
    backend_start TIMESTAMP WITH TIME ZONE,
    xact_start TIMESTAMP WITH TIME ZONE,
    query_start TIMESTAMP WITH TIME ZONE,
    state_change TIMESTAMP WITH TIME ZONE,
    waiting BOOLEAN,
    state TEXT,
    query TEXT
);
CREATE TABLE pg_stat_all_indexes
(
    relid OID,
    indexrelid OID,
    schemaname NAME,
    relname NAME,
    indexrelname NAME,
    idx_scan BIGINT,
    idx_tup_read BIGINT,
    idx_tup_fetch BIGINT
);
CREATE TABLE pg_stat_all_tables
(
    relid OID,
    schemaname NAME,
    relname NAME,
    seq_scan BIGINT,
    seq_tup_read BIGINT,
    idx_scan BIGINT,
    idx_tup_fetch BIGINT,
    n_tup_ins BIGINT,
    n_tup_upd BIGINT,
    n_tup_del BIGINT,
    n_tup_hot_upd BIGINT,
    n_live_tup BIGINT,
    n_dead_tup BIGINT,
    last_vacuum TIMESTAMP WITH TIME ZONE,
    last_autovacuum TIMESTAMP WITH TIME ZONE,
    last_analyze TIMESTAMP WITH TIME ZONE,
    last_autoanalyze TIMESTAMP WITH TIME ZONE,
    vacuum_count BIGINT,
    autovacuum_count BIGINT,
    analyze_count BIGINT,
    autoanalyze_count BIGINT
);
CREATE TABLE pg_stat_bgwriter
(
    checkpoints_timed BIGINT,
    checkpoints_req BIGINT,
    checkpoint_write_time DOUBLE PRECISION,
    checkpoint_sync_time DOUBLE PRECISION,
    buffers_checkpoint BIGINT,
    buffers_clean BIGINT,
    maxwritten_clean BIGINT,
    buffers_backend BIGINT,
    buffers_backend_fsync BIGINT,
    buffers_alloc BIGINT,
    stats_reset TIMESTAMP WITH TIME ZONE
);
CREATE TABLE pg_stat_database
(
    datid OID,
    datname NAME,
    numbackends INTEGER,
    xact_commit BIGINT,
    xact_rollback BIGINT,
    blks_read BIGINT,
    blks_hit BIGINT,
    tup_returned BIGINT,
    tup_fetched BIGINT,
    tup_inserted BIGINT,
    tup_updated BIGINT,
    tup_deleted BIGINT,
    conflicts BIGINT,
    temp_files BIGINT,
    temp_bytes BIGINT,
    deadlocks BIGINT,
    blk_read_time DOUBLE PRECISION,
    blk_write_time DOUBLE PRECISION,
    stats_reset TIMESTAMP WITH TIME ZONE
);
CREATE TABLE pg_stat_database_conflicts
(
    datid OID,
    datname NAME,
    confl_tablespace BIGINT,
    confl_lock BIGINT,
    confl_snapshot BIGINT,
    confl_bufferpin BIGINT,
    confl_deadlock BIGINT
);
CREATE TABLE pg_stat_replication
(
    pid INTEGER,
    usesysid OID,
    usename NAME,
    application_name TEXT,
    client_addr INET,
    client_hostname TEXT,
    client_port INTEGER,
    backend_start TIMESTAMP WITH TIME ZONE,
    state TEXT,
    sent_location TEXT,
    write_location TEXT,
    flush_location TEXT,
    replay_location TEXT,
    sync_priority INTEGER,
    sync_state TEXT
);
CREATE TABLE pg_stat_sys_indexes
(
    relid OID,
    indexrelid OID,
    schemaname NAME,
    relname NAME,
    indexrelname NAME,
    idx_scan BIGINT,
    idx_tup_read BIGINT,
    idx_tup_fetch BIGINT
);
CREATE TABLE pg_stat_sys_tables
(
    relid OID,
    schemaname NAME,
    relname NAME,
    seq_scan BIGINT,
    seq_tup_read BIGINT,
    idx_scan BIGINT,
    idx_tup_fetch BIGINT,
    n_tup_ins BIGINT,
    n_tup_upd BIGINT,
    n_tup_del BIGINT,
    n_tup_hot_upd BIGINT,
    n_live_tup BIGINT,
    n_dead_tup BIGINT,
    last_vacuum TIMESTAMP WITH TIME ZONE,
    last_autovacuum TIMESTAMP WITH TIME ZONE,
    last_analyze TIMESTAMP WITH TIME ZONE,
    last_autoanalyze TIMESTAMP WITH TIME ZONE,
    vacuum_count BIGINT,
    autovacuum_count BIGINT,
    analyze_count BIGINT,
    autoanalyze_count BIGINT
);
CREATE TABLE pg_stat_user_functions
(
    funcid OID,
    schemaname NAME,
    funcname NAME,
    calls BIGINT,
    total_time DOUBLE PRECISION,
    self_time DOUBLE PRECISION
);
CREATE TABLE pg_stat_user_indexes
(
    relid OID,
    indexrelid OID,
    schemaname NAME,
    relname NAME,
    indexrelname NAME,
    idx_scan BIGINT,
    idx_tup_read BIGINT,
    idx_tup_fetch BIGINT
);
CREATE TABLE pg_stat_user_tables
(
    relid OID,
    schemaname NAME,
    relname NAME,
    seq_scan BIGINT,
    seq_tup_read BIGINT,
    idx_scan BIGINT,
    idx_tup_fetch BIGINT,
    n_tup_ins BIGINT,
    n_tup_upd BIGINT,
    n_tup_del BIGINT,
    n_tup_hot_upd BIGINT,
    n_live_tup BIGINT,
    n_dead_tup BIGINT,
    last_vacuum TIMESTAMP WITH TIME ZONE,
    last_autovacuum TIMESTAMP WITH TIME ZONE,
    last_analyze TIMESTAMP WITH TIME ZONE,
    last_autoanalyze TIMESTAMP WITH TIME ZONE,
    vacuum_count BIGINT,
    autovacuum_count BIGINT,
    analyze_count BIGINT,
    autoanalyze_count BIGINT
);
CREATE TABLE pg_stat_xact_all_tables
(
    relid OID,
    schemaname NAME,
    relname NAME,
    seq_scan BIGINT,
    seq_tup_read BIGINT,
    idx_scan BIGINT,
    idx_tup_fetch BIGINT,
    n_tup_ins BIGINT,
    n_tup_upd BIGINT,
    n_tup_del BIGINT,
    n_tup_hot_upd BIGINT
);
CREATE TABLE pg_stat_xact_sys_tables
(
    relid OID,
    schemaname NAME,
    relname NAME,
    seq_scan BIGINT,
    seq_tup_read BIGINT,
    idx_scan BIGINT,
    idx_tup_fetch BIGINT,
    n_tup_ins BIGINT,
    n_tup_upd BIGINT,
    n_tup_del BIGINT,
    n_tup_hot_upd BIGINT
);
CREATE TABLE pg_stat_xact_user_functions
(
    funcid OID,
    schemaname NAME,
    funcname NAME,
    calls BIGINT,
    total_time DOUBLE PRECISION,
    self_time DOUBLE PRECISION
);
CREATE TABLE pg_stat_xact_user_tables
(
    relid OID,
    schemaname NAME,
    relname NAME,
    seq_scan BIGINT,
    seq_tup_read BIGINT,
    idx_scan BIGINT,
    idx_tup_fetch BIGINT,
    n_tup_ins BIGINT,
    n_tup_upd BIGINT,
    n_tup_del BIGINT,
    n_tup_hot_upd BIGINT
);
CREATE TABLE pg_statio_all_indexes
(
    relid OID,
    indexrelid OID,
    schemaname NAME,
    relname NAME,
    indexrelname NAME,
    idx_blks_read BIGINT,
    idx_blks_hit BIGINT
);
CREATE TABLE pg_statio_all_sequences
(
    relid OID,
    schemaname NAME,
    relname NAME,
    blks_read BIGINT,
    blks_hit BIGINT
);
CREATE TABLE pg_statio_all_tables
(
    relid OID,
    schemaname NAME,
    relname NAME,
    heap_blks_read BIGINT,
    heap_blks_hit BIGINT,
    idx_blks_read BIGINT,
    idx_blks_hit BIGINT,
    toast_blks_read BIGINT,
    toast_blks_hit BIGINT,
    tidx_blks_read BIGINT,
    tidx_blks_hit BIGINT
);
CREATE TABLE pg_statio_sys_indexes
(
    relid OID,
    indexrelid OID,
    schemaname NAME,
    relname NAME,
    indexrelname NAME,
    idx_blks_read BIGINT,
    idx_blks_hit BIGINT
);
CREATE TABLE pg_statio_sys_sequences
(
    relid OID,
    schemaname NAME,
    relname NAME,
    blks_read BIGINT,
    blks_hit BIGINT
);
CREATE TABLE pg_statio_sys_tables
(
    relid OID,
    schemaname NAME,
    relname NAME,
    heap_blks_read BIGINT,
    heap_blks_hit BIGINT,
    idx_blks_read BIGINT,
    idx_blks_hit BIGINT,
    toast_blks_read BIGINT,
    toast_blks_hit BIGINT,
    tidx_blks_read BIGINT,
    tidx_blks_hit BIGINT
);
CREATE TABLE pg_statio_user_indexes
(
    relid OID,
    indexrelid OID,
    schemaname NAME,
    relname NAME,
    indexrelname NAME,
    idx_blks_read BIGINT,
    idx_blks_hit BIGINT
);
CREATE TABLE pg_statio_user_sequences
(
    relid OID,
    schemaname NAME,
    relname NAME,
    blks_read BIGINT,
    blks_hit BIGINT
);
CREATE TABLE pg_statio_user_tables
(
    relid OID,
    schemaname NAME,
    relname NAME,
    heap_blks_read BIGINT,
    heap_blks_hit BIGINT,
    idx_blks_read BIGINT,
    idx_blks_hit BIGINT,
    toast_blks_read BIGINT,
    toast_blks_hit BIGINT,
    tidx_blks_read BIGINT,
    tidx_blks_hit BIGINT
);
CREATE TABLE pg_stats
(
    schemaname NAME,
    tablename NAME,
    attname NAME,
    inherited BOOLEAN,
    null_frac REAL,
    avg_width INTEGER,
    n_distinct REAL,
    most_common_vals ANYARRAY,
    most_common_freqs REAL[],
    histogram_bounds ANYARRAY,
    correlation REAL,
    most_common_elems ANYARRAY,
    most_common_elem_freqs REAL[],
    elem_count_histogram REAL[]
);
CREATE TABLE pg_tables
(
    schemaname NAME,
    tablename NAME,
    tableowner NAME,
    tablespace NAME,
    hasindexes BOOLEAN,
    hasrules BOOLEAN,
    hastriggers BOOLEAN
);
CREATE TABLE pg_timezone_abbrevs
(
    abbrev TEXT,
    utc_offset INTERVAL,
    is_dst BOOLEAN
);
CREATE TABLE pg_timezone_names
(
    name TEXT,
    abbrev TEXT,
    utc_offset INTERVAL,
    is_dst BOOLEAN
);
CREATE TABLE pg_user
(
    usename NAME,
    usesysid OID,
    usecreatedb BOOLEAN,
    usesuper BOOLEAN,
    usecatupd BOOLEAN,
    userepl BOOLEAN,
    passwd TEXT,
    valuntil ABSTIME,
    useconfig TEXT[]
);
CREATE TABLE pg_user_mappings
(
    umid OID,
    srvid OID,
    srvname NAME,
    umuser OID,
    usename NAME,
    umoptions TEXT[]
);
CREATE TABLE pg_views
(
    schemaname NAME,
    viewname NAME,
    viewowner NAME,
    definition TEXT
);
CREATE FUNCTION "RI_FKey_cascade_del"() RETURNS TRIGGER;
CREATE FUNCTION "RI_FKey_cascade_upd"() RETURNS TRIGGER;
CREATE FUNCTION "RI_FKey_check_ins"() RETURNS TRIGGER;
CREATE FUNCTION "RI_FKey_check_upd"() RETURNS TRIGGER;
CREATE FUNCTION "RI_FKey_noaction_del"() RETURNS TRIGGER;
CREATE FUNCTION "RI_FKey_noaction_upd"() RETURNS TRIGGER;
CREATE FUNCTION "RI_FKey_restrict_del"() RETURNS TRIGGER;
CREATE FUNCTION "RI_FKey_restrict_upd"() RETURNS TRIGGER;
CREATE FUNCTION "RI_FKey_setdefault_del"() RETURNS TRIGGER;
CREATE FUNCTION "RI_FKey_setdefault_upd"() RETURNS TRIGGER;
CREATE FUNCTION "RI_FKey_setnull_del"() RETURNS TRIGGER;
CREATE FUNCTION "RI_FKey_setnull_upd"() RETURNS TRIGGER;
CREATE FUNCTION abbrev($1 CIDR) RETURNS TEXT;
CREATE FUNCTION abbrev($1 INET) RETURNS TEXT;
CREATE FUNCTION abs($1 BIGINT) RETURNS BIGINT;
CREATE FUNCTION abs($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION abs($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION abs($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION abs($1 REAL) RETURNS REAL;
CREATE FUNCTION abs($1 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION abstimeeq($1 ABSTIME, $2 ABSTIME) RETURNS BOOLEAN;
CREATE FUNCTION abstimege($1 ABSTIME, $2 ABSTIME) RETURNS BOOLEAN;
CREATE FUNCTION abstimegt($1 ABSTIME, $2 ABSTIME) RETURNS BOOLEAN;
CREATE FUNCTION abstimein($1 CSTRING) RETURNS ABSTIME;
CREATE FUNCTION abstimele($1 ABSTIME, $2 ABSTIME) RETURNS BOOLEAN;
CREATE FUNCTION abstimelt($1 ABSTIME, $2 ABSTIME) RETURNS BOOLEAN;
CREATE FUNCTION abstimene($1 ABSTIME, $2 ABSTIME) RETURNS BOOLEAN;
CREATE FUNCTION abstimeout($1 ABSTIME) RETURNS CSTRING;
CREATE FUNCTION abstimerecv($1 INTERNAL) RETURNS ABSTIME;
CREATE FUNCTION abstimesend($1 ABSTIME) RETURNS BYTEA;
CREATE FUNCTION abstime($1 TIMESTAMP) RETURNS ABSTIME;
CREATE FUNCTION abstime($1 TIMESTAMP WITH TIME ZONE) RETURNS ABSTIME;
CREATE FUNCTION aclcontains($1 ACLITEM[], $2 ACLITEM) RETURNS BOOLEAN;
CREATE FUNCTION acldefault($1 "char", $2 OID) RETURNS ACLITEM[];
CREATE FUNCTION aclexplode(acl ACLITEM[], grantor OUT OID, grantee OUT OID, privilege_type OUT TEXT, is_grantable OUT BOOLEAN) RETURNS SETOF RECORD;
CREATE FUNCTION aclinsert($1 ACLITEM[], $2 ACLITEM) RETURNS ACLITEM[];
CREATE FUNCTION aclitemeq($1 ACLITEM, $2 ACLITEM) RETURNS BOOLEAN;
CREATE FUNCTION aclitemin($1 CSTRING) RETURNS ACLITEM;
CREATE FUNCTION aclitemout($1 ACLITEM) RETURNS CSTRING;
CREATE FUNCTION aclremove($1 ACLITEM[], $2 ACLITEM) RETURNS ACLITEM[];
CREATE FUNCTION acos($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION age($1 TIMESTAMP) RETURNS INTERVAL;
CREATE FUNCTION age($1 TIMESTAMP WITH TIME ZONE) RETURNS INTERVAL;
CREATE FUNCTION age($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS INTERVAL;
CREATE FUNCTION age($1 TIMESTAMP, $2 TIMESTAMP) RETURNS INTERVAL;
CREATE FUNCTION age($1 XID) RETURNS INTEGER;
CREATE FUNCTION any_in($1 CSTRING) RETURNS "any";
CREATE FUNCTION any_out($1 "any") RETURNS CSTRING;
CREATE FUNCTION anyarray_in($1 CSTRING) RETURNS ANYARRAY;
CREATE FUNCTION anyarray_out($1 ANYARRAY) RETURNS CSTRING;
CREATE FUNCTION anyarray_recv($1 INTERNAL) RETURNS ANYARRAY;
CREATE FUNCTION anyarray_send($1 ANYARRAY) RETURNS BYTEA;
CREATE FUNCTION anyelement_in($1 CSTRING) RETURNS ANYELEMENT;
CREATE FUNCTION anyelement_out($1 ANYELEMENT) RETURNS CSTRING;
CREATE FUNCTION anyenum_in($1 CSTRING) RETURNS ANYENUM;
CREATE FUNCTION anyenum_out($1 ANYENUM) RETURNS CSTRING;
CREATE FUNCTION anynonarray_in($1 CSTRING) RETURNS ANYNONARRAY;
CREATE FUNCTION anynonarray_out($1 ANYNONARRAY) RETURNS CSTRING;
CREATE FUNCTION anyrange_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS ANYRANGE;
CREATE FUNCTION anyrange_out($1 ANYRANGE) RETURNS CSTRING;
CREATE FUNCTION anytextcat($1 ANYNONARRAY, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION area($1 BOX) RETURNS DOUBLE PRECISION;
CREATE FUNCTION area($1 CIRCLE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION areajoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION area($1 PATH) RETURNS DOUBLE PRECISION;
CREATE FUNCTION areasel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION array_agg_finalfn($1 INTERNAL) RETURNS ANYARRAY;
CREATE FUNCTION array_agg_transfn($1 INTERNAL, $2 ANYELEMENT) RETURNS INTERNAL;
CREATE FUNCTION array_agg($1 ANYELEMENT) RETURNS ANYARRAY;
CREATE FUNCTION array_append($1 ANYARRAY, $2 ANYELEMENT) RETURNS ANYARRAY;
CREATE FUNCTION array_cat($1 ANYARRAY, $2 ANYARRAY) RETURNS ANYARRAY;
CREATE FUNCTION array_dims($1 ANYARRAY) RETURNS TEXT;
CREATE FUNCTION array_eq($1 ANYARRAY, $2 ANYARRAY) RETURNS BOOLEAN;
CREATE FUNCTION array_fill($1 ANYELEMENT, $2 INTEGER[]) RETURNS ANYARRAY;
CREATE FUNCTION array_fill($1 ANYELEMENT, $2 INTEGER[], $3 INTEGER[]) RETURNS ANYARRAY;
CREATE FUNCTION array_ge($1 ANYARRAY, $2 ANYARRAY) RETURNS BOOLEAN;
CREATE FUNCTION array_gt($1 ANYARRAY, $2 ANYARRAY) RETURNS BOOLEAN;
CREATE FUNCTION array_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS ANYARRAY;
CREATE FUNCTION array_larger($1 ANYARRAY, $2 ANYARRAY) RETURNS ANYARRAY;
CREATE FUNCTION array_le($1 ANYARRAY, $2 ANYARRAY) RETURNS BOOLEAN;
CREATE FUNCTION array_length($1 ANYARRAY, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION array_lower($1 ANYARRAY, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION array_lt($1 ANYARRAY, $2 ANYARRAY) RETURNS BOOLEAN;
CREATE FUNCTION array_ndims($1 ANYARRAY) RETURNS INTEGER;
CREATE FUNCTION array_ne($1 ANYARRAY, $2 ANYARRAY) RETURNS BOOLEAN;
CREATE FUNCTION array_out($1 ANYARRAY) RETURNS CSTRING;
CREATE FUNCTION array_prepend($1 ANYELEMENT, $2 ANYARRAY) RETURNS ANYARRAY;
CREATE FUNCTION array_recv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS ANYARRAY;
CREATE FUNCTION array_remove($1 ANYARRAY, $2 ANYELEMENT) RETURNS ANYARRAY;
CREATE FUNCTION array_replace($1 ANYARRAY, $2 ANYELEMENT, $3 ANYELEMENT) RETURNS ANYARRAY;
CREATE FUNCTION array_send($1 ANYARRAY) RETURNS BYTEA;
CREATE FUNCTION array_smaller($1 ANYARRAY, $2 ANYARRAY) RETURNS ANYARRAY;
CREATE FUNCTION array_to_json($1 ANYARRAY) RETURNS JSON;
CREATE FUNCTION array_to_json($1 ANYARRAY, $2 BOOLEAN) RETURNS JSON;
CREATE FUNCTION array_to_string($1 ANYARRAY, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION array_to_string($1 ANYARRAY, $2 TEXT, $3 TEXT) RETURNS TEXT;
CREATE FUNCTION array_typanalyze($1 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION array_upper($1 ANYARRAY, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION arraycontained($1 ANYARRAY, $2 ANYARRAY) RETURNS BOOLEAN;
CREATE FUNCTION arraycontains($1 ANYARRAY, $2 ANYARRAY) RETURNS BOOLEAN;
CREATE FUNCTION arraycontjoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION arraycontsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION arrayoverlap($1 ANYARRAY, $2 ANYARRAY) RETURNS BOOLEAN;
CREATE FUNCTION ascii_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION ascii_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION ascii($1 TEXT) RETURNS INTEGER;
CREATE FUNCTION asin($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION atan2($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION atan($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION avg($1 BIGINT) RETURNS NUMERIC;
CREATE FUNCTION avg($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION avg($1 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION avg($1 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION avg($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION avg($1 REAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION avg($1 SMALLINT) RETURNS NUMERIC;
CREATE FUNCTION big5_to_euc_tw($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION big5_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION big5_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION bit_and($1 BIGINT) RETURNS BIGINT;
CREATE FUNCTION bit_and($1 BIT) RETURNS BIT;
CREATE FUNCTION bit_and($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION bit_and($1 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION bit_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS BIT;
CREATE FUNCTION bit_length($1 BIT) RETURNS INTEGER;
CREATE FUNCTION bit_length($1 BYTEA) RETURNS INTEGER;
CREATE FUNCTION bit_length($1 TEXT) RETURNS INTEGER;
CREATE FUNCTION bit_or($1 BIGINT) RETURNS BIGINT;
CREATE FUNCTION bit_or($1 BIT) RETURNS BIT;
CREATE FUNCTION bit_or($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION bit_or($1 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION bit_out($1 BIT) RETURNS CSTRING;
CREATE FUNCTION bit_recv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS BIT;
CREATE FUNCTION bit_send($1 BIT) RETURNS BYTEA;
CREATE FUNCTION bitand($1 BIT, $2 BIT) RETURNS BIT;
CREATE FUNCTION bit($1 BIGINT, $2 INTEGER) RETURNS BIT;
CREATE FUNCTION bit($1 BIT, $2 INTEGER, $3 BOOLEAN) RETURNS BIT;
CREATE FUNCTION bitcat($1 BIT VARYING, $2 BIT VARYING) RETURNS BIT VARYING;
CREATE FUNCTION bitcmp($1 BIT, $2 BIT) RETURNS INTEGER;
CREATE FUNCTION biteq($1 BIT, $2 BIT) RETURNS BOOLEAN;
CREATE FUNCTION bitge($1 BIT, $2 BIT) RETURNS BOOLEAN;
CREATE FUNCTION bitgt($1 BIT, $2 BIT) RETURNS BOOLEAN;
CREATE FUNCTION bit($1 INTEGER, $2 INTEGER) RETURNS BIT;
CREATE FUNCTION bitle($1 BIT, $2 BIT) RETURNS BOOLEAN;
CREATE FUNCTION bitlt($1 BIT, $2 BIT) RETURNS BOOLEAN;
CREATE FUNCTION bitne($1 BIT, $2 BIT) RETURNS BOOLEAN;
CREATE FUNCTION bitnot($1 BIT) RETURNS BIT;
CREATE FUNCTION bitor($1 BIT, $2 BIT) RETURNS BIT;
CREATE FUNCTION bitshiftleft($1 BIT, $2 INTEGER) RETURNS BIT;
CREATE FUNCTION bitshiftright($1 BIT, $2 INTEGER) RETURNS BIT;
CREATE FUNCTION bittypmodin($1 CSTRING[]) RETURNS INTEGER;
CREATE FUNCTION bittypmodout($1 INTEGER) RETURNS CSTRING;
CREATE FUNCTION bitxor($1 BIT, $2 BIT) RETURNS BIT;
CREATE FUNCTION bool_and($1 BOOLEAN) RETURNS BOOLEAN;
CREATE FUNCTION bool_or($1 BOOLEAN) RETURNS BOOLEAN;
CREATE FUNCTION booland_statefunc($1 BOOLEAN, $2 BOOLEAN) RETURNS BOOLEAN;
CREATE FUNCTION booleq($1 BOOLEAN, $2 BOOLEAN) RETURNS BOOLEAN;
CREATE FUNCTION boolge($1 BOOLEAN, $2 BOOLEAN) RETURNS BOOLEAN;
CREATE FUNCTION boolgt($1 BOOLEAN, $2 BOOLEAN) RETURNS BOOLEAN;
CREATE FUNCTION boolin($1 CSTRING) RETURNS BOOLEAN;
CREATE FUNCTION bool($1 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION boolle($1 BOOLEAN, $2 BOOLEAN) RETURNS BOOLEAN;
CREATE FUNCTION boollt($1 BOOLEAN, $2 BOOLEAN) RETURNS BOOLEAN;
CREATE FUNCTION boolne($1 BOOLEAN, $2 BOOLEAN) RETURNS BOOLEAN;
CREATE FUNCTION boolor_statefunc($1 BOOLEAN, $2 BOOLEAN) RETURNS BOOLEAN;
CREATE FUNCTION boolout($1 BOOLEAN) RETURNS CSTRING;
CREATE FUNCTION boolrecv($1 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION boolsend($1 BOOLEAN) RETURNS BYTEA;
CREATE FUNCTION box_above_eq($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_above($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_add($1 BOX, $2 POINT) RETURNS BOX;
CREATE FUNCTION box_below_eq($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_below($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_center($1 BOX) RETURNS POINT;
CREATE FUNCTION box_contain_pt($1 BOX, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION box_contain($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_contained($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_distance($1 BOX, $2 BOX) RETURNS DOUBLE PRECISION;
CREATE FUNCTION box_div($1 BOX, $2 POINT) RETURNS BOX;
CREATE FUNCTION box_eq($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_ge($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_gt($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_in($1 CSTRING) RETURNS BOX;
CREATE FUNCTION box_intersect($1 BOX, $2 BOX) RETURNS BOX;
CREATE FUNCTION box_le($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_left($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_lt($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_mul($1 BOX, $2 POINT) RETURNS BOX;
CREATE FUNCTION box_out($1 BOX) RETURNS CSTRING;
CREATE FUNCTION box_overabove($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_overbelow($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_overlap($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_overleft($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_overright($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_recv($1 INTERNAL) RETURNS BOX;
CREATE FUNCTION box_right($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_same($1 BOX, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION box_send($1 BOX) RETURNS BYTEA;
CREATE FUNCTION box_sub($1 BOX, $2 POINT) RETURNS BOX;
CREATE FUNCTION box($1 CIRCLE) RETURNS BOX;
CREATE FUNCTION box($1 POINT, $2 POINT) RETURNS BOX;
CREATE FUNCTION box($1 POLYGON) RETURNS BOX;
CREATE FUNCTION bpchar($1 "char") RETURNS CHAR;
CREATE FUNCTION bpchar_larger($1 CHAR, $2 CHAR) RETURNS CHAR;
CREATE FUNCTION bpchar_pattern_ge($1 CHAR, $2 CHAR) RETURNS BOOLEAN;
CREATE FUNCTION bpchar_pattern_gt($1 CHAR, $2 CHAR) RETURNS BOOLEAN;
CREATE FUNCTION bpchar_pattern_le($1 CHAR, $2 CHAR) RETURNS BOOLEAN;
CREATE FUNCTION bpchar_pattern_lt($1 CHAR, $2 CHAR) RETURNS BOOLEAN;
CREATE FUNCTION bpchar_smaller($1 CHAR, $2 CHAR) RETURNS CHAR;
CREATE FUNCTION bpchar($1 CHAR, $2 INTEGER, $3 BOOLEAN) RETURNS CHAR;
CREATE FUNCTION bpcharcmp($1 CHAR, $2 CHAR) RETURNS INTEGER;
CREATE FUNCTION bpchareq($1 CHAR, $2 CHAR) RETURNS BOOLEAN;
CREATE FUNCTION bpcharge($1 CHAR, $2 CHAR) RETURNS BOOLEAN;
CREATE FUNCTION bpchargt($1 CHAR, $2 CHAR) RETURNS BOOLEAN;
CREATE FUNCTION bpchariclike($1 CHAR, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION bpcharicnlike($1 CHAR, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION bpcharicregexeq($1 CHAR, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION bpcharicregexne($1 CHAR, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION bpcharin($1 CSTRING, $2 OID, $3 INTEGER) RETURNS CHAR;
CREATE FUNCTION bpcharle($1 CHAR, $2 CHAR) RETURNS BOOLEAN;
CREATE FUNCTION bpcharlike($1 CHAR, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION bpcharlt($1 CHAR, $2 CHAR) RETURNS BOOLEAN;
CREATE FUNCTION bpchar($1 NAME) RETURNS CHAR;
CREATE FUNCTION bpcharne($1 CHAR, $2 CHAR) RETURNS BOOLEAN;
CREATE FUNCTION bpcharnlike($1 CHAR, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION bpcharout($1 CHAR) RETURNS CSTRING;
CREATE FUNCTION bpcharrecv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS CHAR;
CREATE FUNCTION bpcharregexeq($1 CHAR, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION bpcharregexne($1 CHAR, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION bpcharsend($1 CHAR) RETURNS BYTEA;
CREATE FUNCTION bpchartypmodin($1 CSTRING[]) RETURNS INTEGER;
CREATE FUNCTION bpchartypmodout($1 INTEGER) RETURNS CSTRING;
CREATE FUNCTION broadcast($1 INET) RETURNS INET;
CREATE FUNCTION btabstimecmp($1 ABSTIME, $2 ABSTIME) RETURNS INTEGER;
CREATE FUNCTION btarraycmp($1 ANYARRAY, $2 ANYARRAY) RETURNS INTEGER;
CREATE FUNCTION btbeginscan($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION btboolcmp($1 BOOLEAN, $2 BOOLEAN) RETURNS INTEGER;
CREATE FUNCTION btbpchar_pattern_cmp($1 CHAR, $2 CHAR) RETURNS INTEGER;
CREATE FUNCTION btbuildempty($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION btbuild($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION btbulkdelete($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION btcanreturn($1 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION btcharcmp($1 "char", $2 "char") RETURNS INTEGER;
CREATE FUNCTION btcostestimate($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL, $6 INTERNAL, $7 INTERNAL) RETURNS VOID;
CREATE FUNCTION btendscan($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION btfloat48cmp($1 REAL, $2 DOUBLE PRECISION) RETURNS INTEGER;
CREATE FUNCTION btfloat4cmp($1 REAL, $2 REAL) RETURNS INTEGER;
CREATE FUNCTION btfloat4sortsupport($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION btfloat84cmp($1 DOUBLE PRECISION, $2 REAL) RETURNS INTEGER;
CREATE FUNCTION btfloat8cmp($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS INTEGER;
CREATE FUNCTION btfloat8sortsupport($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION btgetbitmap($1 INTERNAL, $2 INTERNAL) RETURNS BIGINT;
CREATE FUNCTION btgettuple($1 INTERNAL, $2 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION btinsert($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL, $6 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION btint24cmp($1 SMALLINT, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION btint28cmp($1 SMALLINT, $2 BIGINT) RETURNS INTEGER;
CREATE FUNCTION btint2cmp($1 SMALLINT, $2 SMALLINT) RETURNS INTEGER;
CREATE FUNCTION btint2sortsupport($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION btint42cmp($1 INTEGER, $2 SMALLINT) RETURNS INTEGER;
CREATE FUNCTION btint48cmp($1 INTEGER, $2 BIGINT) RETURNS INTEGER;
CREATE FUNCTION btint4cmp($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION btint4sortsupport($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION btint82cmp($1 BIGINT, $2 SMALLINT) RETURNS INTEGER;
CREATE FUNCTION btint84cmp($1 BIGINT, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION btint8cmp($1 BIGINT, $2 BIGINT) RETURNS INTEGER;
CREATE FUNCTION btint8sortsupport($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION btmarkpos($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION btnamecmp($1 NAME, $2 NAME) RETURNS INTEGER;
CREATE FUNCTION btnamesortsupport($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION btoidcmp($1 OID, $2 OID) RETURNS INTEGER;
CREATE FUNCTION btoidsortsupport($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION btoidvectorcmp($1 OIDVECTOR, $2 OIDVECTOR) RETURNS INTEGER;
CREATE FUNCTION btoptions($1 TEXT[], $2 BOOLEAN) RETURNS BYTEA;
CREATE FUNCTION btrecordcmp($1 RECORD, $2 RECORD) RETURNS INTEGER;
CREATE FUNCTION btreltimecmp($1 RELTIME, $2 RELTIME) RETURNS INTEGER;
CREATE FUNCTION btrescan($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL) RETURNS VOID;
CREATE FUNCTION btrestrpos($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION btrim($1 BYTEA, $2 BYTEA) RETURNS BYTEA;
CREATE FUNCTION btrim($1 TEXT) RETURNS TEXT;
CREATE FUNCTION btrim($1 TEXT, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION bttext_pattern_cmp($1 TEXT, $2 TEXT) RETURNS INTEGER;
CREATE FUNCTION bttextcmp($1 TEXT, $2 TEXT) RETURNS INTEGER;
CREATE FUNCTION bttidcmp($1 TID, $2 TID) RETURNS INTEGER;
CREATE FUNCTION bttintervalcmp($1 TINTERVAL, $2 TINTERVAL) RETURNS INTEGER;
CREATE FUNCTION btvacuumcleanup($1 INTERNAL, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION bytea_string_agg_finalfn($1 INTERNAL) RETURNS BYTEA;
CREATE FUNCTION bytea_string_agg_transfn($1 INTERNAL, $2 BYTEA, $3 BYTEA) RETURNS INTERNAL;
CREATE FUNCTION byteacat($1 BYTEA, $2 BYTEA) RETURNS BYTEA;
CREATE FUNCTION byteacmp($1 BYTEA, $2 BYTEA) RETURNS INTEGER;
CREATE FUNCTION byteaeq($1 BYTEA, $2 BYTEA) RETURNS BOOLEAN;
CREATE FUNCTION byteage($1 BYTEA, $2 BYTEA) RETURNS BOOLEAN;
CREATE FUNCTION byteagt($1 BYTEA, $2 BYTEA) RETURNS BOOLEAN;
CREATE FUNCTION byteain($1 CSTRING) RETURNS BYTEA;
CREATE FUNCTION byteale($1 BYTEA, $2 BYTEA) RETURNS BOOLEAN;
CREATE FUNCTION bytealike($1 BYTEA, $2 BYTEA) RETURNS BOOLEAN;
CREATE FUNCTION bytealt($1 BYTEA, $2 BYTEA) RETURNS BOOLEAN;
CREATE FUNCTION byteane($1 BYTEA, $2 BYTEA) RETURNS BOOLEAN;
CREATE FUNCTION byteanlike($1 BYTEA, $2 BYTEA) RETURNS BOOLEAN;
CREATE FUNCTION byteaout($1 BYTEA) RETURNS CSTRING;
CREATE FUNCTION bytearecv($1 INTERNAL) RETURNS BYTEA;
CREATE FUNCTION byteasend($1 BYTEA) RETURNS BYTEA;
CREATE FUNCTION cash_cmp($1 MONEY, $2 MONEY) RETURNS INTEGER;
CREATE FUNCTION cash_div_cash($1 MONEY, $2 MONEY) RETURNS DOUBLE PRECISION;
CREATE FUNCTION cash_div_flt4($1 MONEY, $2 REAL) RETURNS MONEY;
CREATE FUNCTION cash_div_flt8($1 MONEY, $2 DOUBLE PRECISION) RETURNS MONEY;
CREATE FUNCTION cash_div_int2($1 MONEY, $2 SMALLINT) RETURNS MONEY;
CREATE FUNCTION cash_div_int4($1 MONEY, $2 INTEGER) RETURNS MONEY;
CREATE FUNCTION cash_eq($1 MONEY, $2 MONEY) RETURNS BOOLEAN;
CREATE FUNCTION cash_ge($1 MONEY, $2 MONEY) RETURNS BOOLEAN;
CREATE FUNCTION cash_gt($1 MONEY, $2 MONEY) RETURNS BOOLEAN;
CREATE FUNCTION cash_in($1 CSTRING) RETURNS MONEY;
CREATE FUNCTION cash_le($1 MONEY, $2 MONEY) RETURNS BOOLEAN;
CREATE FUNCTION cash_lt($1 MONEY, $2 MONEY) RETURNS BOOLEAN;
CREATE FUNCTION cash_mi($1 MONEY, $2 MONEY) RETURNS MONEY;
CREATE FUNCTION cash_mul_flt4($1 MONEY, $2 REAL) RETURNS MONEY;
CREATE FUNCTION cash_mul_flt8($1 MONEY, $2 DOUBLE PRECISION) RETURNS MONEY;
CREATE FUNCTION cash_mul_int2($1 MONEY, $2 SMALLINT) RETURNS MONEY;
CREATE FUNCTION cash_mul_int4($1 MONEY, $2 INTEGER) RETURNS MONEY;
CREATE FUNCTION cash_ne($1 MONEY, $2 MONEY) RETURNS BOOLEAN;
CREATE FUNCTION cash_out($1 MONEY) RETURNS CSTRING;
CREATE FUNCTION cash_pl($1 MONEY, $2 MONEY) RETURNS MONEY;
CREATE FUNCTION cash_recv($1 INTERNAL) RETURNS MONEY;
CREATE FUNCTION cash_send($1 MONEY) RETURNS BYTEA;
CREATE FUNCTION cash_words($1 MONEY) RETURNS TEXT;
CREATE FUNCTION cashlarger($1 MONEY, $2 MONEY) RETURNS MONEY;
CREATE FUNCTION cashsmaller($1 MONEY, $2 MONEY) RETURNS MONEY;
CREATE FUNCTION cbrt($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION ceil($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION ceiling($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION ceiling($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION ceil($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION center($1 BOX) RETURNS POINT;
CREATE FUNCTION center($1 CIRCLE) RETURNS POINT;
CREATE FUNCTION char_length($1 CHAR) RETURNS INTEGER;
CREATE FUNCTION char_length($1 TEXT) RETURNS INTEGER;
CREATE FUNCTION character_length($1 CHAR) RETURNS INTEGER;
CREATE FUNCTION character_length($1 TEXT) RETURNS INTEGER;
CREATE FUNCTION chareq($1 "char", $2 "char") RETURNS BOOLEAN;
CREATE FUNCTION charge($1 "char", $2 "char") RETURNS BOOLEAN;
CREATE FUNCTION chargt($1 "char", $2 "char") RETURNS BOOLEAN;
CREATE FUNCTION charin($1 CSTRING) RETURNS "char";
CREATE FUNCTION char($1 INTEGER) RETURNS "char";
CREATE FUNCTION charle($1 "char", $2 "char") RETURNS BOOLEAN;
CREATE FUNCTION charlt($1 "char", $2 "char") RETURNS BOOLEAN;
CREATE FUNCTION charne($1 "char", $2 "char") RETURNS BOOLEAN;
CREATE FUNCTION charout($1 "char") RETURNS CSTRING;
CREATE FUNCTION charrecv($1 INTERNAL) RETURNS "char";
CREATE FUNCTION charsend($1 "char") RETURNS BYTEA;
CREATE FUNCTION char($1 TEXT) RETURNS "char";
CREATE FUNCTION chr($1 INTEGER) RETURNS TEXT;
CREATE FUNCTION cideq($1 CID, $2 CID) RETURNS BOOLEAN;
CREATE FUNCTION cidin($1 CSTRING) RETURNS CID;
CREATE FUNCTION cidout($1 CID) RETURNS CSTRING;
CREATE FUNCTION cidr_in($1 CSTRING) RETURNS CIDR;
CREATE FUNCTION cidr_out($1 CIDR) RETURNS CSTRING;
CREATE FUNCTION cidr_recv($1 INTERNAL) RETURNS CIDR;
CREATE FUNCTION cidr_send($1 CIDR) RETURNS BYTEA;
CREATE FUNCTION cidrecv($1 INTERNAL) RETURNS CID;
CREATE FUNCTION cidr($1 INET) RETURNS CIDR;
CREATE FUNCTION cidsend($1 CID) RETURNS BYTEA;
CREATE FUNCTION circle_above($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_add_pt($1 CIRCLE, $2 POINT) RETURNS CIRCLE;
CREATE FUNCTION circle_below($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_center($1 CIRCLE) RETURNS POINT;
CREATE FUNCTION circle_contain_pt($1 CIRCLE, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION circle_contain($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_contained($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_distance($1 CIRCLE, $2 CIRCLE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION circle_div_pt($1 CIRCLE, $2 POINT) RETURNS CIRCLE;
CREATE FUNCTION circle_eq($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_ge($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_gt($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_in($1 CSTRING) RETURNS CIRCLE;
CREATE FUNCTION circle_le($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_left($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_lt($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_mul_pt($1 CIRCLE, $2 POINT) RETURNS CIRCLE;
CREATE FUNCTION circle_ne($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_out($1 CIRCLE) RETURNS CSTRING;
CREATE FUNCTION circle_overabove($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_overbelow($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_overlap($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_overleft($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_overright($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_recv($1 INTERNAL) RETURNS CIRCLE;
CREATE FUNCTION circle_right($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_same($1 CIRCLE, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION circle_send($1 CIRCLE) RETURNS BYTEA;
CREATE FUNCTION circle_sub_pt($1 CIRCLE, $2 POINT) RETURNS CIRCLE;
CREATE FUNCTION circle($1 BOX) RETURNS CIRCLE;
CREATE FUNCTION circle($1 POINT, $2 DOUBLE PRECISION) RETURNS CIRCLE;
CREATE FUNCTION circle($1 POLYGON) RETURNS CIRCLE;
CREATE FUNCTION clock_timestamp() RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION close_lb($1 LINE, $2 BOX) RETURNS POINT;
CREATE FUNCTION close_lseg($1 LSEG, $2 LSEG) RETURNS POINT;
CREATE FUNCTION close_ls($1 LINE, $2 LSEG) RETURNS POINT;
CREATE FUNCTION close_pb($1 POINT, $2 BOX) RETURNS POINT;
CREATE FUNCTION close_pl($1 POINT, $2 LINE) RETURNS POINT;
CREATE FUNCTION close_ps($1 POINT, $2 LSEG) RETURNS POINT;
CREATE FUNCTION close_sb($1 LSEG, $2 BOX) RETURNS POINT;
CREATE FUNCTION close_sl($1 LSEG, $2 LINE) RETURNS POINT;
CREATE FUNCTION col_description($1 OID, $2 INTEGER) RETURNS TEXT;
CREATE FUNCTION concat($1 "any") RETURNS TEXT;
CREATE FUNCTION concat_ws($1 TEXT, $2 "any") RETURNS TEXT;
CREATE FUNCTION contjoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION contsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION convert_from($1 BYTEA, $2 NAME) RETURNS TEXT;
CREATE FUNCTION convert_to($1 TEXT, $2 NAME) RETURNS BYTEA;
CREATE FUNCTION convert($1 BYTEA, $2 NAME, $3 NAME) RETURNS BYTEA;
CREATE FUNCTION corr($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION cos($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION cot($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION count() RETURNS BIGINT;
CREATE FUNCTION count($1 "any") RETURNS BIGINT;
CREATE FUNCTION covar_pop($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION covar_samp($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION cstring_in($1 CSTRING) RETURNS CSTRING;
CREATE FUNCTION cstring_out($1 CSTRING) RETURNS CSTRING;
CREATE FUNCTION cstring_recv($1 INTERNAL) RETURNS CSTRING;
CREATE FUNCTION cstring_send($1 CSTRING) RETURNS BYTEA;
CREATE FUNCTION cume_dist() RETURNS DOUBLE PRECISION;
CREATE FUNCTION current_database() RETURNS NAME;
CREATE FUNCTION current_query() RETURNS TEXT;
CREATE FUNCTION "current_schema"() RETURNS NAME;
CREATE FUNCTION current_schemas($1 BOOLEAN) RETURNS NAME[];
CREATE FUNCTION current_setting($1 TEXT) RETURNS TEXT;
CREATE FUNCTION "current_user"() RETURNS NAME;
CREATE FUNCTION currtid2($1 TEXT, $2 TID) RETURNS TID;
CREATE FUNCTION currtid($1 OID, $2 TID) RETURNS TID;
CREATE FUNCTION currval($1 REGCLASS) RETURNS BIGINT;
CREATE FUNCTION cursor_to_xml(cursor REFCURSOR, count INTEGER, nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION cursor_to_xmlschema(cursor REFCURSOR, nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION database_to_xml_and_xmlschema(nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION database_to_xml(nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION database_to_xmlschema(nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION date_cmp_timestamp($1 DATE, $2 TIMESTAMP) RETURNS INTEGER;
CREATE FUNCTION date_cmp_timestamptz($1 DATE, $2 TIMESTAMP WITH TIME ZONE) RETURNS INTEGER;
CREATE FUNCTION date_cmp($1 DATE, $2 DATE) RETURNS INTEGER;
CREATE FUNCTION date_eq_timestamp($1 DATE, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION date_eq_timestamptz($1 DATE, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION date_eq($1 DATE, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION date_ge_timestamp($1 DATE, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION date_ge_timestamptz($1 DATE, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION date_ge($1 DATE, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION date_gt_timestamp($1 DATE, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION date_gt_timestamptz($1 DATE, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION date_gt($1 DATE, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION date_in($1 CSTRING) RETURNS DATE;
CREATE FUNCTION date_larger($1 DATE, $2 DATE) RETURNS DATE;
CREATE FUNCTION date_le_timestamp($1 DATE, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION date_le_timestamptz($1 DATE, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION date_le($1 DATE, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION date_lt_timestamp($1 DATE, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION date_lt_timestamptz($1 DATE, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION date_lt($1 DATE, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION date_mi_interval($1 DATE, $2 INTERVAL) RETURNS TIMESTAMP;
CREATE FUNCTION date_mi($1 DATE, $2 DATE) RETURNS INTEGER;
CREATE FUNCTION date_mii($1 DATE, $2 INTEGER) RETURNS DATE;
CREATE FUNCTION date_ne_timestamp($1 DATE, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION date_ne_timestamptz($1 DATE, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION date_ne($1 DATE, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION date_out($1 DATE) RETURNS CSTRING;
CREATE FUNCTION date_part($1 TEXT, $2 ABSTIME) RETURNS DOUBLE PRECISION;
CREATE FUNCTION date_part($1 TEXT, $2 DATE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION date_part($1 TEXT, $2 INTERVAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION date_part($1 TEXT, $2 RELTIME) RETURNS DOUBLE PRECISION;
CREATE FUNCTION date_part($1 TEXT, $2 TIME) RETURNS DOUBLE PRECISION;
CREATE FUNCTION date_part($1 TEXT, $2 TIME WITH TIME ZONE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION date_part($1 TEXT, $2 TIMESTAMP) RETURNS DOUBLE PRECISION;
CREATE FUNCTION date_part($1 TEXT, $2 TIMESTAMP WITH TIME ZONE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION date_pl_interval($1 DATE, $2 INTERVAL) RETURNS TIMESTAMP;
CREATE FUNCTION date_pli($1 DATE, $2 INTEGER) RETURNS DATE;
CREATE FUNCTION date_recv($1 INTERNAL) RETURNS DATE;
CREATE FUNCTION date_send($1 DATE) RETURNS BYTEA;
CREATE FUNCTION date_smaller($1 DATE, $2 DATE) RETURNS DATE;
CREATE FUNCTION date_sortsupport($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION date_trunc($1 TEXT, $2 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION date_trunc($1 TEXT, $2 TIMESTAMP) RETURNS TIMESTAMP;
CREATE FUNCTION date_trunc($1 TEXT, $2 TIMESTAMP WITH TIME ZONE) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION date($1 ABSTIME) RETURNS DATE;
CREATE FUNCTION daterange_canonical($1 DATERANGE) RETURNS DATERANGE;
CREATE FUNCTION daterange_subdiff($1 DATE, $2 DATE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION daterange($1 DATE, $2 DATE) RETURNS DATERANGE;
CREATE FUNCTION daterange($1 DATE, $2 DATE, $3 TEXT) RETURNS DATERANGE;
CREATE FUNCTION datetime_pl($1 DATE, $2 TIME) RETURNS TIMESTAMP;
CREATE FUNCTION date($1 TIMESTAMP) RETURNS DATE;
CREATE FUNCTION date($1 TIMESTAMP WITH TIME ZONE) RETURNS DATE;
CREATE FUNCTION datetimetz_pl($1 DATE, $2 TIME WITH TIME ZONE) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION dcbrt($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION decode($1 TEXT, $2 TEXT) RETURNS BYTEA;
CREATE FUNCTION degrees($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dense_rank() RETURNS BIGINT;
CREATE FUNCTION dexp($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION diagonal($1 BOX) RETURNS LSEG;
CREATE FUNCTION diameter($1 CIRCLE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dispell_init($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION dispell_lexize($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION dist_cpoly($1 CIRCLE, $2 POLYGON) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dist_lb($1 LINE, $2 BOX) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dist_pb($1 POINT, $2 BOX) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dist_pc($1 POINT, $2 CIRCLE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dist_pl($1 POINT, $2 LINE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dist_ppath($1 POINT, $2 PATH) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dist_ps($1 POINT, $2 LSEG) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dist_sb($1 LSEG, $2 BOX) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dist_sl($1 LSEG, $2 LINE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION div($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION dlog10($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dlog1($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION domain_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS "any";
CREATE FUNCTION domain_recv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS "any";
CREATE FUNCTION dpow($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dround($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dsimple_init($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION dsimple_lexize($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION dsnowball_init($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION dsnowball_lexize($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION dsqrt($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION dsynonym_init($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION dsynonym_lexize($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION dtrunc($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION elem_contained_by_range($1 ANYELEMENT, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION encode($1 BYTEA, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION enum_cmp($1 ANYENUM, $2 ANYENUM) RETURNS INTEGER;
CREATE FUNCTION enum_eq($1 ANYENUM, $2 ANYENUM) RETURNS BOOLEAN;
CREATE FUNCTION enum_first($1 ANYENUM) RETURNS ANYENUM;
CREATE FUNCTION enum_ge($1 ANYENUM, $2 ANYENUM) RETURNS BOOLEAN;
CREATE FUNCTION enum_gt($1 ANYENUM, $2 ANYENUM) RETURNS BOOLEAN;
CREATE FUNCTION enum_in($1 CSTRING, $2 OID) RETURNS ANYENUM;
CREATE FUNCTION enum_larger($1 ANYENUM, $2 ANYENUM) RETURNS ANYENUM;
CREATE FUNCTION enum_last($1 ANYENUM) RETURNS ANYENUM;
CREATE FUNCTION enum_le($1 ANYENUM, $2 ANYENUM) RETURNS BOOLEAN;
CREATE FUNCTION enum_lt($1 ANYENUM, $2 ANYENUM) RETURNS BOOLEAN;
CREATE FUNCTION enum_ne($1 ANYENUM, $2 ANYENUM) RETURNS BOOLEAN;
CREATE FUNCTION enum_out($1 ANYENUM) RETURNS CSTRING;
CREATE FUNCTION enum_range($1 ANYENUM) RETURNS ANYARRAY;
CREATE FUNCTION enum_range($1 ANYENUM, $2 ANYENUM) RETURNS ANYARRAY;
CREATE FUNCTION enum_recv($1 INTERNAL, $2 OID) RETURNS ANYENUM;
CREATE FUNCTION enum_send($1 ANYENUM) RETURNS BYTEA;
CREATE FUNCTION enum_smaller($1 ANYENUM, $2 ANYENUM) RETURNS ANYENUM;
CREATE FUNCTION eqjoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION eqsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION euc_cn_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION euc_cn_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION euc_jis_2004_to_shift_jis_2004($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION euc_jis_2004_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION euc_jp_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION euc_jp_to_sjis($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION euc_jp_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION euc_kr_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION euc_kr_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION euc_tw_to_big5($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION euc_tw_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION euc_tw_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION event_trigger_in($1 CSTRING) RETURNS EVENT_TRIGGER;
CREATE FUNCTION event_trigger_out($1 EVENT_TRIGGER) RETURNS CSTRING;
CREATE FUNCTION every($1 BOOLEAN) RETURNS BOOLEAN;
CREATE FUNCTION exp($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION exp($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION factorial($1 BIGINT) RETURNS NUMERIC;
CREATE FUNCTION family($1 INET) RETURNS INTEGER;
CREATE FUNCTION fdw_handler_in($1 CSTRING) RETURNS FDW_HANDLER;
CREATE FUNCTION fdw_handler_out($1 FDW_HANDLER) RETURNS CSTRING;
CREATE FUNCTION first_value($1 ANYELEMENT) RETURNS ANYELEMENT;
CREATE FUNCTION float48div($1 REAL, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float48eq($1 REAL, $2 DOUBLE PRECISION) RETURNS BOOLEAN;
CREATE FUNCTION float48ge($1 REAL, $2 DOUBLE PRECISION) RETURNS BOOLEAN;
CREATE FUNCTION float48gt($1 REAL, $2 DOUBLE PRECISION) RETURNS BOOLEAN;
CREATE FUNCTION float48le($1 REAL, $2 DOUBLE PRECISION) RETURNS BOOLEAN;
CREATE FUNCTION float48lt($1 REAL, $2 DOUBLE PRECISION) RETURNS BOOLEAN;
CREATE FUNCTION float48mi($1 REAL, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float48mul($1 REAL, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float48ne($1 REAL, $2 DOUBLE PRECISION) RETURNS BOOLEAN;
CREATE FUNCTION float48pl($1 REAL, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float4_accum($1 DOUBLE PRECISION[], $2 REAL) RETURNS DOUBLE PRECISION[];
CREATE FUNCTION float4abs($1 REAL) RETURNS REAL;
CREATE FUNCTION float4($1 BIGINT) RETURNS REAL;
CREATE FUNCTION float4div($1 REAL, $2 REAL) RETURNS REAL;
CREATE FUNCTION float4($1 DOUBLE PRECISION) RETURNS REAL;
CREATE FUNCTION float4eq($1 REAL, $2 REAL) RETURNS BOOLEAN;
CREATE FUNCTION float4ge($1 REAL, $2 REAL) RETURNS BOOLEAN;
CREATE FUNCTION float4gt($1 REAL, $2 REAL) RETURNS BOOLEAN;
CREATE FUNCTION float4in($1 CSTRING) RETURNS REAL;
CREATE FUNCTION float4($1 INTEGER) RETURNS REAL;
CREATE FUNCTION float4larger($1 REAL, $2 REAL) RETURNS REAL;
CREATE FUNCTION float4le($1 REAL, $2 REAL) RETURNS BOOLEAN;
CREATE FUNCTION float4lt($1 REAL, $2 REAL) RETURNS BOOLEAN;
CREATE FUNCTION float4mi($1 REAL, $2 REAL) RETURNS REAL;
CREATE FUNCTION float4mul($1 REAL, $2 REAL) RETURNS REAL;
CREATE FUNCTION float4ne($1 REAL, $2 REAL) RETURNS BOOLEAN;
CREATE FUNCTION float4($1 NUMERIC) RETURNS REAL;
CREATE FUNCTION float4out($1 REAL) RETURNS CSTRING;
CREATE FUNCTION float4pl($1 REAL, $2 REAL) RETURNS REAL;
CREATE FUNCTION float4recv($1 INTERNAL) RETURNS REAL;
CREATE FUNCTION float4send($1 REAL) RETURNS BYTEA;
CREATE FUNCTION float4smaller($1 REAL, $2 REAL) RETURNS REAL;
CREATE FUNCTION float4($1 SMALLINT) RETURNS REAL;
CREATE FUNCTION float4um($1 REAL) RETURNS REAL;
CREATE FUNCTION float4up($1 REAL) RETURNS REAL;
CREATE FUNCTION float84div($1 DOUBLE PRECISION, $2 REAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float84eq($1 DOUBLE PRECISION, $2 REAL) RETURNS BOOLEAN;
CREATE FUNCTION float84ge($1 DOUBLE PRECISION, $2 REAL) RETURNS BOOLEAN;
CREATE FUNCTION float84gt($1 DOUBLE PRECISION, $2 REAL) RETURNS BOOLEAN;
CREATE FUNCTION float84le($1 DOUBLE PRECISION, $2 REAL) RETURNS BOOLEAN;
CREATE FUNCTION float84lt($1 DOUBLE PRECISION, $2 REAL) RETURNS BOOLEAN;
CREATE FUNCTION float84mi($1 DOUBLE PRECISION, $2 REAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float84mul($1 DOUBLE PRECISION, $2 REAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float84ne($1 DOUBLE PRECISION, $2 REAL) RETURNS BOOLEAN;
CREATE FUNCTION float84pl($1 DOUBLE PRECISION, $2 REAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_accum($1 DOUBLE PRECISION[], $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION[];
CREATE FUNCTION float8_avg($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_corr($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_covar_pop($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_covar_samp($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_regr_accum($1 DOUBLE PRECISION[], $2 DOUBLE PRECISION, $3 DOUBLE PRECISION) RETURNS DOUBLE PRECISION[];
CREATE FUNCTION float8_regr_avgx($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_regr_avgy($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_regr_intercept($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_regr_r2($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_regr_slope($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_regr_sxx($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_regr_sxy($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_regr_syy($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_stddev_pop($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_stddev_samp($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_var_pop($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8_var_samp($1 DOUBLE PRECISION[]) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8abs($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8($1 BIGINT) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8div($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8eq($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS BOOLEAN;
CREATE FUNCTION float8ge($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS BOOLEAN;
CREATE FUNCTION float8gt($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS BOOLEAN;
CREATE FUNCTION float8in($1 CSTRING) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8($1 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8larger($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8le($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS BOOLEAN;
CREATE FUNCTION float8lt($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS BOOLEAN;
CREATE FUNCTION float8mi($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8mul($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8ne($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS BOOLEAN;
CREATE FUNCTION float8($1 NUMERIC) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8out($1 DOUBLE PRECISION) RETURNS CSTRING;
CREATE FUNCTION float8pl($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8($1 REAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8recv($1 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8send($1 DOUBLE PRECISION) RETURNS BYTEA;
CREATE FUNCTION float8smaller($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8($1 SMALLINT) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8um($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION float8up($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION floor($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION floor($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION flt4_mul_cash($1 REAL, $2 MONEY) RETURNS MONEY;
CREATE FUNCTION flt8_mul_cash($1 DOUBLE PRECISION, $2 MONEY) RETURNS MONEY;
CREATE FUNCTION fmgr_c_validator($1 OID) RETURNS VOID;
CREATE FUNCTION fmgr_internal_validator($1 OID) RETURNS VOID;
CREATE FUNCTION fmgr_sql_validator($1 OID) RETURNS VOID;
CREATE FUNCTION format_type($1 OID, $2 INTEGER) RETURNS TEXT;
CREATE FUNCTION format($1 TEXT) RETURNS TEXT;
CREATE FUNCTION format($1 TEXT, $2 "any") RETURNS TEXT;
CREATE FUNCTION gb18030_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION gbk_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION generate_series($1 BIGINT, $2 BIGINT) RETURNS SETOF BIGINT;
CREATE FUNCTION generate_series($1 BIGINT, $2 BIGINT, $3 BIGINT) RETURNS SETOF BIGINT;
CREATE FUNCTION generate_series($1 INTEGER, $2 INTEGER) RETURNS SETOF INTEGER;
CREATE FUNCTION generate_series($1 INTEGER, $2 INTEGER, $3 INTEGER) RETURNS SETOF INTEGER;
CREATE FUNCTION generate_series($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE, $3 INTERVAL) RETURNS SETOF TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION generate_series($1 TIMESTAMP, $2 TIMESTAMP, $3 INTERVAL) RETURNS SETOF TIMESTAMP;
CREATE FUNCTION generate_subscripts($1 ANYARRAY, $2 INTEGER) RETURNS SETOF INTEGER;
CREATE FUNCTION generate_subscripts($1 ANYARRAY, $2 INTEGER, $3 BOOLEAN) RETURNS SETOF INTEGER;
CREATE FUNCTION get_bit($1 BIT, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION get_bit($1 BYTEA, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION get_byte($1 BYTEA, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION get_current_ts_config() RETURNS REGCONFIG;
CREATE FUNCTION getdatabaseencoding() RETURNS NAME;
CREATE FUNCTION getpgusername() RETURNS NAME;
CREATE FUNCTION gin_cmp_prefix($1 TEXT, $2 TEXT, $3 SMALLINT, $4 INTERNAL) RETURNS INTEGER;
CREATE FUNCTION gin_cmp_tslexeme($1 TEXT, $2 TEXT) RETURNS INTEGER;
CREATE FUNCTION gin_extract_tsquery($1 TSQUERY, $2 INTERNAL, $3 SMALLINT, $4 INTERNAL, $5 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gin_extract_tsquery($1 TSQUERY, $2 INTERNAL, $3 SMALLINT, $4 INTERNAL, $5 INTERNAL, $6 INTERNAL, $7 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gin_extract_tsvector($1 TSVECTOR, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gin_extract_tsvector($1 TSVECTOR, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gin_tsquery_consistent($1 INTERNAL, $2 SMALLINT, $3 TSQUERY, $4 INTEGER, $5 INTERNAL, $6 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION gin_tsquery_consistent($1 INTERNAL, $2 SMALLINT, $3 TSQUERY, $4 INTEGER, $5 INTERNAL, $6 INTERNAL, $7 INTERNAL, $8 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION ginarrayconsistent($1 INTERNAL, $2 SMALLINT, $3 ANYARRAY, $4 INTEGER, $5 INTERNAL, $6 INTERNAL, $7 INTERNAL, $8 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION ginarrayextract($1 ANYARRAY, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION ginarrayextract($1 ANYARRAY, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION ginbeginscan($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION ginbuildempty($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION ginbuild($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION ginbulkdelete($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gincostestimate($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL, $6 INTERNAL, $7 INTERNAL) RETURNS VOID;
CREATE FUNCTION ginendscan($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION gingetbitmap($1 INTERNAL, $2 INTERNAL) RETURNS BIGINT;
CREATE FUNCTION gininsert($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL, $6 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION ginmarkpos($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION ginoptions($1 TEXT[], $2 BOOLEAN) RETURNS BYTEA;
CREATE FUNCTION ginqueryarrayextract($1 ANYARRAY, $2 INTERNAL, $3 SMALLINT, $4 INTERNAL, $5 INTERNAL, $6 INTERNAL, $7 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION ginrescan($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL) RETURNS VOID;
CREATE FUNCTION ginrestrpos($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION ginvacuumcleanup($1 INTERNAL, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gist_box_compress($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gist_box_consistent($1 INTERNAL, $2 BOX, $3 INTEGER, $4 OID, $5 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION gist_box_decompress($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gist_box_penalty($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gist_box_picksplit($1 INTERNAL, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gist_box_same($1 BOX, $2 BOX, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gist_box_union($1 INTERNAL, $2 INTERNAL) RETURNS BOX;
CREATE FUNCTION gist_circle_compress($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gist_circle_consistent($1 INTERNAL, $2 CIRCLE, $3 INTEGER, $4 OID, $5 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION gist_point_compress($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gist_point_consistent($1 INTERNAL, $2 POINT, $3 INTEGER, $4 OID, $5 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION gist_point_distance($1 INTERNAL, $2 POINT, $3 INTEGER, $4 OID) RETURNS DOUBLE PRECISION;
CREATE FUNCTION gist_poly_compress($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gist_poly_consistent($1 INTERNAL, $2 POLYGON, $3 INTEGER, $4 OID, $5 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION gistbeginscan($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gistbuildempty($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION gistbuild($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gistbulkdelete($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gistcostestimate($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL, $6 INTERNAL, $7 INTERNAL) RETURNS VOID;
CREATE FUNCTION gistendscan($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION gistgetbitmap($1 INTERNAL, $2 INTERNAL) RETURNS BIGINT;
CREATE FUNCTION gistgettuple($1 INTERNAL, $2 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION gistinsert($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL, $6 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION gistmarkpos($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION gistoptions($1 TEXT[], $2 BOOLEAN) RETURNS BYTEA;
CREATE FUNCTION gistrescan($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL) RETURNS VOID;
CREATE FUNCTION gistrestrpos($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION gistvacuumcleanup($1 INTERNAL, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsquery_compress($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsquery_consistent($1 INTERNAL, $2 INTERNAL, $3 INTEGER, $4 OID, $5 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION gtsquery_decompress($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsquery_penalty($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsquery_picksplit($1 INTERNAL, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsquery_same($1 BIGINT, $2 BIGINT, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsquery_union($1 INTERNAL, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsvector_compress($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsvector_consistent($1 INTERNAL, $2 GTSVECTOR, $3 INTEGER, $4 OID, $5 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION gtsvector_decompress($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsvector_penalty($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsvector_picksplit($1 INTERNAL, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsvector_same($1 GTSVECTOR, $2 GTSVECTOR, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsvector_union($1 INTERNAL, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION gtsvectorin($1 CSTRING) RETURNS GTSVECTOR;
CREATE FUNCTION gtsvectorout($1 GTSVECTOR) RETURNS CSTRING;
CREATE FUNCTION has_any_column_privilege($1 NAME, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_any_column_privilege($1 NAME, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_any_column_privilege($1 OID, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_any_column_privilege($1 OID, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_any_column_privilege($1 OID, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_any_column_privilege($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_column_privilege($1 NAME, $2 OID, $3 SMALLINT, $4 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_column_privilege($1 NAME, $2 OID, $3 TEXT, $4 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_column_privilege($1 NAME, $2 TEXT, $3 SMALLINT, $4 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_column_privilege($1 NAME, $2 TEXT, $3 TEXT, $4 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_column_privilege($1 OID, $2 OID, $3 SMALLINT, $4 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_column_privilege($1 OID, $2 OID, $3 TEXT, $4 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_column_privilege($1 OID, $2 SMALLINT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_column_privilege($1 OID, $2 TEXT, $3 SMALLINT, $4 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_column_privilege($1 OID, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_column_privilege($1 OID, $2 TEXT, $3 TEXT, $4 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_column_privilege($1 TEXT, $2 SMALLINT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_column_privilege($1 TEXT, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_database_privilege($1 NAME, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_database_privilege($1 NAME, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_database_privilege($1 OID, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_database_privilege($1 OID, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_database_privilege($1 OID, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_database_privilege($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_foreign_data_wrapper_privilege($1 NAME, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_foreign_data_wrapper_privilege($1 NAME, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_foreign_data_wrapper_privilege($1 OID, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_foreign_data_wrapper_privilege($1 OID, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_foreign_data_wrapper_privilege($1 OID, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_foreign_data_wrapper_privilege($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_function_privilege($1 NAME, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_function_privilege($1 NAME, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_function_privilege($1 OID, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_function_privilege($1 OID, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_function_privilege($1 OID, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_function_privilege($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_language_privilege($1 NAME, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_language_privilege($1 NAME, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_language_privilege($1 OID, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_language_privilege($1 OID, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_language_privilege($1 OID, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_language_privilege($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_schema_privilege($1 NAME, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_schema_privilege($1 NAME, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_schema_privilege($1 OID, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_schema_privilege($1 OID, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_schema_privilege($1 OID, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_schema_privilege($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_sequence_privilege($1 NAME, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_sequence_privilege($1 NAME, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_sequence_privilege($1 OID, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_sequence_privilege($1 OID, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_sequence_privilege($1 OID, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_sequence_privilege($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_server_privilege($1 NAME, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_server_privilege($1 NAME, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_server_privilege($1 OID, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_server_privilege($1 OID, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_server_privilege($1 OID, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_server_privilege($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_table_privilege($1 NAME, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_table_privilege($1 NAME, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_table_privilege($1 OID, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_table_privilege($1 OID, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_table_privilege($1 OID, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_table_privilege($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_tablespace_privilege($1 NAME, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_tablespace_privilege($1 NAME, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_tablespace_privilege($1 OID, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_tablespace_privilege($1 OID, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_tablespace_privilege($1 OID, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_tablespace_privilege($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_type_privilege($1 NAME, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_type_privilege($1 NAME, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_type_privilege($1 OID, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_type_privilege($1 OID, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_type_privilege($1 OID, $2 TEXT, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION has_type_privilege($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION hash_aclitem($1 ACLITEM) RETURNS INTEGER;
CREATE FUNCTION hash_array($1 ANYARRAY) RETURNS INTEGER;
CREATE FUNCTION hash_numeric($1 NUMERIC) RETURNS INTEGER;
CREATE FUNCTION hash_range($1 ANYRANGE) RETURNS INTEGER;
CREATE FUNCTION hashbeginscan($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION hashbpchar($1 CHAR) RETURNS INTEGER;
CREATE FUNCTION hashbuildempty($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION hashbuild($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION hashbulkdelete($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION hashchar($1 "char") RETURNS INTEGER;
CREATE FUNCTION hashcostestimate($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL, $6 INTERNAL, $7 INTERNAL) RETURNS VOID;
CREATE FUNCTION hashendscan($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION hashenum($1 ANYENUM) RETURNS INTEGER;
CREATE FUNCTION hashfloat4($1 REAL) RETURNS INTEGER;
CREATE FUNCTION hashfloat8($1 DOUBLE PRECISION) RETURNS INTEGER;
CREATE FUNCTION hashgetbitmap($1 INTERNAL, $2 INTERNAL) RETURNS BIGINT;
CREATE FUNCTION hashgettuple($1 INTERNAL, $2 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION hashinet($1 INET) RETURNS INTEGER;
CREATE FUNCTION hashinsert($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL, $6 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION hashint2($1 SMALLINT) RETURNS INTEGER;
CREATE FUNCTION hashint2vector($1 INT2VECTOR) RETURNS INTEGER;
CREATE FUNCTION hashint4($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION hashint8($1 BIGINT) RETURNS INTEGER;
CREATE FUNCTION hashmacaddr($1 MACADDR) RETURNS INTEGER;
CREATE FUNCTION hashmarkpos($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION hashname($1 NAME) RETURNS INTEGER;
CREATE FUNCTION hashoid($1 OID) RETURNS INTEGER;
CREATE FUNCTION hashoidvector($1 OIDVECTOR) RETURNS INTEGER;
CREATE FUNCTION hashoptions($1 TEXT[], $2 BOOLEAN) RETURNS BYTEA;
CREATE FUNCTION hashrescan($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL) RETURNS VOID;
CREATE FUNCTION hashrestrpos($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION hashtext($1 TEXT) RETURNS INTEGER;
CREATE FUNCTION hashvacuumcleanup($1 INTERNAL, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION hashvarlena($1 INTERNAL) RETURNS INTEGER;
CREATE FUNCTION height($1 BOX) RETURNS DOUBLE PRECISION;
CREATE FUNCTION host($1 INET) RETURNS TEXT;
CREATE FUNCTION hostmask($1 INET) RETURNS INET;
CREATE FUNCTION iclikejoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION iclikesel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION icnlikejoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION icnlikesel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION icregexeqjoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION icregexeqsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION icregexnejoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION icregexnesel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION inet_client_addr() RETURNS INET;
CREATE FUNCTION inet_client_port() RETURNS INTEGER;
CREATE FUNCTION inet_in($1 CSTRING) RETURNS INET;
CREATE FUNCTION inet_out($1 INET) RETURNS CSTRING;
CREATE FUNCTION inet_recv($1 INTERNAL) RETURNS INET;
CREATE FUNCTION inet_send($1 INET) RETURNS BYTEA;
CREATE FUNCTION inet_server_addr() RETURNS INET;
CREATE FUNCTION inet_server_port() RETURNS INTEGER;
CREATE FUNCTION inetand($1 INET, $2 INET) RETURNS INET;
CREATE FUNCTION inetmi_int8($1 INET, $2 BIGINT) RETURNS INET;
CREATE FUNCTION inetmi($1 INET, $2 INET) RETURNS BIGINT;
CREATE FUNCTION inetnot($1 INET) RETURNS INET;
CREATE FUNCTION inetor($1 INET, $2 INET) RETURNS INET;
CREATE FUNCTION inetpl($1 INET, $2 BIGINT) RETURNS INET;
CREATE FUNCTION initcap($1 TEXT) RETURNS TEXT;
CREATE FUNCTION int24div($1 SMALLINT, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int24eq($1 SMALLINT, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int24ge($1 SMALLINT, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int24gt($1 SMALLINT, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int24le($1 SMALLINT, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int24lt($1 SMALLINT, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int24mi($1 SMALLINT, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int24mul($1 SMALLINT, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int24ne($1 SMALLINT, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int24pl($1 SMALLINT, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int28div($1 SMALLINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int28eq($1 SMALLINT, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int28ge($1 SMALLINT, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int28gt($1 SMALLINT, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int28le($1 SMALLINT, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int28lt($1 SMALLINT, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int28mi($1 SMALLINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int28mul($1 SMALLINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int28ne($1 SMALLINT, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int28pl($1 SMALLINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int2_accum($1 NUMERIC[], $2 SMALLINT) RETURNS NUMERIC[];
CREATE FUNCTION int2_avg_accum($1 BIGINT[], $2 SMALLINT) RETURNS BIGINT[];
CREATE FUNCTION int2_mul_cash($1 SMALLINT, $2 MONEY) RETURNS MONEY;
CREATE FUNCTION int2_sum($1 BIGINT, $2 SMALLINT) RETURNS BIGINT;
CREATE FUNCTION int2abs($1 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2and($1 SMALLINT, $2 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2($1 BIGINT) RETURNS SMALLINT;
CREATE FUNCTION int2div($1 SMALLINT, $2 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2($1 DOUBLE PRECISION) RETURNS SMALLINT;
CREATE FUNCTION int2eq($1 SMALLINT, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int2ge($1 SMALLINT, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int2gt($1 SMALLINT, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int2in($1 CSTRING) RETURNS SMALLINT;
CREATE FUNCTION int2($1 INTEGER) RETURNS SMALLINT;
CREATE FUNCTION int2larger($1 SMALLINT, $2 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2le($1 SMALLINT, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int2lt($1 SMALLINT, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int2mi($1 SMALLINT, $2 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2mod($1 SMALLINT, $2 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2mul($1 SMALLINT, $2 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2ne($1 SMALLINT, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int2not($1 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2($1 NUMERIC) RETURNS SMALLINT;
CREATE FUNCTION int2or($1 SMALLINT, $2 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2out($1 SMALLINT) RETURNS CSTRING;
CREATE FUNCTION int2pl($1 SMALLINT, $2 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2($1 REAL) RETURNS SMALLINT;
CREATE FUNCTION int2recv($1 INTERNAL) RETURNS SMALLINT;
CREATE FUNCTION int2send($1 SMALLINT) RETURNS BYTEA;
CREATE FUNCTION int2shl($1 SMALLINT, $2 INTEGER) RETURNS SMALLINT;
CREATE FUNCTION int2shr($1 SMALLINT, $2 INTEGER) RETURNS SMALLINT;
CREATE FUNCTION int2smaller($1 SMALLINT, $2 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2um($1 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2up($1 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int2vectoreq($1 INT2VECTOR, $2 INT2VECTOR) RETURNS BOOLEAN;
CREATE FUNCTION int2vectorin($1 CSTRING) RETURNS INT2VECTOR;
CREATE FUNCTION int2vectorout($1 INT2VECTOR) RETURNS CSTRING;
CREATE FUNCTION int2vectorrecv($1 INTERNAL) RETURNS INT2VECTOR;
CREATE FUNCTION int2vectorsend($1 INT2VECTOR) RETURNS BYTEA;
CREATE FUNCTION int2xor($1 SMALLINT, $2 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION int4($1 "char") RETURNS INTEGER;
CREATE FUNCTION int42div($1 INTEGER, $2 SMALLINT) RETURNS INTEGER;
CREATE FUNCTION int42eq($1 INTEGER, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int42ge($1 INTEGER, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int42gt($1 INTEGER, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int42le($1 INTEGER, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int42lt($1 INTEGER, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int42mi($1 INTEGER, $2 SMALLINT) RETURNS INTEGER;
CREATE FUNCTION int42mul($1 INTEGER, $2 SMALLINT) RETURNS INTEGER;
CREATE FUNCTION int42ne($1 INTEGER, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int42pl($1 INTEGER, $2 SMALLINT) RETURNS INTEGER;
CREATE FUNCTION int48div($1 INTEGER, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int48eq($1 INTEGER, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int48ge($1 INTEGER, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int48gt($1 INTEGER, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int48le($1 INTEGER, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int48lt($1 INTEGER, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int48mi($1 INTEGER, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int48mul($1 INTEGER, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int48ne($1 INTEGER, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int48pl($1 INTEGER, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int4_accum($1 NUMERIC[], $2 INTEGER) RETURNS NUMERIC[];
CREATE FUNCTION int4_avg_accum($1 BIGINT[], $2 INTEGER) RETURNS BIGINT[];
CREATE FUNCTION int4_mul_cash($1 INTEGER, $2 MONEY) RETURNS MONEY;
CREATE FUNCTION int4_sum($1 BIGINT, $2 INTEGER) RETURNS BIGINT;
CREATE FUNCTION int4abs($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4and($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4($1 BIGINT) RETURNS INTEGER;
CREATE FUNCTION int4($1 BIT) RETURNS INTEGER;
CREATE FUNCTION int4($1 BOOLEAN) RETURNS INTEGER;
CREATE FUNCTION int4div($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4($1 DOUBLE PRECISION) RETURNS INTEGER;
CREATE FUNCTION int4eq($1 INTEGER, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int4ge($1 INTEGER, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int4gt($1 INTEGER, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int4inc($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4in($1 CSTRING) RETURNS INTEGER;
CREATE FUNCTION int4larger($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4le($1 INTEGER, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int4lt($1 INTEGER, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int4mi($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4mod($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4mul($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4ne($1 INTEGER, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int4not($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4($1 NUMERIC) RETURNS INTEGER;
CREATE FUNCTION int4or($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4out($1 INTEGER) RETURNS CSTRING;
CREATE FUNCTION int4pl($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4range_canonical($1 INT4RANGE) RETURNS INT4RANGE;
CREATE FUNCTION int4range_subdiff($1 INTEGER, $2 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION int4range($1 INTEGER, $2 INTEGER) RETURNS INT4RANGE;
CREATE FUNCTION int4range($1 INTEGER, $2 INTEGER, $3 TEXT) RETURNS INT4RANGE;
CREATE FUNCTION int4($1 REAL) RETURNS INTEGER;
CREATE FUNCTION int4recv($1 INTERNAL) RETURNS INTEGER;
CREATE FUNCTION int4send($1 INTEGER) RETURNS BYTEA;
CREATE FUNCTION int4shl($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4shr($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4smaller($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4($1 SMALLINT) RETURNS INTEGER;
CREATE FUNCTION int4um($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4up($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int4xor($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION int82div($1 BIGINT, $2 SMALLINT) RETURNS BIGINT;
CREATE FUNCTION int82eq($1 BIGINT, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int82ge($1 BIGINT, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int82gt($1 BIGINT, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int82le($1 BIGINT, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int82lt($1 BIGINT, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int82mi($1 BIGINT, $2 SMALLINT) RETURNS BIGINT;
CREATE FUNCTION int82mul($1 BIGINT, $2 SMALLINT) RETURNS BIGINT;
CREATE FUNCTION int82ne($1 BIGINT, $2 SMALLINT) RETURNS BOOLEAN;
CREATE FUNCTION int82pl($1 BIGINT, $2 SMALLINT) RETURNS BIGINT;
CREATE FUNCTION int84div($1 BIGINT, $2 INTEGER) RETURNS BIGINT;
CREATE FUNCTION int84eq($1 BIGINT, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int84ge($1 BIGINT, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int84gt($1 BIGINT, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int84le($1 BIGINT, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int84lt($1 BIGINT, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int84mi($1 BIGINT, $2 INTEGER) RETURNS BIGINT;
CREATE FUNCTION int84mul($1 BIGINT, $2 INTEGER) RETURNS BIGINT;
CREATE FUNCTION int84ne($1 BIGINT, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION int84pl($1 BIGINT, $2 INTEGER) RETURNS BIGINT;
CREATE FUNCTION int8_accum($1 NUMERIC[], $2 BIGINT) RETURNS NUMERIC[];
CREATE FUNCTION int8_avg_accum($1 NUMERIC[], $2 BIGINT) RETURNS NUMERIC[];
CREATE FUNCTION int8_avg($1 BIGINT[]) RETURNS NUMERIC;
CREATE FUNCTION int8_sum($1 NUMERIC, $2 BIGINT) RETURNS NUMERIC;
CREATE FUNCTION int8abs($1 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8and($1 BIGINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8($1 BIT) RETURNS BIGINT;
CREATE FUNCTION int8div($1 BIGINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8($1 DOUBLE PRECISION) RETURNS BIGINT;
CREATE FUNCTION int8eq($1 BIGINT, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int8ge($1 BIGINT, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int8gt($1 BIGINT, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int8inc_any($1 BIGINT, $2 "any") RETURNS BIGINT;
CREATE FUNCTION int8inc_float8_float8($1 BIGINT, $2 DOUBLE PRECISION, $3 DOUBLE PRECISION) RETURNS BIGINT;
CREATE FUNCTION int8inc($1 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8in($1 CSTRING) RETURNS BIGINT;
CREATE FUNCTION int8($1 INTEGER) RETURNS BIGINT;
CREATE FUNCTION int8larger($1 BIGINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8le($1 BIGINT, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int8lt($1 BIGINT, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int8mi($1 BIGINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8mod($1 BIGINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8mul($1 BIGINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8ne($1 BIGINT, $2 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION int8not($1 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8($1 NUMERIC) RETURNS BIGINT;
CREATE FUNCTION int8($1 OID) RETURNS BIGINT;
CREATE FUNCTION int8or($1 BIGINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8out($1 BIGINT) RETURNS CSTRING;
CREATE FUNCTION int8pl_inet($1 BIGINT, $2 INET) RETURNS INET;
CREATE FUNCTION int8pl($1 BIGINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8range_canonical($1 INT8RANGE) RETURNS INT8RANGE;
CREATE FUNCTION int8range_subdiff($1 BIGINT, $2 BIGINT) RETURNS DOUBLE PRECISION;
CREATE FUNCTION int8range($1 BIGINT, $2 BIGINT) RETURNS INT8RANGE;
CREATE FUNCTION int8range($1 BIGINT, $2 BIGINT, $3 TEXT) RETURNS INT8RANGE;
CREATE FUNCTION int8($1 REAL) RETURNS BIGINT;
CREATE FUNCTION int8recv($1 INTERNAL) RETURNS BIGINT;
CREATE FUNCTION int8send($1 BIGINT) RETURNS BYTEA;
CREATE FUNCTION int8shl($1 BIGINT, $2 INTEGER) RETURNS BIGINT;
CREATE FUNCTION int8shr($1 BIGINT, $2 INTEGER) RETURNS BIGINT;
CREATE FUNCTION int8smaller($1 BIGINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8($1 SMALLINT) RETURNS BIGINT;
CREATE FUNCTION int8um($1 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8up($1 BIGINT) RETURNS BIGINT;
CREATE FUNCTION int8xor($1 BIGINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION integer_pl_date($1 INTEGER, $2 DATE) RETURNS DATE;
CREATE FUNCTION inter_lb($1 LINE, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION inter_sb($1 LSEG, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION inter_sl($1 LSEG, $2 LINE) RETURNS BOOLEAN;
CREATE FUNCTION internal_in($1 CSTRING) RETURNS INTERNAL;
CREATE FUNCTION internal_out($1 INTERNAL) RETURNS CSTRING;
CREATE FUNCTION interval_accum($1 INTERVAL[], $2 INTERVAL) RETURNS INTERVAL[];
CREATE FUNCTION interval_avg($1 INTERVAL[]) RETURNS INTERVAL;
CREATE FUNCTION interval_cmp($1 INTERVAL, $2 INTERVAL) RETURNS INTEGER;
CREATE FUNCTION interval_div($1 INTERVAL, $2 DOUBLE PRECISION) RETURNS INTERVAL;
CREATE FUNCTION interval_eq($1 INTERVAL, $2 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION interval_ge($1 INTERVAL, $2 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION interval_gt($1 INTERVAL, $2 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION interval_hash($1 INTERVAL) RETURNS INTEGER;
CREATE FUNCTION interval_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS INTERVAL;
CREATE FUNCTION interval_larger($1 INTERVAL, $2 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION interval_le($1 INTERVAL, $2 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION interval_lt($1 INTERVAL, $2 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION interval_mi($1 INTERVAL, $2 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION interval_mul($1 INTERVAL, $2 DOUBLE PRECISION) RETURNS INTERVAL;
CREATE FUNCTION interval_ne($1 INTERVAL, $2 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION interval_out($1 INTERVAL) RETURNS CSTRING;
CREATE FUNCTION interval_pl_date($1 INTERVAL, $2 DATE) RETURNS TIMESTAMP;
CREATE FUNCTION interval_pl_time($1 INTERVAL, $2 TIME) RETURNS TIME;
CREATE FUNCTION interval_pl_timestamp($1 INTERVAL, $2 TIMESTAMP) RETURNS TIMESTAMP;
CREATE FUNCTION interval_pl_timestamptz($1 INTERVAL, $2 TIMESTAMP WITH TIME ZONE) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION interval_pl_timetz($1 INTERVAL, $2 TIME WITH TIME ZONE) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION interval_pl($1 INTERVAL, $2 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION interval_recv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS INTERVAL;
CREATE FUNCTION interval_send($1 INTERVAL) RETURNS BYTEA;
CREATE FUNCTION interval_smaller($1 INTERVAL, $2 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION interval_transform($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION interval_um($1 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION interval($1 INTERVAL, $2 INTEGER) RETURNS INTERVAL;
CREATE FUNCTION interval($1 RELTIME) RETURNS INTERVAL;
CREATE FUNCTION interval($1 TIME) RETURNS INTERVAL;
CREATE FUNCTION intervaltypmodin($1 CSTRING[]) RETURNS INTEGER;
CREATE FUNCTION intervaltypmodout($1 INTEGER) RETURNS CSTRING;
CREATE FUNCTION intinterval($1 ABSTIME, $2 TINTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION isclosed($1 PATH) RETURNS BOOLEAN;
CREATE FUNCTION isempty($1 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION isfinite($1 ABSTIME) RETURNS BOOLEAN;
CREATE FUNCTION isfinite($1 DATE) RETURNS BOOLEAN;
CREATE FUNCTION isfinite($1 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION isfinite($1 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION isfinite($1 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION ishorizontal($1 LINE) RETURNS BOOLEAN;
CREATE FUNCTION ishorizontal($1 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION ishorizontal($1 POINT, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION iso8859_1_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION iso8859_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION iso_to_koi8r($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION iso_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION iso_to_win1251($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION iso_to_win866($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION isopen($1 PATH) RETURNS BOOLEAN;
CREATE FUNCTION isparallel($1 LINE, $2 LINE) RETURNS BOOLEAN;
CREATE FUNCTION isparallel($1 LSEG, $2 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION isperp($1 LINE, $2 LINE) RETURNS BOOLEAN;
CREATE FUNCTION isperp($1 LSEG, $2 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION isvertical($1 LINE) RETURNS BOOLEAN;
CREATE FUNCTION isvertical($1 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION isvertical($1 POINT, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION johab_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION json_agg_finalfn($1 INTERNAL) RETURNS JSON;
CREATE FUNCTION json_agg_transfn($1 INTERNAL, $2 ANYELEMENT) RETURNS INTERNAL;
CREATE FUNCTION json_agg($1 ANYELEMENT) RETURNS JSON;
CREATE FUNCTION json_array_element_text(from_json JSON, element_index INTEGER) RETURNS TEXT;
CREATE FUNCTION json_array_element(from_json JSON, element_index INTEGER) RETURNS JSON;
CREATE FUNCTION json_array_elements(from_json JSON, value OUT JSON) RETURNS SETOF JSON;
CREATE FUNCTION json_array_length($1 JSON) RETURNS INTEGER;
CREATE FUNCTION json_each_text(from_json JSON, key OUT TEXT, value OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION json_each(from_json JSON, key OUT TEXT, value OUT JSON) RETURNS SETOF RECORD;
CREATE FUNCTION json_extract_path_op(from_json JSON, path_elems TEXT[]) RETURNS JSON;
CREATE FUNCTION json_extract_path_text_op(from_json JSON, path_elems TEXT[]) RETURNS TEXT;
CREATE FUNCTION json_extract_path_text(from_json JSON, path_elems TEXT[]) RETURNS TEXT;
CREATE FUNCTION json_extract_path(from_json JSON, path_elems TEXT[]) RETURNS JSON;
CREATE FUNCTION json_in($1 CSTRING) RETURNS JSON;
CREATE FUNCTION json_object_field_text(from_json JSON, field_name TEXT) RETURNS TEXT;
CREATE FUNCTION json_object_field(from_json JSON, field_name TEXT) RETURNS JSON;
CREATE FUNCTION json_object_keys($1 JSON) RETURNS SETOF TEXT;
CREATE FUNCTION json_out($1 JSON) RETURNS CSTRING;
CREATE FUNCTION json_populate_record(base ANYELEMENT, from_json JSON, use_json_as_text BOOLEAN) RETURNS ANYELEMENT;
CREATE FUNCTION json_populate_recordset(base ANYELEMENT, from_json JSON, use_json_as_text BOOLEAN) RETURNS SETOF ANYELEMENT;
CREATE FUNCTION json_recv($1 INTERNAL) RETURNS JSON;
CREATE FUNCTION json_send($1 JSON) RETURNS BYTEA;
CREATE FUNCTION justify_days($1 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION justify_hours($1 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION justify_interval($1 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION koi8r_to_iso($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION koi8r_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION koi8r_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION koi8r_to_win1251($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION koi8r_to_win866($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION koi8u_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION lag($1 ANYELEMENT) RETURNS ANYELEMENT;
CREATE FUNCTION lag($1 ANYELEMENT, $2 INTEGER) RETURNS ANYELEMENT;
CREATE FUNCTION lag($1 ANYELEMENT, $2 INTEGER, $3 ANYELEMENT) RETURNS ANYELEMENT;
CREATE FUNCTION language_handler_in($1 CSTRING) RETURNS LANGUAGE_HANDLER;
CREATE FUNCTION language_handler_out($1 LANGUAGE_HANDLER) RETURNS CSTRING;
CREATE FUNCTION last_value($1 ANYELEMENT) RETURNS ANYELEMENT;
CREATE FUNCTION lastval() RETURNS BIGINT;
CREATE FUNCTION latin1_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION latin2_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION latin2_to_win1250($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION latin3_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION latin4_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION lead($1 ANYELEMENT) RETURNS ANYELEMENT;
CREATE FUNCTION lead($1 ANYELEMENT, $2 INTEGER) RETURNS ANYELEMENT;
CREATE FUNCTION lead($1 ANYELEMENT, $2 INTEGER, $3 ANYELEMENT) RETURNS ANYELEMENT;
CREATE FUNCTION "left"($1 TEXT, $2 INTEGER) RETURNS TEXT;
CREATE FUNCTION length($1 BIT) RETURNS INTEGER;
CREATE FUNCTION length($1 BYTEA) RETURNS INTEGER;
CREATE FUNCTION length($1 BYTEA, $2 NAME) RETURNS INTEGER;
CREATE FUNCTION length($1 CHAR) RETURNS INTEGER;
CREATE FUNCTION length($1 LSEG) RETURNS DOUBLE PRECISION;
CREATE FUNCTION length($1 PATH) RETURNS DOUBLE PRECISION;
CREATE FUNCTION length($1 TEXT) RETURNS INTEGER;
CREATE FUNCTION length($1 TSVECTOR) RETURNS INTEGER;
CREATE FUNCTION like_escape($1 BYTEA, $2 BYTEA) RETURNS BYTEA;
CREATE FUNCTION like_escape($1 TEXT, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION "like"($1 BYTEA, $2 BYTEA) RETURNS BOOLEAN;
CREATE FUNCTION likejoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION "like"($1 NAME, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION likesel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION "like"($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION line_distance($1 LINE, $2 LINE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION line_eq($1 LINE, $2 LINE) RETURNS BOOLEAN;
CREATE FUNCTION line_horizontal($1 LINE) RETURNS BOOLEAN;
CREATE FUNCTION line_in($1 CSTRING) RETURNS LINE;
CREATE FUNCTION line_interpt($1 LINE, $2 LINE) RETURNS POINT;
CREATE FUNCTION line_intersect($1 LINE, $2 LINE) RETURNS BOOLEAN;
CREATE FUNCTION line_out($1 LINE) RETURNS CSTRING;
CREATE FUNCTION line_parallel($1 LINE, $2 LINE) RETURNS BOOLEAN;
CREATE FUNCTION line_perp($1 LINE, $2 LINE) RETURNS BOOLEAN;
CREATE FUNCTION line_recv($1 INTERNAL) RETURNS LINE;
CREATE FUNCTION line_send($1 LINE) RETURNS BYTEA;
CREATE FUNCTION line_vertical($1 LINE) RETURNS BOOLEAN;
CREATE FUNCTION line($1 POINT, $2 POINT) RETURNS LINE;
CREATE FUNCTION ln($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION ln($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION lo_close($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION lo_create($1 OID) RETURNS OID;
CREATE FUNCTION lo_creat($1 INTEGER) RETURNS OID;
CREATE FUNCTION lo_export($1 OID, $2 TEXT) RETURNS INTEGER;
CREATE FUNCTION lo_import($1 TEXT) RETURNS OID;
CREATE FUNCTION lo_import($1 TEXT, $2 OID) RETURNS OID;
CREATE FUNCTION lo_lseek64($1 INTEGER, $2 BIGINT, $3 INTEGER) RETURNS BIGINT;
CREATE FUNCTION lo_lseek($1 INTEGER, $2 INTEGER, $3 INTEGER) RETURNS INTEGER;
CREATE FUNCTION lo_open($1 OID, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION lo_tell64($1 INTEGER) RETURNS BIGINT;
CREATE FUNCTION lo_tell($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION lo_truncate64($1 INTEGER, $2 BIGINT) RETURNS INTEGER;
CREATE FUNCTION lo_truncate($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION lo_unlink($1 OID) RETURNS INTEGER;
CREATE FUNCTION log($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION log($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION log($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION loread($1 INTEGER, $2 INTEGER) RETURNS BYTEA;
CREATE FUNCTION lower_inc($1 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION lower_inf($1 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION lower($1 ANYRANGE) RETURNS ANYELEMENT;
CREATE FUNCTION lower($1 TEXT) RETURNS TEXT;
CREATE FUNCTION lowrite($1 INTEGER, $2 BYTEA) RETURNS INTEGER;
CREATE FUNCTION lpad($1 TEXT, $2 INTEGER) RETURNS TEXT;
CREATE FUNCTION lpad($1 TEXT, $2 INTEGER, $3 TEXT) RETURNS TEXT;
CREATE FUNCTION lseg_center($1 LSEG) RETURNS POINT;
CREATE FUNCTION lseg_distance($1 LSEG, $2 LSEG) RETURNS DOUBLE PRECISION;
CREATE FUNCTION lseg_eq($1 LSEG, $2 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION lseg_ge($1 LSEG, $2 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION lseg_gt($1 LSEG, $2 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION lseg_horizontal($1 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION lseg_in($1 CSTRING) RETURNS LSEG;
CREATE FUNCTION lseg_interpt($1 LSEG, $2 LSEG) RETURNS POINT;
CREATE FUNCTION lseg_intersect($1 LSEG, $2 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION lseg_le($1 LSEG, $2 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION lseg_length($1 LSEG) RETURNS DOUBLE PRECISION;
CREATE FUNCTION lseg_lt($1 LSEG, $2 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION lseg_ne($1 LSEG, $2 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION lseg_out($1 LSEG) RETURNS CSTRING;
CREATE FUNCTION lseg_parallel($1 LSEG, $2 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION lseg_perp($1 LSEG, $2 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION lseg_recv($1 INTERNAL) RETURNS LSEG;
CREATE FUNCTION lseg_send($1 LSEG) RETURNS BYTEA;
CREATE FUNCTION lseg_vertical($1 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION lseg($1 BOX) RETURNS LSEG;
CREATE FUNCTION lseg($1 POINT, $2 POINT) RETURNS LSEG;
CREATE FUNCTION ltrim($1 TEXT) RETURNS TEXT;
CREATE FUNCTION ltrim($1 TEXT, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION macaddr_and($1 MACADDR, $2 MACADDR) RETURNS MACADDR;
CREATE FUNCTION macaddr_cmp($1 MACADDR, $2 MACADDR) RETURNS INTEGER;
CREATE FUNCTION macaddr_eq($1 MACADDR, $2 MACADDR) RETURNS BOOLEAN;
CREATE FUNCTION macaddr_ge($1 MACADDR, $2 MACADDR) RETURNS BOOLEAN;
CREATE FUNCTION macaddr_gt($1 MACADDR, $2 MACADDR) RETURNS BOOLEAN;
CREATE FUNCTION macaddr_in($1 CSTRING) RETURNS MACADDR;
CREATE FUNCTION macaddr_le($1 MACADDR, $2 MACADDR) RETURNS BOOLEAN;
CREATE FUNCTION macaddr_lt($1 MACADDR, $2 MACADDR) RETURNS BOOLEAN;
CREATE FUNCTION macaddr_ne($1 MACADDR, $2 MACADDR) RETURNS BOOLEAN;
CREATE FUNCTION macaddr_not($1 MACADDR) RETURNS MACADDR;
CREATE FUNCTION macaddr_or($1 MACADDR, $2 MACADDR) RETURNS MACADDR;
CREATE FUNCTION macaddr_out($1 MACADDR) RETURNS CSTRING;
CREATE FUNCTION macaddr_recv($1 INTERNAL) RETURNS MACADDR;
CREATE FUNCTION macaddr_send($1 MACADDR) RETURNS BYTEA;
CREATE FUNCTION makeaclitem($1 OID, $2 OID, $3 TEXT, $4 BOOLEAN) RETURNS ACLITEM;
CREATE FUNCTION masklen($1 INET) RETURNS INTEGER;
CREATE FUNCTION max($1 ABSTIME) RETURNS ABSTIME;
CREATE FUNCTION max($1 ANYARRAY) RETURNS ANYARRAY;
CREATE FUNCTION max($1 ANYENUM) RETURNS ANYENUM;
CREATE FUNCTION max($1 BIGINT) RETURNS BIGINT;
CREATE FUNCTION max($1 CHAR) RETURNS CHAR;
CREATE FUNCTION max($1 DATE) RETURNS DATE;
CREATE FUNCTION max($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION max($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION max($1 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION max($1 MONEY) RETURNS MONEY;
CREATE FUNCTION max($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION max($1 OID) RETURNS OID;
CREATE FUNCTION max($1 REAL) RETURNS REAL;
CREATE FUNCTION max($1 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION max($1 TEXT) RETURNS TEXT;
CREATE FUNCTION max($1 TID) RETURNS TID;
CREATE FUNCTION max($1 TIME) RETURNS TIME;
CREATE FUNCTION max($1 TIME WITH TIME ZONE) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION max($1 TIMESTAMP) RETURNS TIMESTAMP;
CREATE FUNCTION max($1 TIMESTAMP WITH TIME ZONE) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION md5($1 BYTEA) RETURNS TEXT;
CREATE FUNCTION md5($1 TEXT) RETURNS TEXT;
CREATE FUNCTION mic_to_ascii($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_big5($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_euc_cn($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_euc_jp($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_euc_kr($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_euc_tw($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_iso($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_koi8r($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_latin1($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_latin2($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_latin3($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_latin4($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_sjis($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_win1250($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_win1251($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION mic_to_win866($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION min($1 ABSTIME) RETURNS ABSTIME;
CREATE FUNCTION min($1 ANYARRAY) RETURNS ANYARRAY;
CREATE FUNCTION min($1 ANYENUM) RETURNS ANYENUM;
CREATE FUNCTION min($1 BIGINT) RETURNS BIGINT;
CREATE FUNCTION min($1 CHAR) RETURNS CHAR;
CREATE FUNCTION min($1 DATE) RETURNS DATE;
CREATE FUNCTION min($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION min($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION min($1 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION min($1 MONEY) RETURNS MONEY;
CREATE FUNCTION min($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION min($1 OID) RETURNS OID;
CREATE FUNCTION min($1 REAL) RETURNS REAL;
CREATE FUNCTION min($1 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION min($1 TEXT) RETURNS TEXT;
CREATE FUNCTION min($1 TID) RETURNS TID;
CREATE FUNCTION min($1 TIME) RETURNS TIME;
CREATE FUNCTION min($1 TIME WITH TIME ZONE) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION min($1 TIMESTAMP) RETURNS TIMESTAMP;
CREATE FUNCTION min($1 TIMESTAMP WITH TIME ZONE) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION mktinterval($1 ABSTIME, $2 ABSTIME) RETURNS TINTERVAL;
CREATE FUNCTION mod($1 BIGINT, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION mod($1 INTEGER, $2 INTEGER) RETURNS INTEGER;
CREATE FUNCTION mod($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION mod($1 SMALLINT, $2 SMALLINT) RETURNS SMALLINT;
CREATE FUNCTION money($1 BIGINT) RETURNS MONEY;
CREATE FUNCTION money($1 INTEGER) RETURNS MONEY;
CREATE FUNCTION money($1 NUMERIC) RETURNS MONEY;
CREATE FUNCTION mul_d_interval($1 DOUBLE PRECISION, $2 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION name($1 CHAR) RETURNS NAME;
CREATE FUNCTION nameeq($1 NAME, $2 NAME) RETURNS BOOLEAN;
CREATE FUNCTION namege($1 NAME, $2 NAME) RETURNS BOOLEAN;
CREATE FUNCTION namegt($1 NAME, $2 NAME) RETURNS BOOLEAN;
CREATE FUNCTION nameiclike($1 NAME, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION nameicnlike($1 NAME, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION nameicregexeq($1 NAME, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION nameicregexne($1 NAME, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION namein($1 CSTRING) RETURNS NAME;
CREATE FUNCTION namele($1 NAME, $2 NAME) RETURNS BOOLEAN;
CREATE FUNCTION namelike($1 NAME, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION namelt($1 NAME, $2 NAME) RETURNS BOOLEAN;
CREATE FUNCTION namene($1 NAME, $2 NAME) RETURNS BOOLEAN;
CREATE FUNCTION namenlike($1 NAME, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION nameout($1 NAME) RETURNS CSTRING;
CREATE FUNCTION namerecv($1 INTERNAL) RETURNS NAME;
CREATE FUNCTION nameregexeq($1 NAME, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION nameregexne($1 NAME, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION namesend($1 NAME) RETURNS BYTEA;
CREATE FUNCTION name($1 TEXT) RETURNS NAME;
CREATE FUNCTION name($1 VARCHAR) RETURNS NAME;
CREATE FUNCTION neqjoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION neqsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION netmask($1 INET) RETURNS INET;
CREATE FUNCTION network_cmp($1 INET, $2 INET) RETURNS INTEGER;
CREATE FUNCTION network_eq($1 INET, $2 INET) RETURNS BOOLEAN;
CREATE FUNCTION network_ge($1 INET, $2 INET) RETURNS BOOLEAN;
CREATE FUNCTION network_gt($1 INET, $2 INET) RETURNS BOOLEAN;
CREATE FUNCTION network_le($1 INET, $2 INET) RETURNS BOOLEAN;
CREATE FUNCTION network_lt($1 INET, $2 INET) RETURNS BOOLEAN;
CREATE FUNCTION network_ne($1 INET, $2 INET) RETURNS BOOLEAN;
CREATE FUNCTION network_subeq($1 INET, $2 INET) RETURNS BOOLEAN;
CREATE FUNCTION network_sub($1 INET, $2 INET) RETURNS BOOLEAN;
CREATE FUNCTION network_supeq($1 INET, $2 INET) RETURNS BOOLEAN;
CREATE FUNCTION network_sup($1 INET, $2 INET) RETURNS BOOLEAN;
CREATE FUNCTION network($1 INET) RETURNS CIDR;
CREATE FUNCTION nextval($1 REGCLASS) RETURNS BIGINT;
CREATE FUNCTION nlikejoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION nlikesel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION notlike($1 BYTEA, $2 BYTEA) RETURNS BOOLEAN;
CREATE FUNCTION notlike($1 NAME, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION notlike($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION now() RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION npoints($1 PATH) RETURNS INTEGER;
CREATE FUNCTION npoints($1 POLYGON) RETURNS INTEGER;
CREATE FUNCTION nth_value($1 ANYELEMENT, $2 INTEGER) RETURNS ANYELEMENT;
CREATE FUNCTION ntile($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION numeric_abs($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_accum($1 NUMERIC[], $2 NUMERIC) RETURNS NUMERIC[];
CREATE FUNCTION numeric_add($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_avg_accum($1 NUMERIC[], $2 NUMERIC) RETURNS NUMERIC[];
CREATE FUNCTION numeric_avg($1 NUMERIC[]) RETURNS NUMERIC;
CREATE FUNCTION numeric_cmp($1 NUMERIC, $2 NUMERIC) RETURNS INTEGER;
CREATE FUNCTION numeric_div_trunc($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_div($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_eq($1 NUMERIC, $2 NUMERIC) RETURNS BOOLEAN;
CREATE FUNCTION numeric_exp($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_fac($1 BIGINT) RETURNS NUMERIC;
CREATE FUNCTION numeric_ge($1 NUMERIC, $2 NUMERIC) RETURNS BOOLEAN;
CREATE FUNCTION numeric_gt($1 NUMERIC, $2 NUMERIC) RETURNS BOOLEAN;
CREATE FUNCTION numeric_inc($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION numeric_larger($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_le($1 NUMERIC, $2 NUMERIC) RETURNS BOOLEAN;
CREATE FUNCTION numeric_ln($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_log($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_lt($1 NUMERIC, $2 NUMERIC) RETURNS BOOLEAN;
CREATE FUNCTION numeric_mod($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_mul($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_ne($1 NUMERIC, $2 NUMERIC) RETURNS BOOLEAN;
CREATE FUNCTION numeric_out($1 NUMERIC) RETURNS CSTRING;
CREATE FUNCTION numeric_power($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_recv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION numeric_send($1 NUMERIC) RETURNS BYTEA;
CREATE FUNCTION numeric_smaller($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_sqrt($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_stddev_pop($1 NUMERIC[]) RETURNS NUMERIC;
CREATE FUNCTION numeric_stddev_samp($1 NUMERIC[]) RETURNS NUMERIC;
CREATE FUNCTION numeric_sub($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_transform($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION numeric_uminus($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_uplus($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION numeric_var_pop($1 NUMERIC[]) RETURNS NUMERIC;
CREATE FUNCTION numeric_var_samp($1 NUMERIC[]) RETURNS NUMERIC;
CREATE FUNCTION numeric($1 BIGINT) RETURNS NUMERIC;
CREATE FUNCTION numeric($1 DOUBLE PRECISION) RETURNS NUMERIC;
CREATE FUNCTION numeric($1 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION numeric($1 MONEY) RETURNS NUMERIC;
CREATE FUNCTION numeric($1 NUMERIC, $2 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION numeric($1 REAL) RETURNS NUMERIC;
CREATE FUNCTION numeric($1 SMALLINT) RETURNS NUMERIC;
CREATE FUNCTION numerictypmodin($1 CSTRING[]) RETURNS INTEGER;
CREATE FUNCTION numerictypmodout($1 INTEGER) RETURNS CSTRING;
CREATE FUNCTION numnode($1 TSQUERY) RETURNS INTEGER;
CREATE FUNCTION numrange_subdiff($1 NUMERIC, $2 NUMERIC) RETURNS DOUBLE PRECISION;
CREATE FUNCTION numrange($1 NUMERIC, $2 NUMERIC) RETURNS NUMRANGE;
CREATE FUNCTION numrange($1 NUMERIC, $2 NUMERIC, $3 TEXT) RETURNS NUMRANGE;
CREATE FUNCTION obj_description($1 OID) RETURNS TEXT;
CREATE FUNCTION obj_description($1 OID, $2 NAME) RETURNS TEXT;
CREATE FUNCTION octet_length($1 BIT) RETURNS INTEGER;
CREATE FUNCTION octet_length($1 BYTEA) RETURNS INTEGER;
CREATE FUNCTION octet_length($1 CHAR) RETURNS INTEGER;
CREATE FUNCTION octet_length($1 TEXT) RETURNS INTEGER;
CREATE FUNCTION oid($1 BIGINT) RETURNS OID;
CREATE FUNCTION oideq($1 OID, $2 OID) RETURNS BOOLEAN;
CREATE FUNCTION oidge($1 OID, $2 OID) RETURNS BOOLEAN;
CREATE FUNCTION oidgt($1 OID, $2 OID) RETURNS BOOLEAN;
CREATE FUNCTION oidin($1 CSTRING) RETURNS OID;
CREATE FUNCTION oidlarger($1 OID, $2 OID) RETURNS OID;
CREATE FUNCTION oidle($1 OID, $2 OID) RETURNS BOOLEAN;
CREATE FUNCTION oidlt($1 OID, $2 OID) RETURNS BOOLEAN;
CREATE FUNCTION oidne($1 OID, $2 OID) RETURNS BOOLEAN;
CREATE FUNCTION oidout($1 OID) RETURNS CSTRING;
CREATE FUNCTION oidrecv($1 INTERNAL) RETURNS OID;
CREATE FUNCTION oidsend($1 OID) RETURNS BYTEA;
CREATE FUNCTION oidsmaller($1 OID, $2 OID) RETURNS OID;
CREATE FUNCTION oidvectoreq($1 OIDVECTOR, $2 OIDVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION oidvectorge($1 OIDVECTOR, $2 OIDVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION oidvectorgt($1 OIDVECTOR, $2 OIDVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION oidvectorin($1 CSTRING) RETURNS OIDVECTOR;
CREATE FUNCTION oidvectorle($1 OIDVECTOR, $2 OIDVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION oidvectorlt($1 OIDVECTOR, $2 OIDVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION oidvectorne($1 OIDVECTOR, $2 OIDVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION oidvectorout($1 OIDVECTOR) RETURNS CSTRING;
CREATE FUNCTION oidvectorrecv($1 INTERNAL) RETURNS OIDVECTOR;
CREATE FUNCTION oidvectorsend($1 OIDVECTOR) RETURNS BYTEA;
CREATE FUNCTION oidvectortypes($1 OIDVECTOR) RETURNS TEXT;
CREATE FUNCTION on_pb($1 POINT, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION on_pl($1 POINT, $2 LINE) RETURNS BOOLEAN;
CREATE FUNCTION on_ppath($1 POINT, $2 PATH) RETURNS BOOLEAN;
CREATE FUNCTION on_ps($1 POINT, $2 LSEG) RETURNS BOOLEAN;
CREATE FUNCTION on_sb($1 LSEG, $2 BOX) RETURNS BOOLEAN;
CREATE FUNCTION on_sl($1 LSEG, $2 LINE) RETURNS BOOLEAN;
CREATE FUNCTION opaque_in($1 CSTRING) RETURNS OPAQUE;
CREATE FUNCTION opaque_out($1 OPAQUE) RETURNS CSTRING;
CREATE FUNCTION "overlaps"($1 TIME WITH TIME ZONE, $2 TIME WITH TIME ZONE, $3 TIME WITH TIME ZONE, $4 TIME WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION "overlaps"($1 TIME, $2 INTERVAL, $3 TIME, $4 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION "overlaps"($1 TIME, $2 INTERVAL, $3 TIME, $4 TIME) RETURNS BOOLEAN;
CREATE FUNCTION "overlaps"($1 TIMESTAMP WITH TIME ZONE, $2 INTERVAL, $3 TIMESTAMP WITH TIME ZONE, $4 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION "overlaps"($1 TIMESTAMP WITH TIME ZONE, $2 INTERVAL, $3 TIMESTAMP WITH TIME ZONE, $4 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION "overlaps"($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE, $3 TIMESTAMP WITH TIME ZONE, $4 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION "overlaps"($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE, $3 TIMESTAMP WITH TIME ZONE, $4 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION "overlaps"($1 TIMESTAMP, $2 INTERVAL, $3 TIMESTAMP, $4 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION "overlaps"($1 TIMESTAMP, $2 INTERVAL, $3 TIMESTAMP, $4 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION "overlaps"($1 TIMESTAMP, $2 TIMESTAMP, $3 TIMESTAMP, $4 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION "overlaps"($1 TIMESTAMP, $2 TIMESTAMP, $3 TIMESTAMP, $4 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION "overlaps"($1 TIME, $2 TIME, $3 TIME, $4 INTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION "overlaps"($1 TIME, $2 TIME, $3 TIME, $4 TIME) RETURNS BOOLEAN;
CREATE FUNCTION overlay($1 BIT, $2 BIT, $3 INTEGER) RETURNS BIT;
CREATE FUNCTION overlay($1 BIT, $2 BIT, $3 INTEGER, $4 INTEGER) RETURNS BIT;
CREATE FUNCTION overlay($1 BYTEA, $2 BYTEA, $3 INTEGER) RETURNS BYTEA;
CREATE FUNCTION overlay($1 BYTEA, $2 BYTEA, $3 INTEGER, $4 INTEGER) RETURNS BYTEA;
CREATE FUNCTION overlay($1 TEXT, $2 TEXT, $3 INTEGER) RETURNS TEXT;
CREATE FUNCTION overlay($1 TEXT, $2 TEXT, $3 INTEGER, $4 INTEGER) RETURNS TEXT;
CREATE FUNCTION path_add_pt($1 PATH, $2 POINT) RETURNS PATH;
CREATE FUNCTION path_add($1 PATH, $2 PATH) RETURNS PATH;
CREATE FUNCTION path_center($1 PATH) RETURNS POINT;
CREATE FUNCTION path_contain_pt($1 PATH, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION path_distance($1 PATH, $2 PATH) RETURNS DOUBLE PRECISION;
CREATE FUNCTION path_div_pt($1 PATH, $2 POINT) RETURNS PATH;
CREATE FUNCTION path_in($1 CSTRING) RETURNS PATH;
CREATE FUNCTION path_inter($1 PATH, $2 PATH) RETURNS BOOLEAN;
CREATE FUNCTION path_length($1 PATH) RETURNS DOUBLE PRECISION;
CREATE FUNCTION path_mul_pt($1 PATH, $2 POINT) RETURNS PATH;
CREATE FUNCTION path_n_eq($1 PATH, $2 PATH) RETURNS BOOLEAN;
CREATE FUNCTION path_n_ge($1 PATH, $2 PATH) RETURNS BOOLEAN;
CREATE FUNCTION path_n_gt($1 PATH, $2 PATH) RETURNS BOOLEAN;
CREATE FUNCTION path_n_le($1 PATH, $2 PATH) RETURNS BOOLEAN;
CREATE FUNCTION path_n_lt($1 PATH, $2 PATH) RETURNS BOOLEAN;
CREATE FUNCTION path_npoints($1 PATH) RETURNS INTEGER;
CREATE FUNCTION path_out($1 PATH) RETURNS CSTRING;
CREATE FUNCTION path_recv($1 INTERNAL) RETURNS PATH;
CREATE FUNCTION path_send($1 PATH) RETURNS BYTEA;
CREATE FUNCTION path_sub_pt($1 PATH, $2 POINT) RETURNS PATH;
CREATE FUNCTION path($1 POLYGON) RETURNS PATH;
CREATE FUNCTION pclose($1 PATH) RETURNS PATH;
CREATE FUNCTION percent_rank() RETURNS DOUBLE PRECISION;
CREATE FUNCTION pg_advisory_lock_shared($1 BIGINT) RETURNS VOID;
CREATE FUNCTION pg_advisory_lock_shared($1 INTEGER, $2 INTEGER) RETURNS VOID;
CREATE FUNCTION pg_advisory_lock($1 BIGINT) RETURNS VOID;
CREATE FUNCTION pg_advisory_lock($1 INTEGER, $2 INTEGER) RETURNS VOID;
CREATE FUNCTION pg_advisory_unlock_all() RETURNS VOID;
CREATE FUNCTION pg_advisory_unlock_shared($1 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION pg_advisory_unlock_shared($1 INTEGER, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION pg_advisory_unlock($1 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION pg_advisory_unlock($1 INTEGER, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION pg_advisory_xact_lock_shared($1 BIGINT) RETURNS VOID;
CREATE FUNCTION pg_advisory_xact_lock_shared($1 INTEGER, $2 INTEGER) RETURNS VOID;
CREATE FUNCTION pg_advisory_xact_lock($1 BIGINT) RETURNS VOID;
CREATE FUNCTION pg_advisory_xact_lock($1 INTEGER, $2 INTEGER) RETURNS VOID;
CREATE FUNCTION pg_available_extension_versions(name OUT NAME, version OUT TEXT, superuser OUT BOOLEAN, relocatable OUT BOOLEAN, schema OUT NAME, requires OUT NAME[], comment OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION pg_available_extensions(name OUT NAME, default_version OUT TEXT, comment OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION pg_backend_pid() RETURNS INTEGER;
CREATE FUNCTION pg_backup_start_time() RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_cancel_backend($1 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION pg_char_to_encoding($1 NAME) RETURNS INTEGER;
CREATE FUNCTION pg_client_encoding() RETURNS NAME;
CREATE FUNCTION pg_collation_for($1 "any") RETURNS TEXT;
CREATE FUNCTION pg_collation_is_visible($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_column_is_updatable($1 REGCLASS, $2 SMALLINT, $3 BOOLEAN) RETURNS BOOLEAN;
CREATE FUNCTION pg_column_size($1 "any") RETURNS INTEGER;
CREATE FUNCTION pg_conf_load_time() RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_conversion_is_visible($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_create_restore_point($1 TEXT) RETURNS TEXT;
CREATE FUNCTION pg_current_xlog_insert_location() RETURNS TEXT;
CREATE FUNCTION pg_current_xlog_location() RETURNS TEXT;
CREATE FUNCTION pg_cursor(name OUT TEXT, statement OUT TEXT, is_holdable OUT BOOLEAN, is_binary OUT BOOLEAN, is_scrollable OUT BOOLEAN, creation_time OUT TIMESTAMP WITH TIME ZONE) RETURNS SETOF RECORD;
CREATE FUNCTION pg_database_size($1 NAME) RETURNS BIGINT;
CREATE FUNCTION pg_database_size($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_describe_object($1 OID, $2 OID, $3 INTEGER) RETURNS TEXT;
CREATE FUNCTION pg_encoding_max_length($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION pg_encoding_to_char($1 INTEGER) RETURNS NAME;
CREATE FUNCTION pg_event_trigger_dropped_objects(classid OUT OID, objid OUT OID, objsubid OUT INTEGER, object_type OUT TEXT, schema_name OUT TEXT, object_name OUT TEXT, object_identity OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION pg_export_snapshot() RETURNS TEXT;
CREATE FUNCTION pg_extension_config_dump($1 REGCLASS, $2 TEXT) RETURNS VOID;
CREATE FUNCTION pg_extension_update_paths(name NAME, source OUT TEXT, target OUT TEXT, path OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION pg_function_is_visible($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_get_constraintdef($1 OID) RETURNS TEXT;
CREATE FUNCTION pg_get_constraintdef($1 OID, $2 BOOLEAN) RETURNS TEXT;
CREATE FUNCTION pg_get_expr($1 PG_NODE_TREE, $2 OID) RETURNS TEXT;
CREATE FUNCTION pg_get_expr($1 PG_NODE_TREE, $2 OID, $3 BOOLEAN) RETURNS TEXT;
CREATE FUNCTION pg_get_function_arguments($1 OID) RETURNS TEXT;
CREATE FUNCTION pg_get_function_identity_arguments($1 OID) RETURNS TEXT;
CREATE FUNCTION pg_get_function_result($1 OID) RETURNS TEXT;
CREATE FUNCTION pg_get_functiondef($1 OID) RETURNS TEXT;
CREATE FUNCTION pg_get_indexdef($1 OID) RETURNS TEXT;
CREATE FUNCTION pg_get_indexdef($1 OID, $2 INTEGER, $3 BOOLEAN) RETURNS TEXT;
CREATE FUNCTION pg_get_keywords(word OUT TEXT, catcode OUT "char", catdesc OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION pg_get_multixact_members(multixid XID, xid OUT XID, mode OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION pg_get_ruledef($1 OID) RETURNS TEXT;
CREATE FUNCTION pg_get_ruledef($1 OID, $2 BOOLEAN) RETURNS TEXT;
CREATE FUNCTION pg_get_serial_sequence($1 TEXT, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION pg_get_triggerdef($1 OID) RETURNS TEXT;
CREATE FUNCTION pg_get_triggerdef($1 OID, $2 BOOLEAN) RETURNS TEXT;
CREATE FUNCTION pg_get_userbyid($1 OID) RETURNS NAME;
CREATE FUNCTION pg_get_viewdef($1 OID) RETURNS TEXT;
CREATE FUNCTION pg_get_viewdef($1 OID, $2 BOOLEAN) RETURNS TEXT;
CREATE FUNCTION pg_get_viewdef($1 OID, $2 INTEGER) RETURNS TEXT;
CREATE FUNCTION pg_get_viewdef($1 TEXT) RETURNS TEXT;
CREATE FUNCTION pg_get_viewdef($1 TEXT, $2 BOOLEAN) RETURNS TEXT;
CREATE FUNCTION pg_has_role($1 NAME, $2 NAME, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION pg_has_role($1 NAME, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION pg_has_role($1 NAME, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION pg_has_role($1 OID, $2 NAME, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION pg_has_role($1 OID, $2 OID, $3 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION pg_has_role($1 OID, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION pg_identify_object(classid OID, objid OID, subobjid INTEGER, type OUT TEXT, schema OUT TEXT, name OUT TEXT, identity OUT TEXT) RETURNS RECORD;
CREATE FUNCTION pg_indexes_size($1 REGCLASS) RETURNS BIGINT;
CREATE FUNCTION pg_is_in_backup() RETURNS BOOLEAN;
CREATE FUNCTION pg_is_in_recovery() RETURNS BOOLEAN;
CREATE FUNCTION pg_is_other_temp_schema($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_is_xlog_replay_paused() RETURNS BOOLEAN;
CREATE FUNCTION pg_last_xact_replay_timestamp() RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_last_xlog_receive_location() RETURNS TEXT;
CREATE FUNCTION pg_last_xlog_replay_location() RETURNS TEXT;
CREATE FUNCTION pg_listening_channels() RETURNS SETOF TEXT;
CREATE FUNCTION pg_lock_status(locktype OUT TEXT, database OUT OID, relation OUT OID, page OUT INTEGER, tuple OUT SMALLINT, virtualxid OUT TEXT, transactionid OUT XID, classid OUT OID, objid OUT OID, objsubid OUT SMALLINT, virtualtransaction OUT TEXT, pid OUT INTEGER, mode OUT TEXT, granted OUT BOOLEAN, fastpath OUT BOOLEAN) RETURNS SETOF RECORD;
CREATE FUNCTION pg_ls_dir($1 TEXT) RETURNS SETOF TEXT;
CREATE FUNCTION pg_my_temp_schema() RETURNS OID;
CREATE FUNCTION pg_node_tree_in($1 CSTRING) RETURNS PG_NODE_TREE;
CREATE FUNCTION pg_node_tree_out($1 PG_NODE_TREE) RETURNS CSTRING;
CREATE FUNCTION pg_node_tree_recv($1 INTERNAL) RETURNS PG_NODE_TREE;
CREATE FUNCTION pg_node_tree_send($1 PG_NODE_TREE) RETURNS BYTEA;
CREATE FUNCTION pg_notify($1 TEXT, $2 TEXT) RETURNS VOID;
CREATE FUNCTION pg_opclass_is_visible($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_operator_is_visible($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_opfamily_is_visible($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_options_to_table(options_array TEXT[], option_name OUT TEXT, option_value OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION pg_postmaster_start_time() RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_prepared_statement(name OUT TEXT, statement OUT TEXT, prepare_time OUT TIMESTAMP WITH TIME ZONE, parameter_types OUT REGTYPE[], from_sql OUT BOOLEAN) RETURNS SETOF RECORD;
CREATE FUNCTION pg_prepared_xact(transaction OUT XID, gid OUT TEXT, prepared OUT TIMESTAMP WITH TIME ZONE, ownerid OUT OID, dbid OUT OID) RETURNS SETOF RECORD;
CREATE FUNCTION pg_read_binary_file($1 TEXT) RETURNS BYTEA;
CREATE FUNCTION pg_read_binary_file($1 TEXT, $2 BIGINT, $3 BIGINT) RETURNS BYTEA;
CREATE FUNCTION pg_read_file($1 TEXT) RETURNS TEXT;
CREATE FUNCTION pg_read_file($1 TEXT, $2 BIGINT, $3 BIGINT) RETURNS TEXT;
CREATE FUNCTION pg_relation_filenode($1 REGCLASS) RETURNS OID;
CREATE FUNCTION pg_relation_filepath($1 REGCLASS) RETURNS TEXT;
CREATE FUNCTION pg_relation_is_updatable($1 REGCLASS, $2 BOOLEAN) RETURNS INTEGER;
CREATE FUNCTION pg_relation_size($1 REGCLASS) RETURNS BIGINT;
CREATE FUNCTION pg_relation_size($1 REGCLASS, $2 TEXT) RETURNS BIGINT;
CREATE FUNCTION pg_reload_conf() RETURNS BOOLEAN;
CREATE FUNCTION pg_rotate_logfile() RETURNS BOOLEAN;
CREATE FUNCTION pg_sequence_parameters(sequence_oid OID, start_value OUT BIGINT, minimum_value OUT BIGINT, maximum_value OUT BIGINT, increment OUT BIGINT, cycle_option OUT BOOLEAN) RETURNS RECORD;
CREATE FUNCTION pg_show_all_settings(name OUT TEXT, setting OUT TEXT, unit OUT TEXT, category OUT TEXT, short_desc OUT TEXT, extra_desc OUT TEXT, context OUT TEXT, vartype OUT TEXT, source OUT TEXT, min_val OUT TEXT, max_val OUT TEXT, enumvals OUT TEXT[], boot_val OUT TEXT, reset_val OUT TEXT, sourcefile OUT TEXT, sourceline OUT INTEGER) RETURNS SETOF RECORD;
CREATE FUNCTION pg_size_pretty($1 BIGINT) RETURNS TEXT;
CREATE FUNCTION pg_size_pretty($1 NUMERIC) RETURNS TEXT;
CREATE FUNCTION pg_sleep($1 DOUBLE PRECISION) RETURNS VOID;
CREATE FUNCTION pg_start_backup(label TEXT, fast BOOLEAN) RETURNS TEXT;
CREATE FUNCTION pg_stat_clear_snapshot() RETURNS VOID;
CREATE FUNCTION pg_stat_file(filename TEXT, size OUT BIGINT, access OUT TIMESTAMP WITH TIME ZONE, modification OUT TIMESTAMP WITH TIME ZONE, change OUT TIMESTAMP WITH TIME ZONE, creation OUT TIMESTAMP WITH TIME ZONE, isdir OUT BOOLEAN) RETURNS RECORD;
CREATE FUNCTION pg_stat_get_activity(pid OUT INTEGER, datid OUT OID, usesysid OUT OID, application_name OUT TEXT, state OUT TEXT, query OUT TEXT, waiting OUT BOOLEAN, xact_start OUT TIMESTAMP WITH TIME ZONE, query_start OUT TIMESTAMP WITH TIME ZONE, backend_start OUT TIMESTAMP WITH TIME ZONE, state_change OUT TIMESTAMP WITH TIME ZONE, client_addr OUT INET, client_hostname OUT TEXT, client_port OUT INTEGER) RETURNS SETOF RECORD;
CREATE FUNCTION pg_stat_get_analyze_count($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_autoanalyze_count($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_autovacuum_count($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_backend_activity_start($1 INTEGER) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_stat_get_backend_activity($1 INTEGER) RETURNS TEXT;
CREATE FUNCTION pg_stat_get_backend_client_addr($1 INTEGER) RETURNS INET;
CREATE FUNCTION pg_stat_get_backend_client_port($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION pg_stat_get_backend_dbid($1 INTEGER) RETURNS OID;
CREATE FUNCTION pg_stat_get_backend_idset() RETURNS SETOF INTEGER;
CREATE FUNCTION pg_stat_get_backend_pid($1 INTEGER) RETURNS INTEGER;
CREATE FUNCTION pg_stat_get_backend_start($1 INTEGER) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_stat_get_backend_userid($1 INTEGER) RETURNS OID;
CREATE FUNCTION pg_stat_get_backend_waiting($1 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION pg_stat_get_backend_xact_start($1 INTEGER) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_stat_get_bgwriter_buf_written_checkpoints() RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_bgwriter_buf_written_clean() RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_bgwriter_maxwritten_clean() RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_bgwriter_requested_checkpoints() RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_bgwriter_stat_reset_time() RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_stat_get_bgwriter_timed_checkpoints() RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_blocks_fetched($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_blocks_hit($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_buf_alloc() RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_buf_fsync_backend() RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_buf_written_backend() RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_checkpoint_sync_time() RETURNS DOUBLE PRECISION;
CREATE FUNCTION pg_stat_get_checkpoint_write_time() RETURNS DOUBLE PRECISION;
CREATE FUNCTION pg_stat_get_db_blk_read_time($1 OID) RETURNS DOUBLE PRECISION;
CREATE FUNCTION pg_stat_get_db_blk_write_time($1 OID) RETURNS DOUBLE PRECISION;
CREATE FUNCTION pg_stat_get_db_blocks_fetched($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_blocks_hit($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_conflict_all($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_conflict_bufferpin($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_conflict_lock($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_conflict_snapshot($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_conflict_startup_deadlock($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_conflict_tablespace($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_deadlocks($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_numbackends($1 OID) RETURNS INTEGER;
CREATE FUNCTION pg_stat_get_db_stat_reset_time($1 OID) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_stat_get_db_temp_bytes($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_temp_files($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_tuples_deleted($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_tuples_fetched($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_tuples_inserted($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_tuples_returned($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_tuples_updated($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_xact_commit($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_db_xact_rollback($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_dead_tuples($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_function_calls($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_function_self_time($1 OID) RETURNS DOUBLE PRECISION;
CREATE FUNCTION pg_stat_get_function_total_time($1 OID) RETURNS DOUBLE PRECISION;
CREATE FUNCTION pg_stat_get_last_analyze_time($1 OID) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_stat_get_last_autoanalyze_time($1 OID) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_stat_get_last_autovacuum_time($1 OID) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_stat_get_last_vacuum_time($1 OID) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION pg_stat_get_live_tuples($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_numscans($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_tuples_deleted($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_tuples_fetched($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_tuples_hot_updated($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_tuples_inserted($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_tuples_returned($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_tuples_updated($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_vacuum_count($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_wal_senders(pid OUT INTEGER, state OUT TEXT, sent_location OUT TEXT, write_location OUT TEXT, flush_location OUT TEXT, replay_location OUT TEXT, sync_priority OUT INTEGER, sync_state OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION pg_stat_get_xact_blocks_fetched($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_xact_blocks_hit($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_xact_function_calls($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_xact_function_self_time($1 OID) RETURNS DOUBLE PRECISION;
CREATE FUNCTION pg_stat_get_xact_function_total_time($1 OID) RETURNS DOUBLE PRECISION;
CREATE FUNCTION pg_stat_get_xact_numscans($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_xact_tuples_deleted($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_xact_tuples_fetched($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_xact_tuples_hot_updated($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_xact_tuples_inserted($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_xact_tuples_returned($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_get_xact_tuples_updated($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_stat_reset() RETURNS VOID;
CREATE FUNCTION pg_stat_reset_shared($1 TEXT) RETURNS VOID;
CREATE FUNCTION pg_stat_reset_single_function_counters($1 OID) RETURNS VOID;
CREATE FUNCTION pg_stat_reset_single_table_counters($1 OID) RETURNS VOID;
CREATE FUNCTION pg_stop_backup() RETURNS TEXT;
CREATE FUNCTION pg_switch_xlog() RETURNS TEXT;
CREATE FUNCTION pg_table_is_visible($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_table_size($1 REGCLASS) RETURNS BIGINT;
CREATE FUNCTION pg_tablespace_databases($1 OID) RETURNS SETOF OID;
CREATE FUNCTION pg_tablespace_location($1 OID) RETURNS TEXT;
CREATE FUNCTION pg_tablespace_size($1 NAME) RETURNS BIGINT;
CREATE FUNCTION pg_tablespace_size($1 OID) RETURNS BIGINT;
CREATE FUNCTION pg_terminate_backend($1 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION pg_timezone_abbrevs(abbrev OUT TEXT, utc_offset OUT INTERVAL, is_dst OUT BOOLEAN) RETURNS SETOF RECORD;
CREATE FUNCTION pg_timezone_names(name OUT TEXT, abbrev OUT TEXT, utc_offset OUT INTERVAL, is_dst OUT BOOLEAN) RETURNS SETOF RECORD;
CREATE FUNCTION pg_total_relation_size($1 REGCLASS) RETURNS BIGINT;
CREATE FUNCTION pg_trigger_depth() RETURNS INTEGER;
CREATE FUNCTION pg_try_advisory_lock_shared($1 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION pg_try_advisory_lock_shared($1 INTEGER, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION pg_try_advisory_lock($1 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION pg_try_advisory_lock($1 INTEGER, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION pg_try_advisory_xact_lock_shared($1 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION pg_try_advisory_xact_lock_shared($1 INTEGER, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION pg_try_advisory_xact_lock($1 BIGINT) RETURNS BOOLEAN;
CREATE FUNCTION pg_try_advisory_xact_lock($1 INTEGER, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION pg_ts_config_is_visible($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_ts_dict_is_visible($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_ts_parser_is_visible($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_ts_template_is_visible($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_type_is_visible($1 OID) RETURNS BOOLEAN;
CREATE FUNCTION pg_typeof($1 "any") RETURNS REGTYPE;
CREATE FUNCTION pg_xlog_location_diff($1 TEXT, $2 TEXT) RETURNS NUMERIC;
CREATE FUNCTION pg_xlog_replay_pause() RETURNS VOID;
CREATE FUNCTION pg_xlog_replay_resume() RETURNS VOID;
CREATE FUNCTION pg_xlogfile_name_offset(wal_location TEXT, file_name OUT TEXT, file_offset OUT INTEGER) RETURNS RECORD;
CREATE FUNCTION pg_xlogfile_name($1 TEXT) RETURNS TEXT;
CREATE FUNCTION pi() RETURNS DOUBLE PRECISION;
CREATE FUNCTION plainto_tsquery($1 REGCONFIG, $2 TEXT) RETURNS TSQUERY;
CREATE FUNCTION plainto_tsquery($1 TEXT) RETURNS TSQUERY;
CREATE FUNCTION plpgsql_call_handler() RETURNS LANGUAGE_HANDLER;
CREATE FUNCTION plpgsql_inline_handler($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION plpgsql_validator($1 OID) RETURNS VOID;
CREATE FUNCTION point_above($1 POINT, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION point_add($1 POINT, $2 POINT) RETURNS POINT;
CREATE FUNCTION point_below($1 POINT, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION point_distance($1 POINT, $2 POINT) RETURNS DOUBLE PRECISION;
CREATE FUNCTION point_div($1 POINT, $2 POINT) RETURNS POINT;
CREATE FUNCTION point_eq($1 POINT, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION point_horiz($1 POINT, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION point_in($1 CSTRING) RETURNS POINT;
CREATE FUNCTION point_left($1 POINT, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION point_mul($1 POINT, $2 POINT) RETURNS POINT;
CREATE FUNCTION point_ne($1 POINT, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION point_out($1 POINT) RETURNS CSTRING;
CREATE FUNCTION point_recv($1 INTERNAL) RETURNS POINT;
CREATE FUNCTION point_right($1 POINT, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION point_send($1 POINT) RETURNS BYTEA;
CREATE FUNCTION point_sub($1 POINT, $2 POINT) RETURNS POINT;
CREATE FUNCTION point_vert($1 POINT, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION point($1 BOX) RETURNS POINT;
CREATE FUNCTION point($1 CIRCLE) RETURNS POINT;
CREATE FUNCTION point($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS POINT;
CREATE FUNCTION point($1 LSEG) RETURNS POINT;
CREATE FUNCTION point($1 PATH) RETURNS POINT;
CREATE FUNCTION point($1 POLYGON) RETURNS POINT;
CREATE FUNCTION poly_above($1 POLYGON, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION poly_below($1 POLYGON, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION poly_center($1 POLYGON) RETURNS POINT;
CREATE FUNCTION poly_contain_pt($1 POLYGON, $2 POINT) RETURNS BOOLEAN;
CREATE FUNCTION poly_contained($1 POLYGON, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION poly_contain($1 POLYGON, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION poly_distance($1 POLYGON, $2 POLYGON) RETURNS DOUBLE PRECISION;
CREATE FUNCTION poly_in($1 CSTRING) RETURNS POLYGON;
CREATE FUNCTION poly_left($1 POLYGON, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION poly_npoints($1 POLYGON) RETURNS INTEGER;
CREATE FUNCTION poly_out($1 POLYGON) RETURNS CSTRING;
CREATE FUNCTION poly_overabove($1 POLYGON, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION poly_overbelow($1 POLYGON, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION poly_overlap($1 POLYGON, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION poly_overleft($1 POLYGON, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION poly_overright($1 POLYGON, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION poly_recv($1 INTERNAL) RETURNS POLYGON;
CREATE FUNCTION poly_right($1 POLYGON, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION poly_same($1 POLYGON, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION poly_send($1 POLYGON) RETURNS BYTEA;
CREATE FUNCTION polygon($1 BOX) RETURNS POLYGON;
CREATE FUNCTION polygon($1 CIRCLE) RETURNS POLYGON;
CREATE FUNCTION polygon($1 INTEGER, $2 CIRCLE) RETURNS POLYGON;
CREATE FUNCTION polygon($1 PATH) RETURNS POLYGON;
CREATE FUNCTION popen($1 PATH) RETURNS PATH;
CREATE FUNCTION position($1 BIT, $2 BIT) RETURNS INTEGER;
CREATE FUNCTION position($1 BYTEA, $2 BYTEA) RETURNS INTEGER;
CREATE FUNCTION positionjoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION positionsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION position($1 TEXT, $2 TEXT) RETURNS INTEGER;
CREATE FUNCTION postgresql_fdw_validator($1 TEXT[], $2 OID) RETURNS BOOLEAN;
CREATE FUNCTION pow($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION power($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION power($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION pow($1 NUMERIC, $2 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION prsd_end($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION prsd_headline($1 INTERNAL, $2 INTERNAL, $3 TSQUERY) RETURNS INTERNAL;
CREATE FUNCTION prsd_lextype($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION prsd_nexttoken($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION prsd_start($1 INTERNAL, $2 INTEGER) RETURNS INTERNAL;
CREATE FUNCTION pt_contained_circle($1 POINT, $2 CIRCLE) RETURNS BOOLEAN;
CREATE FUNCTION pt_contained_poly($1 POINT, $2 POLYGON) RETURNS BOOLEAN;
CREATE FUNCTION query_to_xml_and_xmlschema(query TEXT, nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION query_to_xmlschema(query TEXT, nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION query_to_xml(query TEXT, nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION querytree($1 TSQUERY) RETURNS TEXT;
CREATE FUNCTION quote_ident($1 TEXT) RETURNS TEXT;
CREATE FUNCTION quote_literal($1 ANYELEMENT) RETURNS TEXT;
CREATE FUNCTION quote_literal($1 TEXT) RETURNS TEXT;
CREATE FUNCTION quote_nullable($1 ANYELEMENT) RETURNS TEXT;
CREATE FUNCTION quote_nullable($1 TEXT) RETURNS TEXT;
CREATE FUNCTION radians($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION radius($1 CIRCLE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION random() RETURNS DOUBLE PRECISION;
CREATE FUNCTION range_adjacent($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_after($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_before($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_cmp($1 ANYRANGE, $2 ANYRANGE) RETURNS INTEGER;
CREATE FUNCTION range_contained_by($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_contains_elem($1 ANYRANGE, $2 ANYELEMENT) RETURNS BOOLEAN;
CREATE FUNCTION range_contains($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_eq($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_ge($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_gist_compress($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION range_gist_consistent($1 INTERNAL, $2 ANYRANGE, $3 INTEGER, $4 OID, $5 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION range_gist_decompress($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION range_gist_penalty($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION range_gist_picksplit($1 INTERNAL, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION range_gist_same($1 ANYRANGE, $2 ANYRANGE, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION range_gist_union($1 INTERNAL, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION range_gt($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS ANYRANGE;
CREATE FUNCTION range_intersect($1 ANYRANGE, $2 ANYRANGE) RETURNS ANYRANGE;
CREATE FUNCTION range_le($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_lt($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_minus($1 ANYRANGE, $2 ANYRANGE) RETURNS ANYRANGE;
CREATE FUNCTION range_ne($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_out($1 ANYRANGE) RETURNS CSTRING;
CREATE FUNCTION range_overlaps($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_overleft($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_overright($1 ANYRANGE, $2 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION range_recv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS ANYRANGE;
CREATE FUNCTION range_send($1 ANYRANGE) RETURNS BYTEA;
CREATE FUNCTION range_typanalyze($1 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION range_union($1 ANYRANGE, $2 ANYRANGE) RETURNS ANYRANGE;
CREATE FUNCTION rangesel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION rank() RETURNS BIGINT;
CREATE FUNCTION record_eq($1 RECORD, $2 RECORD) RETURNS BOOLEAN;
CREATE FUNCTION record_ge($1 RECORD, $2 RECORD) RETURNS BOOLEAN;
CREATE FUNCTION record_gt($1 RECORD, $2 RECORD) RETURNS BOOLEAN;
CREATE FUNCTION record_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS RECORD;
CREATE FUNCTION record_le($1 RECORD, $2 RECORD) RETURNS BOOLEAN;
CREATE FUNCTION record_lt($1 RECORD, $2 RECORD) RETURNS BOOLEAN;
CREATE FUNCTION record_ne($1 RECORD, $2 RECORD) RETURNS BOOLEAN;
CREATE FUNCTION record_out($1 RECORD) RETURNS CSTRING;
CREATE FUNCTION record_recv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS RECORD;
CREATE FUNCTION record_send($1 RECORD) RETURNS BYTEA;
CREATE FUNCTION regclassin($1 CSTRING) RETURNS REGCLASS;
CREATE FUNCTION regclassout($1 REGCLASS) RETURNS CSTRING;
CREATE FUNCTION regclassrecv($1 INTERNAL) RETURNS REGCLASS;
CREATE FUNCTION regclasssend($1 REGCLASS) RETURNS BYTEA;
CREATE FUNCTION regclass($1 TEXT) RETURNS REGCLASS;
CREATE FUNCTION regconfigin($1 CSTRING) RETURNS REGCONFIG;
CREATE FUNCTION regconfigout($1 REGCONFIG) RETURNS CSTRING;
CREATE FUNCTION regconfigrecv($1 INTERNAL) RETURNS REGCONFIG;
CREATE FUNCTION regconfigsend($1 REGCONFIG) RETURNS BYTEA;
CREATE FUNCTION regdictionaryin($1 CSTRING) RETURNS REGDICTIONARY;
CREATE FUNCTION regdictionaryout($1 REGDICTIONARY) RETURNS CSTRING;
CREATE FUNCTION regdictionaryrecv($1 INTERNAL) RETURNS REGDICTIONARY;
CREATE FUNCTION regdictionarysend($1 REGDICTIONARY) RETURNS BYTEA;
CREATE FUNCTION regexeqjoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION regexeqsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION regexnejoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION regexnesel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION regexp_matches($1 TEXT, $2 TEXT) RETURNS SETOF TEXT[];
CREATE FUNCTION regexp_matches($1 TEXT, $2 TEXT, $3 TEXT) RETURNS SETOF TEXT[];
CREATE FUNCTION regexp_replace($1 TEXT, $2 TEXT, $3 TEXT) RETURNS TEXT;
CREATE FUNCTION regexp_replace($1 TEXT, $2 TEXT, $3 TEXT, $4 TEXT) RETURNS TEXT;
CREATE FUNCTION regexp_split_to_array($1 TEXT, $2 TEXT) RETURNS TEXT[];
CREATE FUNCTION regexp_split_to_array($1 TEXT, $2 TEXT, $3 TEXT) RETURNS TEXT[];
CREATE FUNCTION regexp_split_to_table($1 TEXT, $2 TEXT) RETURNS SETOF TEXT;
CREATE FUNCTION regexp_split_to_table($1 TEXT, $2 TEXT, $3 TEXT) RETURNS SETOF TEXT;
CREATE FUNCTION regoperatorin($1 CSTRING) RETURNS REGOPERATOR;
CREATE FUNCTION regoperatorout($1 REGOPERATOR) RETURNS CSTRING;
CREATE FUNCTION regoperatorrecv($1 INTERNAL) RETURNS REGOPERATOR;
CREATE FUNCTION regoperatorsend($1 REGOPERATOR) RETURNS BYTEA;
CREATE FUNCTION regoperin($1 CSTRING) RETURNS REGOPER;
CREATE FUNCTION regoperout($1 REGOPER) RETURNS CSTRING;
CREATE FUNCTION regoperrecv($1 INTERNAL) RETURNS REGOPER;
CREATE FUNCTION regopersend($1 REGOPER) RETURNS BYTEA;
CREATE FUNCTION regprocedurein($1 CSTRING) RETURNS REGPROCEDURE;
CREATE FUNCTION regprocedureout($1 REGPROCEDURE) RETURNS CSTRING;
CREATE FUNCTION regprocedurerecv($1 INTERNAL) RETURNS REGPROCEDURE;
CREATE FUNCTION regproceduresend($1 REGPROCEDURE) RETURNS BYTEA;
CREATE FUNCTION regprocin($1 CSTRING) RETURNS REGPROC;
CREATE FUNCTION regprocout($1 REGPROC) RETURNS CSTRING;
CREATE FUNCTION regprocrecv($1 INTERNAL) RETURNS REGPROC;
CREATE FUNCTION regprocsend($1 REGPROC) RETURNS BYTEA;
CREATE FUNCTION regr_avgx($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION regr_avgy($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION regr_count($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS BIGINT;
CREATE FUNCTION regr_intercept($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION regr_r2($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION regr_slope($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION regr_sxx($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION regr_sxy($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION regr_syy($1 DOUBLE PRECISION, $2 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION regtypein($1 CSTRING) RETURNS REGTYPE;
CREATE FUNCTION regtypeout($1 REGTYPE) RETURNS CSTRING;
CREATE FUNCTION regtyperecv($1 INTERNAL) RETURNS REGTYPE;
CREATE FUNCTION regtypesend($1 REGTYPE) RETURNS BYTEA;
CREATE FUNCTION reltimeeq($1 RELTIME, $2 RELTIME) RETURNS BOOLEAN;
CREATE FUNCTION reltimege($1 RELTIME, $2 RELTIME) RETURNS BOOLEAN;
CREATE FUNCTION reltimegt($1 RELTIME, $2 RELTIME) RETURNS BOOLEAN;
CREATE FUNCTION reltimein($1 CSTRING) RETURNS RELTIME;
CREATE FUNCTION reltime($1 INTERVAL) RETURNS RELTIME;
CREATE FUNCTION reltimele($1 RELTIME, $2 RELTIME) RETURNS BOOLEAN;
CREATE FUNCTION reltimelt($1 RELTIME, $2 RELTIME) RETURNS BOOLEAN;
CREATE FUNCTION reltimene($1 RELTIME, $2 RELTIME) RETURNS BOOLEAN;
CREATE FUNCTION reltimeout($1 RELTIME) RETURNS CSTRING;
CREATE FUNCTION reltimerecv($1 INTERNAL) RETURNS RELTIME;
CREATE FUNCTION reltimesend($1 RELTIME) RETURNS BYTEA;
CREATE FUNCTION repeat($1 TEXT, $2 INTEGER) RETURNS TEXT;
CREATE FUNCTION replace($1 TEXT, $2 TEXT, $3 TEXT) RETURNS TEXT;
CREATE FUNCTION reverse($1 TEXT) RETURNS TEXT;
CREATE FUNCTION "right"($1 TEXT, $2 INTEGER) RETURNS TEXT;
CREATE FUNCTION round($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION round($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION round($1 NUMERIC, $2 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION row_number() RETURNS BIGINT;
CREATE FUNCTION row_to_json($1 RECORD) RETURNS JSON;
CREATE FUNCTION row_to_json($1 RECORD, $2 BOOLEAN) RETURNS JSON;
CREATE FUNCTION rpad($1 TEXT, $2 INTEGER) RETURNS TEXT;
CREATE FUNCTION rpad($1 TEXT, $2 INTEGER, $3 TEXT) RETURNS TEXT;
CREATE FUNCTION rtrim($1 TEXT) RETURNS TEXT;
CREATE FUNCTION rtrim($1 TEXT, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION scalargtjoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION scalargtsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION scalarltjoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION scalarltsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION schema_to_xml_and_xmlschema(schema NAME, nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION schema_to_xml(schema NAME, nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION schema_to_xmlschema(schema NAME, nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION "session_user"() RETURNS NAME;
CREATE FUNCTION set_bit($1 BIT, $2 INTEGER, $3 INTEGER) RETURNS BIT;
CREATE FUNCTION set_bit($1 BYTEA, $2 INTEGER, $3 INTEGER) RETURNS BYTEA;
CREATE FUNCTION set_byte($1 BYTEA, $2 INTEGER, $3 INTEGER) RETURNS BYTEA;
CREATE FUNCTION set_config($1 TEXT, $2 TEXT, $3 BOOLEAN) RETURNS TEXT;
CREATE FUNCTION set_masklen($1 CIDR, $2 INTEGER) RETURNS CIDR;
CREATE FUNCTION set_masklen($1 INET, $2 INTEGER) RETURNS INET;
CREATE FUNCTION setseed($1 DOUBLE PRECISION) RETURNS VOID;
CREATE FUNCTION setval($1 REGCLASS, $2 BIGINT) RETURNS BIGINT;
CREATE FUNCTION setval($1 REGCLASS, $2 BIGINT, $3 BOOLEAN) RETURNS BIGINT;
CREATE FUNCTION setweight($1 TSVECTOR, $2 "char") RETURNS TSVECTOR;
CREATE FUNCTION shell_in($1 CSTRING) RETURNS OPAQUE;
CREATE FUNCTION shell_out($1 OPAQUE) RETURNS CSTRING;
CREATE FUNCTION shift_jis_2004_to_euc_jis_2004($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION shift_jis_2004_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION shobj_description($1 OID, $2 NAME) RETURNS TEXT;
CREATE FUNCTION sign($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION sign($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION similar_escape($1 TEXT, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION sin($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION sjis_to_euc_jp($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION sjis_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION sjis_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION slope($1 POINT, $2 POINT) RETURNS DOUBLE PRECISION;
CREATE FUNCTION smgreq($1 SMGR, $2 SMGR) RETURNS BOOLEAN;
CREATE FUNCTION smgrin($1 CSTRING) RETURNS SMGR;
CREATE FUNCTION smgrne($1 SMGR, $2 SMGR) RETURNS BOOLEAN;
CREATE FUNCTION smgrout($1 SMGR) RETURNS CSTRING;
CREATE FUNCTION spg_kd_choose($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_kd_config($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_kd_inner_consistent($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_kd_picksplit($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_quad_choose($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_quad_config($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_quad_inner_consistent($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_quad_leaf_consistent($1 INTERNAL, $2 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION spg_quad_picksplit($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_range_quad_choose($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_range_quad_config($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_range_quad_inner_consistent($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_range_quad_leaf_consistent($1 INTERNAL, $2 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION spg_range_quad_picksplit($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_text_choose($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_text_config($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_text_inner_consistent($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spg_text_leaf_consistent($1 INTERNAL, $2 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION spg_text_picksplit($1 INTERNAL, $2 INTERNAL) RETURNS VOID;
CREATE FUNCTION spgbeginscan($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION spgbuildempty($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION spgbuild($1 INTERNAL, $2 INTERNAL, $3 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION spgbulkdelete($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION spgcanreturn($1 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION spgcostestimate($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL, $6 INTERNAL, $7 INTERNAL) RETURNS VOID;
CREATE FUNCTION spgendscan($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION spggetbitmap($1 INTERNAL, $2 INTERNAL) RETURNS BIGINT;
CREATE FUNCTION spggettuple($1 INTERNAL, $2 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION spginsert($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL, $6 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION spgmarkpos($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION spgoptions($1 TEXT[], $2 BOOLEAN) RETURNS BYTEA;
CREATE FUNCTION spgrescan($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL, $5 INTERNAL) RETURNS VOID;
CREATE FUNCTION spgrestrpos($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION spgvacuumcleanup($1 INTERNAL, $2 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION split_part($1 TEXT, $2 TEXT, $3 INTEGER) RETURNS TEXT;
CREATE FUNCTION sqrt($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION sqrt($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION statement_timestamp() RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION stddev_pop($1 BIGINT) RETURNS NUMERIC;
CREATE FUNCTION stddev_pop($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION stddev_pop($1 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION stddev_pop($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION stddev_pop($1 REAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION stddev_pop($1 SMALLINT) RETURNS NUMERIC;
CREATE FUNCTION stddev_samp($1 BIGINT) RETURNS NUMERIC;
CREATE FUNCTION stddev_samp($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION stddev_samp($1 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION stddev_samp($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION stddev_samp($1 REAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION stddev_samp($1 SMALLINT) RETURNS NUMERIC;
CREATE FUNCTION stddev($1 BIGINT) RETURNS NUMERIC;
CREATE FUNCTION stddev($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION stddev($1 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION stddev($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION stddev($1 REAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION stddev($1 SMALLINT) RETURNS NUMERIC;
CREATE FUNCTION string_agg_finalfn($1 INTERNAL) RETURNS TEXT;
CREATE FUNCTION string_agg_transfn($1 INTERNAL, $2 TEXT, $3 TEXT) RETURNS INTERNAL;
CREATE FUNCTION string_agg($1 BYTEA, $2 BYTEA) RETURNS BYTEA;
CREATE FUNCTION string_agg($1 TEXT, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION string_to_array($1 TEXT, $2 TEXT) RETURNS TEXT[];
CREATE FUNCTION string_to_array($1 TEXT, $2 TEXT, $3 TEXT) RETURNS TEXT[];
CREATE FUNCTION strip($1 TSVECTOR) RETURNS TSVECTOR;
CREATE FUNCTION strpos($1 TEXT, $2 TEXT) RETURNS INTEGER;
CREATE FUNCTION substr($1 BYTEA, $2 INTEGER) RETURNS BYTEA;
CREATE FUNCTION substr($1 BYTEA, $2 INTEGER, $3 INTEGER) RETURNS BYTEA;
CREATE FUNCTION substring($1 BIT, $2 INTEGER) RETURNS BIT;
CREATE FUNCTION substring($1 BIT, $2 INTEGER, $3 INTEGER) RETURNS BIT;
CREATE FUNCTION substring($1 BYTEA, $2 INTEGER) RETURNS BYTEA;
CREATE FUNCTION substring($1 BYTEA, $2 INTEGER, $3 INTEGER) RETURNS BYTEA;
CREATE FUNCTION substring($1 TEXT, $2 INTEGER) RETURNS TEXT;
CREATE FUNCTION substring($1 TEXT, $2 INTEGER, $3 INTEGER) RETURNS TEXT;
CREATE FUNCTION substring($1 TEXT, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION substring($1 TEXT, $2 TEXT, $3 TEXT) RETURNS TEXT;
CREATE FUNCTION substr($1 TEXT, $2 INTEGER) RETURNS TEXT;
CREATE FUNCTION substr($1 TEXT, $2 INTEGER, $3 INTEGER) RETURNS TEXT;
CREATE FUNCTION sum($1 BIGINT) RETURNS NUMERIC;
CREATE FUNCTION sum($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION sum($1 INTEGER) RETURNS BIGINT;
CREATE FUNCTION sum($1 INTERVAL) RETURNS INTERVAL;
CREATE FUNCTION sum($1 MONEY) RETURNS MONEY;
CREATE FUNCTION sum($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION sum($1 REAL) RETURNS REAL;
CREATE FUNCTION sum($1 SMALLINT) RETURNS BIGINT;
CREATE FUNCTION suppress_redundant_updates_trigger() RETURNS TRIGGER;
CREATE FUNCTION table_to_xml_and_xmlschema(tbl REGCLASS, nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION table_to_xml(tbl REGCLASS, nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION table_to_xmlschema(tbl REGCLASS, nulls BOOLEAN, tableforest BOOLEAN, targetns TEXT) RETURNS XML;
CREATE FUNCTION tan($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION text($1 "char") RETURNS TEXT;
CREATE FUNCTION text_ge($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION text_gt($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION text_larger($1 TEXT, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION text_le($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION text_lt($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION text_pattern_ge($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION text_pattern_gt($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION text_pattern_le($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION text_pattern_lt($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION text_smaller($1 TEXT, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION textanycat($1 TEXT, $2 ANYNONARRAY) RETURNS TEXT;
CREATE FUNCTION text($1 BOOLEAN) RETURNS TEXT;
CREATE FUNCTION textcat($1 TEXT, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION text($1 CHAR) RETURNS TEXT;
CREATE FUNCTION texteq($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION texticlike($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION texticnlike($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION texticregexeq($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION texticregexne($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION textin($1 CSTRING) RETURNS TEXT;
CREATE FUNCTION text($1 INET) RETURNS TEXT;
CREATE FUNCTION textlen($1 TEXT) RETURNS INTEGER;
CREATE FUNCTION textlike($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION text($1 NAME) RETURNS TEXT;
CREATE FUNCTION textne($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION textnlike($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION textout($1 TEXT) RETURNS CSTRING;
CREATE FUNCTION textrecv($1 INTERNAL) RETURNS TEXT;
CREATE FUNCTION textregexeq($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION textregexne($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION textsend($1 TEXT) RETURNS BYTEA;
CREATE FUNCTION text($1 XML) RETURNS TEXT;
CREATE FUNCTION thesaurus_init($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION thesaurus_lexize($1 INTERNAL, $2 INTERNAL, $3 INTERNAL, $4 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION tideq($1 TID, $2 TID) RETURNS BOOLEAN;
CREATE FUNCTION tidge($1 TID, $2 TID) RETURNS BOOLEAN;
CREATE FUNCTION tidgt($1 TID, $2 TID) RETURNS BOOLEAN;
CREATE FUNCTION tidin($1 CSTRING) RETURNS TID;
CREATE FUNCTION tidlarger($1 TID, $2 TID) RETURNS TID;
CREATE FUNCTION tidle($1 TID, $2 TID) RETURNS BOOLEAN;
CREATE FUNCTION tidlt($1 TID, $2 TID) RETURNS BOOLEAN;
CREATE FUNCTION tidne($1 TID, $2 TID) RETURNS BOOLEAN;
CREATE FUNCTION tidout($1 TID) RETURNS CSTRING;
CREATE FUNCTION tidrecv($1 INTERNAL) RETURNS TID;
CREATE FUNCTION tidsend($1 TID) RETURNS BYTEA;
CREATE FUNCTION tidsmaller($1 TID, $2 TID) RETURNS TID;
CREATE FUNCTION time_cmp($1 TIME, $2 TIME) RETURNS INTEGER;
CREATE FUNCTION time_eq($1 TIME, $2 TIME) RETURNS BOOLEAN;
CREATE FUNCTION time_ge($1 TIME, $2 TIME) RETURNS BOOLEAN;
CREATE FUNCTION time_gt($1 TIME, $2 TIME) RETURNS BOOLEAN;
CREATE FUNCTION time_hash($1 TIME) RETURNS INTEGER;
CREATE FUNCTION time_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS TIME;
CREATE FUNCTION time_larger($1 TIME, $2 TIME) RETURNS TIME;
CREATE FUNCTION time_le($1 TIME, $2 TIME) RETURNS BOOLEAN;
CREATE FUNCTION time_lt($1 TIME, $2 TIME) RETURNS BOOLEAN;
CREATE FUNCTION time_mi_interval($1 TIME, $2 INTERVAL) RETURNS TIME;
CREATE FUNCTION time_mi_time($1 TIME, $2 TIME) RETURNS INTERVAL;
CREATE FUNCTION time_ne($1 TIME, $2 TIME) RETURNS BOOLEAN;
CREATE FUNCTION time_out($1 TIME) RETURNS CSTRING;
CREATE FUNCTION time_pl_interval($1 TIME, $2 INTERVAL) RETURNS TIME;
CREATE FUNCTION time_recv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS TIME;
CREATE FUNCTION time_send($1 TIME) RETURNS BYTEA;
CREATE FUNCTION time_smaller($1 TIME, $2 TIME) RETURNS TIME;
CREATE FUNCTION time_transform($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION time($1 ABSTIME) RETURNS TIME;
CREATE FUNCTION timedate_pl($1 TIME, $2 DATE) RETURNS TIMESTAMP;
CREATE FUNCTION time($1 INTERVAL) RETURNS TIME;
CREATE FUNCTION timemi($1 ABSTIME, $2 RELTIME) RETURNS ABSTIME;
CREATE FUNCTION timenow() RETURNS ABSTIME;
CREATE FUNCTION timeofday() RETURNS TEXT;
CREATE FUNCTION timepl($1 ABSTIME, $2 RELTIME) RETURNS ABSTIME;
CREATE FUNCTION timestamp_cmp_date($1 TIMESTAMP, $2 DATE) RETURNS INTEGER;
CREATE FUNCTION timestamp_cmp_timestamptz($1 TIMESTAMP, $2 TIMESTAMP WITH TIME ZONE) RETURNS INTEGER;
CREATE FUNCTION timestamp_cmp($1 TIMESTAMP, $2 TIMESTAMP) RETURNS INTEGER;
CREATE FUNCTION timestamp_eq_date($1 TIMESTAMP, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_eq_timestamptz($1 TIMESTAMP, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_eq($1 TIMESTAMP, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_ge_date($1 TIMESTAMP, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_ge_timestamptz($1 TIMESTAMP, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_ge($1 TIMESTAMP, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_gt_date($1 TIMESTAMP, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_gt_timestamptz($1 TIMESTAMP, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_gt($1 TIMESTAMP, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_hash($1 TIMESTAMP) RETURNS INTEGER;
CREATE FUNCTION timestamp_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS TIMESTAMP;
CREATE FUNCTION timestamp_larger($1 TIMESTAMP, $2 TIMESTAMP) RETURNS TIMESTAMP;
CREATE FUNCTION timestamp_le_date($1 TIMESTAMP, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_le_timestamptz($1 TIMESTAMP, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_le($1 TIMESTAMP, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_lt_date($1 TIMESTAMP, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_lt_timestamptz($1 TIMESTAMP, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_lt($1 TIMESTAMP, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_mi_interval($1 TIMESTAMP, $2 INTERVAL) RETURNS TIMESTAMP;
CREATE FUNCTION timestamp_mi($1 TIMESTAMP, $2 TIMESTAMP) RETURNS INTERVAL;
CREATE FUNCTION timestamp_ne_date($1 TIMESTAMP, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_ne_timestamptz($1 TIMESTAMP, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_ne($1 TIMESTAMP, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION timestamp_out($1 TIMESTAMP) RETURNS CSTRING;
CREATE FUNCTION timestamp_pl_interval($1 TIMESTAMP, $2 INTERVAL) RETURNS TIMESTAMP;
CREATE FUNCTION timestamp_recv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS TIMESTAMP;
CREATE FUNCTION timestamp_send($1 TIMESTAMP) RETURNS BYTEA;
CREATE FUNCTION timestamp_smaller($1 TIMESTAMP, $2 TIMESTAMP) RETURNS TIMESTAMP;
CREATE FUNCTION timestamp_sortsupport($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION timestamp_transform($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION timestamp($1 ABSTIME) RETURNS TIMESTAMP;
CREATE FUNCTION timestamp($1 DATE) RETURNS TIMESTAMP;
CREATE FUNCTION timestamp($1 DATE, $2 TIME) RETURNS TIMESTAMP;
CREATE FUNCTION timestamp($1 TIMESTAMP WITH TIME ZONE) RETURNS TIMESTAMP;
CREATE FUNCTION timestamp($1 TIMESTAMP, $2 INTEGER) RETURNS TIMESTAMP;
CREATE FUNCTION timestamptypmodin($1 CSTRING[]) RETURNS INTEGER;
CREATE FUNCTION timestamptypmodout($1 INTEGER) RETURNS CSTRING;
CREATE FUNCTION timestamptz_cmp_date($1 TIMESTAMP WITH TIME ZONE, $2 DATE) RETURNS INTEGER;
CREATE FUNCTION timestamptz_cmp_timestamp($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP) RETURNS INTEGER;
CREATE FUNCTION timestamptz_cmp($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS INTEGER;
CREATE FUNCTION timestamptz_eq_date($1 TIMESTAMP WITH TIME ZONE, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_eq_timestamp($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_eq($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_ge_date($1 TIMESTAMP WITH TIME ZONE, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_ge_timestamp($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_ge($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_gt_date($1 TIMESTAMP WITH TIME ZONE, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_gt_timestamp($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_gt($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timestamptz_larger($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timestamptz_le_date($1 TIMESTAMP WITH TIME ZONE, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_le_timestamp($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_le($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_lt_date($1 TIMESTAMP WITH TIME ZONE, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_lt_timestamp($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_lt($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_mi_interval($1 TIMESTAMP WITH TIME ZONE, $2 INTERVAL) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timestamptz_mi($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS INTERVAL;
CREATE FUNCTION timestamptz_ne_date($1 TIMESTAMP WITH TIME ZONE, $2 DATE) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_ne_timestamp($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_ne($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timestamptz_out($1 TIMESTAMP WITH TIME ZONE) RETURNS CSTRING;
CREATE FUNCTION timestamptz_pl_interval($1 TIMESTAMP WITH TIME ZONE, $2 INTERVAL) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timestamptz_recv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timestamptz_send($1 TIMESTAMP WITH TIME ZONE) RETURNS BYTEA;
CREATE FUNCTION timestamptz_smaller($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timestamptz($1 ABSTIME) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timestamptz($1 DATE) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timestamptz($1 DATE, $2 TIME) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timestamptz($1 DATE, $2 TIME WITH TIME ZONE) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timestamptz($1 TIMESTAMP) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timestamptz($1 TIMESTAMP WITH TIME ZONE, $2 INTEGER) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timestamptztypmodin($1 CSTRING[]) RETURNS INTEGER;
CREATE FUNCTION timestamptztypmodout($1 INTEGER) RETURNS CSTRING;
CREATE FUNCTION time($1 TIME WITH TIME ZONE) RETURNS TIME;
CREATE FUNCTION time($1 TIME, $2 INTEGER) RETURNS TIME;
CREATE FUNCTION time($1 TIMESTAMP) RETURNS TIME;
CREATE FUNCTION time($1 TIMESTAMP WITH TIME ZONE) RETURNS TIME;
CREATE FUNCTION timetypmodin($1 CSTRING[]) RETURNS INTEGER;
CREATE FUNCTION timetypmodout($1 INTEGER) RETURNS CSTRING;
CREATE FUNCTION timetz_cmp($1 TIME WITH TIME ZONE, $2 TIME WITH TIME ZONE) RETURNS INTEGER;
CREATE FUNCTION timetz_eq($1 TIME WITH TIME ZONE, $2 TIME WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timetz_ge($1 TIME WITH TIME ZONE, $2 TIME WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timetz_gt($1 TIME WITH TIME ZONE, $2 TIME WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timetz_hash($1 TIME WITH TIME ZONE) RETURNS INTEGER;
CREATE FUNCTION timetz_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION timetz_larger($1 TIME WITH TIME ZONE, $2 TIME WITH TIME ZONE) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION timetz_le($1 TIME WITH TIME ZONE, $2 TIME WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timetz_lt($1 TIME WITH TIME ZONE, $2 TIME WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timetz_mi_interval($1 TIME WITH TIME ZONE, $2 INTERVAL) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION timetz_ne($1 TIME WITH TIME ZONE, $2 TIME WITH TIME ZONE) RETURNS BOOLEAN;
CREATE FUNCTION timetz_out($1 TIME WITH TIME ZONE) RETURNS CSTRING;
CREATE FUNCTION timetz_pl_interval($1 TIME WITH TIME ZONE, $2 INTERVAL) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION timetz_recv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION timetz_send($1 TIME WITH TIME ZONE) RETURNS BYTEA;
CREATE FUNCTION timetz_smaller($1 TIME WITH TIME ZONE, $2 TIME WITH TIME ZONE) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION timetzdate_pl($1 TIME WITH TIME ZONE, $2 DATE) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timetz($1 TIME) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION timetz($1 TIME WITH TIME ZONE, $2 INTEGER) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION timetz($1 TIMESTAMP WITH TIME ZONE) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION timetztypmodin($1 CSTRING[]) RETURNS INTEGER;
CREATE FUNCTION timetztypmodout($1 INTEGER) RETURNS CSTRING;
CREATE FUNCTION timezone($1 INTERVAL, $2 TIME WITH TIME ZONE) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION timezone($1 INTERVAL, $2 TIMESTAMP) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timezone($1 INTERVAL, $2 TIMESTAMP WITH TIME ZONE) RETURNS TIMESTAMP;
CREATE FUNCTION timezone($1 TEXT, $2 TIME WITH TIME ZONE) RETURNS TIME WITH TIME ZONE;
CREATE FUNCTION timezone($1 TEXT, $2 TIMESTAMP) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION timezone($1 TEXT, $2 TIMESTAMP WITH TIME ZONE) RETURNS TIMESTAMP;
CREATE FUNCTION tinterval($1 ABSTIME, $2 ABSTIME) RETURNS TINTERVAL;
CREATE FUNCTION tintervalct($1 TINTERVAL, $2 TINTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION tintervalend($1 TINTERVAL) RETURNS ABSTIME;
CREATE FUNCTION tintervaleq($1 TINTERVAL, $2 TINTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION tintervalge($1 TINTERVAL, $2 TINTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION tintervalgt($1 TINTERVAL, $2 TINTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION tintervalin($1 CSTRING) RETURNS TINTERVAL;
CREATE FUNCTION tintervalleneq($1 TINTERVAL, $2 RELTIME) RETURNS BOOLEAN;
CREATE FUNCTION tintervallenge($1 TINTERVAL, $2 RELTIME) RETURNS BOOLEAN;
CREATE FUNCTION tintervallengt($1 TINTERVAL, $2 RELTIME) RETURNS BOOLEAN;
CREATE FUNCTION tintervallenle($1 TINTERVAL, $2 RELTIME) RETURNS BOOLEAN;
CREATE FUNCTION tintervallenlt($1 TINTERVAL, $2 RELTIME) RETURNS BOOLEAN;
CREATE FUNCTION tintervallenne($1 TINTERVAL, $2 RELTIME) RETURNS BOOLEAN;
CREATE FUNCTION tintervalle($1 TINTERVAL, $2 TINTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION tintervallt($1 TINTERVAL, $2 TINTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION tintervalne($1 TINTERVAL, $2 TINTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION tintervalout($1 TINTERVAL) RETURNS CSTRING;
CREATE FUNCTION tintervalov($1 TINTERVAL, $2 TINTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION tintervalrecv($1 INTERNAL) RETURNS TINTERVAL;
CREATE FUNCTION tintervalrel($1 TINTERVAL) RETURNS RELTIME;
CREATE FUNCTION tintervalsame($1 TINTERVAL, $2 TINTERVAL) RETURNS BOOLEAN;
CREATE FUNCTION tintervalsend($1 TINTERVAL) RETURNS BYTEA;
CREATE FUNCTION tintervalstart($1 TINTERVAL) RETURNS ABSTIME;
CREATE FUNCTION to_ascii($1 TEXT) RETURNS TEXT;
CREATE FUNCTION to_ascii($1 TEXT, $2 INTEGER) RETURNS TEXT;
CREATE FUNCTION to_ascii($1 TEXT, $2 NAME) RETURNS TEXT;
CREATE FUNCTION to_char($1 BIGINT, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION to_char($1 DOUBLE PRECISION, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION to_char($1 INTEGER, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION to_char($1 INTERVAL, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION to_char($1 NUMERIC, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION to_char($1 REAL, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION to_char($1 TIMESTAMP WITH TIME ZONE, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION to_char($1 TIMESTAMP, $2 TEXT) RETURNS TEXT;
CREATE FUNCTION to_date($1 TEXT, $2 TEXT) RETURNS DATE;
CREATE FUNCTION to_hex($1 BIGINT) RETURNS TEXT;
CREATE FUNCTION to_hex($1 INTEGER) RETURNS TEXT;
CREATE FUNCTION to_json($1 ANYELEMENT) RETURNS JSON;
CREATE FUNCTION to_number($1 TEXT, $2 TEXT) RETURNS NUMERIC;
CREATE FUNCTION to_timestamp($1 DOUBLE PRECISION) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION to_timestamp($1 TEXT, $2 TEXT) RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION to_tsquery($1 REGCONFIG, $2 TEXT) RETURNS TSQUERY;
CREATE FUNCTION to_tsquery($1 TEXT) RETURNS TSQUERY;
CREATE FUNCTION to_tsvector($1 REGCONFIG, $2 TEXT) RETURNS TSVECTOR;
CREATE FUNCTION to_tsvector($1 TEXT) RETURNS TSVECTOR;
CREATE FUNCTION transaction_timestamp() RETURNS TIMESTAMP WITH TIME ZONE;
CREATE FUNCTION translate($1 TEXT, $2 TEXT, $3 TEXT) RETURNS TEXT;
CREATE FUNCTION trigger_in($1 CSTRING) RETURNS TRIGGER;
CREATE FUNCTION trigger_out($1 TRIGGER) RETURNS CSTRING;
CREATE FUNCTION trunc($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION trunc($1 MACADDR) RETURNS MACADDR;
CREATE FUNCTION trunc($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION trunc($1 NUMERIC, $2 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION ts_debug(config REGCONFIG, document TEXT, alias OUT TEXT, description OUT TEXT, token OUT TEXT, dictionaries OUT REGDICTIONARY[], dictionary OUT REGDICTIONARY, lexemes OUT TEXT[]) RETURNS SETOF RECORD;
CREATE FUNCTION ts_debug(document TEXT, alias OUT TEXT, description OUT TEXT, token OUT TEXT, dictionaries OUT REGDICTIONARY[], dictionary OUT REGDICTIONARY, lexemes OUT TEXT[]) RETURNS SETOF RECORD;
CREATE FUNCTION ts_headline($1 REGCONFIG, $2 TEXT, $3 TSQUERY) RETURNS TEXT;
CREATE FUNCTION ts_headline($1 REGCONFIG, $2 TEXT, $3 TSQUERY, $4 TEXT) RETURNS TEXT;
CREATE FUNCTION ts_headline($1 TEXT, $2 TSQUERY) RETURNS TEXT;
CREATE FUNCTION ts_headline($1 TEXT, $2 TSQUERY, $3 TEXT) RETURNS TEXT;
CREATE FUNCTION ts_lexize($1 REGDICTIONARY, $2 TEXT) RETURNS TEXT[];
CREATE FUNCTION ts_match_qv($1 TSQUERY, $2 TSVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION ts_match_tq($1 TEXT, $2 TSQUERY) RETURNS BOOLEAN;
CREATE FUNCTION ts_match_tt($1 TEXT, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION ts_match_vq($1 TSVECTOR, $2 TSQUERY) RETURNS BOOLEAN;
CREATE FUNCTION ts_parse(parser_oid OID, txt TEXT, tokid OUT INTEGER, token OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION ts_parse(parser_name TEXT, txt TEXT, tokid OUT INTEGER, token OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION ts_rank_cd($1 REAL[], $2 TSVECTOR, $3 TSQUERY) RETURNS REAL;
CREATE FUNCTION ts_rank_cd($1 REAL[], $2 TSVECTOR, $3 TSQUERY, $4 INTEGER) RETURNS REAL;
CREATE FUNCTION ts_rank_cd($1 TSVECTOR, $2 TSQUERY) RETURNS REAL;
CREATE FUNCTION ts_rank_cd($1 TSVECTOR, $2 TSQUERY, $3 INTEGER) RETURNS REAL;
CREATE FUNCTION ts_rank($1 REAL[], $2 TSVECTOR, $3 TSQUERY) RETURNS REAL;
CREATE FUNCTION ts_rank($1 REAL[], $2 TSVECTOR, $3 TSQUERY, $4 INTEGER) RETURNS REAL;
CREATE FUNCTION ts_rank($1 TSVECTOR, $2 TSQUERY) RETURNS REAL;
CREATE FUNCTION ts_rank($1 TSVECTOR, $2 TSQUERY, $3 INTEGER) RETURNS REAL;
CREATE FUNCTION ts_rewrite($1 TSQUERY, $2 TEXT) RETURNS TSQUERY;
CREATE FUNCTION ts_rewrite($1 TSQUERY, $2 TSQUERY, $3 TSQUERY) RETURNS TSQUERY;
CREATE FUNCTION ts_stat(query TEXT, word OUT TEXT, ndoc OUT INTEGER, nentry OUT INTEGER) RETURNS SETOF RECORD;
CREATE FUNCTION ts_stat(query TEXT, weights TEXT, word OUT TEXT, ndoc OUT INTEGER, nentry OUT INTEGER) RETURNS SETOF RECORD;
CREATE FUNCTION ts_token_type(parser_oid OID, tokid OUT INTEGER, alias OUT TEXT, description OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION ts_token_type(parser_name TEXT, tokid OUT INTEGER, alias OUT TEXT, description OUT TEXT) RETURNS SETOF RECORD;
CREATE FUNCTION ts_typanalyze($1 INTERNAL) RETURNS BOOLEAN;
CREATE FUNCTION tsmatchjoinsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 SMALLINT, $5 INTERNAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION tsmatchsel($1 INTERNAL, $2 OID, $3 INTERNAL, $4 INTEGER) RETURNS DOUBLE PRECISION;
CREATE FUNCTION tsq_mcontained($1 TSQUERY, $2 TSQUERY) RETURNS BOOLEAN;
CREATE FUNCTION tsq_mcontains($1 TSQUERY, $2 TSQUERY) RETURNS BOOLEAN;
CREATE FUNCTION tsquery_and($1 TSQUERY, $2 TSQUERY) RETURNS TSQUERY;
CREATE FUNCTION tsquery_cmp($1 TSQUERY, $2 TSQUERY) RETURNS INTEGER;
CREATE FUNCTION tsquery_eq($1 TSQUERY, $2 TSQUERY) RETURNS BOOLEAN;
CREATE FUNCTION tsquery_ge($1 TSQUERY, $2 TSQUERY) RETURNS BOOLEAN;
CREATE FUNCTION tsquery_gt($1 TSQUERY, $2 TSQUERY) RETURNS BOOLEAN;
CREATE FUNCTION tsquery_le($1 TSQUERY, $2 TSQUERY) RETURNS BOOLEAN;
CREATE FUNCTION tsquery_lt($1 TSQUERY, $2 TSQUERY) RETURNS BOOLEAN;
CREATE FUNCTION tsquery_ne($1 TSQUERY, $2 TSQUERY) RETURNS BOOLEAN;
CREATE FUNCTION tsquery_not($1 TSQUERY) RETURNS TSQUERY;
CREATE FUNCTION tsquery_or($1 TSQUERY, $2 TSQUERY) RETURNS TSQUERY;
CREATE FUNCTION tsqueryin($1 CSTRING) RETURNS TSQUERY;
CREATE FUNCTION tsqueryout($1 TSQUERY) RETURNS CSTRING;
CREATE FUNCTION tsqueryrecv($1 INTERNAL) RETURNS TSQUERY;
CREATE FUNCTION tsquerysend($1 TSQUERY) RETURNS BYTEA;
CREATE FUNCTION tsrange_subdiff($1 TIMESTAMP, $2 TIMESTAMP) RETURNS DOUBLE PRECISION;
CREATE FUNCTION tsrange($1 TIMESTAMP, $2 TIMESTAMP) RETURNS TSRANGE;
CREATE FUNCTION tsrange($1 TIMESTAMP, $2 TIMESTAMP, $3 TEXT) RETURNS TSRANGE;
CREATE FUNCTION tstzrange_subdiff($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS DOUBLE PRECISION;
CREATE FUNCTION tstzrange($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE) RETURNS TSTZRANGE;
CREATE FUNCTION tstzrange($1 TIMESTAMP WITH TIME ZONE, $2 TIMESTAMP WITH TIME ZONE, $3 TEXT) RETURNS TSTZRANGE;
CREATE FUNCTION tsvector_cmp($1 TSVECTOR, $2 TSVECTOR) RETURNS INTEGER;
CREATE FUNCTION tsvector_concat($1 TSVECTOR, $2 TSVECTOR) RETURNS TSVECTOR;
CREATE FUNCTION tsvector_eq($1 TSVECTOR, $2 TSVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION tsvector_ge($1 TSVECTOR, $2 TSVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION tsvector_gt($1 TSVECTOR, $2 TSVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION tsvector_le($1 TSVECTOR, $2 TSVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION tsvector_lt($1 TSVECTOR, $2 TSVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION tsvector_ne($1 TSVECTOR, $2 TSVECTOR) RETURNS BOOLEAN;
CREATE FUNCTION tsvector_update_trigger() RETURNS TRIGGER;
CREATE FUNCTION tsvector_update_trigger_column() RETURNS TRIGGER;
CREATE FUNCTION tsvectorin($1 CSTRING) RETURNS TSVECTOR;
CREATE FUNCTION tsvectorout($1 TSVECTOR) RETURNS CSTRING;
CREATE FUNCTION tsvectorrecv($1 INTERNAL) RETURNS TSVECTOR;
CREATE FUNCTION tsvectorsend($1 TSVECTOR) RETURNS BYTEA;
CREATE FUNCTION txid_current() RETURNS BIGINT;
CREATE FUNCTION txid_current_snapshot() RETURNS TXID_SNAPSHOT;
CREATE FUNCTION txid_snapshot_in($1 CSTRING) RETURNS TXID_SNAPSHOT;
CREATE FUNCTION txid_snapshot_out($1 TXID_SNAPSHOT) RETURNS CSTRING;
CREATE FUNCTION txid_snapshot_recv($1 INTERNAL) RETURNS TXID_SNAPSHOT;
CREATE FUNCTION txid_snapshot_send($1 TXID_SNAPSHOT) RETURNS BYTEA;
CREATE FUNCTION txid_snapshot_xip($1 TXID_SNAPSHOT) RETURNS SETOF BIGINT;
CREATE FUNCTION txid_snapshot_xmax($1 TXID_SNAPSHOT) RETURNS BIGINT;
CREATE FUNCTION txid_snapshot_xmin($1 TXID_SNAPSHOT) RETURNS BIGINT;
CREATE FUNCTION txid_visible_in_snapshot($1 BIGINT, $2 TXID_SNAPSHOT) RETURNS BOOLEAN;
CREATE FUNCTION uhc_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION unique_key_recheck() RETURNS TRIGGER;
CREATE FUNCTION unknownin($1 CSTRING) RETURNS UNKNOWN;
CREATE FUNCTION unknownout($1 UNKNOWN) RETURNS CSTRING;
CREATE FUNCTION unknownrecv($1 INTERNAL) RETURNS UNKNOWN;
CREATE FUNCTION unknownsend($1 UNKNOWN) RETURNS BYTEA;
CREATE FUNCTION unnest($1 ANYARRAY) RETURNS SETOF ANYELEMENT;
CREATE FUNCTION upper_inc($1 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION upper_inf($1 ANYRANGE) RETURNS BOOLEAN;
CREATE FUNCTION upper($1 ANYRANGE) RETURNS ANYELEMENT;
CREATE FUNCTION upper($1 TEXT) RETURNS TEXT;
CREATE FUNCTION utf8_to_ascii($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_big5($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_euc_cn($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_euc_jis_2004($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_euc_jp($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_euc_kr($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_euc_tw($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_gb18030($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_gbk($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_iso8859_1($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_iso8859($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_johab($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_koi8r($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_koi8u($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_shift_jis_2004($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_sjis($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_uhc($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION utf8_to_win($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION uuid_cmp($1 UUID, $2 UUID) RETURNS INTEGER;
CREATE FUNCTION uuid_eq($1 UUID, $2 UUID) RETURNS BOOLEAN;
CREATE FUNCTION uuid_ge($1 UUID, $2 UUID) RETURNS BOOLEAN;
CREATE FUNCTION uuid_gt($1 UUID, $2 UUID) RETURNS BOOLEAN;
CREATE FUNCTION uuid_hash($1 UUID) RETURNS INTEGER;
CREATE FUNCTION uuid_in($1 CSTRING) RETURNS UUID;
CREATE FUNCTION uuid_le($1 UUID, $2 UUID) RETURNS BOOLEAN;
CREATE FUNCTION uuid_lt($1 UUID, $2 UUID) RETURNS BOOLEAN;
CREATE FUNCTION uuid_ne($1 UUID, $2 UUID) RETURNS BOOLEAN;
CREATE FUNCTION uuid_out($1 UUID) RETURNS CSTRING;
CREATE FUNCTION uuid_recv($1 INTERNAL) RETURNS UUID;
CREATE FUNCTION uuid_send($1 UUID) RETURNS BYTEA;
CREATE FUNCTION var_pop($1 BIGINT) RETURNS NUMERIC;
CREATE FUNCTION var_pop($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION var_pop($1 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION var_pop($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION var_pop($1 REAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION var_pop($1 SMALLINT) RETURNS NUMERIC;
CREATE FUNCTION var_samp($1 BIGINT) RETURNS NUMERIC;
CREATE FUNCTION var_samp($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION var_samp($1 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION var_samp($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION var_samp($1 REAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION var_samp($1 SMALLINT) RETURNS NUMERIC;
CREATE FUNCTION varbit_in($1 CSTRING, $2 OID, $3 INTEGER) RETURNS BIT VARYING;
CREATE FUNCTION varbit_out($1 BIT VARYING) RETURNS CSTRING;
CREATE FUNCTION varbit_recv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS BIT VARYING;
CREATE FUNCTION varbit_send($1 BIT VARYING) RETURNS BYTEA;
CREATE FUNCTION varbit_transform($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION varbit($1 BIT VARYING, $2 INTEGER, $3 BOOLEAN) RETURNS BIT VARYING;
CREATE FUNCTION varbitcmp($1 BIT VARYING, $2 BIT VARYING) RETURNS INTEGER;
CREATE FUNCTION varbiteq($1 BIT VARYING, $2 BIT VARYING) RETURNS BOOLEAN;
CREATE FUNCTION varbitge($1 BIT VARYING, $2 BIT VARYING) RETURNS BOOLEAN;
CREATE FUNCTION varbitgt($1 BIT VARYING, $2 BIT VARYING) RETURNS BOOLEAN;
CREATE FUNCTION varbitle($1 BIT VARYING, $2 BIT VARYING) RETURNS BOOLEAN;
CREATE FUNCTION varbitlt($1 BIT VARYING, $2 BIT VARYING) RETURNS BOOLEAN;
CREATE FUNCTION varbitne($1 BIT VARYING, $2 BIT VARYING) RETURNS BOOLEAN;
CREATE FUNCTION varbittypmodin($1 CSTRING[]) RETURNS INTEGER;
CREATE FUNCTION varbittypmodout($1 INTEGER) RETURNS CSTRING;
CREATE FUNCTION varchar_transform($1 INTERNAL) RETURNS INTERNAL;
CREATE FUNCTION varcharin($1 CSTRING, $2 OID, $3 INTEGER) RETURNS VARCHAR;
CREATE FUNCTION varchar($1 NAME) RETURNS VARCHAR;
CREATE FUNCTION varcharout($1 VARCHAR) RETURNS CSTRING;
CREATE FUNCTION varcharrecv($1 INTERNAL, $2 OID, $3 INTEGER) RETURNS VARCHAR;
CREATE FUNCTION varcharsend($1 VARCHAR) RETURNS BYTEA;
CREATE FUNCTION varchartypmodin($1 CSTRING[]) RETURNS INTEGER;
CREATE FUNCTION varchartypmodout($1 INTEGER) RETURNS CSTRING;
CREATE FUNCTION varchar($1 VARCHAR, $2 INTEGER, $3 BOOLEAN) RETURNS VARCHAR;
CREATE FUNCTION variance($1 BIGINT) RETURNS NUMERIC;
CREATE FUNCTION variance($1 DOUBLE PRECISION) RETURNS DOUBLE PRECISION;
CREATE FUNCTION variance($1 INTEGER) RETURNS NUMERIC;
CREATE FUNCTION variance($1 NUMERIC) RETURNS NUMERIC;
CREATE FUNCTION variance($1 REAL) RETURNS DOUBLE PRECISION;
CREATE FUNCTION variance($1 SMALLINT) RETURNS NUMERIC;
CREATE FUNCTION version() RETURNS TEXT;
CREATE FUNCTION void_in($1 CSTRING) RETURNS VOID;
CREATE FUNCTION void_out($1 VOID) RETURNS CSTRING;
CREATE FUNCTION void_recv($1 INTERNAL) RETURNS VOID;
CREATE FUNCTION void_send($1 VOID) RETURNS BYTEA;
CREATE FUNCTION width_bucket($1 DOUBLE PRECISION, $2 DOUBLE PRECISION, $3 DOUBLE PRECISION, $4 INTEGER) RETURNS INTEGER;
CREATE FUNCTION width_bucket($1 NUMERIC, $2 NUMERIC, $3 NUMERIC, $4 INTEGER) RETURNS INTEGER;
CREATE FUNCTION width($1 BOX) RETURNS DOUBLE PRECISION;
CREATE FUNCTION win1250_to_latin2($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION win1250_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION win1251_to_iso($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION win1251_to_koi8r($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION win1251_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION win1251_to_win866($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION win866_to_iso($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION win866_to_koi8r($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION win866_to_mic($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION win866_to_win1251($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION win_to_utf8($1 INTEGER, $2 INTEGER, $3 CSTRING, $4 INTERNAL, $5 INTEGER) RETURNS VOID;
CREATE FUNCTION xideqint4($1 XID, $2 INTEGER) RETURNS BOOLEAN;
CREATE FUNCTION xideq($1 XID, $2 XID) RETURNS BOOLEAN;
CREATE FUNCTION xidin($1 CSTRING) RETURNS XID;
CREATE FUNCTION xidout($1 XID) RETURNS CSTRING;
CREATE FUNCTION xidrecv($1 INTERNAL) RETURNS XID;
CREATE FUNCTION xidsend($1 XID) RETURNS BYTEA;
CREATE FUNCTION xml_in($1 CSTRING) RETURNS XML;
CREATE FUNCTION xml_is_well_formed_content($1 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION xml_is_well_formed_document($1 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION xml_is_well_formed($1 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION xml_out($1 XML) RETURNS CSTRING;
CREATE FUNCTION xml_recv($1 INTERNAL) RETURNS XML;
CREATE FUNCTION xml_send($1 XML) RETURNS BYTEA;
CREATE FUNCTION xmlagg($1 XML) RETURNS XML;
CREATE FUNCTION xmlcomment($1 TEXT) RETURNS XML;
CREATE FUNCTION xmlconcat2($1 XML, $2 XML) RETURNS XML;
CREATE FUNCTION xmlexists($1 TEXT, $2 XML) RETURNS BOOLEAN;
CREATE FUNCTION xml($1 TEXT) RETURNS XML;
CREATE FUNCTION xmlvalidate($1 XML, $2 TEXT) RETURNS BOOLEAN;
CREATE FUNCTION xpath_exists($1 TEXT, $2 XML) RETURNS BOOLEAN;
CREATE FUNCTION xpath_exists($1 TEXT, $2 XML, $3 TEXT[]) RETURNS BOOLEAN;
CREATE FUNCTION xpath($1 TEXT, $2 XML) RETURNS XML[];
CREATE FUNCTION xpath($1 TEXT, $2 XML, $3 TEXT[]) RETURNS XML[];
;