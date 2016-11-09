from flask import Flask, current_app
from flask import jsonify, redirect, url_for, escape
from flask import request, session
from cloud_connect import w4111_engine
from flask import g
import sys


# Setup app

app = Flask(__name__, static_url_path='')
app.model = 0


# Before and after each request
@app.before_request
def before_request():
    """ Runs at the start of each web request. "g" is global, from flask. """
    try:
        g.conn = w4111_engine.connect()

    except:
        print("uh oh, problem connecting to database")
        import traceback
        traceback.print_exc()
        g.conn = None


@app.teardown_request
def teardown_request(exception):
    """
    At the end of the web request, this makes sure to close the database connection.
    If you don't, the database could run out of memory!
    """
    try:
        g.conn.close()
    except Exception as e:
        pass


# Initial index.html request
@app.route('/<path:path>')
def static_proxy(path):
    return app.send_static_file(path)


@app.route('/')
def index():
    """ Initial page load. """
    enter("index", request.args)
    exit("index", "")
    return app.send_static_file('index.html')


# Queries

def row_to_dict(row, cols):
    kmap = { k: pos for (k, (x, y, pos)) in row._keymap.items() }
    log("kmap", kmap)
    result = { col : str(row._row[kmap[col]]) for col in cols }
    log("extracted cols", result)

    return result

@app.route('/api', methods=['GET', 'POST'])
def api():
    blob = request.get_json()
    # app.model = update(blob, app.model)

    return jsonify({
        'model': "app.model"
    })


@app.route('/get-table', methods=['GET', 'POST'])
def query_get_table():

    log("starting query_get_table", "")

    table_name = request.json['table_name']
    cols = request.json['columns']

    query_str = "SELECT " + ",".join(cols) + " FROM " + table_name
    cursor = g.conn.execute(query_str)
    info = [row_to_dict(row, cols) for row in cursor]

    log("completed query_init_hospitals", "")

    return jsonify(info)


@app.route('/query-fields', methods=['GET', 'POST'])
def query_fields():

    log("starting query_fields", "")

    table_name = request.json['table_name']
    cols = request.json['columns']
    fields = request.json['fields']

    query_str = "SELECT " + ",".join(cols) + " FROM " + table_name + \
                " WHERE " + " AND ".join([table_name + "." + col + "=" + \
                                      "'%s'" % val for [col, val] in
                                      fields])  # TODO: check query format is ok for non-string type

    cursor = g.conn.execute(query_str)
    info = [row_to_dict(row, cols) for row in cursor]

    log("completed query_init_hospitals", "")

    return jsonify(info)



@app.route('/symptom-by-disease', methods=['GET', 'POST'])
def query_symptom_by_disease():

    log("starting symptom-by-disease", "")

    v_name = request.json['virus_name']
    cols = ["symptom_name", "description"]

    query_str = "SELECT symptom.symptom_name, symptom.description " + \
                "FROM symptom LEFT OUTER JOIN produces " + \
                "ON symptom.symptom_name=produces.symptom_name " + \
                "WHERE produces.virus_name='%s'" % v_name

    cursor = g.conn.execute(query_str)
    info = [row_to_dict(row, cols) for row in cursor]

    log("completed query_init_hospitals", "")

    return jsonify(info)


@app.route('/disease-by-hospital', methods=['GET', 'POST'])
def query_disease_by_hospital():

    log("starting query_disease_by_hospital", "")

    h_name = request.json['hospital_name']
    cols = request.json['columns']

    query_str = "SELECT " + ",".join(["d." + col for col in cols]) + " " +  \
                "FROM disease d LEFT OUTER JOIN treatment_for t " + \
                "ON t.virus_name=d.virus_name " + \
                "WHERE t.hospital_name='%s'" % h_name

    cursor = g.conn.execute(query_str)
    info = [row_to_dict(row, cols) for row in cursor]

    log("completed query_disease_by_hospital", "")

    return jsonify(info)



@app.route('/pid-login/<pid>', methods=['GET', 'POST'])
def validate_pid(pid):

    log("starting validate_pid", pid)

    pid_cursor = g.conn.execute("SELECT * FROM patient WHERE pid=%s" % pid)
    valid = bool(list(pid_cursor))

    log("completed validate_pid, valid returned", valid)

    return jsonify(valid)


@app.route('/mid-login/<mid>', methods=['GET', 'POST'])
def validate_mid(mid):

    log("starting validate_mid", mid)

    mid_cursor = g.conn.execute("SELECT * FROM medic WHERE mid=%s" % mid)
    valid = bool(list(mid_cursor))

    log("completed validate_mid, valid returned", str(valid))

    return jsonify(valid)



@app.route('/medic-me', methods=['GET', 'POST'])
def query_medic_me_page():
    """ Returns a single medic row using specified mid. """

    log("starting query_medic_me_page", "")

    mid = request.json['mid']
    cols = request.json['columns']

    query_str = "SELECT * FROM medic WHERE mid=%s" % mid


    mid_cursor = g.conn.execute(query_str)
    medic = [row_to_dict(row, cols) for row in mid_cursor][0]

    log("completed medic_me_page", "")

    return jsonify(medic)



@app.route('/medic-checks-on', methods=['GET', 'POST'])
def query_medic_checks_on():
    """ Returns a single medic row using specified mid. """

    log("starting medic-checks-on", "")

    mid = request.json['mid']
    cols = request.json['columns']

    query_str = "SELECT " + ",".join(["p." + col for col in cols]) + " " + \
                "FROM patient p LEFT OUTER JOIN checks_on c " + \
                "ON p.pid=c.pid " + \
                "WHERE c.mid='%s'" % mid


    mid_cursor = g.conn.execute(query_str)
    patients = [row_to_dict(row, cols) for row in mid_cursor]

    log("completed medic-checks-on", "")

    return jsonify(patients)



@app.route('/medic-todo', methods=['GET', 'POST'])
def query_medic_todo():
    """ Returns a single medic row using specified mid. """

    log("starting medic-todo", "")

    mid = request.json['mid']
    cols = request.json['columns']

    query_str = \
        """
            SELECT %s, SQRT((geoInfo.latDiff*geoInfo.latDiff)+(geoInfo.longDiff*geoInfo.longDiff)) AS distance
            FROM
                (   SELECT medNum.mid, patNum.pid, (medNum.latitude - patNum.latitude) AS latDiff, (medNum.longitude - patNum.longitude) AS longDiff
                    FROM (  SELECT medic.mid, MedLat.latitude, MedLong.longitude
                            FROM (  SELECT medic1.mid, CAST (medic1.latitude as DECIMAL(30,15)) AS latitude
                                    FROM medic AS medic1)
                         as MedLat,
                         (   SELECT medic2.mid, CAST (medic2.longitude as DECIMAL(30,15)) longitude
                            FROM medic AS medic2)
                         as MedLong,
                         medic
                    WHERE medic.mid= MedLat.mid AND medic.mid = MedLong.mid)
                    AS medNum
                CROSS JOIN
                    (   SELECT patient.pid, PatLat.latitude, PatLong.longitude
                        FROM (  SELECT patient1.pid, CAST (patient1.latitude as DECIMAL(30,15)) AS latitude
                                FROM patient AS patient1)
                             as PatLat,
                             (  SELECT patient2.pid, CAST (patient2.longitude as DECIMAL(30,15)) longitude
                                FROM patient AS patient2)
                            as PatLong,
                            patient
                        WHERE patient.pid = PatLat.pid AND patient.pid = PatLong.pid) AS patNum )
                    AS geoInfo,
                    patient AS pat
            WHERE geoInfo.mid = %s AND pat.pid = geoInfo.pid
            ORDER BY distance ASC
            LIMIT 5
        """ % (",".join(["pat." + col for col in cols]), mid)


    mid_cursor = g.conn.execute(query_str)
    patients = [row_to_dict(row, cols) for row in mid_cursor]

    log("completed medic-todo", "")

    return jsonify(patients)


# @app.route('/?', methods=['GET', 'POST'])
# def deal_with_it():
#     pass


# Shutdown app

def shutdown_server():
    func = request.environ.get('werkzeug.server.shutdown')
    if func is None:
        raise RuntimeError('Not running with the Werkzeug Server')
    func()


@app.route('/shutdown', methods=['GET'])
def shutdown():
    shutdown_server()
    return 'Server shutting down...'

app.secret_key = ''


def enter(msg, x):
    log("--- Enter --->", x)


def exit(msg, x):
    log("<--- Exit --->", str(x))


def log(msg, x):
    print(msg, "  ", str(x))
    sys.stdout.flush()
    pass

# Run app

if __name__ == "__main__":
    app.run(debug=True, port=5000)
