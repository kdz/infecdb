from sqlalchemy import create_engine

user = 'kmg2183'
password = '6266'
host = 'w4111vm.eastus.cloudapp.azure.com'
port = '5432'  # udpdate if necessary
dbname = 'w4111'
info = (user, password, host, port, dbname)

w4111_engine = create_engine('postgresql+psycopg2://%s:%s@%s:%s/%s' % info,
                       echo=True)

