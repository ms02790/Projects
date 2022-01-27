import os
import logging
import time
from flask import Flask
from flask import render_template
from flask import request
import http.client
import queue
import threading
from parallel_lambda import ThreadUrl, parallel_run 
from statistics import mean
from flask_googlecharts import GoogleCharts
from flask_googlecharts import LineChart
from flask_table import Table, Col
import boto3
import os
import requests
import json

app = Flask(__name__)
charts = GoogleCharts(app)


class Risk_Table(Table):
    Resource_Num = Col('Resource NUmber')
    Risk_var95 = Col('   Var 95')
    Risk_var99 = Col('Var 99')

# Referenced from Lab provided by Dr. Lee Gillam
def doRender(tname, values={}):
	if not os.path.isfile( os.path.join(os.getcwd(), 'templates/'+tname) ): #No such file
		return render_template('index.htm')
	return render_template(tname, **values) 




# function for Lambda request
@app.route('/Lambda/output', methods=['POST'])
def RandomHandler():
        import http.client
        if request.method == 'POST':
                history = request.form.get('History')
                shots = request.form.get('Shots')
                resources = int(request.form.get('Resources'))
                signal = str(request.form.get('Signal_type'))
                if history == '' or shots == '' or resources == '':
                    return doRender('index.htm',
                                {'note3': 'Please specify a number for each group!'})
                #noting the start time
                start = time.time()
                results = parallel_run(resources, history, shots, signal)
                Runtime = time.time() -start
                output_dict = {
                    "Risk 95 percent": [results[i][0]*-100 for i in range(0, len(results))],
                    "Risk 99 percent": [results[i][1]*-100 for i in range(0, len(results))],
                    "Mean 95 percent": mean([results[i][0]*-100 for i in range(0, len(results))]),
                    "Mean 99 percent": mean([results[i][1]*-100 for i in range(0, len(results))]),
                }
                items = []
                for x in range(0,len(results)):
                    tempdict = dict(Resource_Num = x+1, Risk_var95 = results[x][0], Risk_var99 = results[x][1])
                    items.append(tempdict)

                risk_chart = LineChart("risk_chart", options={"title": "Risk Chart", "width": 1000, "height": 500})

                cost = Runtime*resources*.125*0.0000000021


                risk_chart.add_column("string", "Today's date")
                risk_chart.add_column("number", "Risk 95")
                risk_chart.add_column("number", "Risk 99")
                risk_chart.add_column("number", "Mean 95")
                risk_chart.add_column("number", "Mean 99")
                rows = [   [i+1 ,output_dict["Risk 95 percent"][i], output_dict["Risk 99 percent"][i], output_dict["Mean 95 percent"], output_dict["Mean 99 percent"]]  for i in range(0, len(results)) ]
                risk_chart.add_rows(rows)

                RiskTable = Risk_Table(items)
                MeanValues = [output_dict['Mean 95 percent'],output_dict['Mean 99 percent']]
                values = str(MeanValues[0]) + ' and ' + str(MeanValues[1])
                charts.register(risk_chart)

                return doRender( 'index.htm',
                        {'note': output_dict,
                        'note1': "True", 'note2': RiskTable, 'note3': values,'note4': Runtime, 'note5': cost} )

#function for EC2
@app.route('/EC2/output', methods=['POST'])
def RandomHandler2():
        import http.client
        
        history = str(request.form.get('History'))
        shots = str(request.form.get('Shots'))
        resources = int(request.form.get('Resources'))
        signal = str(request.form.get('Signal_type'))
        #creating instances for EC2 using boto3
        # Boto3 referenced from Boto3 documentation Amazon EC2 at https://boto3.amazonaws.com/v1/documentation/api/latest/guide/migrationec2.html#launching-new-instances
        start=time.time()
        os.environ['AWS_DEFAULT_REGION'] = 'us-east-1'
        ec2 = boto3.resource('ec2',aws_access_key_id='AKIA6BQKSGY7DJQK6V53',
        aws_secret_access_key='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')
        ec2.create_instances(ImageId='ami-079aa261cc78f16f7', MinCount=1, MaxCount=resources)
        duration = time.time()-start
        host = "ms-359103385.us-east-1.elb.amazonaws.com"
        c = http.client.HTTPConnection(host)
        c.request("GET", "/postform.py"+"?shots="+shots+"&minhistory="+history+"&signal="+signal)

        response = c.getresponse()
        data = response.read().decode('utf-8')
        
        return doRender( 'index.htm',
                    {'note3': data, 'note4': duration} )

#function for terminating the resources of EC2
@app.route('/terminate', methods=['POST'])
def terminate_inst():
    # Boto3 referenced from Boto3 documentation Amazon EC2 at https://boto3.amazonaws.com/v1/documentation/api/latest/guide/migrationec2.html#launching-new-instances
    os.environ['AWS_DEFAULT_REGION'] = 'us-east-1'
    ec2 = boto3.resource('ec2',aws_access_key_id='AKIA6BQKSGY7DJQK6V53',
                aws_secret_access_key='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')
    instances = ec2.instances.filter(
        Filters=[{'Name': 'instance-state-name', 'Values': ['running']}])
    ids=[]
    for instance in instances:
        ids.append(instance.id)
    ec2.instances.filter(InstanceIds=ids).stop()
    return None


# catch all other page requests - doRender checks if a page is available (shows it) or not (index)
@app.route('/', defaults={'path': ''})
@app.route('/<path:path>')
def mainPage(path):
	return doRender(path)

@app.errorhandler(500)
# A small bit of error handling
def server_error(e):
    logging.exception('ERROR!')
    return """
    An  error occurred: <pre>{}</pre>
    """.format(e), 500

if __name__ == '__main__':
    # Entry point for running on the local machine
    # On GAE, endpoints (e.g. /) would be called.
    # Called as: gunicorn -b :$PORT index:app,
    # host is localhost; port is 8080; this file is index (.py)
    app.run(host='127.0.0.1', port=8080, debug=True)
