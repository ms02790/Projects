#!/usr/bin/env python3
import os
import logging
import time
from flask import Flask
from flask import render_template
from flask import request
import http.client
import queue
import threading
app = Flask(__name__)


# Codes based on the parallel_lambda.py file in lab3 Lambda provided by Dr. Lee Gillam.





queue = queue.Queue() 

class ThreadUrl(threading.Thread):
  def __init__(self, queue, task_id, history, shots, signal):
    threading.Thread.__init__(self)
    self.queue = queue
    self.task_id = task_id
    self.history = history
    self.shots = shots
    self.signal = signal
    self.data = None 

  def run(self):
    
      shots = self.queue.get()
      host = "ryop3bowpd.execute-api.us-east-1.amazonaws.com"
      try:
        c = http.client.HTTPSConnection(host)
        json= '{ "Shots": ' + str(self.shots) + ', "History":' + str(self.history) + ', "Signal":' + '"' + self.signal + '"' + '}'
      
        c.request("POST", "/default/mufunc2", json)

        response = c.getresponse()
        self.data = response.read().decode('utf-8')
        print( self.data, " from Thread", self.task_id )
      except IOError:
        print( 'Failed to open ' , host ) 

      
      self.queue.task_done()



def parallel_run(resources, history, shots, signal):
  threads=[]
  
  for i in range(0, resources):
    t = ThreadUrl(queue, i, history, shots, signal)
    threads.append(t)
    t.setDaemon(True)
    t.start()

  
  for x in range(0, resources):
    queue.put(shots)

 
  queue.join()


  results = [eval(t.data) for t in threads]
 
  return results

