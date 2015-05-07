byline = require 'byline'
sleep = require 'sleep'

lineStream = byline(process.stdin)

lineStream.on 'data', (data) ->
  str = data.toString()
  console.error str.replace /^\s+/, ""
  console.log str
  sleep.usleep 1000

lineStream.on 'end', ->
  console.error "didn't fuck up somehow?"
