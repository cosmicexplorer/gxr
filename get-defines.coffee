# node standard modules
fs = require 'fs'
# our local modules
Compp = require './compp/lib/compp/compp'
ComppStreams = Compp.streams

if process.argv.length isnt 3
  console.error "Usage: node get-defines.js INFILE"
  process.exit 1

fs.createReadStream(process.argv[2])
  .pipe(new ComppStreams.ConcatBackslashNewlinesStream
    filename: process.argv[2])
  .pipe(new ComppStreams.PreprocessStream(
    process.argv[2], {}, {}))
    .on('error', (err) ->
      process.stderr.write "error: #{JSON.stringify(err)}")
    .on('add-define', (defineObj) ->
      process.stdout.write "add: #{JSON.stringify(defineObj)}\n")
    .on('remove-define', (removeDefineObj) ->
      process.stdout.write "remove: #{JSON.stringify(removeDefineObj)}\n")
    .on('define-ref', (defineRefObj) ->
      process.stdout.write "ref: #{JSON.stringify(defineRefObj)}\n")
    .on('error', (err) ->
      if err.sourceStream
        console.error err.message
      else
        console.error err.stack
      process.exit 1 if not err.isWarning)
  .pipe(fs.createWriteStream("/dev/null")).on 'error', (err) ->
    console.error err.stack
    process.exit 1
