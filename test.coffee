isInString = no
numChars = 0

process.stdin.on 'data', (data) ->
  str = data.toString()
  for c in str
    ++numChars
    isInString = not isInString if c is "\""
    if c is "," and not isInString
      console.error str
      for i in [0..100] by 1
        console.error "--"
      console.error "exited at char #{numChars}"
      process.exit 1

process.stdin.on 'end', ->
  console.error "didn't fuck up somehow?"
