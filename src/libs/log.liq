# Log a critical message
# @category Liquidsoap
def log.critical(~label="lang",msg) =
  log(label=label,level=1,msg)
end

# Log a severe message
# @category Liquidsoap
def log.severe(~label="lang",msg) =
  log(label=label,level=2,msg)
end

# Log an important message
# @category Liquidsoap
def log.important(~label="lang",msg) =
  log(label=label,level=3,msg)
end

# Log a normal message
# @category Liquidsoap
def log.info(~label="lang",msg) =
  log(label=label,level=4,msg)
end

# Log a debug message
# @category Liquidsoap
def log.debug(~label="lang",msg) =
  log(label=label,level=5,msg)
end

# Get and set the log level.
# @category Liquidsoap
def log.level =
  settings.log.level
end

# Get and set the file logging
# @category Liquidsoap
def log.file =
  settings.log.file
end

# Get and set logging to stdout
# @category Liquidsoap
def log.stdout =
  settings.log.stdout
end
