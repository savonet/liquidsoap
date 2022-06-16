# Test argsof directive.

def f() =
  # file.extension: (?dir_sep : string, ?leading_dot : bool, string) -> string

  # Add all labeled arguments
  def g(%argsof(file.extension)) =
    "#{dir_sep}-#{leading_dot}"
  end

  if g() != "/-true" then test.fail() end
  if g(dir_sep="gni") != "gni-true" then test.fail() end
  if g(leading_dot=false) != "/-false" then test.fail() end
  if g(dir_sep="gni",leading_dot=false) != "gni-false" then test.fail() end

  # Add all but leading dot. Add unlabeled argument to test as well
  def g(%argsof(file.extension[!leading_dot]), x) =
    "#{dir_sep}-#{x}"
  end

  if g("bla") != "/-bla" then test.fail() end
  if g(dir_sep="gni","bla") != "gni-bla" then test.fail() end

  # Add only leading_dot
  def g(%argsof(file.extension[leading_dot]), x) =
    "#{leading_dot}-#{x}"
  end

  if g(leading_dot=false,"bla") != "false-bla" then test.fail() end

  # Test argsof call
  def h(~leading_dot=false,~dir_sep="no_dir_sep") =
    "#{leading_dot}-#{dir_sep}"
  end

  leading_dot = true
  dir_sep = "/"

  if h(%argsof(file.extension)) != "true-/" then test.fail() end
  if h(%argsof(file.extension[!leading_dot])) != "false-/" then test.fail() end
  if h(%argsof(file.extension[leading_dot])) != "true-no_dir_sep" then test.fail() end

  test.pass()
end

test.check(f)
