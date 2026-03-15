def f() =
  let json.parse (test_cases :
    [
      {
        crontab_entry: string,
        time: {day: int, hour: int, min: int, month: int, year: int},
        result: bool
      }
    ]
  ) = file.contents("crontab_test_cases.json")

  list.iter(
    fun ({crontab_entry, time = t, result}) ->
      begin
        let {test = test_fn} = cron.parse(crontab_entry)
        secs = time.make(t.{dst=null, sec=0})

        if
          not (test_fn(time=secs) == result)
        then
          test.fail(
            "Cron pattern #{crontab_entry} should be #{result} at #{t}"
          )
        else
          log.important(
            "Cron pattern #{crontab_entry} is #{result} at #{t}"
          )
        end
      end,
    test_cases
  )

  test.pass()
end

test.check(f)
