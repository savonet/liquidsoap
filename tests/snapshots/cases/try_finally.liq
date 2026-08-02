r = ref(0)
try
  r := 1
finally
  r := 2
end
r()
