def use(x = {int}) = x() end
(use(1), use(fun () -> 2))
