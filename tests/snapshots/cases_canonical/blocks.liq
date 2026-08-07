# Every construct whose canonical JSON carries keyword-anchored block spans:
# `then_block` starts at `then`, `else_block` at `else`, `*_body` at `do`, and
# each spans to the start of the next keyword. Prettier uses those spans to
# decide which block a comment belongs to.
def f(x) =
  if x > 0 then 1 elsif x < 0 then -1 else 0 end
end

while false do f(1)  end

for i = 0 to 2 do f(i)  end

for j = [1, 2] do f(j)  end
