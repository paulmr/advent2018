local fun = require("fun")

local function tick(e1, e2, rs)
   local sum = rs[e1+1] + rs[e2+1]
   local d1 = sum % 10
   if (sum // 10) > 0 then
      table.insert(rs, (sum // 10) % 10)
   end
   table.insert(rs, d1)
   return (e1 + rs[e1+1] + 1) % #rs, (e2 + rs[e2+1] + 1) % #rs, rs
end

function iter(st, i)
   if i > #st.rs then
      e1, e2, rs = tick(st.e1, st.e2, st.rs)
      st.e1 = e1
      st.e2 = e2
   end
   return i+1, st.rs[i]
end

local function gen(e1, e2, rs, from)
   return iter, { e1 = e1, e2 = e2, rs = rs }, from or 1
end

print("Part 1: " ..
   table.concat(
      fun.iter(gen(0, 1, { 3, 7 }))
         :drop(503761):take(10):totable()
   )
)

local pat = { 5,0,3,7,6,1 }
local pi = 1
for i, n in gen(0, 1, { 3, 7 }) do
   if pat[pi] == n then
      pi = pi + 1
      if pi > #pat then
         print("Part 2: " .. (i - #pat - 1))
         break
      end
   else
      pi = 1
   end
end
