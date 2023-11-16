file = single("bus.ogg")

istring = interactive.string

v = add([file,
         # video.text("Hello world..."),
         # video.text("Mip mip!",color=0xff0000,y=-1,speed=300),
         video.text(istring("a",""),color=0xff0000,font=mono,size=30,y=-01),
         video.text(istring("b",""),color=0xff0000,font=mono,size=30,y=-31),
         video.text(istring("c",""),color=0xff0000,font=mono,size=30,y=-61),
])

output.alsa(drop_video(file))

# output.file.theora("out.ogv",
output.sdl(add([video.fade.in(video.fade.in(transition="disc",v)),
                # video.rotate(video.scale(coef=0.3,offset_x=20,offset_y=20,v)),
               video.image("chameau.pnm",width=50,height=50,
                           x=-10,y=10,alpha=0xffffff),
]))

text = fun (v,s) -> ignore(server.execute('var.set #{v} = "#{s}"'))

def pingouin()
  text("c","(o_ ")
  text("b","//\\ ")
  text("a","v_/_")
end

def pan()
  text("c","(X_ ")
end

server.register("pingouin", fun (_) -> begin pingouin() "Oui seigneur..." end)
server.register("pan", fun (_) -> begin pan() "Zog zog!" end)

server.register("anim", fun (s) -> begin
  s = if s=="t" then
    pan()
    "f"
  else
    pingouin()
    "t"
  end
  add_timeout(1.,{ ignore(server.execute("anim #{s}")) (-1.) })
  "Done."
end)
