mono = "mono.ttf"
# BEGIN
file = single("bus.ogg")

output.alsa(file)

file = source.mux.video(video=blank(), file)

istring = interactive.string

v = add([
         # video.text("Hello world..."),
         # video.text("Mip mip!",color=0xff0000,y=-1,speed=300),
         video.translate(video.text(istring("a",""),color=0xff0000,font=mono,size=30), y=-01),
         video.translate(video.text(istring("b",""),color=0xff0000,font=mono,size=30) ,y=-31),
         video.translate(video.text(istring("c",""),color=0xff0000,font=mono,size=30) ,y=-61)
])
v = source.mux.audio(audio=file, v)

# output.file.theora("out.ogv",
output.sdl(add([video.fade.in(video.fade.in(transition="disc", v)),
                # video.rotate(video.scale(coef=0.3,offset_x=20,offset_y=20,v)),
                image("chameau.pnm", width=50, height=50, x=-10, y=10),
]))

text = fun (v,s) -> ignore(server.execute("var.set #{v} = \"#{s}\""))

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
  thread.run(delay=1., { ignore(server.execute("anim #{s}")) })
  "Done."
end)
# END
