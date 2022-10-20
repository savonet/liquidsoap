let liquidsoap.chroot = ()

# Export all the files required to install liquidsoap in a root folder. Useful
# for packaging and docker images.
# @category Liquidsoap
def liquidsoap.chroot.make(chroot) =
  def chroot(p) =
    path.concat(chroot, p)
  end

  def mkdir(p) =
    process.run("mkdir -p #{process.quote(p)}")
  end

  def cp(source) =
    mkdir(chroot(path.dirname(source)))
    process.run("cp -rf #{process.quote(source)} #{process.quote(chroot(source))}")
  end

  cp(configure.libdir)
  cp(configure.bindir)

  if file.exists(configure.default_font) then
    cp(configure.default_font)
  end

  mkdir(chroot(configure.logdir))
  mkdir(chroot(configure.rundir))

  cp(liquidsoap.executable)

  ()
end
