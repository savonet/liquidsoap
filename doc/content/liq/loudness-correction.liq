# This is the EU R128 standard
settings.lufs.track_gain_target := -23.

# Enable LUFS track gain metadata computation.
enable_lufs_track_gain_metadata()

# Initial source
s = playlist("~/playlist")

# Apply pre-track gain normalization.
s = normalize_track_gain(s)

# END
output.dummy(fallible=true, s)
