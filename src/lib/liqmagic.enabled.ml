let magic_cookie = Magic.create ~flags:[Magic.Mime; Magic.Symlink] []
let file_mime f = Some (Magic.file magic_cookie f)
let data_mime d = Some (Magic.buffer magic_cookie d)
