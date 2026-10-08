open Ctypes

module Types (T : Ctypes.TYPE) = struct
  open T
  (* let ns_lv2 = constant "LILV_NS_LV2" string *)

  module LV2 = struct
    type handle = unit ptr

    let handle : handle typ = ptr void

    type descriptor

    let descriptor : descriptor structure typ = structure "LV2_Descriptor"

    let descriptor_instantiate_ptr_type =
      static_funptr
        (ptr descriptor @-> double @-> string @-> ptr void @-> returning handle)

    let descriptor_connect_port_type =
      handle @-> uint32_t @-> ptr void @-> returning void

    let descriptor_connect_port_ptr_type =
      static_funptr descriptor_connect_port_type

    let descriptor_run_type = handle @-> uint32_t @-> returning void
    let descriptor_run_ptr_type = static_funptr descriptor_run_type
    let descriptor_activate_type = handle @-> returning void
    let descriptor_activate_ptr_type = static_funptr descriptor_activate_type
    let descriptor_deactivate_type = handle @-> returning void

    let descriptor_deactivate_ptr_type =
      static_funptr descriptor_deactivate_type

    let descriptor_cleanup_ptr_type = static_funptr (handle @-> returning void)

    let descriptor_extension_data_ptr_type =
      static_funptr (string @-> returning (ptr void))
  end
end
