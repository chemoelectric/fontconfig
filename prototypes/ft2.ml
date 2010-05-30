TYPE_CONV_PATH "ft2"

type face
type glyph_slot

let name_id_string = [|
  "Copyright";
  "Family";
  "Style";
  "UniqueID";
  "Fullname";
  "Version";
  "PSName";
  "Trademark";
  "Manufacturer";
  "Designer";
  "Description";
  "VendorURL";
  "DesignerURL";
  "License";
  "LicenseURL";
  "Reserved";
  "PreferredFamily";
  "PreferredStyle";
  "MacFullname";
  "SampleText";
  "CIDfindfontName";
  "WWSFamily";
  "WWSStyle";
|]

type sfntname = {
  sfntname_platform_id : int;
  sfntname_encoding_id : int;
  sfntname_language_id : int;
  sfntname_name_id     : int;
  sfntname_string      : string;
} with sexp

type sfntname_section = {
  sfntname_start : int;
  sfntname_count : int;
} with sexp

type font_family_and_style = {
  sfntname_family : string;    (* TODO: Make this a Unicode string. *)
  sfntname_style  : string;    (* TODO: Make this a Unicode string. *)
} with sexp

external ft_init_freetype         : unit -> unit                                = "Wrapper_FT_Init_FreeType"
external ft_new_face              : string -> face                              = "Wrapper_FT_New_Face"
external ft_attach_file           : face -> string -> int                       = "Wrapper_FT_Attach_File"
external wrapper_ft_set_char_size : face -> int -> int -> unit                  = "Wrapper_FT_Set_Char_Size"
external ft_get_char_index        : face -> int -> int                          = "Wrapper_FT_Get_Char_Index"
external ft_get_first_char        : face -> (int * int)                         = "Wrapper_FT_Get_First_Char"
external ft_get_next_char         : face -> (int * int) -> (int * int)          = "Wrapper_FT_Get_Next_Char"
external ft_load_glyph            : face -> int -> int -> unit                  = "Wrapper_FT_Load_Glyph"
external ft_get_kerning           : face -> int -> int -> int -> (int * int)    = "Wrapper_FT_Get_Kerning"
external ft_get_postscript_name   : face -> string                              = "Wrapper_FT_Get_Postscript_Name"
external face_num_glyphs          : face -> int                                 = "face_num_glyphs"
external face_glyph               : face -> glyph_slot                          = "face_glyph"
external face_metrics             : face -> (int * int * int * int * int * int) = "face_metrics"
external glyph_metrics            : glyph_slot -> (int * int * int * int * int) = "glyph_metrics"
external ft_get_glyph_name        : face -> int -> string                       = "get_glyph_name"
external ft_is_sfnt               : face -> bool                                = "is_sfnt"
external ft_is_postscript         : face -> bool                                = "is_postscript"
external ft_has_ps_glyph_names    : face -> bool                                = "has_ps_glyph_names"
external ft_glyph_to_bitmap       : glyph_slot -> (int * int * int * int * int * string)
                                                                                = "glyph_to_bitmap"
external ft_get_sfnt_name_count   : face -> int                                 = "Wrapper_FT_Get_Sfnt_Name_Count"
external ft_get_sfnt_name         : face -> int -> sfntname                     = "Wrapper_FT_Get_Sfnt_Name"

let ft_load_default       =    0
let ft_load_no_scale      =    1
let ft_load_no_hinting    =    2
let ft_load_render        =    4
let ft_load_monochrome    = 4096
let ft_load_linear_design = 8192

let ft_kerning_default    =    0
let ft_kerning_unfitted   =    1
let ft_kerning_unscaled   =    2

module Sfnt_name_table :
sig
  type t
  val make           : face -> t
  val platforms      : t -> sfntname_section list
  val encodings      : t -> sfntname_section -> sfntname_section list
  val languages      : t -> sfntname_section -> sfntname_section list
  val get_string     : t -> sfntname_section -> int -> int
  val family_style_4member   : t -> sfntname_section -> font_family_and_style
  val family_style_preferred : t -> sfntname_section -> font_family_and_style
  val family_style_wws       : t -> sfntname_section -> bool -> font_family_and_style
end =
struct
  type t = sfntname array

  let make f =
    let name_count = ft_get_sfnt_name_count f in
    let rec get_part part i =
      if i < 0 then
        part
      else
        get_part (ft_get_sfnt_name f i :: part) (i - 1)
    in
    Array.of_list (get_part [] (name_count - 1))

  let platforms name_table =
    let table_length = Array.length name_table in
    let platform_list = ref [] in
    let i = ref (table_length - 1) in
    while 0 <= !i do
      let platform_id = name_table.(!i).sfntname_platform_id in
      let j = ref (!i - 1) in
      while 0 <= !j && name_table.(!j).sfntname_platform_id = platform_id do
        j := !j - 1
      done;
      platform_list := { sfntname_start = !j + 1;
                         sfntname_count = !i - !j } :: !platform_list;
      i := !j
    done;
    !platform_list

  let encodings name_table platform_section =
    let encoding_list = ref [] in
    let i = ref (platform_section.sfntname_start + platform_section.sfntname_count - 1) in
    while platform_section.sfntname_start <= !i do
      let encoding_id = name_table.(!i).sfntname_encoding_id in
      let j = ref (!i - 1) in
      while platform_section.sfntname_start <= !j && name_table.(!j).sfntname_encoding_id = encoding_id do
        j := !j - 1
      done;
      encoding_list := { sfntname_start = !j + 1;
                         sfntname_count = !i - !j } :: !encoding_list;
      i := !j
    done;
    !encoding_list

  let languages name_table encoding_section =
    let language_list = ref [] in
    let i = ref (encoding_section.sfntname_start + encoding_section.sfntname_count - 1) in
    while encoding_section.sfntname_start <= !i do
      let language_id = name_table.(!i).sfntname_language_id in
      let j = ref (!i - 1) in
      while encoding_section.sfntname_start <= !j && name_table.(!j).sfntname_language_id = language_id do
        j := !j - 1
      done;
      language_list := { sfntname_start = !j + 1;
                         sfntname_count = !i - !j } :: !language_list;
      i := !j
    done;
    !language_list

  let get_string name_table language_section name_id =
    let rec get i count =
      if count = 0 then
        (-1)
      else if name_table.(i).sfntname_name_id = name_id then
        i
      else
        get (i + 1) (count - 1)
    in
    get language_section.sfntname_start language_section.sfntname_count

  let family_style_4member name_table language_section =
    let family = get_string name_table language_section 1 in
    if family < 0 then
      { sfntname_family = "";
        sfntname_style  = "" }
    else
      let style = get_string name_table language_section 2 in
      if style < 0 then
        { sfntname_family = name_table.(family).sfntname_string;
          sfntname_style  = "Regular" }
      else
        { sfntname_family = name_table.(family).sfntname_string;
          sfntname_style  = name_table.(style).sfntname_string }

  let family_style_preferred name_table language_section =
    let family =
      let preferred_family = get_string name_table language_section 16 in
      if 0 <= preferred_family then
        preferred_family
      else
        get_string name_table language_section 1
    in
    if family < 0 then
      { sfntname_family = "";
        sfntname_style  = "" }
    else
      let style =
        let preferred_style = get_string name_table language_section 17 in
        if 0 <= preferred_style then
          preferred_style
        else
          get_string name_table language_section 2
      in
      if style < 0 then
        { sfntname_family = name_table.(family).sfntname_string;
          sfntname_style  = "Regular" }
      else
        { sfntname_family = name_table.(family).sfntname_string;
          sfntname_style  = name_table.(style).sfntname_string }

  let family_style_wws name_table language_section has_wws = (* has_wws = fsSelection bit 8 *)
    if not has_wws then
      family_style_preferred name_table language_section
    else
      let family = get_string name_table language_section 21 in
      if family < 0 then
        { sfntname_family = "";
          sfntname_style  = "" }
      else
        let style = get_string name_table language_section 22 in
        if style < 0 then
          { sfntname_family = name_table.(family).sfntname_string;
            sfntname_style  = "Regular" }
        else
          { sfntname_family = name_table.(family).sfntname_string;
            sfntname_style  = name_table.(style).sfntname_string }
end

let main () =
  ft_init_freetype ();
  let my_face = ft_new_face "/home/trashman/.fonts/BriosoPro-MediumItSubh.otf" in
  let table = Sfnt_name_table.make my_face in
  let platforms = Sfnt_name_table.platforms table in
  let _ =
    List.iter
      (fun plat ->
        print_endline (Sexplib.Sexp.to_string (sexp_of_sfntname_section plat));
        let encodings = Sfnt_name_table.encodings table plat in
        (List.iter
          (fun enc ->
            print_endline (Sexplib.Sexp.to_string (sexp_of_sfntname_section enc));
            let languages = Sfnt_name_table.languages table enc in
            print_endline (Sexplib.Sexp.to_string (Sexplib.Conv.sexp_of_list sexp_of_sfntname_section languages));
            print_endline "")
          encodings))
      platforms
  in
  ()
;;

main ()
