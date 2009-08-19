(********************) 
(* Module signature *)
(********************) 
open Shredded_main_memory_basetypes
module type Main_Memory_Type =  
sig 
  type t
end
(* Get rid of these! *)
module Int_Type = struct
  type t = int  
end

module String_Type = struct
  type t = string
end

(* These are 'more' strongly typed then the above *)
module Textid_Type = struct
  type t = shredded_textid
end

(* The encoding here should be in *big* endian order,
   because of a quirk in BDB *)
module Preorder_Type = struct
  type t = preorder
end

module Text_Type = struct
  type t = shredded_text
end

module Qnameid_Type = struct
  type t = shredded_qnameid
end

module Qname_Type = struct 
  type t = shredded_qname
end

module Namespaceid_Type = struct
  type t = shredded_namespaceid
end

module Uri_prefix_mapid = struct
  type t = shredded_uri_prefix_mapid
end

(*****************************************)
(* This depends on the size of the types *)
(*****************************************)
module Binding_Type = struct
  type t = shredded_namespaceid * shredded_uri_prefix_mapid list
end
