(* For consistency between compiling with mlton-spoonhower and smlnj *)
structure TransparentSerialArraySequence :> SEQUENCE where type 'a t = 'a ArraySlice.slice = TransparentArraySequence
