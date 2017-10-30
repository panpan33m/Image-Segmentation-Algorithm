structure Primitives : PRIMITIVES =
struct
    fun par (f, g) = (| f (), g () |)

    fun par3 (f, g, h) = (| f (), g (), h () |)

    fun par4 (f, g, h, i) = (| f (), g (), h (), i () |)

    fun parTab (n, f) = let val v = [| f i | i in [| 0 to n |] |]
                        in fn i => PArray.sub (v, i)
                        end
end
