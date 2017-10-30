signature SEQUENCE_EXTRA =
sig
  include SEQUENCE

  (* Useful "fusioned" operations *)
  structure Fusion :
  sig
    (* (mapMerge cmp (f, g) (s, t)) merges (map f s) with (map f t), except
     * that it compares elements according to their values before the map.
     * Formally, equivalent to
     *   map #2 (merge cmp' (zip (s, s'), zip (t, t')))
     *   where s' = map f s
     *         t' = map g t
     *         cmp' = fn ((x, _), (y, _)) => cmp (x, y) *)
    val mapMerge : 'a ord -> (('a -> 'b) * ('a -> 'b)) -> 'a seq * 'a seq -> 'b seq

    (* (mapReduce f g b s) is logically equivalent to (reduce g b (map f s)) *)
    val mapReduce : ('a -> 'b) -> ('b * 'b -> 'b) -> 'b -> 'a seq -> 'b

    (* (mapScan f g b s) is logically equivalent to (scan g b (map f s)) *)
    val mapScan : ('a -> 'b) -> ('b * 'b -> 'b) -> 'b -> 'a seq -> 'b seq * 'b

    (* (mapScanIncl f g b s) is logically equivalent to (scanIncl g b (map f s)) *)
    val mapScanIncl : ('a -> 'b) -> ('b * 'b -> 'b) -> 'b -> 'a seq -> 'b seq

    (* (mapFilter f s) is logically equivalent to
     * (map valOf (filter isSome (map f s))) *)
    val mapFilter : ('a -> 'b option) -> 'a seq -> 'b seq

    (* (mapFilterIdx f s) is logically equivalent to
     * (map valOf (filter isSome (mapIdx f s))) *)
    val mapFilterIdx : (int * 'a -> 'b option) -> 'a seq -> 'b seq

    (* (shallowReduce f g s) performs a reduce which switches to a
     * (usually sequential) implementation at small input sizes.
     * Formally it is equivalent to (reduce g b (map f s')) for some s' where
     * flatten s' = s, and where b = f (empty ()) *)
    val shallowReduce : ('a seq -> 'b) -> ('b * 'b -> 'b) -> 'a seq -> 'b
  end

end
