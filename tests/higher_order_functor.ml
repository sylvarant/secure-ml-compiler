(* test that the compiler can securely compile a functor
that returns another functor, and thus supports multi argument functors *)
struct

  module IntOrder =
    struct
      type t = int
      val equal x y = x == y
      val less  x y = x < y
    end

  module LexicographicOrder =
    functor (Ord1: sig type t = int val equal: t->t->bool val less: t->t->bool end)
    functor (Ord2: sig type t = int val equal: t->t->bool val less: t->t->bool end)
      struct
        type z = Ord1.t * Ord2.t
        val equal p : z q : z =
          if Ord1.equal (fst p) (fst q)
          then Ord2.equal (snd p) (snd q)
          else false
        val less p : z q : z =
          if Ord1.less (fst p) (fst q) then true
          else if Ord1.equal (fst p) (fst q) then Ord2.less (snd p) (snd q)
          else false
      end

  module IntPairOrder = LexicographicOrder(IntOrder)(IntOrder)

end

