struct

  module IntOrder =
    struct
      type t = int
      val equal x y = x == y
      val less  x y = x < y
    end

  module OrderFunctor =
    functor (Ord: sig type t val equal: t->t->bool val less: t->t->bool end)
      struct
        type t = Ord.t
        val equal p q = Ord.equal (fst p) (fst q)
        val less p q = Ord.less (fst p) (fst q) 
      end

  module IntPairOrder = OrderFunctor(IntOrder)

end
