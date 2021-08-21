module AltRep where


    import Diagrams.Prelude (Semigroup)


    data Edge a = Edge a (Node a)
    data Node a = Source [Edge a] [Edge a]
                | Sink [Edge a] [Edge a]
                | Node [Edge a] [Edge a]

    getSource :: Node a -> Maybe Node a
    getSource Source x y = Source x y
    getSource Node xs _ = 

    getSink :: Node a -> Maybe Node a
    getSink = error "not implemented"


    example = Source [] [Edge 1 (Sink [] [])]

    instance Semigroup a => Semigroup (Node a) where
        Sink xpre xpost <> Source ypre ypost = Node (xpre++ypre) (xpost ++ ypost)
        x <> y = getSink x <> getSource y

