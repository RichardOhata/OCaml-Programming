type 'a ref' = {mutable contents': 'a}

let ref' x = {contents' = x}

let ( ! ) {contents'} = contents'

let ( := ) r x = r.contents' <- x

