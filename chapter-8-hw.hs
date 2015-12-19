sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (m:ms) = m >>= \ a -> do as <- sequence' ms
                                   return (a: as)

sequence'' ms = foldr func (return []) ms
  where
    func :: (Monad m) => m a -> m [a] -> m [a]
    func m acc = do x <- m
                    xs <- acc
                    return (x: xs)

sequence''' [] = return []
sequence''' (m : ms) = do a <- m
                          as <- sequence''' ms
                          return (a:as)
{--
sequence'''' [] = return []
sequence'''' (m : ms) = m >>= \ a -> do as <- sequence'''' ms
                                      return (a : as)
--}

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f as = sequence' (map f as)

mapM'' f [] = return []
mapM'' f (a : as) = f a >>= \ b -> mapM'' f as >>= \ bs -> return (b : bs)

sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (m : ms) = m >> sequence_' ms

--mapM''' :: Monad m => (a -> m b) -> [a] -> m [b]
--mapM''' f as = sequence_' (map f as)

mapMM f [] = return []
mapMM f (a : as) = do b <- f a
                      bs <- mapMM f as
                      return (b : bs)

mapMM' f [] = return []
mapMM' f (a : as) = f a >>= \ b ->
                            do bs <- mapMM' f as
                               return (b : bs)

mapMM'' f [] = return []
mapMM'' f (a : as) = f a >>= \ b ->
                              do bs <- mapMM'' f as
                                 return (bs ++ [b])

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' _ [] = return []
filterM' p (x : xs) = do flag <- p x
                         ys <- filterM' p xs
                         if flag then return (x: ys) else return ys

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldLeftM f a [] = return a
foldLeftM f a (x: xs) = do z <- f a x
                           foldLeftM f z xs

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b
foldRightM f a [] = return a
foldRightM f a as = do z <- f (last as) a
                       foldRightM f z $ init as


liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = do a <- m
               return (f a)

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f m = m >>= \ a -> return (f a)

liftM'' :: Monad m => (a -> b) -> m a -> m b
liftM'' f m = m >>= \ a -> m >>= \ b -> return (f a)

liftM''' :: Monad m => (a -> b) -> m a -> m b
liftM''' f m = m >>= \ a -> m >>= \ b -> return (f b)

--liftMM f m = mapM f [m]

