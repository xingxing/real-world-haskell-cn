module VCard where

-- file: ch15/VCard.hs

data Context = Home | Mobile | Business
               deriving (Eq, Show)

type Phone = String
             
albulena = [(Home, "+355-652-55512")]

nils = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"),
        (Home, "+47-925-55-121"), (Business, "+47-922-25-551")]

twalumba = [(Business, "+260-02-55-5121")]             

-- file: ch15/VCard.hs

onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
                        Nothing -> lookup Mobile ps
                        Just  n -> Just n

-- file: ch15/VCard.hs

allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd numbers
    where numbers = case filter (contextIs Business) ps of
                      [] -> filter (contextIs Mobile) ps
                      ns -> ns

contextIs a (b, _) = a == b
              
-- file: ch15/VCard.hs
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a

-- file: ch15/VCard.hs
instance MonadPlus [] where
    mzero = []
    mplus = (++)

instance MonadPlus Maybe where
    mzero = Nothing

    Nothing `mplus` ys = ys
    xs      `mplus` _  = xs 

-- file: ch15/VCard.hs
oneBussinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBussinessPhone ps = lookup Business ps `mplus` lookup Mobile ps

allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd $ filter (contextIs Home) ps `mplus`
                                  filter (contextIs Mobile) ps

-- file: ch15/VCard.hs
lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup _ []                       = Nothing
lookup k ((x,y):xys) | x == k     = Just y
                      | otherwise  = lookup k xys

-- file: ch15/VCard.hs
lookup :: (Eq a) => a -> [(a, b)] -> Maybe b
lookup _ []        =  Nothing
lookup k ((x,y):xys) | x == k = Just y
                     | otherwise = lookup k xys
