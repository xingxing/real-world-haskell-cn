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

allPersonalPhone :: [(Context, Phone)] -> Phone
allPersonalPhone ps = map snd number
    where numbers = case filter (contextIs Business) ps of
                      [] -> filter (contextIs Mobile) ps
                      ns -> ns

contextIs a (b, _) = a == b
