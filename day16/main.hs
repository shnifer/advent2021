main::IO ()
main = do
    print $ solve1 input
    print $ solve2 input
    print "Done"

type Bits = [Int]
data Packet = Packet {version::Int, typeID::Int, payload::Payload} deriving (Eq,Show)
data Payload = Literal Int | Operator [Packet] deriving (Eq,Show)

data Parser a = Parser {runParser :: Bits -> (a, Bits)}
instance Functor Parser where
    fmap g (Parser f) = Parser $ \b -> let (a, t) = f b in (g a, t)
instance Applicative Parser where
    pure x = Parser $ \b -> (x, b)
    pf <*> pv = Parser $ \b -> let
        (f, t) = runParser pf b
        (v, t2) = runParser pv t
        in (f v, t2)
instance Monad Parser where
-- Parser a >>= (a->Parser b) = Parser b
    pa >>= f = Parser $ \x -> let
     (a, t) = runParser pa x
     pb = f a
     in runParser pb t


solve1 str = let
    bits = hexToBits str
    (packet, _) = runParser parsePacket bits
    in sumVersions packet

solve2 str = let
    bits = hexToBits str
    (packet, _) = runParser parsePacket bits
    in calcValue packet

calcValue::Packet -> Int
calcValue (Packet _ 4 (Literal v)) = v
calcValue (Packet _ tid (Operator subs)) = let
    vals = map calcValue subs
    in case tid of
        0 -> sum vals
        1 -> product vals
        2 -> minimum vals
        3 -> maximum vals
        5 -> if vals!!0 > vals!!1 then 1 else 0
        6 -> if vals!!0 < vals!!1 then 1 else 0
        7 -> if vals!!0 == vals!!1 then 1 else 0

sumVersions::Packet->Int
sumVersions (Packet v _ (Literal _)) = v
sumVersions (Packet v _ (Operator subs)) = v + ( sum $ map sumVersions subs )

parsePacket::Parser Packet
parsePacket = do
    version <- parseNBitsInt 3
    typeID <- parseNBitsInt 3
    payload <- parsePayload typeID
    pure $ Packet {version = version, typeID = typeID, payload = payload}

parsePayload::Int->Parser Payload
parsePayload 4 = Literal <$> bitsToInt <$> parseLiteral
parsePayload _ = Operator <$> parseOperator

parseOperator::Parser [Packet]
parseOperator = do
    lt <- parseBool
    if lt
        then do
            count <- parseNBitsInt 11
            packets <- parseNPackets count
            pure packets
        else do
            l <- parseNBitsInt 15
            packets <- parsePacketsFromBits l
            pure packets

parseNPackets::Int -> Parser [Packet]
parseNPackets count = sequence $ (replicate count parsePacket)

parsePacketsFromBits::Int -> Parser [Packet]
parsePacketsFromBits len = do
    bits <- parseNBits len
    pure $ allPackets bits

allPackets::[Int] -> [Packet]
allPackets [] = []
allPackets bits = let
    (p, t) = runParser parsePacket bits
    in p:(allPackets t)

parseLiteral::Parser Bits
parseLiteral = do
    (v, more) <- parseLitChunk
    if not more then pure v
        else do
            tv <- parseLiteral
            pure $ v ++ tv


parseLitChunk::Parser (Bits, Bool)
parseLitChunk = do
    more <- parseBool
    val <- parseNBits 4
    pure $ (val, more)

parseBool::Parser Bool
parseBool = Parser $ \bits -> (head bits == 1, tail bits)

parseNBits::Int -> Parser Bits
parseNBits n = Parser $ \bits -> (take n bits, drop n bits)

parseNBitsInt::Int -> Parser Int
parseNBitsInt n = bitsToInt <$> parseNBits n

bitsToInt::Bits->Int
bitsToInt bits = foldl (\v b -> v*2+b) 0 bits

hexToBits::String->Bits
hexToBits str = concatMap f str where
    f '0' = [0,0,0,0]
    f '1' = [0,0,0,1]
    f '2' = [0,0,1,0]
    f '3' = [0,0,1,1]
    f '4' = [0,1,0,0]
    f '5' = [0,1,0,1]
    f '6' = [0,1,1,0]
    f '7' = [0,1,1,1]
    f '8' = [1,0,0,0]
    f '9' = [1,0,0,1]
    f 'A' = [1,0,1,0]
    f 'B' = [1,0,1,1]
    f 'C' = [1,1,0,0]
    f 'D' = [1,1,0,1]
    f 'E' = [1,1,1,0]
    f 'F' = [1,1,1,1]

test1 = "D2FE28"
test2 = "38006F45291200"
test3 = "EE00D40C823060"
input = "020D74FCE27E600A78020200DC298F1070401C8EF1F21A4D6394F9F48F4C1C00E3003500C74602F0080B1720298C400B7002540095003DC00F601B98806351003D004F66011148039450025C00B2007024717AFB5FBC11A7E73AF60F660094E5793A4E811C0123CECED79104ECED791380069D2522B96A53A81286B18263F75A300526246F60094A6651429ADB3B0068937BCF31A009ADB4C289C9C66526014CB33CB81CB3649B849911803B2EB1327F3CFC60094B01CBB4B80351E66E26B2DD0530070401C82D182080803D1C627C330004320C43789C40192D002F93566A9AFE5967372B378001F525DDDCF0C010A00D440010E84D10A2D0803D1761045C9EA9D9802FE00ACF1448844E9C30078723101912594FEE9C9A548D57A5B8B04012F6002092845284D3301A8951C8C008973D30046136001B705A79BD400B9ECCFD30E3004E62BD56B004E465D911C8CBB2258B06009D802C00087C628C71C4001088C113E27C6B10064C01E86F042181002131EE26C5D20043E34C798246009E80293F9E530052A4910A7E87240195CC7C6340129A967EF9352CFDF0802059210972C977094281007664E206CD57292201349AA4943554D91C9CCBADB80232C6927DE5E92D7A10463005A4657D4597002BC9AF51A24A54B7B33A73E2CE005CBFB3B4A30052801F69DB4B08F3B6961024AD4B43E6B319AA020020F15E4B46E40282CCDBF8CA56802600084C788CB088401A8911C20ECC436C2401CED0048325CC7A7F8CAA912AC72B7024007F24B1F789C0F9EC8810090D801AB8803D11E34C3B00043E27C6989B2C52A01348E24B53531291C4FF4884C9C2C10401B8C9D2D875A0072E6FB75E92AC205CA0154CE7398FB0053DAC3F43295519C9AE080250E657410600BC9EAD9CA56001BF3CEF07A5194C013E00542462332DA4295680"