import Data.Char
import Data.List

type Version = Int
type Type = Int
type Bits = [Bool]
data Packet = Literal Version Int
            | Operator Version Type [Packet]
            deriving (Eq, Show)

parseHex :: Char -> [Bool]
parseHex c | ord c >= ord '0' && ord c <= ord '9' = toBits (ord c - ord '0')
           | ord c >= ord 'A' && ord c <= ord 'F' = toBits (10 + ord c - ord 'A')
           | c == '\n' = []
           | otherwise = error ("Not a hex: '" ++ [c] ++ "'")

toBits :: Int -> [Bool]
toBits 0 = [False]
toBits n = reverse $ toBits' n
        where toBits' 0 = []
              toBits' x | x `mod` 2 == 1 = True : toBits' (x `div` 2)
                        | otherwise = False : toBits' (x `div` 2)

bitsToBinary :: Bits -> String
bitsToBinary = map toBit
        where toBit False = '0'
              toBit True = '1'

leadingBits :: [Bool] -> [Bool]
leadingBits bools = replicate (4 - length bools) False ++ bools

bitsToNumber :: Bits -> Int
bitsToNumber = sum . reverse . zipWith (\n b -> if b then n else 0) binaryValues . reverse
        where binaryValues = [2^n | n <- [0..]]

parseHeader :: [Bool] -> (Version, Type, [Bool])
parseHeader bools = (bitsToNumber (take 3 bools), bitsToNumber (take 3 $ drop 3 bools), drop 6 bools)

hexToBits :: String -> Bits
hexToBits = concatMap (leadingBits . parseHex) . filter (/= '\n')

readPacket :: Bits -> (Packet, Bits)
readPacket bits = readPacket' version typeId packetBits
        where (version, typeId, packetBits) = parseHeader bits

readPacket' :: Version -> Type -> Bits -> (Packet, Bits)
readPacket' version 4 bits = readLiteralPacket version bits
readPacket' version typeId bits = readOperatorPacket version typeId bits

readLiteralPacket :: Version -> Bits -> (Packet, Bits)
readLiteralPacket version bits = (Literal version (bitsToNumber numBits), restBits)
        where (numBits, restBits) = literalNumBits bits

literalNumBits :: Bits -> (Bits, Bits)
literalNumBits bits = (numBits, drop usedBits bits)
        where (numBits, usedBits) = literalNumBits' bits [] 0

literalNumBits' :: Bits -> Bits -> Int -> (Bits, Int)
literalNumBits' (False:b1:b2:b3:b4:_) result n = (reverse ([b4, b3, b2, b1] ++ result), n + 5)
literalNumBits' (True:b1:b2:b3:b4:bs) result n = literalNumBits' bs (b4:b3:b2:b1:result) (n + 5)
literalNumBits' _ _ _ = undefined

readOperatorPacket :: Version -> Type -> Bits -> (Packet, Bits)
readOperatorPacket v t [] = undefined
readOperatorPacket version typeId (False:bits) = (Operator version typeId subPackets, restBits ++ afterSubPacketBits)
        where subPacketBitLength = bitsToNumber (take 15 bits)
              subPacketBits = take subPacketBitLength (drop 15 bits)
              afterSubPacketBits = drop (15 + subPacketBitLength) bits
              (subPackets, restBits) = readSubPacketsWithLength subPacketBits
readOperatorPacket version typeId (True:bits) = (Operator version typeId subPackets, restBits)
        where subPacketCount = bitsToNumber (take 11 bits)
              subPacketBits = drop 11 bits
              (subPackets, restBits) = readSubPacketsCount subPacketBits subPacketCount

readSubPacketsWithLength :: Bits -> ([Packet], Bits)
readSubPacketsWithLength bits = readSubPacketsWithLength' bits []

readSubPacketsWithLength' :: Bits -> [Packet] -> ([Packet], Bits)
readSubPacketsWithLength' [] packets = (reverse packets, [])
readSubPacketsWithLength' bits packets = readSubPacketsWithLength' restBits (packet:packets)
        where (packet, restBits) = readPacket bits

readSubPacketsCount :: Bits -> Int -> ([Packet], Bits)
readSubPacketsCount bits n = readSubPacketsCount' bits n []

readSubPacketsCount' :: Bits -> Int -> [Packet] -> ([Packet], Bits)
readSubPacketsCount' bits 0 packets = (reverse packets, bits)
readSubPacketsCount' bits n packets = readSubPacketsCount' restBits (n - 1) (packet:packets)
        where (packet, restBits) = readPacket bits


sumAllVersions :: Packet -> Int
sumAllVersions (Literal version _) = version
sumAllVersions (Operator version _ packets) = version + sum (map sumAllVersions packets)

evalPacket :: Packet -> Int
evalPacket (Literal _ val) = val
evalPacket (Operator _ 0 packets) = sum $ map evalPacket packets
evalPacket (Operator _ 1 packets) = product $ map evalPacket packets
evalPacket (Operator _ 2 packets) = minimum $ map evalPacket packets
evalPacket (Operator _ 3 packets) = maximum $ map evalPacket packets
evalPacket (Operator _ 5 [p1, p2]) = if evalPacket p1 > evalPacket p2 then 1 else 0
evalPacket (Operator _ 6 [p1, p2]) = if evalPacket p1 < evalPacket p2 then 1 else 0
evalPacket (Operator _ 7 [p1, p2]) = if evalPacket p1 == evalPacket p2 then 1 else 0
evalPacket Operator {} = undefined



main = do input <- getContents
          let bits = hexToBits input
              (packet, restBits) = readPacket bits
          --print packet
          --print restBits
          print $ sumAllVersions packet
          --print $ readPacket $ hexToBits "8A004A801A8002F478"
          --print $ sumAllVersions $ fst $ readPacket $ hexToBits "8A004A801A8002F478"
          --print $ readPacket $ hexToBits "620080001611562C8802118E34"
          --print $ sumAllVersions $ fst $ readPacket $ hexToBits "620080001611562C8802118E34"
          --print $ readPacket $ hexToBits "C0015000016115A2E0802F182340"
          --print $ sumAllVersions $ fst $ readPacket $ hexToBits "C0015000016115A2E0802F182340"
          --print $ readPacket $ hexToBits "A0016C880162017C3686B18A3D4780"
          --print $ sumAllVersions $ fst $ readPacket $ hexToBits "A0016C880162017C3686B18A3D4780"
          print $ evalPacket packet
