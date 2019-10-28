{-# LANGUAGE ScopedTypeVariables #-}
module Validation  where
import qualified Data.Bifunctor as DB
import qualified Text.Read as TR
import qualified System.Directory as Dir

validateSingleFileParameter :: [a] -> Either String a
validateSingleFileParameter fs = case fs of
    [f] -> Right f
    _   -> Left "Zero or multiple files sent to server."



validateStringIntParameter :: (String, String) -> String -> (Int,Int) -> Either String (String, Int)
validateStringIntParameter (parameterName, parameterValue) name (min', max') =
    checkName >> convertToInteger >>= checkRange >>= (\x -> return (parameterName, x))
    where
        checkName :: Either String ()
        checkName
            | parameterName == name = Right ()
            | otherwise = Left $ "Actual parameter name: " ++ parameterName ++ " does not match expected parameter name: " ++ name
        convertToInteger :: Either String Int
        convertToInteger =
            DB.first (\err -> "Failed to convert " ++ parameterName ++ " to an Int. Error: " ++ err) (TR.readEither parameterValue)
        checkRange value
            | value < min' = Left ("Error: " ++ parameterName ++ " < " ++ show min')
            | value > max' = Left ("Error: " ++ parameterName ++ " > " ++ show max')
            | otherwise = Right value

validateEnoughSize :: FilePath -> Integer -> IO Bool
validateEnoughSize path sizeLimitGb =
    let gbToBytes = 1024 ^ (3::Integer)
        sizeLimitBytes = sizeLimitGb * gbToBytes in
        do
    dirSize <- Dir.getFileSize path
    return $ dirSize < toInteger sizeLimitBytes
