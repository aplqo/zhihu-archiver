module ZhArchiver.RawParser.Util (appUnless, unlessMaybe) where

appUnless :: (a -> Bool) -> (a -> b) -> a -> Maybe b
appUnless p r v =
  if p v
    then Nothing
    else Just (r v)

unlessMaybe :: (a -> Bool) -> a -> Maybe a
unlessMaybe p a =
  if p a
    then Nothing
    else Just a