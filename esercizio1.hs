type Name = String

type PhoneNumber = String

type PhoneBook = [(Name, PhoneNumber)]

getPhoneNumber :: Name -> PhoneBook -> Maybe PhoneNumber
getPhoneNumber name = foldr (\(k, v) acc -> if k == name then Just v else acc) Nothing