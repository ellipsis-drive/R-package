token <- EDPackage::account.logIn("BStarkenburg", "Xpj3YhajXuE3yJw")

EDPackage::account.accessToken.create(description = "hoi", accessList = list('pathId'= '46e1e919-8b73-42a3-a575-25c6d45fd93b' , 'access' = list('accessLevel'=100)), token = token)
