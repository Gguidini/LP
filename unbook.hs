-- main types
type UnBook = [Pessoa]
type Pessoa = (Nome,Mensagens,Amigos)
type Mensagens = [Mensagem]
type Mensagem = String
type Amigos = [Amigo]
type Amigo = String
type Nome = String

-- main functions signatures
adicionarPessoa :: UnBook -> Nome -> UnBook
removerPessoa :: UnBook -> Nome -> UnBook
adicionarAmizade :: UnBook -> Amigo -> Amigo -> UnBook
removerAmizade :: UnBook -> Amigo -> Amigo -> UnBook
pesquisarAmigos :: UnBook -> Amigo -> Amigos
sugerirAmigos :: UnBook -> Amigo -> Amigos
postarMensagem :: UnBook -> Amigo -> Mensagem -> UnBook

-- auxiliary functions
name :: Pessoa -> Nome
name (n, _, _) = n

msg :: Pessoa -> Mensagens
msg (_, m, _) = m
amgs :: Pessoa -> Amigos
amgs (_, _, a) = a

addFriend :: Pessoa -> Amigo -> Pessoa
addFriend (n, m, as) a = (n, m, a : as)




-- adiciona uma pessoa no book
adicionarPessoa book p = (p, [], []) : book

-- tira uma pessoa do book
removerPessoa book p = [x | x <- book, (name x) /= p]


-- adiciona v na lista de amizade de u e vice-versa
adicionarAmizade (x:xs) v u 
      | name x == v = (addFriend x u) : adicionarAmizade xs v u
      | name x == u = (addFriend x v) : adicionarAmizade xs v u
      | otherwise = x : adicionarAmizade xs v u
adicionarAmizade [] _ _ = []

-- remove v da lista de amizades de u e vice-versa
removerAmizade _ _ _ = error "Function not implemented!"

pesquisarAmigos (x:xs) a
      | name x == a = amgs x 
      | name x /= a = pesquisarAmigos xs a
pesquisarAmigos [] _ = []

-- sugere amigos para a pessoa p
sugerirAmigos _ _ = error "Function not inplemented!"
-- adiciona uma mensagem m na lista de mensagens de a
postarMensagem (x:xs) a m 
      | name x == a =   let   n = name x
                              mensagens = msg x
                              a = amgs x
                        in    (n, m : mensagens, a) : xs 
      | otherwise = x : postarMensagem xs a m
postarMensagem [] a _ = error ("Person " ++ a ++ " not found.")

