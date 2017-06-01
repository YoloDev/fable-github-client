namespace YoloDev.GitHubClient.Core

type DateTime = System.DateTime
type Url = string
type Scope = string
type Fingerprint = string

type App = {
  url: Url
  name: string
  clientId: string
}

type Grant = {
  id: int
  url: Url
  app: App
  createdAt: DateTime
  updatedAt: DateTime
  scopes: Scope list
}

type Token =
| LastEight of lastEight: string * hash: string
| Token of token: string * lastEight: string * hash: string

module Token =
  let token = function
    | LastEight _     -> None
    | Token (t, _, _) -> Some t

  let lastEight = function
    | LastEight (s, _) -> s
    | Token (_, s, _)  -> s
  
  let hash = function
    | LastEight (_, h) -> h
    | Token (_, _, h)  -> h

type Note = {
  description: string
  url: Url option
}

type Authorization = {
  id: int
  url: Url
  scopes: Scope list
  token: Token
  app: App
  note: Note
  updatedAt: DateTime
  createdAt: DateTime
  fingerprint: Fingerprint
}