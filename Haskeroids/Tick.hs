module Haskeroids.Tick where

class Tickable t where
    tick :: t -> t