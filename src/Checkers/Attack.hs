module Checkers.Attack where
import           InternalIR.SimplePath
import           Symex.Symex.Symex

data PreAttack a = PreAttack { shouldExecute :: SimpleInstruction -> Symex a Bool }

data PostAttack a = PostAttack { postAttack :: SimpleInstruction -> Symex a () }

data Attack a = Attack { attack :: SimpleInstruction -> Symex a Bool }

