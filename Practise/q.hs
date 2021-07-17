data Queue a = Nil | QueueNode a (Queue a)
data Stack a = Nil | StackNode a (Stack a)

push :: Stack a -> a -> Stack a
push Nil key = QueueNode key Nil
push s key = push 