
sayHello :: String -> IO()
sayHello x = putStrLn("Hello, " ++ x ++ "!")

calcCircleSurface x = pi * (x * x)
