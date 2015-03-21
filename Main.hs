import Gui.LambGui



main = do 
	mapM putStrLn  ["Welcome to LambDraw! :)",
					"Please open your Browser and enter the address specified in the next lines.",
					"(Its the one after binding to. For example: binding to [http://127.0.0.1:8023/])"]
	startGui