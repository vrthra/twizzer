all:
	ghc6 -idb:. --make db/Bot.lhs -o bot
	rm -f db/*.hi db/*.o
