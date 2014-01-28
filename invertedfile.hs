
buildFile [] sw docnum invF = invF
buildFile (x:xs) sw docnum invF
	|x == "*" = buildFile xs sw (docnum + 1) invF
	|(elem x (words sw)) = buildFile xs sw docnum invF
	|otherwise = buildFile xs sw docnum newinvF
		where
		newinvF = (addtoFile x docnum invF)

addtoFile x docnum [] = [(x, [docnum])]
addtoFile x docnum ((word,numlist):ts)
	|word == x && (elem docnum numlist) = ((word, numlist):ts)
	|word == x = ((word, docnum:numlist):ts)
	|otherwise = (word, numlist):(addtoFile x docnum ts)

buildQlist [] dw s inVer = inVer
buildQlist (q:qw) dw s inVer 
	|(elem q (words dw)) = buildQlist qw dw s newinVer
	|otherwise = buildQlist qw dw s (addStuff q [0] inVer)
			where
			newinVer = (addStuff q (findNum q (buildFile (words dw) s 1 [])) inVer)

addStuff q numlist [] = [(q,numlist)]
addStuff q numlist ((word,list):ts)
	|otherwise = (word, list):(addStuff q numlist ts)

findNum q [] = [0]
findNum q ((word,numlist):ts)
	|q == "*" = [0]
	|word == q = numlist
	|otherwise = findNum q ts 

findMuti [] printFile = printFile
findMuti ((word,list):qwx) printFile
	|word == "*" = findMuti qwx (addFile word [] printFile)
	|word /= "*" && (getNewh qwx) /= "*" = findMuti (returnS qwx) (addFile "m" (mutiF list qwx) printFile) 
	|(getNewh qwx) == "*" = findMuti qwx (addFile "" list  printFile) 
	|otherwise = (findMuti qwx printFile)

getNewh [] = []
getNewh ((word,numlist):ts)
	|otherwise = word

returnS [] = []
returnS ((word,list):ts)
	|(getNewh ts) == "*" = ts
	|otherwise = returnS ts

addFile word numlist [] = [(word,numlist)]
addFile word numlist ((wordl,list):ts)
	|word == "*" = (wordl,list):(addFile word [] ts)
	|otherwise = (wordl,list):(addFile word numlist ts)

mutiF numlist [] = [0]
mutiF numlist ((word,list):ts)
	|word == "*" = numlist
	|otherwise = mutiF (list ++ numlist) ts    

findM [] printStuff = printStuff
findM ((word,list):ts) printStuff
	|word == "*" = findM ts (addM "*" [] printStuff)
	|word == "m" = findM ts (addM "" (makeList list []) printStuff)
	|otherwise = findM ts (addM word list printStuff) 

addM word numlist [] = [(word,numlist)]
addM word numlist ((wordl,list):ts)
	|word == "*" = (wordl,list):(addM word [] ts)
	|otherwise = (wordl,list):(addM word numlist ts)

makeList [] docnum = docnum
makeList (x:xs) docnum
	|x == 0 = [0]
	|(elem x xs) && (elem x docnum) = makeList xs docnum
	|(elem x xs)  = makeList xs (x:docnum)
	|otherwise = makeList xs docnum

printall [] printS = printS
printall ((words,list):ts) 
	|words == "*" = printall ts (addString words)
	|otherwise = printall ts (addString (verT list))

verT [] = [String]
verT (x:xy)
	|otherwise = (showS x)++(verT xy) 


main = do
	d <-readFile "/home/buckeyb/PL/documents.txt"
	s <-readFile "/home/buckeyb/PL/stopwords.txt"
	q <-readFile "/home/buckeyb/PL/queries.txt"
	print (buildFile (words d) s 1 [])
	print ("\n")
--	writeFile "/home/buckeyb/PL/houtput.txt" " "
	print (buildQlist (words q) d s [])
	print ("")
	print (lines q)
	print ("")
	print (findM (findMuti (buildQlist (words q) d s []) []) [])









--	print (findNum "material" (buildFile(words d) s 1 []))
--	print (findNum (head (words q)) (buildFile (words d) s 1 [])) 
--	print (buildQq (words q) [])



--buildQq [] vertF = vertF
--buildQq (q:qw) vertF
--	|otherwise = buildQq qw (addVer q vertF)

--addVer q [] = [q]
--addVer q (qw:nqw)
--	|otherwise = (qw):(addVer q nqw)

--findMuti [] d s 
--	|q /= (words q) = buildQlist qw dw s (addStuff q (findMuti (words q) 
--	|q == "*" = ("",[]):(addStuff q numlist ts)
--	|word == "" = ((word,numlist):ts)

--type Tuple = (String, [Int])
--data BST = Nil | Node Tuple BST BST deriving (Eq, Ord, Show)

--tree::BST
--tree = Node ("meat",[1,2,3]) (Node ("cat",[3,4,5]) Nil Nil) (Node ("sheep",[6,4,5] Nil Nil))

--returnStr::Tuple -> String
--returnStr = (x,y) = x

--returnlist :: Tuple -> [Int]
--returnlist (x,y) = y

--insert Tuple Nil = Node Tuple Nil Nil
--insert Tuple (Node Tup a b)
--	|(returnStr Tuple) < (returnStr Tup) = Node Tup (insert Tuple a) b
--	|(returnStr Tuple) > (returnStr Tup) = Node Tup a (insert Tuple b)
--	|otherwise = Node Tup a b 





--buildqFile [][] docnum qFile = qFile
--buildqFile ((docw,numlist):ts) (q:qw) docnum qFile
--	|q == buildqFile ts q docnum (addqFile word numlist docnum)
--	|otherwise = buildqFile ts q docnum (addqFile word [0] docnum)

--addqFile querie [] docnum = [(querie, [docnum])]
--addqFile querie numlist docnum
--	|otherwise = (querie,numlist):(addqFile querie numlist docnum)

--loopDoc [] [] q = Bool
--loopDoc (words:doc) (numlist:num) q
--	|q == word = True
--	|q \= word = loopDoc doc num q
--	|otherwise = False


--rDocword [] lDoc = lDoc
--rDocword ((word,numlist):ts) lDoc
--	|otherwise = rDocword ts (addDl word lDoc)

--addDl word [] = [word]
--addD1 word (nword:x)
--	|otherwise = word:(addDl word x)

--rNumlist [] docnum nDoc = nDoc
--rNumlist ((word,numlist):ts) docnum nDoc
--	|otherwise = rNumlist ts docnum (addNl numlist nDoc)

--addNl [] docnum [] = [[docnum]]
--addNl (numlist:num) docnum (numl:nums)
--	|otherwise = numlist:(addNl num docnum nums)

--sStuff fileT [] fileQ = fileQ
--sStuff fileT (q:qw) fileQ
--	|otherwise = sStuff fileT qw (addStuff fileT q [0] fileQ)

--addStuff [] q docnum [] = [(q, [docnum])]
--addStuff ((word,numlist):ts) q docnum ((qword,qnumlist):qs)
--	|ts == [(null,[])] = (q,docnum):(addStuff qs q docnum)
--	|q == word = (q,numlist):(addStuff qs q docnum)
--	|otherwise = ((qword,qnumlist):qs)