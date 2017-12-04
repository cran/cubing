### R code from vignette source 'cubingintro.Rnw'

###################################################
### code chunk number 1: cubingintro.Rnw:49-52
###################################################
library(cubing)
hello.cube <- getCubieCube()
hello.cube


###################################################
### code chunk number 2: cubingintro.Rnw:57-60
###################################################
aCube <- getCubieCube("superflip")
bCube <- getCubieCube("easychecker") 
cCube <- getCubieCube("henrysnake")


###################################################
### code chunk number 3: sflip1
###################################################
plot(aCube)


###################################################
### code chunk number 4: cubingintro.Rnw:71-72
###################################################
plot(aCube)


###################################################
### code chunk number 5: cubingintro.Rnw:84-85
###################################################
getOption("cubing.colors")


###################################################
### code chunk number 6: cubingintro.Rnw:92-95
###################################################
mycol <- c("yellow", "dodgerblue", "red", 
           "ghostwhite", "limegreen", "orange")
options(cubing.colors = mycol)


###################################################
### code chunk number 7: cubingintro.Rnw:100-103
###################################################
jcol <- c("ghostwhite", "red", "limegreen", 
          "dodgerblue", "orange", "yellow")
options(cubing.colors = jcol)


###################################################
### code chunk number 8: cubingintro.Rnw:108-111
###################################################
acol <- c("black", "darkred", "darkgreen",
          "yellow", "orange", "purple")
options(cubing.colors = acol)


###################################################
### code chunk number 9: cubingintro.Rnw:116-119
###################################################
ocol <- c("ghostwhite", "red", "limegreen",
          "yellow", "orange", "dodgerblue")
options(cubing.colors = ocol)


###################################################
### code chunk number 10: cubingintro.Rnw:127-130 (eval = FALSE)
###################################################
## aCube <- randCube()
## plot(aCube)
## plot3d(aCube)


###################################################
### code chunk number 11: cubingintro.Rnw:139-140 (eval = FALSE)
###################################################
## aCube <- getMovesCube("RU'F2")


###################################################
### code chunk number 12: cubingintro.Rnw:147-149 (eval = FALSE)
###################################################
## rCube <- randCube()
## rCube <- rCube %v% aCube


###################################################
### code chunk number 13: cubingintro.Rnw:160-164 (eval = FALSE)
###################################################
## aCube <- getCubieCube("anaconda") %v% getCubieCube("fourspot")
## bCube <- getCubieCube("fourspot") %v% getCubieCube("anaconda")
## plot3d(aCube)
## plot3d(bCube)


###################################################
### code chunk number 14: cubingintro.Rnw:172-173
###################################################
rCube <- getMovesCube(randMoves(1, nm = 22))


###################################################
### code chunk number 15: cubescheme
###################################################
plot(getCubieCube(), numbers = TRUE, blank = TRUE)


###################################################
### code chunk number 16: cubingintro.Rnw:188-189
###################################################
plot(getCubieCube(), numbers = TRUE, blank = TRUE)


###################################################
### code chunk number 17: cubingintro.Rnw:199-202
###################################################
aCube <- getCubieCube("wire")
bCube <- cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB
                    DDDDDDDDD LRRLLLRRL FFBBBBBFF")


###################################################
### code chunk number 18: cubingintro.Rnw:209-211
###################################################
cCube <- cubieCube("UUUUUUUU RLLRRLLR BBFFFFBB
                    DDDDDDDD LRRLLRRL FFBBBBFF")


###################################################
### code chunk number 19: cubingintro.Rnw:216-221 (eval = FALSE)
###################################################
## cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLLRRL FFBBBBBF")
## cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLLRRL FFBBBBBFU")
## cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLBLRRL FFBBLBBFF")
## cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLLRRF FFBBBBBFL")
## cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLBRRL FFBBBLBFF")


###################################################
### code chunk number 20: cubingintro.Rnw:224-229
###################################################
try(cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLLRRL FFBBBBBF"), outFile = stdout())
try(cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLLRRL FFBBBBBFU"), outFile = stdout())
try(cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLBLRRL FFBBLBBFF"), outFile = stdout())
try(cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLLRRF FFBBBBBFL"), outFile = stdout())
try(cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLBRRL FFBBBLBFF"), outFile = stdout())


###################################################
### code chunk number 21: cubingintro.Rnw:244-247
###################################################
aCube <- randCube()
aCube
as.stickerCube(aCube)


###################################################
### code chunk number 22: cubingintro.Rnw:267-268
###################################################
sum(sapply(randCube(100, solvable = FALSE), is.solvable))


###################################################
### code chunk number 23: cubingintro.Rnw:273-275
###################################################
rCube <- randCube(1, solvable = FALSE)
is.solvable(rCube, split = TRUE)


###################################################
### code chunk number 24: cubingintro.Rnw:314-325
###################################################
tcon <- textConnection("D2 F2 U F2 D R2 D B L' B R U L R U L2 F L' U'" ) 
aCube <- getMovesCube(scan(tcon, what = character())) 
close(tcon)
tcon <- textConnection("x2 // inspection
D' R' L2' U' F U' F' D' U' U' R' // XXcross
y' R' U' R // 3rd pair
y' R U' R' U' R U R' // 4th pair
U' R' U' F' U F R // OLL(CP)
U' // AUF") 
mv <- scan(tcon, what = character(), comment.char = "/") 
close(tcon)


###################################################
### code chunk number 25: cubingintro.Rnw:330-332
###################################################
result <- move(aCube, mv)
is.solved(result)


###################################################
### code chunk number 26: cubingintro.Rnw:346-347
###################################################
scramble(3, state = TRUE)


###################################################
### code chunk number 27: cubingintro.Rnw:360-362 (eval = FALSE)
###################################################
## res.seq <- move(aCube, mv, history = TRUE)
## plot(res.seq)


###################################################
### code chunk number 28: cubingintro.Rnw:365-366
###################################################
res.seq <- move(aCube, mv, history = TRUE)


###################################################
### code chunk number 29: cubingintro.Rnw:373-377 (eval = FALSE)
###################################################
## res.seq <- move(aCube, mv, history = TRUE)
## pdf("flick.pdf")
## plot(res.seq, title = "SeungBeom Cho\nWorld Record Solve\n4.59")
## dev.off()


###################################################
### code chunk number 30: cubingintro.Rnw:385-386 (eval = FALSE)
###################################################
## animate(aCube, mv)


###################################################
### code chunk number 31: cubingintro.Rnw:391-392 (eval = FALSE)
###################################################
## animate(aCube, mv, movie = "wrecord")


###################################################
### code chunk number 32: cubingintro.Rnw:414-415
###################################################
rCubes <- rotations(randCube())


###################################################
### code chunk number 33: cubingintro.Rnw:418-420 (eval = FALSE)
###################################################
## rCubes <- rotations(randCube())
## plot(rCubes)


###################################################
### code chunk number 34: cubingintro.Rnw:427-431 (eval = FALSE)
###################################################
## rCubes <- rotations(randCube())
## pdf("rotations.pdf")
## plot(rCubes)
## dev.off()


###################################################
### code chunk number 35: cubingintro.Rnw:438-444
###################################################
aCube <- getCubieCube("easychecker")
bCube <- move(aCube, "x2")
all.equal(aCube, bCube)  # rotation equivalent ?
aCube == bCube  # recoloring equivalent ?  
identical(aCube, bCube)  # identical ?
identical(aCube, getMovesCube("U2D2R2L2F2B2"))  # identical ?


###################################################
### code chunk number 36: cubingintro.Rnw:454-458
###################################################
aCube <- getCubieCube("bmamba")
solver(aCube, divide = TRUE)
tCube <- getCubieCube("easychecker")
solver(aCube, tCube, collapse = "-")


###################################################
### code chunk number 37: cubingintro.Rnw:467-469 (eval = FALSE)
###################################################
## aCube <- getCubieCube("superflip")
## solver(aCube, maxMoves = 20)


###################################################
### code chunk number 38: cubingintro.Rnw:480-482
###################################################
solver(randCube(), type = "ZT")
solver(randCube(), type = "TF")


