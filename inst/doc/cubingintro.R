### R code from vignette source 'cubingintro.Rnw'

###################################################
### code chunk number 1: cubingintro.Rnw:49-50
###################################################
options(width=70)


###################################################
### code chunk number 2: cubingintro.Rnw:53-56
###################################################
library(cubing)
hello.cube <- getCubieCube()
hello.cube


###################################################
### code chunk number 3: cubingintro.Rnw:61-64
###################################################
aCube <- getCubieCube("Superflip")
bCube <- getCubieCube("EasyCheckerboard") 
cCube <- getCubieCube("HenrysSnake")


###################################################
### code chunk number 4: sflip1
###################################################
plot(aCube)


###################################################
### code chunk number 5: cubingintro.Rnw:75-76
###################################################
plot(aCube)


###################################################
### code chunk number 6: cubingintro.Rnw:88-89
###################################################
getOption("cubing.colors")


###################################################
### code chunk number 7: cubingintro.Rnw:96-99
###################################################
mycol <- c("yellow", "dodgerblue", "red", 
           "ghostwhite", "limegreen", "orange")
options(cubing.colors = mycol)


###################################################
### code chunk number 8: cubingintro.Rnw:104-107
###################################################
jcol <- c("ghostwhite", "red", "limegreen", 
          "dodgerblue", "orange", "yellow")
options(cubing.colors = jcol)


###################################################
### code chunk number 9: cubingintro.Rnw:112-115
###################################################
acol <- c("black", "darkred", "darkgreen",
          "yellow", "orange", "purple")
options(cubing.colors = acol)


###################################################
### code chunk number 10: cubingintro.Rnw:120-123
###################################################
ocol <- c("ghostwhite", "red", "limegreen",
          "yellow", "orange", "dodgerblue")
options(cubing.colors = ocol)


###################################################
### code chunk number 11: cubingintro.Rnw:131-134 (eval = FALSE)
###################################################
## aCube <- randCube()
## plot(aCube)
## plot3D(aCube)


###################################################
### code chunk number 12: cubingintro.Rnw:143-144 (eval = FALSE)
###################################################
## aCube <- getMovesCube("RU'F2")


###################################################
### code chunk number 13: cubingintro.Rnw:151-153 (eval = FALSE)
###################################################
## rCube <- randCube()
## rCube <- rCube %v% aCube


###################################################
### code chunk number 14: cubingintro.Rnw:164-168 (eval = FALSE)
###################################################
## aCube <- getCubieCube("Anaconda") %v% getCubieCube("FourSpots")
## bCube <- getCubieCube("FourSpots") %v% getCubieCube("Anaconda")
## plot3D(aCube)
## plot3D(bCube)


###################################################
### code chunk number 15: cubingintro.Rnw:176-177
###################################################
rCube <- getMovesCube(randMoves(1, nm = 22))


###################################################
### code chunk number 16: cubescheme
###################################################
plot(getCubieCube(), numbers = TRUE, blank = TRUE)


###################################################
### code chunk number 17: cubingintro.Rnw:192-193
###################################################
plot(getCubieCube(), numbers = TRUE, blank = TRUE)


###################################################
### code chunk number 18: cubingintro.Rnw:203-206
###################################################
aCube <- getCubieCube("Wire")
bCube <- cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB
                    DDDDDDDDD LRRLLLRRL FFBBBBBFF")


###################################################
### code chunk number 19: cubingintro.Rnw:213-215
###################################################
cCube <- cubieCube("UUUUUUUU RLLRRLLR BBFFFFBB
                    DDDDDDDD LRRLLRRL FFBBBBFF")


###################################################
### code chunk number 20: cubingintro.Rnw:220-225 (eval = FALSE)
###################################################
## cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLLRRL FFBBBBBF")
## cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLLRRL FFBBBBBFU")
## cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLBLRRL FFBBLBBFF")
## cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLLRRF FFBBBBBFL")
## cubieCube("UUUUUUUUU RLLRRRLLR BBFFFFFBB DDDDDDDDD LRRLLBRRL FFBBBLBFF")


###################################################
### code chunk number 21: cubingintro.Rnw:259-262
###################################################
aCube <- randCube()
aCube
as.stickerCube(aCube)


###################################################
### code chunk number 22: cubingintro.Rnw:282-283
###################################################
sum(sapply(randCube(100, solvable = FALSE), is.solvable))


###################################################
### code chunk number 23: cubingintro.Rnw:288-290
###################################################
rCube <- randCube(1, solvable = FALSE)
is.solvable(rCube, split = TRUE)


###################################################
### code chunk number 24: cubingintro.Rnw:329-340
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
### code chunk number 25: cubingintro.Rnw:345-347
###################################################
result <- move(aCube, mv)
is.solved(result)


###################################################
### code chunk number 26: cubingintro.Rnw:361-362
###################################################
scramble(3, state = TRUE)


###################################################
### code chunk number 27: cubingintro.Rnw:375-377 (eval = FALSE)
###################################################
## res.seq <- move(aCube, mv, history = TRUE)
## plot(res.seq)


###################################################
### code chunk number 28: cubingintro.Rnw:380-381
###################################################
res.seq <- move(aCube, mv, history = TRUE)


###################################################
### code chunk number 29: cubingintro.Rnw:388-392 (eval = FALSE)
###################################################
## res.seq <- move(aCube, mv, history = TRUE)
## pdf("flick.pdf")
## plot(res.seq, title = "SeungBeom Cho\nWorld Record Solve\n4.59")
## dev.off()


###################################################
### code chunk number 30: cubingintro.Rnw:400-401 (eval = FALSE)
###################################################
## animate(aCube, mv)


###################################################
### code chunk number 31: cubingintro.Rnw:406-407 (eval = FALSE)
###################################################
## animate(aCube, mv, movie = "wrecord")


###################################################
### code chunk number 32: cubingintro.Rnw:429-430
###################################################
rCubes <- rotations(randCube())


###################################################
### code chunk number 33: cubingintro.Rnw:433-435 (eval = FALSE)
###################################################
## rCubes <- rotations(randCube())
## plot(rCubes)


###################################################
### code chunk number 34: cubingintro.Rnw:442-446 (eval = FALSE)
###################################################
## rCubes <- rotations(randCube())
## pdf("rotations.pdf")
## plot(rCubes)
## dev.off()


###################################################
### code chunk number 35: cubingintro.Rnw:453-460
###################################################
aCube <- getCubieCube("EasyCheckerboard")
bCube <- move(aCube, "x2")
all.equal(aCube, bCube)  # rotation equivalent ?
aCube == bCube  # recoloring equivalent ?  
identical(aCube, bCube)  # identical ?
identical(aCube, getMovesCube("U2D2R2L2F2B2"))  # identical ?
identical(aCube, invCube(aCube))  # self-inverse ?


###################################################
### code chunk number 36: cubingintro.Rnw:470-474
###################################################
aCube <- getCubieCube("BlackMamba")
solver(aCube, divide = TRUE)
tCube <- getCubieCube("EasyCheckerboard")
solver(aCube, tCube, collapse = "-")


###################################################
### code chunk number 37: cubingintro.Rnw:483-485 (eval = FALSE)
###################################################
## tp <- microbenchmark(solver(randCube()), times = 100000)
## round(quantile(tp$time/10^6, prob = c(0,.01,seq(.05,.95,.05),.99,1)))


###################################################
### code chunk number 38: cubingintro.Rnw:501-503
###################################################
solver(randCube(), type = "ZT")
solver(randCube(), type = "TF")


