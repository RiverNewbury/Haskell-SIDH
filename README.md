# Haskell-SIDH
The goal of this Repo is to provide a novice to ECC the resources to build up their understanding to the point of understanding Supersingular Elliptic Curve Diffie Hellman



## Disclaimers <a name="Disclaimers"></a>

 1. I do not own either of the papers in the folder Research and if you are the copyholder of them I will remove them if asked so just pop a message in issues
 2. This is not cryptographically secure please don't use this for any purpose except educational
 3. I don't have any formal qualifications to talk about any of this stuff (yet) so I may be wrong with the finer details





## Todo <a name="Todo"></a>

 - More Comments and explanation in code
 - A thorough explanation of all ECC + SIDH in this file (building off https://github.com/RiverNewbury/Python-Elliptic-Curve-Cryptography)
 - Work out why code didn't work with the base points in Wikipedia
 - explanation of how to run the code
 - links to more resources
 - understand then explain Nats and TypeNats
 - Make abs return length and signum return unit vector
 - Make Points work with other curve types






## Explanation of design choices <a name="Explanation_of_design_choices"></a>

 - I chose Haskell for this project as I
   - Am most comfortable in this language as I've done other projects in it
   - I needed to do this quickly and work around Uni work so I needed a quick prototyping language
   - I think that for explanatory purposes Haskell is a very good





## Haskell Background <a name="BackgroundHaskell"></a>

I won't be explaining the basics of Haskell but [here's](#ReferencesHaskell) some good places to start.

- understand then explain Nats and TypeNats


## Cryptography Background <a name="BachkgroundCryptography"></a>

## Elliptic Curves Background <a name="BackgroundEllipticCurve"></a>

### What is an Elliptic Curve <a name="WhatisanEllipticCurve"></a>

An elliptic curve has formula
![\Large x=\frac{-b\pm\sqrt{b^2-4ac}}{2a}](https://latex.codecogs.com/svg.latex?\Large&space;x=\frac{-b\pm\sqrt{b^2-4ac}}{2a})

![](https://latex.codecogs.com/svg.latex?)

### Types

| Name              	| Formula                                                                        	| The unit of group 	| Addition Formula 	| Doubling Formula 	|
|-------------------	|--------------------------------------------------------------------------------	|-------------------	|------------------	|------------------	|
| Short Weierstrass 	| y<sup>2</sup> = x<sup>3</sup> + ax + b                                         	| Poif              	| [Here]()         	| [Here]()         	|
| Montgomery        	| ay<sup>2</sup> = x<sup>3</sup> + bx<sup>2</sup> + x                            	| Poif              	| [Here]()         	| [Here]()         	|
| Edwards           	| x<sup>2</sup> + y<sup>2</sup> = a<sup>2</sup>(1 + bx<sup>2</sup>y<sup>2</sup>) 	| (0, 1)            	| [Here]()         	| [Here]()         	|


| Name 	| Formula 	| The unit of group 	| Addition Formula 	| Doubling Formula 	|
|---	|---	|---	|---	|---	|
| Short Weierstrass 	| y<sup>2</sup> = x<sup>3</sup> + ax + b 	| Poif 	| ![g=\frac{y_2-y_1}{x_2-x_1}](https://latex.codecogs.com/svg.latex?g=\frac{y_2-y_1}{x_2-x_1})<br>![x_3=g^2-x_1-x_2](https://latex.codecogs.com/svg.latex?x_3=g^2-x_1-x_2)<br>![y_3=(2x_1+x_2)*g-g^3-y_1](https://latex.codecogs.com/svg.latex?y_3=(2x_1+x_2)*g-g^3-y_1) 	| [Here]() 	|
| Montgomery 	| ay<sup>2</sup> = x<sup>3</sup> + bx<sup>2</sup> + x 	| Poif 	| [Here]() 	| [Here]() 	|
| Edwards 	| x<sup>2</sup> + y<sup>2</sup> = a<sup>2</sup>(1 + bx<sup>2</sup>y<sup>2</sup>) 	| (0, 1) 	| [Here]() 	| [Here]() 	|

## Elliptic Curve Cryptography Background <a name="BackgroundECC"></a>

## SIDH/SIKE Background <a name="BackgroundSIDH"></a>





## References <a name="References"></a>

### Haskell <a name="ReferencesHaskell"></a>

### Cryptography <a name="ReferencesCryptography"></a>

### Elliptic Curve Cryptography <a name="ReferencesECC"></a>

 - [A video explanation of ECC (not mine just thought it was a good series) (also if you're generally interested in cryptography his other videos are also good)](https://youtube.com/playlist?list=PLUQANcrG3720AzBFM0eDKEyYtMM8FdcNX)

### SIDH <a name="ReferencesSIDH"></a>

 - [A beginners guide to SSID (recommended after you understand ECC as a jumping off point for SIDH)](https://eprint.iacr.org/2019/1321.pdf)
 - [SIKE Spec (recommended for a in depth review of how it is practically performed)](https://sike.org/files/SIDH-spec.pdf)
