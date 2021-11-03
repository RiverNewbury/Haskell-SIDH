# Haskell-SIDH
A repo for a haskell implementation of SIDH and general resources for understanding SIDH

## Disclaimers

 1. I do not own either of the papers in Research and if you are the copyholder of them I will remove them if asked so just pop a message in issues
 2. This is not cryptographically secure please don't use this for any purpose except educational
 3. I don't have any formal qualifications to talk about any of this stuff (yet) so I may be wrong with the finer details

## Todo

 - More Comments and explanation in code
 - A thorough explanation of all ECC + SIDH in this file (building off https://github.com/RiverNewbury/Python-Elliptic-Curve-Cryptography)
 - Work out why code didn't work with the basepoints in wikipedia
 - explanation of how to run the code
 - links to more resources
 - understand then explain Nats and TypeNats

## Explanation of design choices

 - I chose Haskell for this project as I
  - Am most comfortable in this language as I've done other projects in it
  - I needed to do this quickly and work around Uni work so I needed a quick prototyping langauge
  - I think that for explanatory purposes Haskell is a very good 

## Links

 - [A video explaination of ECC (not me just thought it was a good series) (also if you're generally interested in cryptography his other videos are also good)](https://youtube.com/playlist?list=PLUQANcrG3720AzBFM0eDKEyYtMM8FdcNX)
 - [A beginners guide to SSID (recommended after you understand ECC as a jumping off point for SIDH)](https://eprint.iacr.org/2019/1321.pdf)
 - [SIKE Spec (recommended for a in depth review of how it is practically performed)](https://sike.org/files/SIDH-spec.pdf)
