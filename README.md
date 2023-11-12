# Asteroids on Steroids

This is an implementation for the classic arcade game "Asteroids" within the context of Functional Programming at  Utrecht Univerity. It was created for the course Functional Programming taught by Matthijs Vákár and Frank Staals.

## Getting it set up

Please unzip the folder and open a command window with Cabal or Stack enabled to run the following commands: 

Cabal:
```bash
    cd [BuildDirectory]
    Cabal init
    Cabal build
    Cabal run
```
Stack: 
```bash
    cd [BuildDirectory]
    Stack init
    Stack build
    Stack run
```
    
## Playing the game

To correctly pilot your very own spaceship you will need the following controls:

- W: Will move you foward by supplying rocket fuel to your thrusters
- A: Yaw left
- D: Yaw right
- LMB: Fire your laser rifle
- Escape: Will pause the game if you need a break.
- N: Will start a new game (and reset your score!)
