# FIFA2017

Distributed soccer simulation. Erlang based.

## Project screenshot

![alt text](https://github.com/danpora/FIFA2017/blob/master/media/fifa2017_start_position.jpg)

![alt text](https://github.com/danpora/FIFA2017/blob/master/media/fifa2017_midfield.jpg)

### Live game demo
![alt text](https://github.com/danpora/FIFA2017/blob/master/media/goal2.gif)

## Getting Started

FIFA2017 is a distributed simulation application - it runs on several machines simultaneously. This repository is set to run five different machines (4 machies as game engines and one machine as a gui and controller).
In order to run this project on you local machine take the following steps:
1. clone/download FIFA2017 repository
2. open 5 terminals in the following directories: FIFA2017/n1, FIFA2017/n2, FIFA2017/n3, FIFA2017/n4 and FIFA2017/GServer.
3. in terminals n1, n2, n3, n4, run command: 
 `erl -setcookie cook -name n$@127.0.0.1` (replace '$' with n1,n2,n3,n4 respectivly)
4. in terminal GServer, run command:
 `erl -setcookie cook -name n_gfx@127.0.0.1`
5. compile all .erl files in each directory with: 
 `>>c(file_name).`
6. in terminals n1, n2, n3, n4 type: 
 `>>field_manager:start().`
7. in terminal GServer type:
 `>>fifa_gserver:start().`
8. in the pop up project window click 'Start'
9. Enjoy a good soccer game (:

### Prerequisites

You'll need an Erlang/OTP environment to run this project.

Follow steps at http://erlang.org/doc/installation_guide/INSTALL.html

## Authors

* **Dan Porat** 
* **Or Koren** 
