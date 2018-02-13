#Create door, the 1 can be changed to the users choice 

pick<-function(x=1) {
  choice<-x
  if (choice>3 | choice<1) { choice<-1}
  class(choice)<-"Door"
  return(choice)
}

#choice is equal to pick(x), if the contestant wants to pick door x 
#if they pick something other than 1, 2, or 3, the function will defualt to door 1. 

#Initialized the playgame function 

playGame<-function(x) {
  UseMethod("playGame")
}

#assigning it to work for the class "door" 

playGame.Door<-function(x) {
  y<-sample(1:3, 1)
  print(y)
  z<-pick(y)
  text<-"Oh no! You recieved a goat!"
  if(z==x) { text<-"Hooray! You recieved a car!"}
  return(text)
}

#It works like so: 

Ian<-pick(2)
playGame(Ian)

#Now for S4: 

#First, I create a validity checker called checkdoor

checkDoor<-function(object) {
  errors<-character()
  p<-object@pick
  if(p!=1 & p!=2 & p!=3) {errors<-"You may only pick doors 1-3."}
  if(length(errors)==0) return(TRUE) else return(errors)
}

#Then the setclass to specify what constitutes a "Door"

setClass("Door", representation(pick="numeric"), validity=checkDoor)

#To test the condition, I put this in. This incorrect new door choice will throw an error

Incorrect<-new("Door", pick=4)

#This one is correct and will not throw an error 

Ian<-new("Door", pick=2)

#Creating the generic function

setGeneric("playGame", function(object="Door"){
  standardGeneric("playGame")
})

#specifying what the generic method "playGame" actually does

setMethod("playGame", "Door", function(object){
  y<-sample(1:3, 1)
  print(y)
  z<-object@pick
  text<-"Oh no! You recieved a goat!"
  if(y==z) { text<-"Hooray! You recieved a car!"}
  return(text)
})

#Play the game! 

playGame(Ian)
