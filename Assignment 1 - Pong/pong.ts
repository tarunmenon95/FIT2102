import { interval, fromEvent, from, zip, Scheduler, animationFrameScheduler, combineLatest, Observable } from 'rxjs'
import { AnimationFrameScheduler } from 'rxjs/internal/scheduler/AnimationFrameScheduler'
import { map, scan, filter,merge, flatMap, take, concat, takeUntil, withLatestFrom, mergeAll} from 'rxjs/operators'


//Programming Paradigms Assignment 1 
//Tarun Menon 29739861


/**
 * The function pong is our main function encapsulating our functionally designed pong game
 */
function pong() {
  
    //Global immutable constants used throughtout the program
    const WorldConstants = new class {
    readonly canvas = document.getElementById("canvas");
    readonly width =  800
    readonly height =  800
    readonly paddleHeight = 70
    readonly playerPaddleX = 780
    readonly enemyPaddleX = 10
    readonly enemyOffset = 15
    }


    //The following types allow us to strictly define the allowed scope of observable events

    //A Type defining the allowed keys for our gameEvent Observable 
    type allowedKeys = 'ArrowUp' | 'ArrowDown' | 'Enter'
    //A Type defining the allowed key events for the gameEvent Observable
    type allowedEvents = 'keydown' | 'keyup'
    //A type defining the allowed classes to be created by the gameEvent Observable
    type allowedClasses = Move | IncrementTime
   


    /**The following are classes which are created by our gameEvent observable upon the event
     * of specific key events aswell as incrementing time. These classes are what dictate changes in 
     * the state of the program
    */

    class Move {constructor(public readonly direction:number) {}}
    class IncrementTime {constructor(public readonly time:number) {}}
    class Restart {constructor(public readonly num:number) {}}


    /** Based on the FRP Asteroids example, gameEvent creates different Observable streams based on
     * different unique key events and maps these to creating our different classes defined above. Based
     * on these classes we create different states changing our view of the game.
     * 
     */

    const gameEvent = <V>(e:allowedEvents, k:allowedKeys, exec:()=>V)=>
      fromEvent<KeyboardEvent>(document,e)
          .pipe(
              filter(({repeat})=>!repeat),
              filter(({code})=>code === k),
              map(exec)),

    //The following are our different input streams which map to different classes        
    moveUp = gameEvent('keydown','ArrowUp',()=>new Move(-3)),
    stopUp = gameEvent('keyup','ArrowUp',()=>new Move(0)),
    moveDown = gameEvent('keydown','ArrowDown',()=>new Move(3)),
    stopDown = gameEvent('keyup','ArrowDown',()=>new Move(0)),
    restart = gameEvent('keydown','Enter',()=>new Restart(1))
    
    
    /**
     * The following code defines the different "types" that we have for different game mechanics throughout our program. 
     * We ensure that to mainain purity/immutability in our code, the types are defined as ReadOnly to ensure that it is impossible
     * to actually mutate our type values at any point during execution. Rather in functional style we instead create new states for the
     * types.
     */


    //The following type defines the constraints which a "Ball" must adhere too
    type Ball = Readonly<{
      ballX: number
      ballY: number
      ballXVelocity: number
      ballYVelocity: number
    }>

    //The following type defines the constraints which a "Paddle" must adhere too
    type Paddle = Readonly<{
    Y: number
    Direction: number
    }>


    //The "State" type defines the structure that our state must take throughout the entire program.
    type State = Readonly<{
    player: Paddle
    enemy: Paddle
    ball: Ball
    enemyScore: number
    playerScore: number
    gameOver: boolean
    }>
    

    //The initialState constant is an immutable data structure which houses the initial starting state of our game,
    //This structure is immutable and maintains our functional style of programming as it is never mutatated.
    const initialState: State = {
    player: {Y: 325, Direction: 0},
    enemy: {Y: 325, Direction: 0},
    ball: {ballX: 400, ballY: 400, ballXVelocity: 1, ballYVelocity: 0 },
    enemyScore: 0,
    playerScore: 0,
    gameOver: false
    }     
    

    /**
     * Below are all the core functions used throughout our program. All the functions were based on a functional programming style and as such rather than 
     * mutable variables they are based on consistent transformation and creation of states. The functions all take in the current state as parameter, do their 
     * required transformations and output an entirley new state. By doing so we ensure that all of our functions cause no side effects on outside variables, always 
     * return the same value given the same input and as such, all functions are pure.
     */



    /**
     * The function checkPaddleBounds is used to check whether the proposed direction of a paddle is deemed in bounds, if it is we
     * allow the move, else we do not.
     * This function takes in state as parameter and uses it to calculate a new Paddle object that is valid based on the constraints of the canvas.
     * As our object has no side effects and always returns the same value based on the same input, it is a pure function
     * 
     * @param s This is the state which is being passed in
     * @param y This is the current Y co-ordinate for the paddle
     * @param direction This is the direction the paddle is heading
     */
    function checkPaddleBounds(s:State,y:number, direction:number):Paddle {

      //Check whether going over the top of the canvas
      if (direction > 0) {
        return y + direction > WorldConstants.height - WorldConstants.paddleHeight ? {Y:y, Direction:direction} : {Y:y + direction, Direction:direction}}
      //Check whether going below the canvas  
      if (direction < 0) {    
        return y + direction < 0 ? {Y:y, Direction:direction} : {Y:y + direction, Direction:direction}}
      //Reset paddle positions if the ball has been scored  
      if (!checkBallBounds(s)){
          return {Y: initialState.player.Y, Direction:initialState.player.Direction}}
      //Otherwise return same paddle
      else{
        return {Y:y, Direction:direction}}
    }


    /**
     * The doCollision function is where we calculate the trajectory of the ball after a collision with a paddle has been detected. We calculate the angle 
     * based on where it collided with the paddle and correctly calculate a new "Ball" in which new X,Y velocities and positions have been calculated. This 
     * function is pure as it cases no side effects and always returns the same value given the same input.
     * 
     * @param s This is the state being passed in
     * @param paddle This is the paddle object we are calculating the collision for
     * @param dir This is the orientation of the ball post collision
     */
    function doCollision(s:State, paddle:Paddle, dir: number):Ball{
      
      //Calculate point of impact
      const fromTop = s.ball.ballY - paddle.Y
      //Calculate future trajectory
      const angle = Math.PI /4 * (fromTop/WorldConstants.paddleHeight) - Math.PI / 8
    
      //Calculate post impact y velocity
      const speedMx = Math.cos(angle) * 3 
      const yVelo = Math.sin(angle) * speedMx * 1.2
      
      //Create a new Ball with the updated positions and velocities
      return {ballYVelocity: yVelo , ballXVelocity: -s.ball.ballXVelocity, ballY: s.ball.ballY + yVelo, ballX: s.ball.ballX + s.ball.ballXVelocity + dir}
    }

    /**
     * The moveBall function takes in the current state as parameter and then runs calculations on the ball to check whats its next postion
     * should be. It checks for collisions with the walls,paddles and goals and based on these possibilites it creates a different Ball.
     * This function maintains purity like all others as we simply create a new state for our updateView rather than any modifications.
     * 
     * @param s This is the state being passed in
     */
    function moveBall(s:State):Ball{
    
        //Check for hitting the top wall
      return s.ball.ballY < 0 ? {ballXVelocity: s.ball.ballXVelocity, ballYVelocity: -s.ball.ballYVelocity, ballX: s.ball.ballX + s.ball.ballXVelocity, ballY: -(s.ball.ballY + s.ball.ballYVelocity)}
      : // Check for hitting the bottom wall
      s.ball.ballY > 800 ? {ballXVelocity: s.ball.ballXVelocity, ballYVelocity: -s.ball.ballYVelocity, ballX: s.ball.ballX + s.ball.ballXVelocity, ballY: s.ball.ballY - (2 * s.ball.ballYVelocity)}
      : // Check for player paddle collision
      s.ball.ballX >= WorldConstants.playerPaddleX && s.ball.ballX <= WorldConstants.playerPaddleX + 5 && s.ball.ballY >= s.player.Y && s.ball.ballY <= s.player.Y + WorldConstants.paddleHeight ? doCollision(s,s.player, -5)
      : // Check for enemey paddle collision
      s.ball.ballX >= WorldConstants.enemyPaddleX  && s.ball.ballX <= WorldConstants.enemyPaddleX + 5 && s.ball.ballY >= s.enemy.Y && s.ball.ballY<= s.enemy.Y + WorldConstants.paddleHeight ? doCollision(s,s.enemy, +5)
      : // Check for scored goal
      !checkBallBounds(s) ? {ballXVelocity: initialState.ball.ballXVelocity, ballX: initialState.ball.ballX, ballY: initialState.ball.ballY, ballYVelocity: initialState.ball.ballYVelocity} :
      //else keep moving in normal direction
      {ballXVelocity: s.ball.ballXVelocity, ballYVelocity: s.ball.ballYVelocity, ballX: s.ball.ballX + s.ball.ballXVelocity, ballY: s.ball.ballY + s.ball.ballYVelocity}
    }


    /**This function is used to for the enemy paddle to track and follow the ball. It uses a simple ternary statement to
     * check whether the ball is above or below the paddle, based on this we output a number which tells what direction we
     * should go to.
     * @param paddleY This is the y-coordinate of the paddle
     * @param ballY This is the y-coordinate of the ball
     */
    function trackBall(paddleY:number, ballY: number):number{
        return ballY > paddleY ? 1 : ballY < paddleY ? -1 : 0
    }


    /**
     * The function CheckBallBounds simply checks whether the ball is currently in bounds and outputs a boolean based on this.
     * @param s The state of the program
     */
    function checkBallBounds(s:State):boolean{
      return s.ball.ballX > 800 ? false : s.ball.ballX < 0 ? false: true
    }

    /**
     * The function checkEnemyScore checks whether the enemy has scored the ball.
     * Returns a number based on whether to increment score or not
     */
    function checkEnemyScore(s:State):number{  
      return checkBallBounds(s) ? 0 : s.ball.ballX > 800 ? 1 : 0
    }
    /**
     The function checkPlayerScore checks whether the player has scored the ball.
     * Returns a number based on whether to increment score or not
     */
    function checkPlayerScore(s:State):number {  
      return checkBallBounds(s) ? 0 : s.ball.ballX < 0 ? 1 : 0
    }

    /**
     * The function checkGameState takes in the current state of the program and checks whether either player has met 
     * the winning conditions.
     * @param s The state of the program
     */
    function checkGameState(s:State):boolean{
       return (s.enemyScore == 7|| s.playerScore == 7) ? true : false
    }
    
    /**
     * The selectState function based on the FRP Asteroids example is a functional styled function which takes in a state and class based on 
     * the gameEvent Observable. Depending on the input Observable event the function conducts different transformations. 
     * The concept of this function is to encapsulate all the different state transformations that can occur within our game and encapsulate
     * them within one function. All our state transformations/creations are done within selectState. 
     * This function is used in conjuction with our main game observable which uses scan to maintain an accumulated state of our game without using
     * any mutable variables or non-functional styles.
     * 
     * This function is causes no side effects and is pure
     * @param s The current accumulated state of the program
     * @param e The class being passed in based on the gameEvent observable
     */
    const selectState = (s:State, e:Move|IncrementTime)=>
        //If Move Event
        e instanceof Move ? {...s,
        player: checkPaddleBounds(s,s.player.Y,e.direction)
        }:
        //Else time is incremented
        {...s,
        enemyScore: s.enemyScore + checkEnemyScore(s),
        playerScore: s.playerScore + checkPlayerScore(s),
        player: checkPaddleBounds(s,s.player.Y,s.player.Direction),
        ball: moveBall(s),
        enemy: checkPaddleBounds(s,s.enemy.Y, trackBall(s.enemy.Y+WorldConstants.enemyPaddleX,s.ball.ballY)),
        gameOver: checkGameState(s)
        }
 
    /**
     * The updateView function is what allows us to "view" our game. It takes in the current state of the program and modify's the HTML objects.
     * By doing so we are transforming the objects in our game based on the accumulated state.
     * This contains side effects which are contained within the function as we are mutating the global HTML (This is required however so can't work around it)
     * @param state The state of our program
     */    
    function updateView(state:State): void {
        //Constants connecting to appropriate HTML Elements
        const paddle = document.getElementById("playerPaddle")!;
        const p2 = document.getElementById("enemyPaddle")!;
        const ball = document.getElementById("ball")
        const score = document.getElementById("score")
        const winner = document.getElementById("winner")
        const res = document.getElementById("res")
        //Modify HTML elements based on state
        paddle.setAttribute('transform', `translate(${WorldConstants.playerPaddleX},${state.player.Y})`)
        p2.setAttribute('transform', `translate(${WorldConstants.enemyPaddleX},${state.enemy.Y})`)
        ball.setAttribute('transform', `translate(${state.ball.ballX},${state.ball.ballY})`)
        score.innerHTML = 'Red Score: ' + state.enemyScore.toString() + "  Blue Score: " + state.playerScore.toString()

        //If the game is over do
        if(state.gameOver){
            game.unsubscribe()
            res.innerHTML ="Press Enter to Restart"
            state.enemyScore == 7 ? winner.innerHTML = "Red Player Wins" : winner.innerHTML = "Blue Player Wins" 
        }
      }

    /**
     * The function restartGame is in parallel with our restartObserver to restart the game when the game has ended.
     * It modifies HTML elements aswell, which can be considered as contained required side effects.
     */  
    const restartGame = () => {
        if (game.closed){ 
            const res = document.getElementById("res")
            const winner = document.getElementById("winner")
            winner.innerHTML = ''
            res.innerHTML =""
            restartObserver.unsubscribe()
            pong()
            }
    }

    //This observer runs on our restart gameEvent observable
    const restartObserver = restart.subscribe(restartGame)

    /**
     * This is the main game observable which merges all our gameEvent Observables into a single stream. Then calls scan
     * to accumulate our current state with the one based on the current event. We create an accumulated state starting with the
     * initial game state and with each event, we transform and accumulate a state which is then passed to updateView for "viewing"
     */
    const game = interval(0)
      .pipe(
        map(incremental=>new IncrementTime(incremental)),
        merge(moveUp,moveDown,stopUp,stopDown),
        scan(selectState, initialState))
      .subscribe(updateView);
  }
   

    

  // the following simply runs your pong function on window load.  Make sure to leave it in place.
  if (typeof window != 'undefined')
    window.onload = ()=>{
    pong();
    
    }
  
  

  