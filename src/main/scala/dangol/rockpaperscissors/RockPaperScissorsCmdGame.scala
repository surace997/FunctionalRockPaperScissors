package dangol.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{Paper, RPSHistoryBasedPlayer, RPSMove, Rock, Scissors}

import scala.io.StdIn.readLine

object RockPaperScissorsCmdGame {
  def main(args: Array[String]) = {
    greeting
    print("Enter 1 for live game play. Enter 2 for automatic tournament season: ")
    readLine() match {
      case "1" => liveGamePlay
      case "2" => handleTournamentSeason
      case _ => println("Response not understood.")
    }
    goodbye
  }
  private def greeting = println("Welcome to Rock-Paper-Scissors")
  private def goodbye = println("Thanks for playing Rock-Paper-Scissors")
  private def liveGamePlay = {
    handleOneTurn
    while ( {
      print("Play again? (true/false): ");
      readLine().toBoolean
    }) handleOneTurn
  }
  private def handleOneTurn = {
    print("Enter first move: ")
    val firstMove = readLine()
    print("Enter second move: ")
    val secondMove = readLine()
    val decision = DeciderMapper.beats(firstMove)(secondMove) match {
      case Some(value) => value
      case None => "invalid input given"
    }
    println(s"decision: ${decision}")
  }
  private def handleTournamentSeason = {
    val weights1 = Map[RPSMove, Double](Rock -> 0.75, Paper -> 0.13, Scissors -> 0.12)
    val weights2 = Map[RPSMove, Double](Rock -> 0.1, Paper -> 0.8, Scissors -> 0.1)
    val weights3 = Map[RPSMove, Double](Rock -> 0.05, Paper -> 0.05, Scissors -> 0.9)
    val weights4 = Map[RPSMove, Double](Rock -> 0.6, Paper -> 0.2, Scissors -> 0.2)
    val weights5 = Map[RPSMove, Double](Rock -> 0.5, Paper -> 0.25, Scissors -> 0.25)
    val players: List[RPSHistoryBasedPlayer] = List(
      new LastLosingMovePlayer("Last Losing Move Player"),
      new BiasedRandomMovePlayer(s"Biased Random Move Player: ${weights1}", weights1),
      new LastWinningMovePlayer("Last Winning Move Player"),
      new RandomMovePlayer("Random Move Player 1"),
      new BiasedRandomMovePlayer(s"Biased Random Move Player: ${weights2}", weights2),
      new MajorityWinningMovePlayer("Majority Winning Move Player"),
      new BiasedRandomMovePlayer(s"Biased Random Move Player: ${weights3}", weights3),
      new MajorityLosingMovePlayer("Majority Losing Move Player"),
      new BiasedRandomMovePlayer(s"Biased Random Move Player: ${weights4}", weights4),
      new RandomMovePlayer("Random Move Player 1"),
      new BiasedRandomMovePlayer(s"Biased Random Move Player: ${weights5}", weights5),
    )
    println("Enter number of rounds per match: ")
    val numRounds = readLine().toInt
    val tournaments = List(
      new IndividualMatchRoundRobinTournament(numRounds),
      new IndividualMatchRoundRobinTournament(numRounds),
      new IndividualMatchRoundRobinTournament(numRounds),
      new IndividualMatchRoundRobinTournament(numRounds)
    )
    resultScore(players,tournaments)
  }
  private def handleTournamentSeasonWithConfigFile = {
    val fname = "put file name here"
    val (players, tournaments) = getInitialValuesFromConfig(fname)
    resultScore(players,tournaments)

  }
  def resultScore(players: List[RPSHistoryBasedPlayer], tournaments: List[IndividualMatchRoundRobinTournament]) = {
    val seasonResults: Map[RPSHistoryBasedPlayer, Int] = MixedTournamentSeason.handleTournamentSeason(players)(tournaments)
    println(s"Summary of tournament season:\n${seasonResults}")
    for (r <- seasonResults)
      println(s"Player: ${r._1.playerInfo}   Points: ${r._2}")
  }

  def getInitialValuesFromConfig(fname){
    

  }
}
