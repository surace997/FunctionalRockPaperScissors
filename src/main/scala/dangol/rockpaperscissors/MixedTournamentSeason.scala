package dangol.rockpaperscissors

import benjamingarrett.rockpaperscissorstools.{AWins, BWins, RPSHistoryBasedPlayer, RPSOutcome, RPSPointsSchema, RPSTournament, RPSTournamentSeason}

object MixedTournamentSeason extends RPSTournamentSeason {
  def handleTournamentSeason(players: List[RPSHistoryBasedPlayer])(tournaments: List[RPSTournament]): Map[RPSHistoryBasedPlayer, Int] = {
    val tournamentResults = tournaments.map(event => event.playTournament(players))
    val pointsPerEvent = tournamentResults.map(
      result => result.flatMap(
        r => Map(
          r._1._1 -> r._2.count(outcome => outcome == AWins),
          r._1._2 -> r._2.count(outcome => outcome == BWins)
        )
      )
    ).map(x => x.toList.sortWith((x, y) => x._2 > y._2)).map(ranking => applyPoints(ranking)).flatten
    pointsPerEvent.foldLeft(Map[RPSHistoryBasedPlayer, Int]())({
      case (ranking, pair) => updatePlayerRankings(pair._1, pair._2, ranking)
    })
  }
  private def applyPoints(ranking: List[(RPSHistoryBasedPlayer, Int)]) = {
    def points(level: Int, results: List[(RPSHistoryBasedPlayer, Int)]): List[(RPSHistoryBasedPlayer, Int)] =
      if (level > 4) List.empty
      else results.headOption match {
        case Some(tuple) => (tuple._1, RPSPointsSchema.pointsAwarded(level)) :: points(level + 1, results.tail)
        case None => List.empty
      }
    points(1, ranking)
  }
  private def updatePlayerRankings(player: RPSHistoryBasedPlayer, newPoints: Int, ranking: Map[RPSHistoryBasedPlayer,Int]) = {
    ranking.get(player) match {
      case Some(accumulatedPoints) => ranking.updated(player, accumulatedPoints + newPoints)
      case None => ranking.updated(player, newPoints)
    }
  }
  // was using this for debugging
  private def getRawPoints(tournamentResults: List[Map[(RPSHistoryBasedPlayer, RPSHistoryBasedPlayer), List[RPSOutcome]]]): List[(RPSHistoryBasedPlayer, Int)] =
    tournamentResults.map(
      result => result.flatMap(
        r => Map(
          r._1._1 -> r._2.count(outcome => outcome == AWins),
          r._1._2 -> r._2.count(outcome => outcome == BWins)
        )
      )
    ).map(x => x.toList.sortWith((x, y) => x._2 > y._2)).flatten
}
