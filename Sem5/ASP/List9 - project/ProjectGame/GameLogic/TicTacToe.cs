namespace GameLogic
{
    public class TicTacToeGame
    {
        public char[][] Board { get; set; }
        public List<string> Players { get; set; }
        public List<char> PlayerSymbols { get; set; }
        public string CurrentPlayer { get; set; }
        public char CurrentPlayerRole { get; set; }

        public TicTacToeGame()
        {
            Board = new char[3][];
            for(int i = 0; i < 3; i++)
            {
                Board[i] = new char[3]{' ', ' ', ' '};
            }
            Players = new List<string>();
            CurrentPlayer = null;
            PlayerSymbols = new List<char>{ 'X', 'O' };
        }

        public char? CheckWinner()
        {
            char[] players = { 'X', 'O' };
            foreach(var player in players)
            {
                for(int i = 0; i < 3; i++)
                {
                    if(Board[i][0] == player && Board[i][1] == player && Board[i][2] == player)
                        return player;
                    if(Board[0][i] == player && Board[1][i] == player && Board[2][i] == player)
                        return player;
                }
                if(Board[0][0] == player && Board[1][1] == player && Board[2][2] == player)
                    return player;
                if(Board[0][2] == player && Board[1][1] == player && Board[2][0] == player)
                    return player;
            }
            return null;
        }
    }
}
