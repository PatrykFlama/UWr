namespace GameLogic
{
    public class DotsAndBoxesGame
    {
        public int Rows { get; }
        public int Columns { get; }
        public string[,] HorizontalEdges { get; }
        public string[,] VerticalEdges { get; }
        public string[,] Boxes { get; }
        public string CurrentPlayer { get; set; }
        public Dictionary<string, int> Scores { get; } = new();

        public DotsAndBoxesGame(int rows, int columns, List<string> players)
        {
            Rows = rows;
            Columns = columns;
            HorizontalEdges = new string[rows + 1, columns];
            VerticalEdges = new string[rows, columns + 1];
            Boxes = new string[rows, columns];
            CurrentPlayer = players[0];

            foreach(var player in players)
            {
                Scores[player] = 0;
            }
        }

        public (bool Valid, bool ExtraTurn) MakeMove(string player, bool isHorizontal, int row, int col)
        {
            if(player != CurrentPlayer) return (false, false);

            if(isHorizontal)
            {
                if(!string.IsNullOrEmpty(HorizontalEdges[row, col])) return (false, false);
                HorizontalEdges[row, col] = player;
            } else
            {
                if(!string.IsNullOrEmpty(VerticalEdges[row, col])) return (false, false);
                VerticalEdges[row, col] = player;
            }

            bool extraTurn = false;
            int completedBoxes = 0;

            if(isHorizontal && row > 0 && CheckBoxComplete(row - 1, col))
            {
                Boxes[row - 1, col] = player;
                Scores[player]++;
                completedBoxes++;
            }

            if(isHorizontal && row < Rows && CheckBoxComplete(row, col))
            {
                Boxes[row, col] = player;
                Scores[player]++;
                completedBoxes++;
            }

            if(!isHorizontal && col > 0 && CheckBoxComplete(row, col - 1))
            {
                Boxes[row, col - 1] = player;
                Scores[player]++;
                completedBoxes++;
            }

            if(!isHorizontal && col < Columns && CheckBoxComplete(row, col))
            {
                Boxes[row, col] = player;
                Scores[player]++;
                completedBoxes++;
            }

            extraTurn = completedBoxes > 0;

            if(!extraTurn)
            {
                var players = Scores.Keys.ToList();
                CurrentPlayer = players[(players.IndexOf(CurrentPlayer) + 1) % players.Count];
            }

            return (true, extraTurn);
        }

        private bool CheckBoxComplete(int row, int col)
        {
            return !string.IsNullOrEmpty(HorizontalEdges[row, col]) &&
                   !string.IsNullOrEmpty(HorizontalEdges[row + 1, col]) &&
                   !string.IsNullOrEmpty(VerticalEdges[row, col]) &&
                   !string.IsNullOrEmpty(VerticalEdges[row, col + 1]);
        }
    }
}
