using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace Zad2
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window, INotifyPropertyChanged
    {
        public event PropertyChangedEventHandler PropertyChanged;

        private bool _playerXTurn = true;
        private string[,] _board = new string[3, 3];
        private bool _gameFinished = false;
        private string _gameStatus;
        public string GameStatus
        {
            get { return _gameStatus; }
            set
            {
                if (_gameStatus != value)
                {
                    _gameStatus = value;
                    OnPropertyChanged();
                }
            }
        }

        protected virtual void OnPropertyChanged([CallerMemberName] string propertyName = null)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }




        public MainWindow()
        {
            InitializeComponent();
            DataContext = this;
            GameStatus = "X turn";
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            Button button = (Button)sender;
            int row = Grid.GetRow(button);
            int column = Grid.GetColumn(button);

            if (_board[row, column] == null && !_gameFinished)
            {
                if (_playerXTurn)
                {
                    button.Content = "X";
                    _board[row, column] = "X";
                    GameStatus = "O turn";
                }
                else
                {
                    button.Content = "O";
                    _board[row, column] = "O";
                    GameStatus = "X turn";
                }

                _playerXTurn = !_playerXTurn;
                CheckWin();
            }
        }

        private void CheckWin()
        {
            for (int i = 0; i < 3; i++)
            {
                if (_board[i, 0] != null && _board[i, 0] == _board[i, 1] && _board[i, 1] == _board[i, 2])
                {
                    GameStatus = $"{_board[i, 0]} wins!";
                    _gameFinished = true;
                    return;
                }

                if (_board[0, i] != null && _board[0, i] == _board[1, i] && _board[1, i] == _board[2, i])
                {
                    GameStatus = $"{_board[0, i]} wins!";
                    _gameFinished = true;
                    return;
                }
            }

            if (_board[0, 0] != null && _board[0, 0] == _board[1, 1] && _board[1, 1] == _board[2, 2])
            {
                GameStatus = $"{_board[0, 0]} wins!";
                _gameFinished = true;
                return;
            }

            if (_board[0, 2] != null && _board[0, 2] == _board[1, 1] && _board[1, 1] == _board[2, 0])
            {
                GameStatus = $"{_board[0, 2]} wins!";
                _gameFinished = true;
                return;
            }

            // check for a tie
            bool fullBoard = true;
            foreach (var cell in _board)
            {
                if (cell == null)
                {
                    fullBoard = false;
                    break;
                }
            }

            if (fullBoard)
            {
                GameStatus = "It's a tie!";
                _gameFinished = true;
            }
        }
    }
}