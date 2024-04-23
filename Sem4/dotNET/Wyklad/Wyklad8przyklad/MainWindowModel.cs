using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;

namespace WpfApplication1
{
    public class MainWindowModel : INotifyPropertyChanged
    {
        private int _sliderValue;

        public int SliderValue
        {
            get => _sliderValue;
            set
            {
                _sliderValue = value;
                RaisePropertyChangedNew();
            }
        }

        public string Name
        {
            get; set;
        }

        public ICommand TheButtonClick
        {
            get
            {
                this.SliderValue = new Random().Next( 0, 100 );

                return new ButtonClickCommand( p =>
                {
                    MessageBox.Show($"Selected parent: {this.SelectedParent}, selected child: {this.SelectedChild}, name: {this.Name}, slider: {this.SliderValue}");
                });
            }
        }

        public IEnumerable<string> Parents
        {
            get
            {
                return new[]
                {
                    "foo",
                    "bar",
                    "qux"
                };
            }
        }

        private string _selectedParent;

        /// <summary>
        /// "Stary styl"
        /// </summary>
        private void RaisePropertyChanged( string propertyName )
        {
            if (PropertyChanged != null)
                PropertyChanged(this, new PropertyChangedEventArgs(propertyName));
        }

        /// <summary>
        /// "Nowy styl"
        /// </summary>
        /// <param name="name"></param>
        protected void RaisePropertyChangedNew( [CallerMemberName] string name = null )
        {
            PropertyChanged?.Invoke( this, new PropertyChangedEventArgs( name ) );
        }

        public event PropertyChangedEventHandler PropertyChanged;

        public string SelectedParent
        {
            get
            {
                return _selectedParent;
            }
            set
            {
                _selectedParent = value;
                RaisePropertyChanged("Children");
            }
        }

        public IEnumerable<string> Children
        {
            get
            {
                if (string.IsNullOrEmpty(this.SelectedParent))
                    yield break;

                for (int i = 0; i < 5; i++)
                    yield return string.Format("{0} {1}", this.SelectedParent, i);
            }
        }

        public string SelectedChild
        {
            get; set;
        }
    }

    public class ButtonClickCommand : ICommand
    {
        private Action<object> _action;
        public ButtonClickCommand( Action<object> action )
        {
            this._action = action;
        }

        public event EventHandler CanExecuteChanged;

        public bool CanExecute(object parameter)
        {
            return true;
        }

        public void Execute(object parameter)
        {
            this._action(parameter);            
        }
    }
}
