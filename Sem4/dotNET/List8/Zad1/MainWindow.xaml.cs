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

namespace Zad1
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }


        private void AcceptButton_Click(object sender, RoutedEventArgs e)
        {
            string name = textBoxName.Text;
            string address = textBoxSurname.Text;
            string studyDuration = GetComboBoxItemText(comboBoxOptions.SelectedItem);
            string studyType = (checkBoxDzienne.IsChecked == true ? "dzienne" : (checkBoxUzup.IsChecked == true ? "uzupełniające" : "brak"));

            string message = $"Nazwa: {name}\nAdres: {address}\nCzas trwania studiów: {studyDuration}\nTyp studiów: {studyType}";

            MessageBox.Show(message, "Informacja", MessageBoxButton.OK, MessageBoxImage.Information);
        }

        private string GetComboBoxItemText(object selectedItem)
        {
            if (selectedItem is ComboBoxItem comboBoxItem)
            {
                return comboBoxItem.Content.ToString(); 
            }

            return string.Empty;
        }


        private void CancelButton_Click(object sender, RoutedEventArgs e)
        {
            Close(); 
        }

        private void CheckBoxDzienne_Click(object sender, RoutedEventArgs e)
        {
            if (checkBoxDzienne.IsChecked == true)
            {
                checkBoxUzup.IsChecked = false;
            }
        }

        private void CheckBoxUzup_Click(object sender, RoutedEventArgs e)
        {
            if (checkBoxUzup.IsChecked == true)
            {
                checkBoxDzienne.IsChecked = false;
            }
        }

        private void checkBoxUzup_Click(object sender, RoutedEventArgs e)
        {

        }
    }
}