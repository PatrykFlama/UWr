using Xunit;
using Zad3;

namespace zad3Tests
{
    public class zad3Tests
    {
        [Fact]
        public void ComparisonComparer_ShouldSortCorrectly()
        {
            // Arrange
            List<int> numbers = new List<int> { 3, 1, 4, 1, 5 };
            Comparison<int> comparison = (x, y) => x.CompareTo(y);
            IComparer<int> comparer = new ComparisonComparer<int>(comparison);

            // Act
            numbers.Sort(comparer);

            // Assert
            Assert.Equal(new List<int> { 1, 1, 3, 4, 5 }, numbers);
        }

        [Fact]
        public void ComparerExtensions_ToComparison_ShouldSortCorrectly()
        {
            // Arrange
            List<int> numbers = new List<int> { 9, 7, 2, 8 };
            IComparer<int> comparer = Comparer<int>.Default;
            Comparison<int> comparison = comparer.ToComparison();

            // Act
            numbers.Sort(comparison);

            // Assert
            Assert.Equal(new List<int> { 2, 7, 8, 9 }, numbers);
        }
    }
}