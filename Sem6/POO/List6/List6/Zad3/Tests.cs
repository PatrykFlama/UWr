using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;
using Zad3;

namespace Zad3Tests
{
    public class zad3Tests
    {

        [Fact]
        public void DepthTreeVisitor_CalculatesCorrectDepth()
        {
            Tree root = new TreeNode()
            {
                Left = new TreeNode()
                {
                    Left = new TreeLeaf() { Value = 1 },
                    Right = new TreeLeaf() { Value = 2 },
                },
                Right = new TreeLeaf() { Value = 3 }
            };

            var depthVisitor = new DepthTreeVisitor();

            depthVisitor.Visit(root);

            Assert.Equal(3, depthVisitor.MaxDepth);
        }

        [Fact]
        public void DepthTreeVisitor_ReturnsZeroForEmptyTree()
        {
            Tree root = null;

            var depthVisitor = new DepthTreeVisitor();

            depthVisitor.Visit(root);

            Assert.Equal(0, depthVisitor.MaxDepth);
        }


        [Fact]
        public void DepthTreeVisitor_CalculatesCorrectDepthWithSingleLeaf()
        {
            Tree root = new TreeLeaf() { Value = 5 };

            var depthVisitor = new DepthTreeVisitor();

            depthVisitor.Visit(root);

            Assert.Equal(1, depthVisitor.MaxDepth);
        }
    }

}
