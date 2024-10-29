<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="DeclarationForm.aspx.cs" Inherits="Zad2.WebForm1" %>

<!DOCTYPE html>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title>Exercise Points Calculator</title>
</head>
<body>
    <form id="form1" runat="server">
        <div>
            <asp:PlaceHolder ID="phError" runat="server"></asp:PlaceHolder>
            <br />

            <h2>Enter Exercise Details</h2>

            <!-- name -->
            <label for="txtName">Name:</label>
            <asp:TextBox ID="txtName" runat="server"></asp:TextBox>
            <br /><br />

            <!-- date -->
            <label for="txtDate">Date:</label>
            <asp:TextBox ID="txtDate" runat="server" TextMode="Date"></asp:TextBox>
            <br /><br />

            <!-- list details -->
            <label for="txtCourseName">Course:</label>
            <asp:TextBox ID="txtCourseName" runat="server"></asp:TextBox>
            <br /><br />

            <label for="txtListNumber">List number:</label>
            <asp:TextBox ID="txtListNumber" runat="server" TextMode="Number"></asp:TextBox>
            <br /><br />

            <!-- points for each exercise -->
            <asp:PlaceHolder ID="phExercises" runat="server"></asp:PlaceHolder>
            <br />

            <!-- submit -->
            <asp:Button ID="btnCalculate" runat="server" Text="Calculate Total Points" AutoPostBack="true" OnClick="btnCalculate_Click" />
            <br /><br />

            <asp:Button ID="btnSubmit" runat="server" Text="Generate table" AutoPostBack="true" OnClick="btnSubmit_Click" />
            <br /><br />

            <!-- sum -->
            <label for="lblTotalPoints">Total Points:</label>
            <asp:Label ID="lblTotalPoints" runat="server" Text="0"></asp:Label>
        </div>
    </form>
</body>
</html>