<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="DeclarationForm.aspx.cs" Inherits="Zad2.WebForm1" %>

<!DOCTYPE html>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title>Exercise Points Calculator</title>
</head>
<body>
    <form id="form1" runat="server">
        <div>
            <h2>Enter Exercise Details</h2>

            <!-- name -->
            <label for="txtName">Name:</label>
            <asp:TextBox ID="txtName" runat="server"></asp:TextBox>
            <br /><br />

            <!-- date -->
            <label for="txtDate">Date:</label>
            <asp:TextBox ID="txtDate" runat="server" TextMode="Date"></asp:TextBox>
            <br /><br />

            <!-- number of exercises -->
            <label for="numExercises">Number of Exercises:</label>
            <asp:TextBox ID="numExercises" runat="server" Text = "1" TextMode="Number" AutoPostBack="true" OnTextChanged="numExercises_SelectedIndexChanged"></asp:TextBox>
            <br /><br />

            <!-- points for each exercise -->
            <!-- <asp:PlaceHolder ID="phExercises" runat="server"></asp:PlaceHolder>
            <!-- <br /> -->

            <div id="exercises" runat="server">

            </div>

            <!-- submit -->
            <asp:Button ID="btnCalculate" runat="server" Text="Calculate Total Points" AutoPostBack="true" OnClick="btnCalculate_Click" />
            <br /><br />

            <!-- sum -->
            <label for="lblTotalPoints">Total Points:</label>
            <asp:Label ID="lblTotalPoints" runat="server" Text="0"></asp:Label>
        </div>
    </form>
</body>
</html>