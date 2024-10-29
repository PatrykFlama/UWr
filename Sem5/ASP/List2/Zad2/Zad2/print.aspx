<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="print.aspx.cs" Inherits="Zad2.print" %>

<!DOCTYPE html>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title></title>
</head>
<body>
    <form id="form1" runat="server">
        <div>
            <asp:GridView ID="GridView1" runat="server" AutoGenerateColumns="False">
    <Columns>
        <asp:BoundField DataField="Imie" HeaderText="Imię" />

        <asp:BoundField DataField="Data" HeaderText="Data" />

        <asp:BoundField DataField="Kurs" HeaderText="Kurs" />

        <asp:BoundField DataField="Lista" HeaderText="Lista" />

        <asp:BoundField DataField="Punkty1" HeaderText="Punkty 1" />
        <asp:BoundField DataField="Punkty2" HeaderText="Punkty 2" />
        <asp:BoundField DataField="Punkty3" HeaderText="Punkty 3" />
        <asp:BoundField DataField="Punkty4" HeaderText="Punkty 4" />
        <asp:BoundField DataField="Punkty5" HeaderText="Punkty 5" />
        <asp:BoundField DataField="Punkty6" HeaderText="Punkty 6" />
        <asp:BoundField DataField="Punkty7" HeaderText="Punkty 7" />
        <asp:BoundField DataField="Punkty8" HeaderText="Punkty 8" />
        <asp:BoundField DataField="Punkty9" HeaderText="Punkty 9" />
        <asp:BoundField DataField="Punkty10" HeaderText="Punkty 10" />

        <asp:BoundField DataField="Suma" HeaderText="Suma" />
    </Columns>
</asp:GridView>

        </div>
    </form>
</body>
</html>
