<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="Version2.aspx.cs" Inherits="deklaracja.WebForm1" %>

<!DOCTYPE html>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title></title>
    <style>
        .container {
            display: flex;
            flex-direction: column;
            height: 100%;
        }
        .item {
            display: flex;
            flex-direction: row;
        }
        .task {
            display: flex;
            flex-direction: column;
            width: 100px;
        }
        #submit-button {
            width: 100px;
        }
    </style>
</head>
<body>
    <form id="form1" runat="server">
        <div class="container">
            <h1>Formularz zgłoszeniowy</h1>
            <div class="item">
                <asp:Label Text="Imię: " runat="server"/>
                <asp:TextBox runat="server" id="name"/> 
            </div>
            <div class="item">
                <asp:Label Text="Nazwisko: " runat="server" />
                <asp:TextBox runat="server" id="surname"/>
            </div>
            <div class="item">
                <asp:Label Text="Data: " runat="server" />
                <asp:TextBox runat="server" id="date"/>
            </div>
             <div class="item">
                 <asp:Label Text="Nazwa zajęć" runat="server" />
                 <asp:TextBox runat="server" id="name_of_class"/>
            </div>
            <div class="item">
                <asp:Label Text="Numer listy" runat="server" />
                <asp:TextBox runat="server" id="list_number"/>
            </div>
            <div class="task" id="tasks" runat="server">

            </div>
        </div>
        <asp:Button runat="server" ID="submit" Text="Zatwierdź" OnClick="Submit_Deklaracja" />
    </form>
</body>
</html>