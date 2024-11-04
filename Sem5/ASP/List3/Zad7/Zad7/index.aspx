<%@ Page Language="C#" AutoEventWireup="true" CodeBehind="index.aspx.cs" Inherits="Zad7.WebForm1" %>

<!DOCTYPE html>

<html xmlns="http://www.w3.org/1999/xhtml">
<head runat="server">
    <title></title>
</head>
<body>
    <form id="form1" runat="server">
        <div>
            <asp:label runat="server" ID="lblError"></asp:label>

            <h1>Prześlij plik</h1>
            <asp:FileUpload runat="server" ID="fileUpload" />
            <asp:Button runat="server" ID="btnUpload" Text="Prześlij" OnClick="btnUpload_Click" />
        </div>
    </form>
</body>
</html>
